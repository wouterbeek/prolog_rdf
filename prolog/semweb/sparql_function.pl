:- module(
  sparql_function,
  [
    '_<_'/2,                   % +A, +B
    '_>_'/2,                   % +A, +B
    'fn:concat'/2,             % +Literals, -Result
    'fn:matches'/2,            % +Literal, +Pattern
    'fn:matches'/3,            % +Literal, +Pattern, +Flags
    'fn:substring'/3,          % +Source, +Start, -Var
    'fn:year-from-dateTime'/2, % +Arg, -Year
    isBlank/1,                 % +Term
    isIri/1,                   % +Term
    isLiteral/1,               % +Term
    isUri/1,                   % +Term
    'STR'/2,                   % +Term, -A
    'STRDT'/3                  % +Lex, +D, -Literal
  ]
).

/** <module> SPARQL: Functions

op:numeric-equal(fn:compare(X,Y),1) → compare(>,X,Y)
op:numeric-equal(fn:compare(X,Y),0) → compare(=,X,Y)
op:numeric-equal(fn:compare(X,Y),-1) → compare(<,X,Y)

@author Wouter Beek
@version 2017/05-2017/10
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(sgml)).

:- dynamic
    pattern_cache/3.

:- rdf_meta
   '_<_'(o, o),
   '_>_'(o, o),
    dt_year(+, r, -),
   'fn:concat'(t, o),
   'fn:matches'(o, o),
   'fn:matches'(o, o, o),
   'fn:substring'(o, o, o),
   'fn:year-from-dateTime'(o, o),
   'STR'(o, ?),
   'STRDT'(+, r, o).





%! '_<_'(+A, +B) is semidet.

'_<_'(literal(type(D1,Lex1)), literal(type(D2,Lex2))) :-
  rdf11:xsd_date_time_type(D1),
  rdf11:xsd_date_time_type(D2), !,
  xsd_time_string(Val1, D1, Lex1),
  xsd_time_string(Val2, D2, Lex2),
  'op:dateTime-less-than'(Val1, Val2).
'_<_'(literal(type(xsd:boolean,Lex1)), literal(type(xsd:boolean,Lex2))) :- !,
  rdf11:in_boolean(Lex1, Bool1),
  rdf11:in_boolean(Lex2, Bool2),
  'op:boolean-less-than'(Bool1, Bool2).
'_<_'(literal(type(xsd:string,Lex1)), literal(type(xsd:string,Lex2))) :- !,
  compare(<, Lex1, Lex2).
'_<_'(literal(type(D1,Lex1)), literal(type(D2,Lex2))) :-
  rdf11:xsd_numerical(D1, Domain1, _Type1),
  rdf11:xsd_numerical(D2, Domain2, _Type2), !,
  rdf11:xsd_number_string(N1, Lex1),
  rdf11:xsd_number_string(N2, Lex2),
  is_of_type(Domain1, N1),
  is_of_type(Domain2, N2),
  'op:numeric-less-than'(N1, N2).
'_<_'(A, B) :-
  compare(<, A, B).



%! '_>_'(+A, +B) is semidet.

'_>_'(A^^xsd:dateTime, B^^xsd:dateTime) :- !,
  'op:dateTime-greater-than'(A, B).
'_>_'(A^^xsd:boolean, B^^xsd:boolean) :- !,
  'op:boolean-greater-than'(A, B).
'_>_'(A^^xsd:string, B^^xsd:string) :-
  compare(>, A, B).
'_>_'(A^^D1, B^^D2) :-
  rdf11:xsd_numerical(D1, _, _),
  rdf11:xsd_numerical(D2, _, _), !,
  'op:numeric-greater-than'(A, B).
'_>_'(A, B) :-
  compare(>, A, B).



%! 'fn:compare'(+A, +B)
%
% § 7.3.2 fn:compare
%
% ```
% fn:compare($comparand1 as xs:string?,
%            $comparand2 as xs:string?) as xs:integer?
% ```
%
% fn:compare($comparand1 as xs:string?,  
%            $comparand2 as xs:string?,  
%            $collation  as xs:string) as xs:integer?  
% ```
%
% Summary: Returns -1, 0, or 1, depending on whether the value of the
% $comparand1 is respectively less than, equal to, or greater than the
% value of $comparand2, according to the rules of the collation that
% is used.
%
% The collation used by the invocation of this function is determined
% according to the rules in 7.3.1 Collations.
%
% If either argument is the empty sequence, the result is the empty
% sequence.
%
% This function, invoked with the first signature, backs up the "eq",
% "ne", "gt", "lt", "le" and "ge" operators on string values.

'fn:compare'(A, B, Order) :-
  compare(Order, A, B).



%! 'fn:concat'(+Literals:list(rdf_literal), -Literal:rdf_literal) is det.

'fn:concat'(Literals, Literal) :-
  maplist(rdf_literal, Literals, Ds, LTags, Lexs),
  atomic_list_concat(Lexs, Lex),
  rdf_create_string_literal(Literal, Ds, LTags, Lex).



%! 'fn:matches'(+Literal:compound, +Pattern:compound) is semidet.
%! 'fn:matches'(+Literal:compound, +Pattern:compound,
%!              +Flags:compound) is semidet.

'fn:matches'(Literal, literal(Pattern)) :-
  'fn:matches'(
    Literal,
    literal(Pattern),
    literal('')
  ).


'fn:matches'(Literal, literal(Pattern), literal(Flags)) :-
  rdf_literal(Literal, _, _, String),
  re_match(Pattern/Flags, String).



%! 'fn:substring'(+Source:compound, +Start:compound, -String:compound) is det.

'fn:substring'(
  Literal1,
  literal(type(xsd:integer,Start)),
  Literal2
) :-
  rdf_literal(Literal1, D, LTag, Lex1),
  atom_codes(Lex1, Codes1),
  length(Prefix, Start),
  append(Prefix, Codes2, Codes1),
  atom_codes(Lex2, Codes2),
  rdf_create_string_literal(Literal2, D, LTag, Lex2).



%! 'fn:year-from-dateTime'(+Arg, -Year) is det.
%
% § 10.5.7 fn:year-from-dateTime
%
% ```
% fn:year-from-dateTime($arg as xs:dateTime?) as xs:integer?
% ```
%
% Summary: Returns an xs:integer representing the year component in
% the localized value of $arg.  The result may be negative.
%
% If $arg is the empty sequence, returns the empty sequence.
%
% § 10.5.7.1 Examples
%
%  * fn:year-from-dateTime(xs:dateTime("1999-05-31T13:20:00-05:00")) returns 1999
%
%  * fn:year-from-dateTime(xs:dateTime("1999-05-31T21:30:00-05:00")) returns 1999
%
%  * fn:year-from-dateTime(xs:dateTime("1999-12-31T19:20:00")) returns 1999
%
%  * fn:year-from-dateTime(xs:dateTime("1999-12-31T24:00:00")) returns 2000

'fn:year-from-dateTime'(
  literal(type(D,Lex1)),
  literal(type(xsd:integer,Lex2))
) :-
  xsd_time_string(Date, D, Lex1),
  dt_year(Date, D, Y),
  rdf11:in_number(integer, integer, xsd:integer, Y, Lex2).

dt_year(date(Y,_,_), xsd:date, Y).
dt_year(date_time(Y,_,_,_,_,_), xsd:dateTime, Y).
dt_year(date_time(Y,_,_,_,_,_,_), xsd:dateTime, Y).
dt_year(year_month(Y,_), xsd:gYearMonth, Y).
dt_year(Y, xsd:gYear, Y).



%! isBlank(+Term) is det.

isBlank(Term) :-
  rdf_is_bnode(Term).



%! isIri(+Term) is det.

isIri(Term) :-
  rdf_is_iri(Term).



%! isLiteral(+Term) is det.

isLiteral(Term) :-
  Term = literal(X),
  ground(X).



%! isUri(+Term) is det.

isUri(Term) :-
  isIri(Term).



%! 'op:boolean-greater-than'(+A, +B) is semidet.
%
% § 9.2.3 op:boolean-greater-than
%
% ```
% op:boolean-greater-than($arg1 as xs:boolean,
%                         $arg2 as xs:boolean) as xs:boolean
% ```
%
% Summary: Returns true if $arg1 is true and $arg2 is
% false. Otherwise, returns false.
%
% This function backs up the "gt" and "le" operators on xs:boolean
% values.

'op:boolean-greater-than'(true, false).



%! 'op:boolean-less-than'(+A, +B) is semidet.
%
% § 9.2.2 op:boolean-less-than
%
% ```
% op:boolean-less-than($arg1 as xs:boolean, $arg2 as xs:boolean) as xs:boolean
% ```
%
% Summary: Returns true if $arg1 is false and $arg2 is
% true. Otherwise, returns false.
%
% This function backs up the "lt" and "ge" operators on xs:boolean
% values.

'op:boolean-less-than'(false, true).



%! 'op:dateTime-greater-than'(+A, +B) is semidet.
%
% § 10.4.8 op:dateTime-greater-than
%
% ```
% op:dateTime-greater-than(  $arg1 as xs:dateTime,  
%   $arg2 as xs:dateTime) as xs:boolean  
% ```
%
% Summary: Returns true if and only if the value of $arg1 is greater
% than the value of $arg2 according to the algorithm defined in
% section 3.2.7.4 of [XML Schema Part 2: Datatypes Second Edition]
% "Order relation on dateTime" for xs:dateTime values with
% timezones.  Returns false otherwise.
%
% This function backs up the "gt" and "ge" operators on xs:dateTime
% values.
%
% The ordering between two dateTimes P and Q is defined by the
% following algorithm:
%
% A. Normalize P and Q. That is, if there is a timezone present, but
%    it is not Z, convert it to Z using the addition operation defined
%    in Adding durations to dateTimes (§E).
%
%    Thus 2000-03-04T23:00:00+03:00 normalizes to 2000-03-04T20:00:00Z
%
% B. If P and Q either both have a time zone or both do not have a
%    time zone, compare P and Q field by field from the year field
%    down to the second field, and return a result as soon as it can
%    be determined.  That is:
%
%    For each i in {year, month, day, hour, minute, second}
%        If P[i] and Q[i] are both not specified, continue to the next i
%        If P[i] is not specified and Q[i] is, or vice versa, stop and return P <> Q
%        If P[i] < Q[i], stop and return P < Q
%        If P[i] > Q[i], stop and return P > Q
%    Stop and return P = Q
%
% C. Otherwise, if P contains a time zone and Q does not, compare as
%    follows:
%
%    P < Q if P < (Q with time zone +14:00)
%    P > Q if P > (Q with time zone -14:00)
%    P <> Q otherwise, that is, if (Q with time zone +14:00) < P <
%                                  (Q with time zone -14:00)
%
% D. Otherwise, if P does not contain a time zone and Q does, compare
%    as follows:
%
%    P < Q if (P with time zone -14:00) < Q.
%    P > Q if (P with time zone +14:00) > Q.
%    P <> Q otherwise, that is, if (P with time zone +14:00) < Q <
%                                  (P with time zone -14:00)
%
% Examples:
%
% | *Determinate*                              | *Indeterminate*                             |
% |--------------------------------------------|---------------------------------------------|
% | 2000-01-15T00:00:00 < 2000-02-15T00:00:00  | 2000-01-01T12:00:00 <> 1999-12-31T23:00:00Z |
% | 2000-01-15T12:00:00 < 2000-01-16T12:00:00Z | 2000-01-16T12:00:00 <> 2000-01-16T12:00:00Z |
% |                                            | 2000-01-16T00:00:00 <> 2000-01-16T12:00:00Z |
%
% @tbd

'op:dateTime-greater-than'(A, B) :-
  A > B.



%! 'op:dateTime-less-than'(+A, +B) is semidet.
%
% § 10.4.7 op:dateTime-less-than
%
% ```
% op:dateTime-less-than($arg1 as xs:dateTime,
%                       $arg2 as xs:dateTime) as xs:boolean
% ```
%
% Summary: Returns true if and only if the value of $arg1 is less than
% the value of $arg2 according to the algorithm defined in section
% 3.2.7.4 of [XML Schema Part 2: Datatypes Second Edition] "Order
% relation on dateTime" for xs:dateTime values with timezones.
% Returns false otherwise.
%
% This function backs up the "lt" and "le" operators on xs:dateTime
% values.
%
% @tbd

'op:dateTime-less-than'(A, B) :-
  A < B.



%! 'op:numeric-equal'(+A, +B) is semidet.
%
% § 6.3.1 op:numeric-equal
%
% ```
% op:numeric-equal($arg1 as numeric, $arg2 as numeric) as xs:boolean
% ```
%
% Summary: Returns true if and only if the value of $arg1 is equal to
% the value of $arg2.  For xs:float and xs:double values, positive
% zero and negative zero compare equal.  INF equals INF and -INF
% equals -INF. NaN does not equal itself.
%
% This function backs up the "eq", "ne", "le" and "ge" operators on
% numeric values.

'op:numeric-equal'(A, B) :-
  A =:= B.



%! 'op:numeric-greater-than'(+A, +B) is semidet.
%
% § 6.3.3 op:numeric-greater-than
%
% ```
% op:numeric-greater-than($arg1 as numeric, $arg2 as numeric) as xs:boolean
% ```
%
% Summary: Returns true if and only if $arg1 is greater than $arg2.
% For xs:float and xs:double values, positive infinity is greater than
% all other non-NaN values; negative infinity is less than all other
% non-NaN values.  If $arg1 or $arg2 is NaN, the function returns
% false.
%
% This function backs up the "gt" and "ge" operators on numeric
% values.

'op:numeric-greater-than'(A, B) :-
  A > B.



%! 'op:numeric-less-than'(+A, +B) is semidet.
%
% § 6.3.2 op:numeric-less-than
%
% ```
% op:numeric-less-than($arg1 as numeric, $arg2 as numeric) as xs:boolean
% ```
%
% Summary: Returns true if and only if $arg1 is less than $arg2.  For
% xs:float and xs:double values, positive infinity is greater than all
% other non-NaN values; negative infinity is less than all other
% non-NaN values.  If $arg1 or $arg2 is NaN, the function returns
% false.
%
% This function backs up the "lt" and "le" operators on numeric
% values.

'op:numeric-less-than'(A, B) :-
  A < B.



%! rdf_create_string_literal(-Literal:rdf_literal, ?Ds:list(iri), ?LTag:list(atom),
%!                           +Lex:atom) is det.
%
% If all input literals are typed literals of type `xsd:string', then
% the returned literal is also of type `xsd:string', if all input
% literals are plain literals with identical language tag, then the
% returned literal is a plain literal with the same language tag, in
% all other cases, the returned literal is a simple literal.

% xsd:string
rdf_create_string_literal(literal(type(xsd:string,Lex)), Ds, _, Lex) :-
  rdf_equal(D, xsd:string),
  maplist(==(D), Ds), !.
% rdf:langString
rdf_create_string_literal(literal(lang(LTag,Lex)), _, [LTag|LTags], Lex) :-
  atom(LTag),
  maplist(==(LTag), LTags), !.
% simple literal
rdf_create_string_literal(literal(Lex), _, _, Lex).



%! 'STR'(+Term, +Atom) is semidet.
%! 'STR'(+Term, -Atom) is det.
%
% Returns the lexical form of a literal or the codepoint
% representation of an IRI.
%
% This is useful for examining parts of an IRI, for instance, the
% host-name.
%
% @compat SPARQL 1.1 Query §17.4.2.5

'STR'(literal(type(_,Lex)), Lex) :- !.
'STR'(literal(lang(_,Lex)), Lex) :- !.
'STR'(literal(Lex), Lex) :- !.
'STR'(Iri, Iri).



%! 'STRDT'(+Lex:atom, +D:atom, -Literal:compound) is det.
%! 'STRDT'(-Lex:atom, -D:atom, +Literal:compound) is det.

'STRDT'(Lex, D, literal(type(D,Lex))).
