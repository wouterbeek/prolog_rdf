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
    'op:numeric-multiply'/3,   % +X, +Y, -Z
    'op:numeric-subtract'/3,   % +X, +Y, -Z
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
:- use_module(library(call_ext)).
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
   numeric_cast(+, r, -),
   numeric_type_promotion(r, r),
   'STR'(o, ?),
   'STRDT'(+, r, o),
   subtype_substitution(r, r).





%! '_<_'(+A, +B) is semidet.

'_<_'(A^^D1, B^^D2) :-
  rdf11:xsd_date_time_type(D1),
  rdf11:xsd_date_time_type(D2), !,
  'op:dateTime-less-than'(A, B).
'_<_'(A^^xsd:boolean, B^^xsd:boolean) :- !,
  'op:boolean-less-than'(A, B).
'_<_'(A^^xsd:string, B^^xsd:string) :- !,
  compare(<, A, B).
'_<_'(A^^D1, B^^D2) :-
  rdf11:xsd_numerical(D1, Dom1, _),
  rdf11:xsd_numerical(D2, Dom2, _), !,
  is_of_type(Dom1, A),
  is_of_type(Dom2, B),
  'op:numeric-less-than'(A, B).
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
  rdf11:xsd_numerical(D1, Dom1, _),
  rdf11:xsd_numerical(D2, Dom2, _), !,
  is_of_type(Dom1, A),
  is_of_type(Dom2, B),
  'op:numeric-greater-than'(A, B).
'_>_'(A, B) :-
  compare(>, A, B).



eval(Lit, Lit) :-
  rdf_is_literal(Lit), !.
eval(Goal_1, Y) :-
  call(Goal_1, X),
  eval(X, Y).



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



%! 'fn:concat'(+Literals:list(string_literal), -Literal:string_literal) is det.

'fn:concat'(Lits, Lit) :-
  maplist(string_literal, Lits, Ds, LTags, Strs),
  atomics_to_string(Strs, Str),
  (   % xsd:string
      rdf_equal(xsd:string, D),
      maplist(=(D), Ds)
  ->  semlit(Lit, D, _, Str)
  ;   % language-tagged string
      maplist(=(LTag), LTags),
      atom(LTag)
  ->  semlit(Lit, rdf:langString, LTag, Str)
  ;   % plain literal
      Lit = Str
  ).



%! 'fn:matches'(+Literal:string_literal, +Pattern:string) is semidet.
%! 'fn:matches'(+Literal:string_literal, +Pattern:string, +Flags:string) is semidet.

'fn:matches'(Literal, Pattern) :-
  'fn:matches'(Literal, Pattern, "").


'fn:matches'(Literal, Pattern, Flags) :-
  string_literal(Literal, _, _, Str),
  atom_string(Flags0, Flags),
  re_match(Pattern/Flags0, Str).



%! 'fn:substring'(+Source:compound, +Start:compound, -String:compound) is det.

'fn:substring'(Literal1, Start^^xsd:integer, Literal2) :-
  semlit(Literal1, D, LTag, Str1),
  string_codes(Str1, Codes1),
  length(Prefix, Start),
  append(Prefix, Codes2, Codes1),
  string_codes(Str2, Codes2),
  rdf_create_string_literal(Literal2, D, LTag, Str2).



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

'fn:year-from-dateTime'(DT^^D, Y^^xsd:integer) :-
  dt_year(DT, D, Y).

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



%! numeric_cast(+N1:number, +D:atom, -N2:number) is det.

numeric_cast(N, xsd:decimal, N) :- !,
  rational(N).
numeric_cast(N1, D, N2) :-
  rdf11:xsd_numerical(D, Domain, Type),
  (   Type == integer
  ->  N2 is integer(N1)
  ;   Type == double
  ->  N2 is float(N1)
  ),
  must_be(Domain, N2).



%! numeric_datatype(+D1:atom, +D2:atom, -D3:atom) is semidet.

numeric_datatype(A, B, C) :-
  closure0(rel, A, C),
  closure0(rel, B, C), !.

rel(X, Y) :-
  subtype_substitution(X, Y).
rel(X, Y) :-
  numeric_type_promotion(X, Y).



%! numeric_type_promotion(?D1:atom, ?D2:atom) is nondet.
%
% D1 can be promoted to D2.

numeric_type_promotion(xsd:decimal, xsd:float).
numeric_type_promotion(xsd:decimal, xsd:double).
numeric_type_promotion(xsd:float, xsd:double).



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



%! 'op:numeric-multiply'(+X:rdf_literal, +Y:rdf_literal, -Z:rdf_literal) is det.
%
% @see /XQuery 1.0 and XPath 2.0/, §6.2.3 op:numeric-multiply
%
% op:numeric-multiply($arg1 as numeric, $arg2 as numeric) as numeric
%
% Summary: Backs up the "*" operator and returns the arithmetic
% product of its operands: ($arg1 * $arg2).
%
% Note: For `xs:float' or `xs:double' values, if one of the operands
% is a zero and the other is an infinity, `NaN' is returned.  If one
% of the operands is a non-zero number and the other is an infinity,
% an infinity with the appropriate sign is returned.

'op:numeric-multiply'(Arg1, Arg2, Val^^D) :-
  eval(Arg1, A^^D1),
  eval(Arg2, B^^D2),
  numeric_datatype(D1, D2, D),
  C is A * B,
  numeric_cast(C, D, Val),
  debug(ws(function), "~w = ~w * ~w", [C,A,B]).



%! 'op:numeric-subtract'(+X:rdf_literal, +Y:rdf_literal, -Z:rdf_literal) is det.

'op:numeric-subtract'(Arg1, Arg2, Val^^D) :-
  eval(Arg1, A^^D1),
  eval(Arg2, B^^D2),
  numeric_datatype(D1, D2, D),
  C is A - B,
  numeric_cast(C, D, Val),
  debug(ws(function), "~w = ~w - ~w", [C,A,B]).



%! rdf_create_string_literal(-SemLit:rdf_literal, ?Ds:list(atom), ?LTag:list(atom), +Lex:atom) is det.
%
% If all input literals are typed literals of type `xsd:string', then
% the returned literal is also of type `xsd:string', if all input
% literals are plain literals with identical language tag, then the
% returned literal is a plain literal with the same language tag, in
% all other cases, the returned literal is a simple literal.

% xsd:string
rdf_create_string_literal(Str^^xsd:string, Ds, _, Str) :-
  rdf_equal(D, xsd:string),
  maplist(==(D), Ds), !.
% rdf:langString
rdf_create_string_literal(Str@LTag, _, [LTag|LTags], Str) :-
  atom(LTag),
  maplist(==(LTag), LTags), !.
% simple literal
rdf_create_string_literal(Str, _, _, Str).



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

'STR'(SemLit, Lex) :-
  synlit_semlit(SynLit, SemLit), !,
  synlit(SynLit, _, _, Lex).
'STR'(A, A).



%! 'STRDT'(+Lex:atom, +D:atom, -Literal:compound) is det.
%! 'STRDT'(-Lex:atom, -D:atom, +Literal:compound) is det.

'STRDT'(Lex, D, SemLit) :-
  synlit(literal(type(D,Lex)), SemLit).



%! string_literal(+Lit:string_literal, -D:atom, -LTag:atom, -String:string) is det.
%! string_literal(-Lit:string_literal, +D:atom, +LTag:atom, +String:string) is det.

% xsd:string
string_literal(SemLit, xsd:string, LTag, Str) :-
  semlit(SemLit, xsd:string, LTag, Str), !.
% rdf:langString
string_literal(SemLit, rdf:langString, LTag, Str) :-
  semlit(SemLit, rdf:langString, LTag, Str), !.
string_literal(Str, _, _, Str).



%! subtype_substitution(?D1:atom, ?D2:atom) is nondet.
%
% D2 can be substituted for D1.

subtype_substitution(xsd:integer, xsd:decimal).
subtype_substitution(xsd:long, xsd:integer).
subtype_substitution(xsd:int, xsd:long).
subtype_substitution(xsd:short, xsd:int).
subtype_substitution(xsd:byte, xsd:short).
subtype_substitution(xsd:nonNegativeInteger, xsd:integer).
subtype_substitution(xsd:positiveInteger, xsd:nonNegativeInteger).
subtype_substitution(xsd:unsignedLong, xsd:nonNegativeInteger).
subtype_substitution(xsd:unsignedInt, xsd:unsignedLong).
subtype_substitution(xsd:unsignedShort, xsd:unsignedInt).
subtype_substitution(xsd:unsignedByte, xsd:unsignedShort).
subtype_substitution(xsd:nonPositiveInteger, xsd:integer).
subtype_substitution(xsd:negativeInteger, xsd:nonPositiveInteger).
