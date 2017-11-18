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
   'fn:compare'(o, o, ?),
   'fn:concat'(t, o),
   'fn:matches'(o, o),
   'fn:matches'(o, o, o),
   'fn:substring'(o, o, o),
   'fn:year-from-dateTime'(o, o),
   numeric_cast(+, r, -),
   numeric_datatype(r),
   numeric_type_promotion(r, r),
   'op:boolean-greater-than'(o, o),
   'op:boolean-less-than'(o, o),
   'op:dateTime-greater-than'(o, o),
   'op:dateTime-less-than'(o, o),
   'op:numeric-greater-than'(o, o, o),
   'op:numeric-less-than'(o, o, o),
   'op:numeric-multiply'(o, o, o),
   'op:numeric-subtract'(o, o, o),
   'STR'(o, ?),
   'STRDT'(+, r, o),
   subtype_substitution(r, r).





%! '_<_'(+A:rdf_literal, +B:rdf_literal) is semidet.

'_<_'(A1, B1) :-
  eval(A1, A2),
  eval(B1, B2),
  (   'op:boolean-less-than'(A2, B2), !
  ;   'op:dateTime-less-than'(A2, B2), !
  ;   'op:numeric-less-than'(A2, B2), !
  ;   'fn:compare'(A2, B2, <)
  ).



%! '_>_'(+A:rdf_literal, +B:rdf_literal) is semidet.

'_>_'(A1, B1) :-
  eval(A1, A2),
  eval(B1, B2),
  (   'op:boolean-greater-than'(A2, B2), !
  ;   'op:dateTime-greater-than'(A2, B2), !
  ;   'op:numeric-greater-than'(A2, B2), !
  ;   'fn:compare'(A2, B2, >)
  ).



eval(Lit, Lit) :-
  rdf_is_literal(Lit), !.
eval(Goal_1, Y) :-
  call(Goal_1, X),
  eval(X, Y).



%! 'fn:compare'(+A:rdf_literal, +B:rdf_literal, +Order:oneof([-1,0,1])) is semidet.
%! 'fn:compare'(+A:rdf_literal, +B:rdf_literal, -Order:oneof([-1,0,1])) is det.

'fn:compare'(A, B, Sign) :-
  compare(Order, A, B),
  order_sign(Order, Sign).

order_sign(<, -1).
order_sign(=, 0).
order_sign(>, 1).



%! 'fn:concat'(+Literals:list(rdf_literal), -Literal:rdf_literal) is det.

'fn:concat'(Literals, Literal) :-
  maplist(rdf_literal, Ds, LTags, Lexs, Literals),
  atomics_to_string(Lexs, Lex),
  (   % xsd:string
      rdf_equal(xsd:string, D),
      maplist(=(D), Ds)
  ->  rdf_typed_literal(D, Lex, Literal)
  ;   % language-tagged string
      maplist(=(LTag), LTags),
      atom(LTag)
  ->  rdf_language_tagged_string(LTag, Lex, Literal)
  ;   % plain literal
      Literal = Lex
  ).



%! 'fn:matches'(+Literal:rdf_literal, +Pattern:rdf_literal) is semidet.
%! 'fn:matches'(+Literal:rdf_literal, +Pattern:rdf_literal,
%!              +Flags:rdf_literal) is semidet.

'fn:matches'(Literal, Pattern) :-
  'fn:matches'(Literal, Pattern, "").


'fn:matches'(Literal, Pattern, Flags) :-
  maplist(rdf_literal, _, _, [Lex,PatternLex,FlagsLex], [Literal,Pattern,Flags]),
  re_match(PatternLex/FlagsLex, Lex).



%! 'fn:substring'(+Source:compound, +Start:compound, -String:compound) is det.

'fn:substring'(Literal1, Start^^xsd:integer, Literal2) :-
  rdf_literal(D, LTag, Lex, Literal1),
  atom_codes(Lex, Codes1),
  length(Prefix, Start),
  append(Prefix, Codes2, Codes1),
  atom_codes(Lex2, Codes2),
  rdf_literal(D, LTag, Lex2, Literal2).



%! 'fn:year-from-dateTime'(+Arg, -Year) is det.

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



%! numeric_datatype(+D:atom) is semidet.
%! numeric_datatype(-D:atom) is nondet.

numeric_datatype(xsd:byte).
numeric_datatype(xsd:double).
numeric_datatype(xsd:decimal).
numeric_datatype(xsd:float).
numeric_datatype(xsd:int).
numeric_datatype(xsd:integer).
numeric_datatype(xsd:long).
numeric_datatype(xsd:negativeInteger).
numeric_datatype(xsd:nonNegativeInteger).
numeric_datatype(xsd:nonPositiveInteger).
numeric_datatype(xsd:positiveInteger).
numeric_datatype(xsd:short).
numeric_datatype(xsd:unsignedByte).
numeric_datatype(xsd:unsignedInt).
numeric_datatype(xsd:unsignedLong).
numeric_datatype(xsd:unsignedShort).



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



%! 'op:boolean-greater-than'(+A:rdf_literal, +B:rdf_literal) is semidet.

'op:boolean-greater-than'(true^^xsd:boolean, false^^xsd:boolean).



%! 'op:boolean-less-than'(+A:rdf_literal, +B:rdf_literal) is semidet.

'op:boolean-less-than'(false^^xsd:boolean, true^^xsd:boolean).



%! 'op:dateTime-greater-than'(+A:rdf_literal, +B:rdf_literal) is semidet.

'op:dateTime-greater-than'(A^^D1, B^^D2) :-
  rdf11:xsd_date_time_type(D1),
  rdf11:xsd_date_time_type(D2), !,
  A > B.



%! 'op:dateTime-less-than'(+A:rdf_literal, +B:rdf_literal) is semidet.

'op:dateTime-less-than'(A^^D1, B^^D2) :-
  rdf11:xsd_date_time_type(D1),
  rdf11:xsd_date_time_type(D2), !,
  A < B.



%! 'op:numeric-equal'(+A:rdf_literal, +B:rdf_literal) is semidet.

'op:numeric-equal'(A, B) :-
  A =:= B.



%! 'op:numeric-greater-than'(+A:rdf_literal, +B:rdf_literal) is semidet.

'op:numeric-greater-than'(A^^D1, B^^D2) :-
  numeric_datatype(D1, D2, -),
  A > B,
  debug(ws(function), "~w > ~w", [A,B]).



%! 'op:numeric-less-than'(+A:rdf_literal, +B:rdf_literal) is semidet.

'op:numeric-less-than'(A^^D1, B^^D2) :-
  numeric_datatype(D1, D2, _),
  A < B,
  debug(ws(function), "~w < ~w", [A,B]).



%! 'op:numeric-multiply'(+A:eval, +B:eval, -C:rdf_literal) is det.

'op:numeric-multiply'(Arg1, Arg2, Val^^D) :-
  eval(Arg1, A^^D1),
  eval(Arg2, B^^D2),
  numeric_datatype(D1, D2, D),
  C is A * B,
  numeric_cast(C, D, Val),
  debug(ws(function), "~w = ~w * ~w", [C,A,B]).



%! 'op:numeric-subtract'(+A:eval, +B:eval, -C:rdf_literal) is det.

'op:numeric-subtract'(Arg1, Arg2, Val^^D) :-
  eval(Arg1, A^^D1),
  eval(Arg2, B^^D2),
  numeric_datatype(D1, D2, D),
  C is A - B,
  numeric_cast(C, D, Val),
  debug(ws(function), "~w = ~w - ~w", [C,A,B]).



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

'STR'(Literal, Lex) :-
  rdf_literal_lexical_form(Literal, Lex).
'STR'(A, A).



%! 'STRDT'(+Lex:atom, +D:atom, -Literal:compound) is det.
%! 'STRDT'(-Lex:atom, -D:atom, +Literal:compound) is det.

'STRDT'(Lex, D, Literal) :-
  rdf_typed_literal(D, Lex, Literal).



%! string_literal(+Lit:rdf_literal, -String:string) is det.

string_literal(Lit, Str) :-
  string_literal(Lit, _, _, Str).



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
