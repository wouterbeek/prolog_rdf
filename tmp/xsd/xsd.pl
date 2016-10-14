:- module(
  xsd,
  [
    xsd_canonical_map/3, % +Datatype:iri
                         % +Value
                         % ?LexicalForm:atom
    xsd_compare_value/4, % +Datatype:iri
                         % ?Order:oneof([incomparable,<,=,>])
                         % +Value1
                         % +Value2
    xsd_datatype/1, % ?Datatype:iri
    xsd_datatype/2, % ?Datatype:iri
                    % ?PrologType
    xsd_equiv_value/3, % +Datatype:iri
                       % +Value1
                       % +Value2
    xsd_guess_datatype/2, % +Value
                          % -Datatype:iri
    xsd_lexical_map/3 % +Datatype:iri
                      % +LexicalForm:atom
                      % ?Value
  ]
).

/** <module> XSD

Support for XML Schema Datatypes.

@author Wouter Beek
@compat XSD 1.1 Part 2: Datatypes
@license MIT License
@see http://www.w3.org/TR/xmlschema11-2
@version 2015/07-2015/09, 2015/11-2015/12, 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(typecheck)).
:- use_module(library(xsd/xsd_boolean)).
:- use_module(library(xsd/xsd_hexBinary)).
:- use_module(library(xsd/xsd_string)).
:- use_module(library(xsd/datetime/xsd_date)).
:- use_module(library(xsd/datetime/xsd_datetime)).
:- use_module(library(xsd/datetime/xsd_datetime_functions)).
:- use_module(library(xsd/datetime/xsd_gDay)).
:- use_module(library(xsd/datetime/xsd_gMonth)).
:- use_module(library(xsd/datetime/xsd_gMonthDay)).
:- use_module(library(xsd/datetime/xsd_gYear)).
:- use_module(library(xsd/datetime/xsd_gYearMonth)).
:- use_module(library(xsd/datetime/xsd_time)).
:- use_module(library(xsd/duration/xsd_duration)).
:- use_module(library(xsd/duration/xsd_duration_functions)).
:- use_module(library(xsd/number/xsd_decimal)).
:- use_module(library(xsd/number/xsd_double)).
:- use_module(library(xsd/number/xsd_float)).
:- use_module(library(xsd/number/xsd_number_fragments)).
:- use_module(library(xsdp_types)).

:- rdf_meta(xsd_canonical_map(r,+,?)).
:- rdf_meta(xsd_compare_value(r,?,+,+)).
:- rdf_meta(xsd_datatype(r)).
:- rdf_meta(xsd_datatype(r,?)).
:- rdf_meta(xsd_equiv_value(r,+,+)).
:- rdf_meta(xsd_lexical_map(r,+,?)).
:- rdf_meta(xsd_subtype_of(r,r)).





%! xsd_canonical_map(+Datatype:iri, +Value, -LexicalForm:atom) is det.

xsd_canonical_map(xsd:anyURI, Val, Lex):- !,
  xsd_canonical_map(xsd:string, Val, Lex).
xsd_canonical_map(xsd:boolean, Val, Lex):- !,
  boolean(Val),
  atom_phrase(booleanCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:date, Val, Lex):- !,
  atom_phrase(dateCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:dateTime, Val, Lex):- !,
  atom_phrase(datetimeCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:dayTimeDuration, Val, Lex):- !,
  atom_phrase(dayTimeDurationCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:decimal, N, Lex):- !,
  rational(N),
  atom_phrase(decimalCanonicalMap(N), Lex).
xsd_canonical_map(xsd:double, N, Lex):- !,
  xsd_canonical_map(xsd:float, N, Lex).
xsd_canonical_map(xsd:duration, Val, Lex):- !,
  atom_phrase(durationMap(Val), Lex).
xsd_canonical_map(xsd:float, notANumber, 'NaN'):- !.
xsd_canonical_map(xsd:float, N, Lex):- !,
  number_codes(N, [H|_]),
  (   N =:= 0.0
  ->  (H == 0'- -> Lex = '-0.0E0' ; Lex = '0.0E0')
  ;   format(atom(Lex), "~E", [N])
  ).
xsd_canonical_map(xsd:gDay, Val, Lex):- !,
  atom_phrase(gDayCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:gMonth, Val, Lex):- !,
  atom_phrase(gMonthCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:gMonthDay, Val, Lex):- !,
  atom_phrase(gMonthDayCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:gYear, Val, Lex):- !,
  atom_phrase(gYearCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:gYearMonth, Val, Lex):- !,
  atom_phrase(gYearMonthCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:hexBinary, Val, Lex):- !,
  atom_phrase(hexBinaryCanonical(Val), Lex).
xsd_canonical_map(xsd:int, N, Lex):- !,
  between(-2147483648, 2147483647, N),
  xsd_canonical_map(xsd:long, N, Lex).
xsd_canonical_map(xsd:integer, N, Lex):- !,
  integer(N),
  atom_phrase(noDecimalPtCanonicalMap(N), Lex).
xsd_canonical_map(xsd:long, N, Lex):- !,
  between(-9223372036854775808, 9223372036854775807, N),
  xsd_canonical_map(xsd:integer, N, Lex).
xsd_canonical_map(xsd:negativeInteger, N, Lex):- !,
  negative_integer(N),
  xsd_canonical_map(xsd:nonPositiveInteger, N, Lex).
xsd_canonical_map(xsd:nonNegativeInteger, N, Lex):- !,
  nonneg(N),
  xsd_canonical_map(xsd:integer, N, Lex).
xsd_canonical_map(xsd:nonPositiveInteger, N, Lex):- !,
  negative_integer(N),
  xsd_canonical_map(xsd:integer, N, Lex).
xsd_canonical_map(xsd:positiveInteger, N, Lex):- !,
  positive_integer(N),
  xsd_canonical_map(xsd:nonNegativeInteger, N, Lex).
xsd_canonical_map(xsd:string, Val, Lex):- !,
  atom(Val),
  atom_phrase(stringCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:time, Val, Lex):- !,
  atom_phrase(timeCanonicalMap(Val), Lex).
xsd_canonical_map(xsd:yearMonthDuration, Val, Lex):- !,
  atom_phrase(yearMonthDurationCanonicalMap(Val), Lex).



%! xsd_compare_value(
%!   +Datatype:iri,
%!   +Order:oneof([<,=,>]),
%!   +Value1,
%!   +Value2
%! ) is semidet.
%! xsd_compare_value(
%!   +Datatype:iri,
%!   -Order:oneof([<,=,>]),
%!   +Value1,
%!   +Value2
%! ) is det.
% Fails only if the given values are incomparable.

% Date-time comparisons.
xsd_compare_value(D, Order, DT1, DT2):-
  (   rdf_equal(xsd:date, D)
  ;   rdf_equal(xsd:dateTime, D)
  ;   rdf_equal(xsd:gDay, D)
  ;   rdf_equal(xsd:gMonth, D)
  ;   rdf_equal(xsd:gMonthDay, D)
  ;   rdf_equal(xsd:gYear, D)
  ;   rdf_equal(xsd:gYearMonth, D)
  ), !,
  xsd_datetime_compare(Order, DT1, DT2).
% Duration comparisons.
xsd_compare_value(D, Order, Duration1, Duration2):-
  (   rdf_equal(xsd:duration, D)
  ;   rdf_equal(xsd:xsd_yearMonthDuration, D)
  ), !,
  xsd_duration_compare(Order, Duration1, Duration2).
% Numeric comparators.
xsd_compare_value(D, Order, Val1, Val2):-
  (   rdf_equal(xsd:decimal, D)
  ;   rdf_equal(xsd:double, D)
  ;   rdf_equal(xsd:float, D)
  ;   rdf_equal(xsd:integer, D)
  ;   rdf_equal(xsd:nonNegativeInteger, D)
  ), !,
  compare(Order, Val1, Val2).



%! xsd_datatype(+Datatype:iri) is semidet.
%! xsd_datatype(-Datatype:iri) is multi.

xsd_datatype(D):- xsd_datatype(D, _).


%! xsd_datatype(+Datatype:iri, +PrologType) is semidet.
%! xsd_datatype(+Datatype:iri, -PrologType) is det.
%! xsd_datatype(-Datatype:iri, +PrologType) is nondet.
%! xsd_datatype(-Datatype:iri, -PrologType) is nondet.

xsd_datatype(xsd:anyURI            , atom              ).
xsd_datatype(xsd:boolean           , boolean           ).
xsd_datatype(xsd:date              , datetime          ).
xsd_datatype(xsd:dateTime          , datetime          ).
xsd_datatype(xsd:decimal           , rational          ).
xsd_datatype(xsd:double            , float             ).
xsd_datatype(xsd:duration          , datetime          ).
xsd_datatype(xsd:float             , float             ).
xsd_datatype(xsd:gDay              , datetime          ).
xsd_datatype(xsd:gMonth            , datetime          ).
xsd_datatype(xsd:gMonthDay         , datetime          ).
xsd_datatype(xsd:gYear             , datetime          ).
xsd_datatype(xsd:gYearMonth        , datetime          ).
xsd_datatype(xsd:hexBinary         , list(between(0,1))).
xsd_datatype(xsd:int               , integer           ).
xsd_datatype(xsd:integer           , integer           ).
xsd_datatype(xsd:long              , integer           ).
xsd_datatype(xsd:negativeInteger   , negative_integer  ).
xsd_datatype(xsd:nonNegativeInteger, nonneg            ).
xsd_datatype(xsd:nonPositiveInteger, nonpos            ).
xsd_datatype(xsd:positiveInteger   , positive_integer  ).
xsd_datatype(xsd:string            , atom              ).
xsd_datatype(xsd:time              , datetime          ).



%! xsd_equiv_value(+Datatype:iri, +Value1, +Value2) is semidet.
% XSD value equivalence w.r.t. a datatype.

xsd_equiv_value(D, Val1, Val2):-
  xsd_compare_value(D, =, Val1, Val2).



%! xsd_guess_datatype(+Value, -Datatype:iri) is semidet.

% Date-time.
xsd_guess_datatype(datetime(Y,Mo,D,H,Mi,S,_), D):- !,
  (   % xsd:dateTime
      ground(date(Y,Mo,D,H,Mi,S))
  ->  rdf_equal(xsd:dateTime, D)
  ;   % xsd:date
      ground(date(Y,Mo,D))
  ->  rdf_equal(xsd:date, D)
  ;   % xsd:time
      ground(date(H,Mi,S))
  ->  rdf_equal(xsd:time, D)
  ;   % xsd:gMonthDay
      ground(date(Mo,D))
  ->  rdf_equal(xsd:gMonthDay, D)
  ;   % xsd:gYearMonth
      ground(date(Y,Mo))
  ->  rdf_equal(xsd:gYearMonth, D)
  ;   % xsd:gMonth
      ground(date(Mo))
  ->  rdf_equal(xsd:gMonth, D)
  ;   % xsd:gYear
      ground(date(Y))
  ->  rdf_equal(xsd:gYear, D)
  ).
% Number.
xsd_guess_datatype(N, D):-
  (   % xsd:float
      float(N)
  ->  rdf_equal(xsd:float, D)
  ;   % xsd:integer
      integer(N)
  ->  rdf_equal(xsd:integer, D)
  ;   % xsd:decimal
      rational(N)
  ->  rdf_equal(xsd:decimal, D)
  ), !.
% String
xsd_guess_datatype(A, D):-
  % xsd:string
  atom(A), !,
  rdf_equal(xsd:string, D).



%! xsd_lexical_map(+Datatype:iri, +LexicalForm:atom, -Value) is nondet.

xsd_lexical_map(xsd:anyURI, Lex, Val):- !,
  xsd_lexical_map(xsd:string, Lex, Val).
xsd_lexical_map(xsd:boolean, Lex, Boolean):- !,
  atom_phrase(booleanLexicalMap(Boolean), Lex).
xsd_lexical_map(xsd:date, Lex, DT):- !,
  atom_phrase(dateLexicalMap(DT), Lex).
xsd_lexical_map(xsd:dateTime, Lex, DT):- !,
  atom_phrase(datetimeLexicalMap(DT), Lex).
xsd_lexical_map(xsd:dayTimeDuration, Lex, Du):- !,
  atom_phrase(dayTimeDurationMap(Du), Lex).
xsd_lexical_map(xsd:decimal, Lex, N):- !,
  atom_phrase(decimalLexicalMap(N), Lex).
xsd_lexical_map(xsd:double, Lex, N):- !,
  xsd_lexical_map(xsd:float, Lex, N).
xsd_lexical_map(xsd:duration, Lex, Du):- !,
  atom_phrase(durationMap(Du), Lex).
xsd_lexical_map(xsd:float, Lex, N):- !,
  atom_codes(Lex, Cs),
  catch(number_codes(N, Cs), _, N = notANumber).
xsd_lexical_map(xsd:gDay, Lex, DT):- !,
  atom_phrase(gDayLexicalMap(DT), Lex).
xsd_lexical_map(xsd:gMonth, Lex, DT):- !,
  atom_phrase(gMonthLexicalMap(DT), Lex).
xsd_lexical_map(xsd:gMonthDay, Lex, DT):- !,
  atom_phrase(gMonthDayLexicalMap(DT), Lex).
xsd_lexical_map(xsd:gYear, Lex, DT):- !,
  atom_phrase(gYearLexicalMap(DT), Lex).
xsd_lexical_map(xsd:gYearMonth, Lex, DT):- !,
  atom_phrase(gYearMonthLexicalMap(DT), Lex).
xsd_lexical_map(xsd:hexBinary, Lex, HexBinary):- !,
  atom_phrase(hexBinaryMap(HexBinary), Lex).
xsd_lexical_map(xsd:int, Lex, N):- !,
  xsd_lexical_map(xsd:long, Lex, N),
  between(-2147483648, 2147483647, N).
xsd_lexical_map(xsd:integer, Lex, N):- !,
  atom_phrase(noDecimalMap(N), Lex).
xsd_lexical_map(xsd:long, Lex, N):- !,
  xsd_lexical_map(xsd:integer, Lex, N),
  between(-9223372036854775808, 9223372036854775807, N).
xsd_lexical_map(xsd:negativeInteger, Lex, N):- !,
  xsd_lexical_map(xsd:nonPositiveInteger, Lex, N),
  negative_integer(N).
xsd_lexical_map(xsd:nonNegativeInteger, Lex, N):- !,
  xsd_lexical_map(xsd:integer, Lex, N),
  nonneg(N).
xsd_lexical_map(xsd:nonPositiveInteger, Lex, N):- !,
  xsd_lexical_map(xsd:integer, Lex, N),
  nonpos(N).
xsd_lexical_map(xsd:positiveInteger, Lex, N):- !,
  xsd_lexical_map(xsd:nonNegativeInteger, Lex, N),
  positive_integer(N).
xsd_lexical_map(xsd:string, Lex, S):- !,
  atom_phrase(stringLexicalMap(S), Lex).
xsd_lexical_map(xsd:time, Lex, DT):- !,
  atom_phrase(timeLexicalMap(DT), Lex).
xsd_lexical_map(xsd:yearMonthDuration, Lex, Du):- !,
  atom_phrase(yearMonthDurationMap(Du), Lex).
