:- module(
  xsd_number_fragments,
  [
    decimalPtMap//1, % -Decimal:rational
    decimalPtNumeral//1, % ?Decimal:rational
    decimalPtCanonicalMap//1, % +Decimal:rational
    fractionFragValue//1, % -Fraction:rational
    minimalNumericalSpecialRep//1, % ?SpecialValue:atom
    noDecimalMap//1, % -Integer:integer
    noDecimalPtCanonicalMap//1, % +Integer:integer
    noDecimalPtNumeral//1, % ?Integer:integer
    numericalSpecialRep//1, % ?SpecialValue:atom
    scientificCanonicalMap//1, % +Decimal:rational
    scientificMap//1, % -Decimal:rational
    scientificNotationNumeral//1, % ?Decimal:rational
    specialRepCanonicalMap//1, % +SpecialValue:atom
    specialRepValue//1, % -SpecialValue:atom
    unsignedDecimalPtCanonicalMap//1, % +Decimal:rational
    unsignedDecimalPtMap//1, % -Decimal:rational
    unsignedDecimalPtNumeral//1, % ?Decimal:rational
    unsignedNoDecimalMap//1, % -Integer:nonneg
    unsignedNoDecimalPtCanonicalMap//1, % +Integer:nonneg
    unsignedNoDecimalPtNumeral//1, % ?Integer:nonneg
    unsignedScientificCanonicalMap//1, % +Decimal:rational
    unsignedScientificNotationNumeral//1 % +Decimal:rational
  ]
).
:- reexport(library(dcg/dcg_ext), [
     digit//1 % ?Weight:between(0,9)
   ]).

/** <module> XSD: Grammar fragments for numbers

@author Wouter Beek
@compat XSD 1.1
@version 2015/07, 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(xsd/number/xsd_number_aux)).





%! fracFrag(?Decimal:rational)// .
% ```ebnf
% fracFrag ::= [0-9]+
% ```
%
% @compat XSD 1.1 [48].
% @see Parser fractionFragValue//1

fracFrag(N) --> {var(N)}, !, fractionFragValue(N).



%! decimalPtNumeral(?Decimal:rational)// .
% ```ebnf
% decimalPtNumeral ::= [+-]? unsignedDecimalPtNumeral
% ```
%
% @see Parser decimalPtMap//1
% @see Generator decimalPtCanonicalMap//1

decimalPtNumeral(N) --> {var(N)}, !, decimalPtMap(N).
decimalPtNumeral(N) --> decimalPtCanonicalMap(N).


%! minimalNumericalSpecialRep(?SpecialValue:atom)// .
% ```ebnf
% minimalNumericalSpecialRep ::= 'INF' | '-INF' | 'NaN'
% ```
%
% @compat XSD 1.1 [54].

minimalNumericalSpecialRep(negativeInfinity) --> "-INF", !.
minimalNumericalSpecialRep(notANumber)       --> "NaN",  !.
minimalNumericalSpecialRep(positiveInfinity) --> "INF".



%! numericalSpecialRep(?SpecialValue:atom)// .
% ```ebnf
% numericalSpecialRep ::= '+INF' | minimalNumericalSpecialRep
% ```
%
% @compat XSD 1.1 [55].
% Parser: specialRepValue//1
% Generator: specialRepCanonicalMap//1

numericalSpecialRep(SpecialValue) -->
  {var(SpecialValue)}, !,
  specialRepValue(SpecialValue).
numericalSpecialRep(SpecialValue) -->
  specialRepCanonicalMap(SpecialValue).


%! noDecimalPtNumeral(?Integer:integer)// .
% ```ebnf
% noDecimalPtNumeral ::= [+-]? unsignedNoDecimalPtNumeral
% ```
%
% @see Parser noDecimalMap//1
% @see Generator noDecimalPtCanonicalMap//1

noDecimalPtNumeral(N) --> {var(N)}, !, noDecimalMap(N).
noDecimalPtNumeral(N) --> noDecimalPtCanonicalMap(N).


%! scientificNotationNumeral(?Decimal:rational)// .
% ```ebnf
% scientificNotationNumeral ::= [+-]?
%                               unsignedScientificNotationNumeral
% ```
%
% @see Parser scientificMap//1
% @see Generator scientificCanonicalMap//1

scientificNotationNumeral(N) --> {var(N)}, !, scientificMap(N).
scientificNotationNumeral(N) --> scientificCanonicalMap(N).


%! unsignedDecimalPtNumeral(?Decimal:rational)// .
% ```ebnf
% unsignedDecimalPtNumeral ::=   (unsignedNoDecimalPtNumeral '.' fracFrag?)
%                              | ('.' fracFrag)
% ```
%
% @see Parser unsignedDecimalPtMap//1
% @see Generator unsignedDecimalPtCanonicalMap//1

unsignedDecimalPtNumeral(N) --> {var(N)}, !, unsignedDecimalPtMap(N).
unsignedDecimalPtNumeral(N) --> unsignedDecimalPtCanonicalMap(N).


% ```ebnf
% unsignedFullDecimalPtNumeral ::= unsignedNoDecimalPtNumeral '.' fracFrag
% ```
%
% @tbd This rule has not lexical-to-value and no (canonical)
%      value-to-lexical map.



%! unsignedNoDecimalPtNumeral(?Integer:nonneg)// .
% ```ebnf
% unsignedNoDecimalPtNumeral ::= [0-9]+
% ```
%
% @see Parser unsignedNoDecimalMap//1
% @see Generator unsignedNoDecimalPtCanonicalMap//1

unsignedNoDecimalPtNumeral(N) --> {var(N)}, !, unsignedNoDecimalMap(N).
unsignedNoDecimalPtNumeral(N) --> unsignedNoDecimalPtCanonicalMap(N).


%! unsignedScientificNotationNumeral(+Decimal:rational)// .
% ```ebnf
% unsignedScientificNotationNumeral ::= ( unsignedNoDecimalPtNumeral
%                                       | unsignedDecimalPtNumeral)
%                                       [eE]
%                                       noDecimalPtNumeral
% ```
%
% @see Generator unsignedScientificCanonicalMap//1

unsignedScientificNotationNumeral(N) --> unsignedScientificCanonicalMap(N).
