:- module(
  xsd_decimal,
  [
    decimalCanonicalMap//1, % +Decimal:rational
    decimalLexicalMap//1 % -Decimal:rational
  ]
).

/** <module> XSD decimal datatype

```ebnf
decimalLexicalRep ::= decimalPtNumeral | noDecimalPtNumeral
```

@author Wouter Beek
@compat [XSD 1.1](http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/)
@version 2015/07, 2015/11
*/

:- use_module(library(xsd/number/xsd_number_fragments)).






% integerLexicalRep(?Integer:integer)//.
% **`integer`** is derived from decimal by fixing the value of =fractionDigits=
% to be 0 and disallowing the trailing decimal point. This results in the
% standard mathematical concept of the integer numbers. The value space of
% integer is the infinite set `{...,-2,-1,0,1,2,...}`. The base type of
% integer is decimal.
%
% # Lexical representation
%
% Integer has a lexical representation consisting of a finite-length sequence
% of one or more decimal digits (`#x30`-`#x39`) with an optional leading sign.
% If the sign is omitted, "`+`" is assumed.
%
% # Canonical representation
%
% The canonical representation for integer is defined by prohibiting certain
% options from the Lexical representation. Specifically, the preceding
% optional `"+"` sign is prohibited and leading zeroes are prohibited.

integerLexicalRep(I) --> {var(I)}, !, noDecimalMap(I).
integerLexicalRep(I) --> noDecimalPtCanonicalMap(I).
