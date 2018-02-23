:- module(
  xsd_hexBinary,
  [
    hexBinaryCanonical//1, % +HexBinary:list(between(0,1))
    hexBinaryMap//1 % -HexBinary:list(between(0,7))
  ]
).

/** <module> XSD: hexBinary datatype

**`hexBinary`** represents arbitrary hex-encoded binary data.

```ebnf
hexDigit  ::= [0-9a-fA-F]
hexOctet  ::= hexDigit hexDigit
hexBinary ::= hexOctet*
```

# Value Space

The value space of hexBinary is the set of finite-length sequences of zero or
more binary octets. The length of a value is the number of octets.

# Lexical Mapping

hexBinary's lexical space consists of strings of hex (hexadecimal) digits,
two consecutive digits representing each octet in the corresponding value
(treating the octet as the binary representation of a number between 0 and
255). For example, `0FB7` is a lexical representation of the two-octet value
`00001111 10110111`.

The set recognized by hexBinary is the same as that recognized by
 the regular expression `([0-9a-fA-F]{2})*`.

The lexical mapping of hexBinary is hexBinaryMap.

The canonical mapping of hexBinary is given formally in hexBinaryCanonical.
3.3.15.3 Facets

---

@author Wouter Beek
@compat XSD 1.1
@see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/
@version 2015/07, 2015/11
*/

:- use_module(library(dcg)).
:- use_module(library(dlist)).





% CANONICAL MAP %

%! hexBinaryCanonical(+HexBinary:list(between(0,1)))// is det.
% Maps a hexBinary value to a literal matching the hexBinary production.
%
% Let `h` be the sequence of literals formed by applying hexOctetCanonical//1
% to each octet in `o`, in order, and concatenating the results.
% Return h.

hexBinaryCanonical([High1,High2,High3,High4,Low1,Low2,Low3,Low4|T]) --> !,
  hexOctetCanonical([High1,High2,High3,High4,Low1,Low2,Low3,Low4]),
  hexBinaryCanonical(T).
hexBinaryCanonical([]) --> "".



%! hexDigitCanonical(+HexDigit:list(between(0,1)))// is det.
% Maps a four-bit sequence to a hexadecimal digit
% (a literal matching the hexDigit production).
%
% Return
%   * '0' when d = 0000,
%   * '1' when d = 0001,
%   * '2' when d = 0010,
%   * '3' when d = 0011,
%   * ...
%   * 'E' when d = 1110,
%   * 'F' when d = 1111.

hexDigitCanonical([0,0,0,0]) --> !, "0".
hexDigitCanonical([0,0,0,1]) --> !, "1".
hexDigitCanonical([0,0,1,0]) --> !, "2".
hexDigitCanonical([0,0,1,1]) --> !, "3".
hexDigitCanonical([0,1,0,0]) --> !, "4".
hexDigitCanonical([0,1,0,1]) --> !, "5".
hexDigitCanonical([0,1,1,0]) --> !, "6".
hexDigitCanonical([0,1,1,1]) --> !, "7".
hexDigitCanonical([1,0,0,0]) --> !, "8".
hexDigitCanonical([1,0,0,1]) --> !, "9".
hexDigitCanonical([1,0,1,0]) --> !, "A".
hexDigitCanonical([1,0,1,1]) --> !, "B".
hexDigitCanonical([1,1,0,0]) --> !, "C".
hexDigitCanonical([1,1,0,1]) --> !, "D".
hexDigitCanonical([1,1,1,0]) --> !, "E".
hexDigitCanonical([1,1,1,1]) -->    "F".



%! hexOctetCanonical(+HexBinary:list(between(0,1)))// is det.
% Maps a binary octet to a literal matching the hexOctet production.
%
% Let `lo` be the four low-order bits of `o`, and `hi` be the four `high-order` bits.
% Return `hexDigitCanonical(hi) & hexDigitCanonical(lo)`.

hexOctetCanonical([High1,High2,High3,High4,Low1,Low2,Low3,Low4]) -->
  hexDigitCanonical([High1,High2,High3,High4]),
  hexDigitCanonical([Low1,Low2,Low3,Low4]).





% LEXICAL MAP %

%! hexBinaryMap(-HexBinary:list(between(0,1)))// is det.
% Maps a literal matching the `hexBinary` production to a sequence of octets
% in the form of a hexBinary value.
%
% `LEX` necessarily includes a sequence of zero or more substrings matching
% the `hexOctet` production.
% Let `o` be the sequence of octets formed by applying hexOctetMap//1 to each
% hexOctet in `LEX`, in order, and concatenating the results.
% Return o.

hexBinaryMap(L) --> *(hexOctetMap, DLs), {dappend_to_list(DLs, L)}.



%! hexDigitMap(?BitSequence:dlist(between(0,1)))// is det.
% Maps a hexadecimal digit (a character matching the `hexDigit` production)
% to a sequence of four binary digits.
%
% Return
%   * 0000 when d = '0',
%   * 0001 when d = '1',
%   * 0010 when d = '2',
%   * 0011 when d = '3',
%   * ...
%   * 1110 when d = 'E' or 'e',
%   * 1111 when d = 'F' or 'f'.

hexDigitMap([0,0,0,0|H]-H) --> "0", !.
hexDigitMap([0,0,0,1|H]-H) --> "1", !.
hexDigitMap([0,0,1,0|H]-H) --> "2", !.
hexDigitMap([0,0,1,1|H]-H) --> "3", !.
hexDigitMap([0,1,0,0|H]-H) --> "4", !.
hexDigitMap([0,1,0,1|H]-H) --> "5", !.
hexDigitMap([0,1,1,0|H]-H) --> "6", !.
hexDigitMap([0,1,1,1|H]-H) --> "7", !.
hexDigitMap([1,0,0,0|H]-H) --> "8", !.
hexDigitMap([1,0,0,1|H]-H) --> "9", !.
hexDigitMap([1,0,1,0|H]-H) --> "A", !.
hexDigitMap([1,0,1,0|H]-H) --> "a", !.
hexDigitMap([1,0,1,1|H]-H) --> "B", !.
hexDigitMap([1,0,1,1|H]-H) --> "b", !.
hexDigitMap([1,1,0,0|H]-H) --> "C", !.
hexDigitMap([1,1,0,0|H]-H) --> "c", !.
hexDigitMap([1,1,0,1|H]-H) --> "D", !.
hexDigitMap([1,1,0,1|H]-H) --> "d", !.
hexDigitMap([1,1,1,0|H]-H) --> "E", !.
hexDigitMap([1,1,1,0|H]-H) --> "e", !.
hexDigitMap([1,1,1,1|H]-H) --> "F", !.
hexDigitMap([1,1,1,1|H]-H) --> "f".



%! hexOctetMap(-Octet:dlist(between(0,1)))// is det.
% Maps a literal matching the `hexOctet` production to a single octet.
%
% `LEX` necessarily includes exactly two hexadecimal digits.
% Let `d0` be the first hexadecimal digit in `LEX`.  Let `d1` be the second
% hexadecimal digit in `LEX`.
% Return the octet whose four high-order bits are `hexDigitMap(d0)`
% and whose four low-order bits are `hexDigitMap(d1)`.

hexOctetMap(Octet) -->
  hexDigitMap(High), hexDigitMap(Low), {dappend(High, Low, Octet)}.
