:- op(400, yfx, round).
:- arithmetic_function(round/2).





% Auxiliary Functions for Binary Floating-point Lexical/Canonical %
% Mappings                                                        %

%! floatingPointRound(
%!   +NV:number,
%!   +CWidth:positive_integer,
%!   +EMin:integer,
%!   +EMax:integer,
%!   -N:or([atom,compound])
%! ) is det.
%
% Rounds a non-zero decimal number to the nearest floating-point
% value.
%
% # Arguments
%
% @arg NV     An initially non-zero decimal number (may be set to
%             zero during calculations).
%
% @arg CWidth A positive integer.
%
% @arg EMin   An integer.
%
% @arg EMax   An integer greater than EMin.
%
% @arg N      A decimal number or special value (`INF` or `−INF`).
%
% # Algorithm
%
% Let:
%
%   - `s` be an integer initially `1`
%
%   - `c` be a nonnegative integer
%
%   - `e` be an integer
%
%   - Set `s` to `−1`, when `nV < 0`
%
%   - So select `e` that `2^cWidth × 2^(e−1) ≤ |nV| < 2^cWidth × 2^e`
%
%   - So select `c` that `(c − 1) × 2^e ≤ |nV| < c × 2^e`
%
%   - Now:
%
%     - When `eMax < e` (overflow) return:
%
%       - `positiveInfinity`, when `s` is positive, and
%
%       - `negativeInfinity`, otherwise
%
%     - Otherwise:
%
%       - When `e < eMin` (underflow):
%
%         - Set `e = eMin`
%
%         - So select `c` that `(c − 1) × 2e ≤ |nV| < c × 2^e`
%
%       - Set `nV` to:
%
%         - `c × 2^e`, when `|nV| > c × 2^e − 2^(e−1)`
%
%         - `(c − 1) × 2^e`, when `|nV| < c × 2^e − 2^(e−1)`
%
%         - `c × 2^e` or `(c − 1) × 2^e`, according to whether `c` is
%            even or `c − 1` is even, otherwise (i.e., `|nV | = c ×
%            2^e − 2^(e−1)`, the midpoint between the two values).
%
%       - Return:
%
%         - `s × nV`, when `nV < 2^cWidth × 2^eMax`,
%
%         - `positiveInfinity`, when `s` is positive, and
%
%         - `negativeInfinity`, otherwise.
%
% @note Implementers will find the algorithms of [Clinger, WD (1990)]
%       more efficient in memory than the simple abstract algorithm
%       employed above.

floatingPointRound(NV, CWidth, EMin, EMax, N):-
  (NV < 0 -> Sg = -1 ; Sg = 1),
  NV1 is abs(NV),
  select_e(NV1, CWidth, E),
  (   EMax < E
  ->  (Sg =:= 1 -> N = positiveInfinity ; N = negativeInfinity)
  ;   (E < EMin -> select_c(NV1, EMin, C) ; select_c(NV1, E, C)),
      Cutoff is C * 2 ^ E - 2 ^ (E - 1),
      (   NV1 > Cutoff
      ->  NV2 is C * 2 ^ E
      ;   NV1 < Cutoff
      ->  NV2 is (C - 1) * 2 ^ E
      ;   (   is_even(C)
          ->  even_value(C, E, NV2)
          ;   succ(CMinus, C),
              is_even(CMinus)
          ->  even_value(CMinus, E, NV2)
          ;   even_value(C, E, NV2a),
              succ(CMinus, C),
              even_value(CMinus, E, NV2b),
              NV2 is (NV2a + NV2b) / 2
          )
      ),
      (   NV2 < 2 ^ CWidth * 2 ^ EMax
      ->  N is Sg * NV2
      ;   Sg =:= 1
      ->  N = positiveInfinity
      ;   N = negativeInfinity
      )
  ).


even_value(C, E, NV):-
  NV is C * 2 ^ E.


select_c(NV, E, C):-
  C is ceil(NV / 2 ^ E).


select_e(NV, CWidth, E):-
  NV =\= 0,
  E is ceil(log10(NV / 2 ^ CWidth) / log10(2)).



%! round(+N:decimal, +K:nonneg, -D:decimal) is det.
%
% Maps a decimal number to that value rounded by some power of 10.
%
% # Arguments
%
% @arg N A decimal number.
%
% @arg K A nonnegative integer.
%
% @arg D A decimal number.
%
% # Algorithm
%
% Return `((n / 10^k + 0.5) div 1) × 10^k`.

round(N, K, D):-
  D is ((N / 10 ^ K + 0.5) xsd_div 1) * 10 ^ K.



%! floatApprox(+C:nonneg, +E:integer, +J:nonneg, -N:compound) is det.
%
% Maps a decimal number (`c × 10^e`) to successive approximations.
%
% # Arguments
%
% @arg C A nonnegative integer.
%
% @arg E An integer.
%
% @arg J A nonnegative integer.
%
% @arg N A decimal number.
%
% # Algorithm
%
% Return `round(c, j ) × 10^e`.

floatApprox(C, E, J, N):-
  N is (C round J) * 10 ^ E.





% Lexical Mapping %

%! floatLexicalMap(-N)// is det.
%! doubleLexicalMap(-N)// is det.
%
% Let `nV` be a decimal number or special value (`INF` or `−INF`).
%
% Return:
%
%   - `specialRepValue(LEX)` when `LEX` is an instance of
%     numericalSpecialRep//1
%
%   - Otherwise (`LEX` is a numeral):
%
%     - Set `nV` to:
%
%       - `noDecimalMap(LEX)`, when `LEX` is an instance of
%         noDecimalPtNumeral//1
%
%       - `decimalPtMap(LEX)`, when `LEX` is an instance of
%         decimalPtNumeral//1
%
%       - `scientificMap(LEX)`, otherwise (`LEX` is an instance of
%         scientificNotationNumeral//1).
%
%     - Set `nV` to `floatingPointRound(nV, 24, −149, 104)` when `nV`
%       is not zero.  (`floatingPointRound` may nonetheless return
%       zero, or `INF` or `−INF`.)
%
%     - Return:
%
%       - When `nV` is zero:
%
%         - `negativeZero`, when the first character of `LEX` is `'-'`
%
%         - `positiveZero`, otherwise
%
%       - `nV` otherwise

floatLexicalMap(N) -->
  floatDoubleLexicalMap0(24, -149, 104, N).


doubleLexicalMap(N) -->
  floatDoubleLexicalMap0(53, -1074, 971, N).





% Canonical Mapping %

%! floatCanonicalMap(+Float:or([atom,float]))// is det.
%! doubleCanonicalMap(+Double:or([atom,float]))// is det.
%
% Let:
%
%   - `l` be a nonnegative integer
%
%   - `s` be an integer intially `1`
%
%   - `c` be a positive integer
%
%   - `e` be an integer
%
% - Return `specialRepCanonicalMap(f)`, when `f` is one of
%   `positiveInfinity`, `negativeInfinity`, or `notANumber`
%
% - Return `'0.0E0'`, when `f` is `positiveZero`
%
% - Return `'-0.0E0'`, when `f` is `negativeZero`
%
% - Otherwise (`f` is numeric and non-zero):
%
%   - Set `s` to `−1`, when `f < 0`
%
%   - Let `c` be the smallest integer for which there exists an
%     integer `e` for which `|f| = c × 10^e`
%
%   - Let `e` be `log10(|f| / c)` (so that `|f| = c × 10^e`)
%
%   - Let `l` be the largest nonnegative integer for which `c × 10^e =
%     floatingPointRound(floatApprox(c, e, l), 24, −149, 104)`
%
%   - Return `scientificCanonicalMap(s × floatApprox(c, e, l ))`

floatCanonicalMap(N) -->
  floatDoubleCanonicalMap0(24, -149, 104, N).


doubleCanonicalMap(N) -->
  floatDoubleCanonicalMap0(53, -1074, 971, N).





% HELPERS %

floatDoubleLexicalMap0(_, _, _, N) -->
  specialRepValue(N), !.
floatDoubleLexicalMap0(CWidth, EMin, EMax, N) -->
  dcg_peek_code(Code1),
  (scientificMap(NV1), ! ; decimalPtMap(NV1), ! ; noDecimalMap(NV1)),
  {
    (   NV1 =:= 0.0
    ->  NV2 = NV1
    ;   floatingPointRound(NV1, CWidth, EMin, EMax, NV2)
    ),
    (   number(NV2),
        NV2 =:= 0.0
    ->  (Code1 == 0'- -> N = negativeZero ; N = positiveZero)
    ;   N = NV2
    )
  }.



floatDoubleCanonicalMap0(_, _, _, N) -->
  specialRepCanonicalMap(N), !.
floatDoubleCanonicalMap0(_, _, _, positiveZero) --> !,
  "0.0E0".
floatDoubleCanonicalMap0(_, _, _, negativeZero) --> !,
  "-0.0E0".
floatDoubleCanonicalMap0(CWidth, EMin, EMax, N) -->
  {
    (N < 0.0 -> Sg = -1 ; Sg = 1),
    smallest_c(N, C, E),
    largest_l(CWidth, EMin, EMax, C, E, L),
    floatApprox(C, E, L, Approx),
    N0 is rationalize(Sg * Approx)
  },
  scientificCanonicalMap(N0).


smallest_c(N, C, E):-
  between(1, inf, C),
  E is round(log10(abs(N) / C)),
  abs(N) =:= C * 10 ^ E, !.


largest_l(CWidth, EMin, EMax, C, E, L):-
  largest_l(CWidth, EMin, EMax, C, E, 0, L).


largest_l(CWidth, EMin, EMax, C, E, L1, LargestL):-
  X is C * 10 ^ E,
  floatApprox(C, E, L1, Approx),
  floatingPointRound(Approx, CWidth, EMin, EMax, Y),
  X  =:= Y, !,
  succ(L1, L2),
  largest_l(CWidth, EMin, EMax, C, E, L2, LargestL).
largest_l(_, _, _, _, _, L1, L2):-
  succ(L2, L1).
