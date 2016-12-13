:- module(
  xsd_duration,
  [
    dayTimeDurationCanonicalMap//1,   % +Duration
    dayTimeDurationMap//1,            % -Duration
    durationCanonicalMap//1,          % +Duration
    durationMap//1,                   % -Duration
    yearMonthDurationCanonicalMap//1, % +Duration
    yearMonthDurationMap//1           % -Duration
  ]
).

/** <module> XSD Duration

@author Wouter Beek
@compat XSD 1.1
@see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/
@version 2015/07, 2015/11, 2016/08, 2016/12
*/

:- use_module(library(apply)).
:- use_module(library(xsd/xsd_number)).

% @tbd Duplication is needed due to unclear arithmetic operator
%      support.
:- op(400, yfx, xsd_div).
:- arithmetic_function(xsd_div/2).

% xsd_div(+M, +N, -Z) is det.
%
% # Definition
%
% If `M` and `N` are numbers, then `M div N` is the greatest integer
% less than or equal to `M / N`.

xsd_div(X, Y, Z):-
  Z is floor(X rdiv Y).

:- op(400, yfx, xsd_mod).
:- arithmetic_function(xsd_mod/2).

%! xsd_mod(+M, +N, -X) is det.
%
% # Definition
%
% If `M` and `N` are numbers, then `M mod N` is `m − n * ( m div n)`.

xsd_mod(X, Y, Z):-
  Z is X - Y * (X xsd_div Y).





% E.2 DURATION-RELATED DEFINITIONS %

% AUXILIARY DURATION-RELATED FUNCTIONS OPERATING ON REPRESENTATION %
% FRAGMENTS                                                        %

%! duYearFragmentMap(-Y)// is det.
%
% Maps a duYearFrag//1 to an integer, intended as part of the value of
% the months property of a duration value.
%
% # Arguments
%
% @arg Y A nonnegative integer.
%
% # Algorithm
%
% Y is necessarily the letter 'Y' followed by a numeral N.
%
% Return noDecimalMap(N)
%
% @bug Swap letter and numeral.

duYearFragmentMap(Y) -->
  noDecimalMap(Y),
  "Y".



%! duMonthFragmentMap(-Mo)// is det.
%
% Maps a duMonthFrag//1 to an integer, intended as part of the value
% of the months property of a duration value.
%
% # Arguments
%
% @arg Mo A nonnegative integer.
%
% # Algorithm
%
% M is necessarily the letter 'M' followed by a numeral N.
%
% Return noDecimalMap(N)
%
% @bug Swap letter and numeral.

duMonthFragmentMap(Mo) -->
  noDecimalMap(Mo),
  "M".



%! duDayFragmentMap(-D)// is det.
%
% Maps a duDayFrag//1 to an integer, intended as part of the value of
% the seconds property of a duration value.
%
% # Arguments
%
% @arg D A nonnegative integer.
%
% # Algorithm
%
% D is necessarily the letter 'D' followed by a numeral N.
%
% Return noDecimalMap(N)
%
% @bug Swap letter and numeral.

duDayFragmentMap(D) -->
  noDecimalMap(D),
  "D".



%! duHourFragmentMap(-H)// is det.
%
% Maps a duHourFrag//1 to an integer, intended as part of the value of
% the seconds property of a duration value.
%
% # Arguments
%
% @arg H A nonnegative integer.
%
% # Algorithm
%
%   - D is necessarily the letter 'D' followed by a numeral N.
%
%   - Return noDecimalMap(N).
%
% @bug Swap letter and numeral.
%
% @bug D' should be H'.

duHourFragmentMap(H) -->
  noDecimalMap(H),
  "H".



%! duMinuteFragmentMap(-Mi)// is det.
%
% Maps a duMinuteFrag//1 to an integer, intended as part of the value
% of the seconds property of a duration value.
%
% # Arguments
%
% @arg Mi A nonnegative integer.
%
% # Algorithm
%
%   - M is necessarily the letter 'M' followed by a numeral N.
%
%   - Return noDecimalMap(N)
%
% @bug Swap letter and numeral.

duMinuteFragmentMap(Mi) -->
  noDecimalMap(Mi),
  "M".



%! duSecondFragmentMap(-S:rational)// is det.
%
% Maps a duSecondFrag//1 to a decimal number, intended as part of the
% value of the seconds property of a duration value.
%
% # Arguments
%
% @arg Second A nonnegative decimal number.
%
% # Algorithm
%
%   - S is necessarily 'S' followed by a numeral N.
%
%   - Return:
%
%     - decimalPtMap(N), when '.' occurs in N
%
%     - noDecimalMap(N), otherwise

duSecondFragmentMap(S) -->
  "S",
  (decimalPtMap(S) ; noDecimalMap(S)), !.



%! duYearMonthFragmentMap(-Month:nonneg)// is det.
%
% Maps a duYearMonthFrag//1 into an integer, intended as part of the
% months property of a duration value.
%
% # Arguments
%
% @arg Month A nonnegative integer.
%
% # Algorithm
%
%   - YM necessarily consists of an instance Y of duYearFrag//1 and/or
%     an instance M of duMonthFrag//1.
%
%   - Let:
%
%     - y be duYearFragmentMap(Y) (or 0 if Y is not present)
%
%     - m be duMonthFragmentMap(M) (or 0 if M is not present)
%
%   - Return  12 × y + m .

duYearMonthFragmentMap(Mo) -->
  (   duYearFragmentMap(Y0)
  ->  (duMonthFragmentMap(Mo0) -> "" ; {Mo0 = 0})
  ;   {Y0 = 0},
      duMonthFragmentMap(Mo0)
  ),
  {Mo is 12 * Y0 + Mo0}.



%! duTimeFragmentMap(-Seconds:rational)// is det.
%
% Maps a duTimeFrag//1 into a decimal number, intended as part of the
% seconds property of a duration value.
%
% # Arguments
%
% @arg Seconds A nonnegative decimal number
%
% # Algorithm
%
% T necessarily consists of an instance H of duHourFrag//1, and/or an
% instance M of duMinuteFrag//1, and/or an instance S of
% duSecondFrag//1.
%
% Let:
%
%   - h be duDayFragmentMap(H) (or 0 if H is not present)
%
%   - m be duMinuteFragmentMap(M) (or 0 if M is not present)
%
%   - s be duSecondFragmentMap(S) (or 0 if S is not present)
%
% Return 3600 × h + 60 × m + s
%
% @tbd duDayFragmentMap should be duHourFragmentMap.

duTimeFragmentMap(S) -->
  "T",
  (   duHourFragmentMap(H0)
  ->  (duMinuteFragmentMap(Mi0) -> "" ; {Mi0 = 0}),
      (duSecondFragmentMap(S0)  -> "" ; {S0  = 0})
  ;   duMinuteFragmentMap(Mi0)
  ->  {H0 = 0},
      (duSecondFragmentMap(S0)  -> "" ; {S0  = 0})
  ;   {H0 = 0, Mi0 = 0},
      duSecondFragmentMap(S0)
  ),
  {S is 3600 * H0 + 60 * Mi0 + S0}.



%! duDayTimeFragmentMap(-Second:rational)// is det.
%
% Maps a duDayTimeFrag//1 into a decimal number, which is the
% potential value of the seconds property of a duration value.
%
% # Arguments
%
% Second A nonnegative decimal number.
%
% # Algorithm
%
% DT necessarily consists of an instance D of duDayFrag//1 and/or an
% instance T of duTimeFrag//1.
%
% Let:
%
%   - d be duDayFragmentMap(D) (or 0 if D is not present)
%
%   - t be duTimeFragmentMap(T) (or 0 if T is not present)
%
% Return 86400 × d + t

duDayTimeFragmentMap(S) -->
  (   duDayFragmentMap(D0)
  ->  (duTimeFragmentMap(S0) -> "" ; {S0 = 0})
  ;   {D0 = 0},
      duTimeFragmentMap(S0)
  ),
  {S is 86400 * D0 + S0}.





% THE DURATION LEXICAL MAPPING %

%! durationMap(-Duration)// is det.
%
% Separates the durationLexicalRep//1 into the month part and the
% seconds part, then maps them into the months and seconds of the
% duration value.
%
% # Arguments
%
% @arg Duration A complete duration value.
%
% # Algorithm
%
% DUR consists of:
%
%   - possibly a leading '-',
%
%   - followed by 'P'
%
%   - and then an instance Y of duYearMonthFrag//1 and/or an instance
%     D of duDayTimeFrag//1
%
% Return a duration whose:
%
%   - months value is:
%
%     - 0, if Y is not present
%
%     - −duYearMonthFragmentMap(Y), if both '-' and Y are present
%
%     - duYearMonthFragmentMap(Y), otherwise
%
%   - seconds value is:
%
%     - 0, if D is not present
%
%     - −duDayTimeFragmentMap(D), if both '-' and D are present
%
%     - duDayTimeFragmentMap(D), otherwise

durationMap(duration(Mo,S)) -->
  ("-" -> {Sg = -1} ; {Sg = 1}),
  "P",
  (   duYearMonthFragmentMap(MoAbs)
  ->  (duDayTimeFragmentMap(SAbs) -> "" ; {SAbs = 0})
  ;   {MoAbs = 0},
      duDayTimeFragmentMap(SAbs)
  ),
  {
    Mo is copysign(MoAbs, Sg),
    S is copysign(SAbs, Sg)
  }.



%! yearMonthDurationMap(-Duration)// is det.
%
% Maps the lexical representation into the months of a
% yearMonthDuration value.  (A yearMonthDuration's seconds is always
% zero.)  yearMonthDurationMap//1 is a restriction of durationMap//1.
%
% # Arguments
%
% @arg Duration A complete yearMonthDuration value.
%
% # Algorithm
%
% YM necessarily consists of
%
%   - an optional leading '-'
%
%   - followed by 'P'
%
%   - and then an instance Y of duYearMonthFrag//1
%
% Return a yearMonthDuration whose:
%
%   - months value is:
%
%     - −duYearMonthFragmentMap(Y), if '-' is present in YM
%
%     - duYearMonthFragmentMap(Y), otherwise
%
%   - seconds value is (necessarily) 0

yearMonthDurationMap(duration(Mo,0)) -->
  ("-" -> {Sg = -1} ; {Sg = 1}),
  "P", duYearMonthFragmentMap(Moabs),
  {Mo is Sg * Moabs}.



%! dayTimeDurationMap(+Duration)// is det.
%
% Maps the lexical representation into the seconds of a
% dayTimeDuration value.  (A dayTimeDuration's months is always zero.)
% dayTimeDurationMap//1 is a restriction of durationMap//1.
%
% # Arguments
%
% @arg Duration A dayTimeDuration value.
%
% # Algorithm
%
% DT necessarily consists of possibly a leading '-', followed by 'P'
% and then an instance D of duDayTimeFrag//1.
%
% Return a dayTimeDuration whose:
%
%   - months value is (necessarily) 0
%
%   - seconds value is
%
%     - −duDayTimeFragmentMap(D), if '-' is present in DT
%
%     - duDayTimeFragmentMap(D), otherwise

dayTimeDurationMap(duration(0,S)) -->
  ("-" -> {Sg = -1} ; {Sg = 1}),
  "P", duDayTimeFragmentMap(Sabs),
  {S is Sg * Sabs}.





% AUXILIARY DURATION-RELATED FUNCTIONS PRODUCING REPRESENTATION %
% FRAGMENTS                                                     %

%! duYearMonthCanonicalFragmentMap(+Mo)// is det.
%
% Maps a nonnegative integer, presumably the absolute value of the
% months of a duration value, to a duYearMonthFrag//1, a fragment of a
% duration lexical representation.
%
% # Arguments
%
% @arg Mo A nonnegative integer.
%
% # Algorithm
%
% Let:
%
%   - y be ym div 12
%
%   - m be ym mod 12
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(y) & 'Y' &
%     unsignedNoDecimalPtCanonicalMap(m) & 'M', when neither y nor m
%     is zero
%
%   - unsignedNoDecimalPtCanonicalMap(y) & 'Y', when y is not zero but
%     m is
%
%   - unsignedNoDecimalPtCanonicalMap(m) & 'M', when y is zero

duYearMonthCanonicalFragmentMap(YM) -->
  {
    Y is YM xsd_div 12,
    Mo is YM xsd_mod 12
  },
  (   {Y =:= 0}
  ->  unsignedNoDecimalPtCanonicalMap(Mo),
      "M"
  ;   {Mo =:= 0}
  ->  unsignedNoDecimalPtCanonicalMap(Y),
      "Y"
  ;   unsignedNoDecimalPtCanonicalMap(Y),
      "Y",
      unsignedNoDecimalPtCanonicalMap(Mo),
      "M"
  ).



%! duDayCanonicalFragmentMap(+Day)// is det.
%
% Maps a nonnegative integer, presumably the day normalized value from
% the seconds of a duration value, to a duDayFrag//1, a fragment of a
% duration lexical representation.
%
% # Arguments
%
% @arg Day A nonnegative integer.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(d) & 'D', when d is not zero
%
%   - the empty string (''), when d is zero

duDayCanonicalFragmentMap(0) --> !, [].
duDayCanonicalFragmentMap(D) -->
  unsignedNoDecimalPtCanonicalMap(D),
  "D".



%! duHourCanonicalFragmentMap(+Hour:nonneg)// is det.
%
% Maps a nonnegative integer, presumably the hour normalized value
% from the seconds of a duration value, to a duHourFrag//1, a fragment
% of a duration lexical representation.
%
% # Arguments
%
% @arg Hour A nonnegative integer.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(h) & 'H', when h is not zero
%
%   - the empty string (''), when h is zero

duHourCanonicalFragmentMap(0) --> !, [].
duHourCanonicalFragmentMap(H) -->
  unsignedNoDecimalPtCanonicalMap(H),
  "H".



%! duMinuteCanonicalFragmentMap(+Minute:nonneg)// is det.
%
% Maps a nonnegative integer, presumably the minute normalized value
% from the seconds of a duration value, to a duMinuteFrag//1, a
% fragment of a duration lexical representation.
%
% # Arguments
%
% @arg Minute A nonnegative integer.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(m) & 'M', when m is not zero
%
%   - the empty string (''), when m is zero

duMinuteCanonicalFragmentMap(0) --> !, [].
duMinuteCanonicalFragmentMap(M) -->
  unsignedNoDecimalPtCanonicalMap(M),
  "M".



%! duSecondCanonicalFragmentMap(+Second:rational)// is det.
%
% Maps a nonnegative decimal number, presumably the second normalized
% value from the seconds of a duration value, to a duSecondFrag//1, a
% fragment of a duration lexical representation.
%
% # Arguments
%
% @arg Second A nonnegative decimal number.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(s) & 'S', when s is a non-zero
%     integer
%
%   - unsignedDecimalPtCanonicalMap(s) & 'S', when s is not an integer
%
%   - the empty string (''), when s is zero

duSecondCanonicalFragmentMap(S) -->
  {S =:= 0}, !, [].
duSecondCanonicalFragmentMap(S) -->
  {integer(S)}, !,
  unsignedNoDecimalPtCanonicalMap(S),
  "S".
duSecondCanonicalFragmentMap(S) -->
  unsignedDecimalPtCanonicalMap(S),
  "S".



%! duTimeCanonicalFragmentMap(
%!   +Hour:nonneg,
%!   +Minute:nonneg,
%!   +Second:rational
%! )// is det.
%
% Maps three nonnegative numbers, presumably the hour, minute, and
% second normalized values from a duration's seconds, to a
% duTimeFrag//1, a fragment of a duration lexical representation.
%
% # Arguments
%
% @arg Hour A nonnegative integer.
%
% @arg Minute A nonnegative integer.
%
% @arg Seconds A nonnegative decimal number.
%
% # Algorithm
%
% Return:
%
%   - 'T' & duHourCanonicalFragmentMap(h) &
%     duMinuteCanonicalFragmentMap(m) &
%     duSecondCanonicalFragmentMap(s), when h, m, and s are not all
%     zero
%
%   - the empty string ('') when all arguments are zero

duTimeCanonicalFragmentMap(0, 0, 0) --> !, [].
duTimeCanonicalFragmentMap(H, Mi, S) -->
  "T",
  duHourCanonicalFragmentMap(H),
  duMinuteCanonicalFragmentMap(Mi),
  duSecondCanonicalFragmentMap(S).



%! duDayTimeCanonicalFragmentMap(+Second:rational)// is det.
%
% Maps a nonnegative decimal number, presumably the absolute value of
% the seconds of a duration value, to a duDayTimeFrag//1, a fragment
% of a duration lexical representation.
%
% # Arguments
%
% @arg Second A nonnegative decimal number.
%
% # Algorithm
%
% Let:
%
%   - d is ss div 86400
%
%   - h is (ss mod 86400) div 3600
%
%   - m is (ss mod 3600) div 60
%
%   - s is ss mod 60
%
% Return:
%
%   - duDayCanonicalFragmentMap(d) & duTimeCanonicalFragmentMap(h, m,
%     s), when ss is not zero
%
%   - 'T0S', when ss is zero

duDayTimeCanonicalFragmentMap(0) --> !,
  "T0S".
duDayTimeCanonicalFragmentMap(S0) -->
  {
    % Days
    xsd_div(S0, 86400, D),

    % Hours
    xsd_mod(S0, 86400, H0),
    xsd_div(H0, 3600, H),

    % Minutes
    xsd_mod(S0, 3600, Mi0),
    xsd_div(Mi0, 60, Mi),

    % Seconds
    xsd_mod(S0, 60, S)
  },
  duDayCanonicalFragmentMap(D),
  duTimeCanonicalFragmentMap(H, Mi, S).





% THE DURATION CANONICAL MAPPING %

%! durationCanonicalMap(+Duration)// is det.
%
% Maps a duration's property values to durationLexicalRep//1 fragments
% and combines the fragments into a complete durationLexicalRep//1.
%
% # Arguments
%
% @arg Duration A complete duration value.
%
% # Algorithm
%
% Let:
%
%   - m be v's months
%
%   - s be v's seconds
%
%   - sgn be '-' if m or s is negative and the empty string ('')
%     otherwise
%
% Return:
%
%   - sgn & 'P' & duYearMonthCanonicalFragmentMap(|m|) &
%     duDayTimeCanonicalFragmentMap(|s|), when neither m nor s is zero
%
%   - sgn & 'P' & duYearMonthCanonicalFragmentMap(|m|), when m is not
%     zero but s is
%
%   - sgn & 'P' & duDayTimeCanonicalFragmentMap(|s|), when m is zero

durationCanonicalMap(duration(Mo,S)) -->
  ({(Mo < 0 ; S < 0.0)} -> "-" ; ""),
  "P",
  (   {Mo =:= 0}
  ->  {SAbs is abs(S)},
      duDayTimeCanonicalFragmentMap(SAbs)
  ;   {S =:= 0}
  ->  {MoAbs is abs(Mo)},
      duYearMonthCanonicalFragmentMap(MoAbs)
  ;   {
        MoAbs is abs(Mo),
        SAbs is abs(S)
      },
      duYearMonthCanonicalFragmentMap(MoAbs),
      duDayTimeCanonicalFragmentMap(SAbs)
  ).



%! yearMonthDurationCanonicalMap(+Duration)// is det.
%
% Maps a yearMonthDuration's months value to a
% yearMonthDurationLexicalRep//1.  (The seconds value is necessarily
% zero and is ignored.)  yearMonthDurationCanonicalMap//1 is a
% restriction of durationCanonicalMap//1.
%
% # Arguments
%
% @arg Duration A complete yearMonthDuration value.
%
% # Algorithm
%
% Let:
%
%   - m be ym's months
%
%   - sgn be '-' if m is negative and the empty string ('') otherwise.
%
% Return sgn & 'P' & duYearMonthCanonicalFragmentMap(|m|)

yearMonthDurationCanonicalMap(duration(Mo,0)) -->
  ({Mo < 0} -> "-", {MoAbs = -Mo} ; {MoAbs = Mo}),
  "P",
  duYearMonthCanonicalFragmentMap(MoAbs).



%! dayTimeDurationCanonicalMap(+Duration)// is det.
%
% Maps a dayTimeDuration's seconds value to a
% dayTimeDurationLexicalRep//1.  (The months value is necessarily zero
% and is ignored.)  dayTimeDurationCanonicalMap//1 is a restriction of
% durationCanonicalMap//1.
%
% # Algorithm
%
% Let:
%
%   - s be dt's months
%
%   - sgn be '-' if s is negative and the empty string ('') otherwise
%
% Return sgn & 'P' & duYearMonthCanonicalFragmentMap(|s|)
%
% @bug s' should be dt''s seconds rather than months.
%
% @bug duYearMonthCanonicalFragmentMap should be
%      duDayTimeCanonicalFragmentMap.

dayTimeDurationCanonicalMap(duration(0,S)) -->
  ({S < 0} -> "-" ; ""),
  "P",
  duDayTimeCanonicalFragmentMap(S).
