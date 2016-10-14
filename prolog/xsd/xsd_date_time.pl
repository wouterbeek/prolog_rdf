:- module(
  xsd_date_time,
  [
    dateCanonicalMap//1,    % +DT
    dateTimePlusDuration/3, % +Duration, +DT1, -DT2
    dayFragValue//1,        % -D
    daysInMonth/3,          % ?Y, ?Mo, ?DaysInMonth
    monthFragValue//1,      % -Mo
    newDatetime/8,          % ?Y, ?Mo, ?D, ?H, ?Mi, ?S, ?Off, -DT
    normalizeDay/6,         % +Y, +Mo, +D, -NormY, -NormMo, -NormD
    normalizeMinute/10,     % +Y, +Mo, +D, +H, +Mi
                            % -NormY, -NormMo, -NormD, -NormH, -NormMi
    normalizeMonth/4,       % +Y, +Mo, -NormY, -NormMo
    normalizeSecond/12,     % +Y, +Mo, +D, +H, +Mi, +S:rational
                            % -NormY, -NormMo, -NormD, -NormH, -NormMi
                            % -NormS:rational
    timeOnTimeline/2,       % +DT, -Ss:rational
    yearFragValue//1        % -Y
  ]
).

/** <module> XSD date/time-related definitions

Section E.3. “Date/time-related Definitions”

@author Wouter Beek
@version 2016/08, 2016/10
*/

:- use_module(library(arithmetic)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xsd/xsd_number)).

% @tbd Duplication is needed due to unclear arithmetic operator
% support.
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





% E.3. DATE/TIME-RELATED DEFINITIONS %

% E.3.1 NORMALIZATION OF PROPERTY VALUES %

%! normalizeMonth(+Y, +Mo, -NormY, -NormMo) is det.
%
% If month (mo) is out of range, adjust month and year (yr)
% accordingly; otherwise, make no change.
%
% # Algorithm
%
%   - Add (mo − 1) div 12 to yr
%
%   - Set mo to (mo − 1) mod 12 + 1

normalizeMonth(Y1, Mo1, Y2, Mo2):-
  % Add (mo − 1) div 12 to yr.
  Y2 is Y1 + (Mo1 - 1) xsd_div 12,
  % Set mo to (mo − 1) mod 12 + 1.
  Mo2 is (Mo1 - 1) xsd_mod 12 + 1.



%! normalizeDay(+Y, +Mo, +D, -NormY, -NormMo, -NormD) is det.
%
% If month (mo) is out of range, or day (da) is out of range for the
% appropriate month, then adjust values accordingly, otherwise make no
% change.
%
% # Algorithm
%
%  - normalizeMonth(yr, mo)
%
%  - Repeat until da is positive and not greater than daysInMonth(yr,
%    mo):
%
%    - If da exceeds daysInMonth(yr, mo) then:
%
%      - Subtract that limit from da.
%
%      - Add 1 to mo.
%
%      - normalizeMonth(yr, mo)
%
%    - If da is not positive then:
%
%      - Subtract 1 from mo.
%
%      - normalizeMonth(yr, mo)
%
%      - Add the new upper limit from the table to da.
%
% @tbd It is unclear what "from the table to" means.

normalizeDay(Y1, Mo1, D1, Y3, Mo3, D3):-
  normalizeMonth(Y1, Mo1, Y2, Mo2),
  normalizeDay0(Y2, Mo2, D1, Y3, Mo3, D3).


normalizeDay0(Y1, Mo1, D1, Y3, Mo3, D3):-
  daysInMonth(Y1, Mo1, D1_max),
  (   D1 > D1_max
  ->  D2 is D1 - D1_max,
      Mo1_succ is Mo1 + 1,
      normalizeMonth(Y1, Mo1_succ, Y2, Mo2),
      normalizeDay0(Y2, Mo2, D2, Y3, Mo3, D3)
  ;   D1 < 0
  ->  Mo1_pred is Mo1 - 1,
      normalizeMonth(Y1, Mo1_pred, Y2, Mo2),
      daysInMonth(Y2, Mo2, D1_max),
      D2 is D1 + D1_max,
      normalizeDay0(Y2, Mo2, D2, Y3, Mo3, D3)
  ;   Y3 = Y1,
      Mo3 = Mo1,
      D3 = D1
  ).



%! normalizeMinute(
%!   +Y,     +Mo,     +D,     +H,     +Mi,
%!   -NormY, -NormMo, -NormD, -NormH, -NormMi
%! ) is det.
%
% Normalizes minute, hour, month, and year values to values that obey
% the appropriate constraints.
%
% Algorithm:
%
% - Add mi div 60 to hr.
%
% - Set mi to mi mod 60.
%
% - Add hr div 24 to da.
%
% - Set hr to  hr mod 24.
%
% - normalizeDay(yr, mo, da)

normalizeMinute(Y1, Mo1, D1, H1, Mi1, Y2, Mo2, D2, H2, Mi2):-
  H1a is H1 + Mi1 xsd_div 60,
  Mi2 is      Mi1 xsd_mod 60,
  D1a is D1 + H1a xsd_div 24,
  H2  is      H1a xsd_mod 24,
  normalizeDay(Y1, Mo1, D1a, Y2, Mo2, D2).



%! normalizeSecond(
%!   +Y,     +Mo,     +D,     +H,     +Mi,     +S:rational,
%!   -NormY, -NormMo, -NormD, -NormH, -NormMi, -NormS:rational
%! ) is det.
%
% Normalizes second, minute, hour, month, and year values to values
% that obey the appropriate constraints (ignoring leap seconds).
%
% Algorithm:
%
% - Add se div 60 to mi
%
% - Set se to se mod 60
%
% - normalizeMinute(yr, mo, da, hr, mi)

normalizeSecond(Y1, Mo1, D1, H1, Mi1, S1, Y2, Mo2, D2, H2, Mi2, S2):-
  Mi0 is Mi1 + S1 xsd_div 60,
  S2 is S1 xsd_mod 60,
  normalizeMinute(Y1, Mo1, D1, H1, Mi0, Y2, Mo2, D2, H2, Mi2).





% E.3.2 AUXILIARY FUNCTIONS %

%! daysInMonth(?Y, +Mo:between(1,12), -DaysInMonth:between(28,31)) is det.
%
% Returns the number of the last day of the month for any combination
% of year and month.
%
% # Algorithm
%
% Return:
%
%   - 28, when m is 2 and y is not evenly divisible by 4, or is evenly
%         divisible by 100 but not by 400, or is absent
%
%   - 29, when m is 2 and y is evenly divisible by 400, or is evenly
%         divisible by 4 but not by 100
%
%   - 30, when m is 4, 6, 9, or 11
%
%   - 31, otherwise (m is 1, 3, 5, 7, 8, 10, or 12)

% 28 when y is absent.
daysInMonth(Y, 2, 28):-
  var(Y), !.
% 28 when m is 2 and y is not evenly divisible by 4.
daysInMonth(Y, 2, 28):-
  Y rem 4 =\= 0, !.
% 28 when m is 2 and y is evenly divisible by 100 but not by 400.
daysInMonth(Y, 2, 28):-
  Y rem 100 =:= 0,
  Y rem 400 =\= 0, !.
% 29 when m is 2 and y is evenly divisible by 400.
daysInMonth(Y, 2, 29):-
  Y rem 400 =:= 0, !.
% 29 when m is 2 and y is evenly divisible by 4 but not by 100.
daysInMonth(Y, 2, 29):-
  Y rem 4 =:= 0,
  Y rem 100 =\= 0, !.
% 30 when m is 4, 6, 9, or 11.
daysInMonth(_, Month, 30):-
  memberchk(Month, [4,6,9,11]), !.
% 31 otherwise (m is 1, 3, 5, 7, 8, 10, or 12).
daysInMonth(_, _, 31).



%! newDatetime(
%!   ?Y,
%!   ?Mo:between(1,12),
%!   ?D:between(1,31),
%!   ?H:between(0,24),
%!   ?Mi:between(0,59),
%!   ?S:rational,
%!   ?Off:between(-840,840),
%!   -DT
%! ) is det.
%
% Returns an instance of the date/timeSevenPropertyModel with property
% values as specified in the arguments. If an argument is omitted, the
% corresponding property is set to absent.
%
% # Arguments
%
% @arg Y  An optional integer.
%
% @arg Mo An optional integer between 1 and 12 inclusive.
%
% @arg D  An optional integer between 1 and 31 inclusive.
%
% @arg H  An optional integer between 0 and 24 inclusive.
%
% @arg Mi An optional integer between 0 and 59 inclusive.
%
% @arg S  An optional decimal number greater than or equal to 0 and
%         less than 60.
%
% @arg Off An optional integer between −840 and 840 inclusive.
%
% # Algorithm
%
% Let:
%
%   - dt be an instance of the date/timeSevenPropertyModel.
%
%   - yr be Yr when Yr is not absent, otherwise 1.
%
%   - mo be Mo when Mo is not absent, otherwise 1.
%
%   - da be Da when Da is not absent, otherwise 1.
%
%   - hr be Hr when Hr is not absent, otherwise 0.
%
%   - mi be Mi when Mi is not absent, otherwise 0.
%
%   - se be Se when Se is not absent, otherwise 0.
%
% Steps:
%
%   - normalizeSecond(yr, mo, da, hr, mi, se)
%
%   - Set the year property of dt to absent when Yr is absent,
%     otherwise yr.
%
%   - Set the month property of dt to absent when Mo is absent,
%     otherwise mo.
%
%   - Set the day property of dt to absent when Da is absent,
%     otherwise da.
%
%   - Set the hour property of dt to absent when Hr is absent,
%     otherwise hr.
%
%   - Set the minute property of dt to absent when Mi is absent,
%     otherwise mi.
%
%   - Set the second property of dt to absent when Se is absent,
%     otherwise se.
%
%   - Set the timezoneOffset property of dt to Tz.
%
%   - Return dt.
%
% @tbd Add type checking.

newDatetime(
  Y1, Mo1, D1, H1, Mi1, S1, Off,
  date_time(Y4,Mo4,D4,H4,Mi4,S4,Off)
) :-
  % Set the values that are used for performing the normalization.
  defval(Y1, 1, Y2),
  defval(Mo1, 1, Mo2),
  defval(D1, 1, D2),
  defval(H1, 0, H2),
  defval(Mi1, 0, Mi2),
  defval(S1, 0, S2),
  normalizeSecond(Y2, Mo2, D2, H2, Mi2, S2, Y3, Mo3, D3, H3, Mi3, S3),
  % Variables stay variable.  Non-variables get the normalized value.
  var_or_val(Y1, Y3, Y4),
  var_or_val(Mo1, Mo3, Mo4),
  var_or_val(D1, D3, D4),
  var_or_val(H1, H3, H4),
  var_or_val(Mi1, Mi3, Mi4),
  var_or_val(S1, S3, S4).





% E.3.3 ADDING DURATIONS TO DATETIMES %

%! dateTimePlusDuration(+Duration, +DT1, -DT2) is det.
%
% Adds a du to a dt value, producing another date/time value.
%
% # Arguments
%
% @arg du A duration value.
%
% @arg dt A date/time value.
%
% # Result
%
% A date/time value.
%
% # Algorithm
%
% Let:
%
%   - yr be dt's year
%
%   - mo be dt's month
%
%   - da be dt's day
%
%   - hr be dt's hour
%
%   - mi be dt's minute
%
%   - se be dt's second
%
%   - tz be dt's timezoneOffset
%
% Steps:
%
%   - Add du's months to mo
%
%   - normalizeMonth(yr, mo), i.e., carry any over- or underflow,
%     adjust month.
%
%   - Set da to min(da, daysInMonth(yr, mo)), i.e., pin the value if
%     necessary.
%
%   - Add du's seconds to se.
%
%   - normalizeSecond(yr, mo, da, hr, mi, se), i.e., carry over- or
%     underflow of seconds up to minutes, hours, etc.
%
%   - Return newDateTime(yr, mo, da, hr, mi, se, tz) 

dateTimePlusDuration(
  duration(Mo0,S0),
  date_time(Y1,Mo1,D1,H1,Mi1,S1,Off),
  DT
) :-
  Mo2_ is Mo1 + Mo0,
  normalizeMonth(Y1, Mo2_, Y2, Mo2),
  daysInMonth(Y2, Mo2, DaysInMonth),
  D2 is min(D1, DaysInMonth),
  S2 is S1 + S0,
  normalizeSecond(Y2, Mo2, D2, H1, Mi1, S2, Y3, Mo3, D3, H3, Mi3, S3),
  newDateTime(Y3, Mo3, D3, H3, Mi3, S3, Off, DT).





% E.3.4 TIME ON TIMELINE %

% TIME ON TIMELINE FOR DATE/TIME SEVEN-PROPERTY MODEL DATATYPES %

%! timeOnTimeline(+DT, -Ss:rational) is det.
%
% Maps a date/timeSevenPropertyModel value to the decimal number
% representing its position on the “time line”.
%
% # Arguments
%
% @arg DT A date/timeSevenPropertyModel value.
%
% @arg Ss A decimal number.
%
% # Algorithm
%
% Let:
%
%   - yr be 1971, when dt's year is absent, and dt's year − 1
%     otherwise
%
%   - mo be 12 or dt's month, similarly
%
%   - da be daysInMonth(yr + 1, mo) − 1 or (dt's day) − 1,
%     similarly
%
%   - hr be 0 or dt's hour, similarly
%
%   - mi be 0 or dt's minute, similarly
%
%   - se be 0 or dt's second, similarly
%
% Steps:
%
%   - Subtract timezoneOffset from mi, when timezoneOffset is not
%     absent.
%
%   - (year)
%
%     - Set ToTl to 31536000 × yr
%
%   - (Leap-year Days, month, and day)
%
%     - Add 86400 × (yr div 400 − yr div 100 + yr div 4) to ToTl
%
%     - Add 86400 × Sum_{m < mo} daysInMonth(yr + 1, m) to ToTl
%
%     - Add 86400 × da to ToTl
%
%   - (hour, minute, and second)
%
%     - Add 3600 × hr + 60 × mi + se to ToTl
%
%   - Return ToTl

timeOnTimeline(datetime(Y1,Mo1,D1,H1,Mi1,S1,Off), N):-
  % yr be 1971 when dt's year is absent, and dt's year − 1 otherwise.
  (var(Y1) -> Y2 = 1971 ; Y2 is Y1 - 1),
  % mo be 12 or dt's month, similarly.
  defval(Mo1, 12, Mo2),
  % da be daysInMonth(yr + 1, mo) − 1 or (dt's day) − 1, similarly.
  Y2_succ is Y2 + 1,
  (   var(D1)
  ->  daysInMonth(Y2_succ, Mo2, D2_succ),
      D2 is D2_succ - 1
  ;   D2 is D1 - 1
  ),
  % hr be 0 or dt's hour, similarly.
  defval(0, H1),
  % mi be 0 or dt's minute, similarly.
  defval(0, Mi1),
  % se be 0 or dt's second, similarly.
  defval(0, S1),
  % Subtract timezoneOffset from mi when timezoneOffset is not absent.
  (var(Off) -> Mi2 = Mi1 ; Mi2 is Mi1 - Off),
  % Add 86400 × Sum_{m < mo} daysInMonth(yr + 1, m) to ToTl.
  Mo2_pred is Mo2 - 1,
  aggregate_all(
    sum(D_aggr),
    (
      between(1, Mo2_pred, Mo2_iterator),
      daysInMonth(Y2, Mo2_iterator, D_aggr)
    ),
    DaysInMonth
  ),
  N is % Set ToTl to 31536000 × yr.
       31536000 * Y2
       % Leap-year Days: add 86400 × (yr div 400 − yr div 100 + yr div
       % 4) to ToTl.
       + 86400 * ((Y2 xsd_div 400) - (Y2 xsd_div 100) + (Y2 xsd_div 4))
       % Month: add 86400 × Sum_{m < mo} daysInMonth(yr + 1, m) to ToTl
       + 86400 * DaysInMonth
       % Day: add 86400 × da to ToTl
       + 86400 * D2
       % Hour, minute, second: add 3600 × hr + 60 × mi + se to ToTl
       + 3600 * H1
       + 60 * Mi2
       + S1.





% E.3.5 LEXICAL MAPPINGS %

% PARTIAL DATE/TIME LEXICAL MAPPINGS %

%! yearFragValue(-Y)// is det.
%
% Maps a yearFrag//1, part of a date/timeSevenPropertyModel's lexical
% representation, onto an integer, presumably the year property of a
% date/timeSevenPropertyModel value.
%
% # Arguments
%
% @arg Year An integer.
%
% # Algorithm
%
% Return noDecimalMap(YR).

yearFragValue(YR) -->
  noDecimalMap(YR).



%! monthFragValue(-Mo)// is det.
%
% Maps a monthFrag//1, part of a date/timeSevenPropertyModel's lexical
% representation, onto an integer, presumably the month property of a
% date/timeSevenPropertyModel value.
%
% # Arguments
%
% @arg Month An integer.
%
% # Algorithm
%
% Return unsignedNoDecimalMap(MO).

monthFragValue(Mo) -->
  #(2, digit, Ds),
  {
    pos_sum(Ds, Mo),
    between(1, 12, Mo)
  }.



%! dayFragValue(-D)// is det.
%
% Maps a dayFrag//1, part of a date/timeSevenPropertyModel's lexical
% representation, onto an integer, presumably the day property of a
% date/timeSevenPropertyModel value.
%
% # Arguments
%
% @arg Day An integer.
%
% # Algorithm
%
% Return unsignedNoDecimalMap(DA).

dayFragValue(D) -->
  #(2, digit, Ds),
  {
    pos_sum(Ds, D),
    between(1, 31, D)
  }.



%! hourFragValue(-H)// is det.
%
% Maps a hourFrag//1, part of a date/timeSevenPropertyModel's lexical
% representation, onto an integer, presumably the hour property of a
% date/timeSevenPropertyModel value.
%
% # Arguments
%
% @arg Hour An integer.
%
% # Algorithm
%
% Return unsignedNoDecimalMap(HR).
%
% @bug This is more generic than the grammar, which requires the hour
%      to consist of exactly two digits.

hourFragValue(H) -->
  #(2, digit, Ds),
  {
    pos_sum(Ds, H),
    between(0, 23, H)
  }.



%! minuteFragValue(-Mi)// is det.
%
% Maps a minuteFrag//1, part of a date/timeSevenPropertyModel's
% lexical representation, onto an integer, presumably the minute
% property of a date/timeSevenPropertyModel value.
%
% # Arguments
%
% @arg Minute An integer.
%
% # Algorithm
%
% Return unsignedNoDecimalMap(MI).

minuteFragValue(Mi) -->
  #(2, digit, Ds),
  {
    pos_sum(Ds, Mi),
    between(0, 59, Mi)
  }.



%! secondFragValue(-S:rational)// is det.
%
% Maps a secondFrag//1, part of a date/timeSevenPropertyModel's
% lexical representation, onto a decimal number, presumably the second
% property of a date/timeSevenPropertyModel value.
%
% # Arguments
%
% @arg Second A decimal number.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalMap(SE), when no decimal point occurs in SE
%
%   - unsignedDecimalPtMap(SE)  otherwise

secondFragValue(S) -->
  unsignedDecimalPtMap(S), !,
  {
    0 =< S,
    S < 60
  }.
secondFragValue(S) -->
  unsignedNoDecimalMap(S),
  {between(0, 59, S)}.



%! timezoneFragValue(-Off:integer)// is det.
%
% Maps a timezoneFrag//1, part of a date/timeSevenPropertyModel's
% lexical representation, onto an integer, presumably the
% timezoneOffset property of a date/timeSevenPropertyModel value.
%
% # Arguments
%
% @arg Timezone An integer.
%
% # Algorithm
%
% TZ necessarily consists of either:
%
%    - just 'Z', or
%
%    - a sign ('+' or '-') followed by an instance H of hourFrag//1, a
%      colon, and an instance M of minuteFrag//1.
%
% Return:
%
%   - 0, when TZ is 'Z'
%
%   - −(unsignedDecimalPtMap(H) × 60 + unsignedDecimalPtMap(M)), when
%     the sign is '-'
%
%   - unsignedDecimalPtMap(H) × 60 + unsignedDecimalPtMap(M),
%     otherwise

timezoneFragValue(0) -->
  "Z", !.
timezoneFragValue(Off) -->
  ("-" -> {Sg = -1} ; "+" -> {Sg = 1}),
  hourFragValue(H),
  ":",
  minuteFragValue(Mi),
  {Off is copysign(H * 60 + Mi, Sg)}.



%! dateTimeLexicalMap(-DT)// is det.
%
% Maps a dateTimeLexicalRep//1 to a dateTime value.
%
% # Arguments
%
% @arg Date A complete dateTime value.
%
% # Algorithm
%
% LEX necessarily includes:
%
%   - substrings that are instances of yearFrag//1, monthFrag//1, and
%     dayFrag//1 (below referred to as Y, Mo, and D respectively)
%
%   - it also contains either:
%
%     - instances of hourFrag//1, minuteFrag//1, and secondFrag//1
%       (Y, MI, and S)
%
%     - or else an instance of endOfDayFrag//1
%
%   - finally, it may optionally contain an instance oftimezoneFrag//1
%     (T)
%
% Let tz be:
%
%   - timezoneFragValue(T), when T is present
%
%   - otherwise absent.
%
% Return:
%
%   - newDateTime(yearFragValue(Y), monthFragValue(MO),
%     dayFragValue(D), 24, 0, 0, tz), when endOfDayFrag//1 is present
%
%   - newDateTime(yearFragValue(Y), monthFragValue(MO),
%     dayFragValue(D), hourFragValue(H), minuteFragValue(MI),
%     secondFragValue(S), tz), otherwise

dateTimeLexicalMap(DT) -->
  yearFragValue(Y),
  "-",
  monthFragValue(Mo),
  "-",
  dayFragValue(D),
  "T",
  (   hourFragValue(H),
      ":",
      minuteFragValue(Mi),
      ":",
      secondFragValue(S)
  ;   endOfDayFrag(Y, Mi, S)
  ), !,
  opt(timezoneFragValue, Off),
  {newDatetime(Y, Mo, D, H, Mi, S, Off, DT)}.



%! timeLexicalMap(-DT)// is det.
%
% Maps a timeLexicalRep//1 to a time value.
%
% # Arguments
%
% @arg DT A complete time value.
%
% # Algorithm
%
% LEX necessarily includes:
%
%   - either:
%
%     - substrings that are instances of hourFrag//1, minuteFrag//1,
%       and secondFrag//1, (H, M, and S), or else
%
%     - an instance of endOfDayFrag//1; and
%
%   - optionally an instance of timezoneFrag//1 (T).
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return:
%
%   - newDateTime(absent, absent, absent, 0, 0, 0, tz), when
%     endOfDayFrag//1 is present
%
%   - newDateTime(absent, absent, absent, hourFragValue(H),
%     minuteFragValue(MI), secondFragValue(S), tz), otherwise

timeLexicalMap(DT) -->
  (   hourFragValue(H),
      ":",
      minuteFragValue(Mi),
      ":",
      secondFragValue(S)
  ;   endOfDayFrag(H, Mi, S)
  ), !,
  opt(timezoneFragValue, Off),
  {newDateTime(_, _, _, H, Mi, S, Off, DT)}.



%! dateLexicalMap(-DT)// is det.
%
% Maps a dateLexicalRep//1 to a date value.
%
% Arguments
%
% @arg Date A complete date value.
%
% # Algorithm
%
% LEX necessarily includes:
%
%   - hyphen-separated:
%
%     - an instance Y of yearFrag//1,
%
%     - an instance M of monthFrag//1, and
%
%     - an instance D of dayFrag//1,
%
%   - and optionally followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(yearFragValue(Y), monthFragValue(M),
% dayFragValue(D), absent, absent, absent, tz).

dateLexicalMap(DT) -->
  yearFragValue(Y),
  "-",
  monthFragValue(Mo),
  "-",
  dayFragValue(D),
  opt(timezoneFragValue, Off),
  {newDatetime(Y, Mo, D, _, _, _, Off, DT)}.



%! gYearMonthLexicalMap(-DT)// is det.
%
% Maps a gYearMonthLexicalRep//1 to a gYearMonth value.
%
% # Arguments
%
% @arg Date A complete gYearMonth value.
%
% # Algorithm
%
% LEX necessarily includes:
%
%   - an instance Y of yearFrag//1
%
%   - an instance M of monthFrag//1, hyphen-separated and
%
%   - optionally followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(yearFragValue(Y), monthFragValue(M), absent,
% absent, absent, absent, tz)

gYearMonthLexicalMap(DT) -->
  yearFragValue(Y),
  "-",
  monthFragValue(Mo),
  opt(timezoneFragValue, Off),
  {newDateTime(Y, Mo, _, _, _, _, Off, DT)}.



%! gYearLexicalMap(-DT)// is det.
%
% Maps a gYearLexicalRep//1 to a gYear value.
%
% # Arguments
%
% @arg DT A complete gYear value.
%
% # Algorithm
%
% LEX necessarily includes an instance Y of yearFrag//1, optionally
% followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(yearFragValue(Y), absent, absent, absent, absent,
% absent, tz)

gYearLexicalMap(DT) -->
  yearFragValue(Y),
  opt(timezoneFragValue, Off),
  {newDateTime(Y, _, _, _, _, _, Off, DT)}.



%! gMonthDayLexicalMap(-DT)// is det.
%
% Maps a gMonthDayLexicalRep//1 to a gMonthDay value.
%
% # Arguments
%
% @arg DT A complete gMonthDay value.
%
% # Algorithm
%
% LEX necessarily includes an instance M of monthFrag//1 and an
% instance D of dayFrag//1, hyphen-separated and optionally followed
% by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(absent, monthFragValue(M), dayFragValue(D),
% absent, absent, absent, tz)

gMonthDayLexicalMap(DT) -->
  "--",
  monthFragValue(Mo),
  "-",
  dayFragValue(D),
  opt(timezoneFragValue, Off),
  {newDateTime(_, Mo, D, _, _, _, Off, DT)}.



%! gDayLexicalMap(-DT)// is det.
%
% Maps a gDayLexicalRep//1 to a gDay value.
%
% # Arguments
%
% @arg DT A complete gDay value.
%
% #Algorithm
%
% LEX necessarily includes an instance D of dayFrag//1, optionally
% followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(gD, absent, absent, dayFragValue(D), absent,
% absent, absent, tz)
%
% Return newDateTime(absent, absent, dayFragValue(D), absent, absent,
% absent, tz)
%
% @bug First return line is erroneous.

gDayLexicalMap(DT) -->
  "---",
  dayFragValue(D),
  opt(timezoneFragValue, Off),
  {newDateTime(_, _, D, _, _, _, Off, DT)}.



%! gMonthLexicalMap(-DT)// is det.
%
% Maps a gMonthLexicalRep//1 to a gMonth value.
%
% # Arguments
%
% @arg Date A complete gMonth value.
%
% # Algorithm
%
% LEX necessarily includes an instance M of monthFrag//1, optionally
% followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(absent, monthFragValue(M), absent, absent,
% absent, absent, tz)

gMonthLexicalMap(DT) -->
  "--",
  monthFragValue(Mo),
  opt(timezoneFragValue, Off),
  {newDateTime(_, Mo, _, _, _, _, Off, DT)}.




% E.3.6 CANONICAL MAPPINGS %

% AUXILIARY FUNCTIONS FOR DATE/TIME CANONICAL MAPPINGS %

%! unsTwoDigitCanonicalFragmentMap(+Integer:between(0,99))// is det.
%
% Maps a nonnegative integer less than 100 onto an unsigned
% always-two-digit numeral.
%
% # Arguments
%
% @arg Integer A nonnegative integer less than 100
%
% # Algorithm
%
% Return digit(i div 10) & digit(i mod 10).

unsTwoDigitCanonicalFragmentMap(N) -->
  {N1 is N xsd_div 10},
  digit(N1),
  {N2 is N xsd_mod 10},
  digit(N2).



%! fourDigitCanonicalFragmentMap(+Integer:between(-9999,9999))// is det.
%
% Maps an integer between -10000 and 10000 onto an always-four-digit
% numeral.
%
% # Arguments
%
% @arg Integer An integer whose absolute value is less than 10000.
%
% # Algorithm
%
% Return:
%
%   - '-' & unsTwoDigitCanonicalFragmentMap(−i div 100) &
%     unsTwoDigitCanonicalFragmentMap(−i mod 100), when i is negative
%
%   - unsTwoDigitCanonicalFragmentMap(i div 100) &
%     unsTwoDigitCanonicalFragmentMap(i mod 100), otherwise

fourDigitCanonicalFragmentMap(N) -->
  ({N < 0} -> "-", {N0 is -N} ; {N0 = N}),
  {N1 is N0 xsd_div 100},
  unsTwoDigitCanonicalFragmentMap(N1),
  {N2 is N0 xsd_mod 100},
  unsTwoDigitCanonicalFragmentMap(N2).





% PARTIAL DATE/TIME CANONICAL MAPPINGS %

%! yearCanonicalFragmentMap(+Y)// is det.
%
% Maps an integer, presumably the year property of a
% date/timeSevenPropertyModel value, onto a yearFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @arg Year An integer.
%
% # Algorithm
%
% Return:
%
%   - noDecimalPtCanonicalMap(y), when |y| > 9999
%
%   - fourDigitCanonicalFragmentMap(y), otherwise

yearCanonicalFragmentMap(Y) -->
  {abs(Y) > 9999}, !,
  noDecimalPtCanonicalMap(Y).
yearCanonicalFragmentMap(Y) -->
  fourDigitCanonicalFragmentMap(Y).



%! monthCanonicalFragmentMap(+Mo:between(0,99))// is det.
%
% Maps an integer, presumably the month property of a
% date/timeSevenPropertyModel value, onto a monthFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @arg Month An integer between 1 and 12 inclusive.
%
% # Algorithm
%
% Return unsTwoDigitCanonicalFragmentMap(m).

monthCanonicalFragmentMap(Mo) -->
  {between(1, 12, Mo)},
  unsTwoDigitCanonicalFragmentMap(Mo).



%! dayCanonicalFragmentMap(+D:between(0,99))// is det.
%
% Maps an integer, presumably the day property of a
% date/timeSevenPropertyModel value, onto a dayFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @arg Day An integer between 1 and 31 inclusive (may be limited
%          further depending on associated year and month).
%
% # Algorithm
%
% Return unsTwoDigitCanonicalFragmentMap(d).

dayCanonicalFragmentMap(D) -->
  {between(1, 31, D)},
  unsTwoDigitCanonicalFragmentMap(D).



%! hourCanonicalFragmentMap(+H:between(0,99))// is det.
%
% Maps an integer, presumably the hour property of a
% date/timeSevenPropertyModel value, onto a hourFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @arg Hour An integer between 0 and 23 inclusive.
%
% # Algorithm
%
% Return unsTwoDigitCanonicalFragmentMap(h).

hourCanonicalFragmentMap(H) -->
  {between(0, 23, H)},
  unsTwoDigitCanonicalFragmentMap(H).



%! minuteCanonicalFragmentMap(+Mi:between(0,99))// is det.
%
% Maps an integer, presumably the minute property of a
% date/timeSevenPropertyModel value, onto a minuteFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @arg Minute An integer between 0 and 59 inclusive.
%
% # Algorithm
%
% Return unsTwoDigitCanonicalFragmentMap(m).

minuteCanonicalFragmentMap(Mi) -->
  {between(0, 59, Mi)},
  unsTwoDigitCanonicalFragmentMap(Mi).



%! secondCanonicalFragmentMap(+S:rational)// is det.
%
% Maps a decimal number, presumably the second property of a
% date/timeSevenPropertyModel value, onto a secondFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @arg Second A nonnegative decimal number less than 70.
%
% # Algorithm
%
% Return:
%
%   - unsTwoDigitCanonicalFragmentMap(s), when s is an integer
%
%   - unsTwoDigitCanonicalFragmentMap(s div 1) & '.' &
%     fractionDigitsCanonicalFragmentMap(s mod 1), otherwise

secondCanonicalFragmentMap(S) -->
  {integer(S)}, !,
  unsTwoDigitCanonicalFragmentMap(S).
secondCanonicalFragmentMap(S) -->
  {
    I is S xsd_div 1,
    between(0, 59, I)
  },
  unsTwoDigitCanonicalFragmentMap(I),
  ".",
  {Frac is S xsd_mod 1},
  fractionDigitsCanonicalFragmentMap(Frac).



%! timezoneCanonicalFragmentMap(+Off:between(-840,840))// is det.
%
% Maps an integer, presumably the timezoneOffset property of a
% date/timeSevenPropertyModel value, onto a timezoneFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @arg Off An integer between −840 and 840 inclusive.
%
% # Algorithm
%
% Return:
%
%   - 'Z', when t is zero
%
%   - '-' & unsTwoDigitCanonicalFragmentMap(−t div 60) & ':' &
%     unsTwoDigitCanonicalFragmentMap(−t mod 60), when t is negative
%
%   - '+' & unsTwoDigitCanonicalFragmentMap(t div 60) & ':' &
%     unsTwoDigitCanonicalFragmentMap(t mod 60), otherwise

timezoneCanonicalFragmentMap(0) --> !,
  "Z".
timezoneCanonicalFragmentMap(Off) -->
  {between(-840, 840, Off)},
  ({Off < 0} -> "-", {Off_abs = -Off} ; "+", {Off_abs = Off}),
  {H is Off_abs xsd_div 60},
  unsTwoDigitCanonicalFragmentMap(H),
  ":",
  {Mi is Off_abs xsd_mod 60},
  unsTwoDigitCanonicalFragmentMap(Mi).





% CANONICAL MAPPING %

%! dateTimeCanonicalMap(+DT)// is det.
%
% Maps a datetime value to a datetimeLexicalRep//1.
%
% # Arguments
%
% @arg Date A complete datetime value.
%
% # Algorithm
%
% Let DT be:
%
%   - yearCanonicalFragmentMap(dt's year) &
%
%   - '-' &
%
%   - monthCanonicalFragmentMap(dt's month) &
%
%   - '-' &
%
%   - dayCanonicalFragmentMap(dt's day) &
%
%   - 'T' &
%
%   - hourCanonicalFragmentMap(dt's hour) &
%
%   - ':' &
%
%   - minuteCanonicalFragmentMap(dt's minute) &
%
%   - ':' &
%
%   - secondCanonicalFragmentMap(dt's second)
%
% Return:
%
%   - DT, when dt's timezoneOffset is absent
%
%   - DT & timezoneCanonicalFragmentMap(dt's timezoneOffset),
%     otherwise

dateTimeCanonicalMap(date_time(Y,Mo,D,H,Mi,S,Off)) -->
  yearCanonicalFragmentMap(Y),
  "-",
  monthCanonicalFragmentMap(Mo),
  "-",
  dayCanonicalFragmentMap(D),
  "T",
  hourCanonicalFragmentMap(H),
  ":",
  minuteCanonicalFragmentMap(Mi),
  ":",
  secondCanonicalFragmentMap(S),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! timeCanonicalMap(+DT)// is det.
%
% Maps a time value to a timeLexicalRep//1.
%
% # Arguments
%
% @arg Time A complete time value.
%
% # Algorithm
%
% Let T be:
%
%   - hourCanonicalFragmentMap(ti's hour) &
%
%   - ':' &
%
%   - minuteCanonicalFragmentMap(ti's minute) &
%
%   - ':' &
%
%   - secondCanonicalFragmentMap(ti's second)
%
% Return:
%
%   - T, when ti's timezoneOffset is absent
%
%   - T & timezoneCanonicalFragmentMap(ti's timezoneOffset), otherwise

timeCanonicalMap(date_time(_,_,_,H,Mi,S,Off)) -->
  hourCanonicalFragmentMap(H),
  ":",
  minuteCanonicalFragmentMap(Mi),
  ":",
  secondCanonicalFragmentMap(S),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! dateCanonicalMap(+DT)// is det.
%
% Maps a date value to a dateLexicalRep//1.
%
% # Arguments
%
% @arg Date A complete date value.
%
% # Algorithm
%
% Let D be:
%
%   - yearCanonicalFragmentMap(da's year) &
%
%   - '-' &
%
%   - monthCanonicalFragmentMap(da's month) &
%
%   - '-' &
%
%   - dayCanonicalFragmentMap(da's day)
%
% Return:
%
%   - D, when da's timezoneOffset is absent
%
%   - D & timezoneCanonicalFragmentMap(da's timezoneOffset), otherwise

dateCanonicalMap(date_time(Y,Mo,D,_,_,_,Off)) -->
  yearCanonicalFragmentMap(Y),
  "-",
  monthCanonicalFragmentMap(Mo),
  "-",
  dayCanonicalFragmentMap(D),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gYearMonthCanonicalMap(+DT)// is det.
%
% Maps a gYearMonth value to a gYearMonthLexicalRep//1.
%
% # Arguments
%
% @arg YM A complete gYearMonth value.
%
% # Algorithm
%
% Let YM be
%
%   - yearCanonicalFragmentMap(ym's year) &
%
%   - '-' &
%
%   - monthCanonicalFragmentMap(ym's month)
%
% Return:
%
%   - YM, when ym's timezoneOffset is absent
%
%   - YM & timezoneCanonicalFragmentMap(ym's timezoneOffset),
%     otherwise

gYearMonthCanonicalMap(date_time(Y,Mo,_,_,_,_,Off)) -->
  yearCanonicalFragmentMap(Y),
  "-",
  monthCanonicalFragmentMap(Mo),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gYearCanonicalMap(+DT)// is det.
%
% Maps a gYear value to a gYearLexicalRep//1.
%
% # Arguments
%
% @arg DT A complete gYear value.
%
% # Algorithm
%
% Return:
%
%   - yearCanonicalFragmentMap(gY's year), when gY's timezoneOffset is
%     absent
%
%   - yearCanonicalFragmentMap(gY's year) &
%     timezoneCanonicalFragmentMap(gY's timezoneOffset), otherwise

gYearCanonicalMap(date_time(Y,_,_,_,_,_,Off)) -->
  yearCanonicalFragmentMap(Y),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gMonthDayCanonicalMap(+DT)// is det.
%
% Maps a gMonthDay value to a gMonthDayLexicalRep//1.
%
% # Arguments
%
% @arg DT A complete gMonthDay value.
%
% # Algorithm
%
% Let MD be:
%
%   - '--' &
%
%   - monthCanonicalFragmentMap(md's month) &
%
%   - '-' &
%
%   - dayCanonicalFragmentMap(md's day)
%
% Return:
%
%   - MD, when md's timezoneOffset is absent
%
%   - MD & timezoneCanonicalFragmentMap(md's timezoneOffset),
%     otherwise

gMonthDayCanonicalMap(date_time(_,Mo,D,_,_,_,Off)) -->
  "--",
  monthCanonicalFragmentMap(Mo),
  "-",
  dayCanonicalFragmentMap(D),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gDayCanonicalMap(+DT)// is det.
%
% Maps a gDay value to a gDayLexicalRep//1.
%
% # Arguments
%
% @arg Datetime A complete gDay value.
%
% # Algorithm
%
% Return:
%
%   - '---' & dayCanonicalFragmentMap(gD's day), when gD's
%     timezoneOffset is absent
%
%   - '---' & dayCanonicalFragmentMap(gD's day) &
%     timezoneCanonicalFragmentMap(gD's timezoneOffset), otherwise.

gDayCanonicalMap(date_time(_,_,D,_,_,_,Off)) -->
  "---",
  dayCanonicalFragmentMap(D),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gMonthCanonicalMap(+DT)// is det.
%
% Maps a gMonth value to a gMonthLexicalRep//1.
%
% # Arguments
%
% @arg DT A complete gMonth value.
%
% # Algorithm
%
% Return:
%
%   - '--' & monthCanonicalFragmentMap(gM's day), when gM's
%     timezoneOffset is absent
%
%   - '--' & monthCanonicalFragmentMap(gM's day) &
%     timezoneCanonicalFragmentMap(gM's timezoneOffset), otherwise.

gMonthCanonicalMap(date_time(_,Mo,_,_,_,_,Off)) -->
  "--",
  monthCanonicalFragmentMap(Mo),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).





% HELPERS %

%! defval(?FromVal, +DefVal, -ToVal) is det.
%
% Returns the given value, unless the given value is a variable.  In
% the latter case, the default value is returned instead.
%
% @note We are not usie defval/2 from library default here, because we
%       do not want variable occurrences of FromVal to get
%       instantiated.

defval(X, Y, Y):-
  var(X), !.
defval(X, _, X).




%! endOfDayFrag(-H, -Mi, -S:rational)// is det.
%
% ```ebnf
% endOfDayFrag ::= '24:00:00' ('.' '0'+)?
% ```
%
% @bug Missing from the standard.

endOfDayFrag(24, 0, 0) -->
  "24:00:00",
  ("." -> +("0") ; "").



%! var_or_val(+Arg, +Val, -VarOrVal) is det.
%
% Makes sure that a variable Arg returns a fresh variable, and that a
% non-variable Arg returns Val.

var_or_val(Arg, _, _):-
  var(Arg), !.
var_or_val(_, Val, Val).
