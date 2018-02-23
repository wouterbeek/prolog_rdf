:- module(
  w3c_dtf,
  [
    cdtf//1, % +DT
    ldtf//1  % +DT
  ]
).

/** <module> W3C date/time format note

Year:
    YYYY (eg 1997)
Year and month:
    YYYY-MM (eg 1997-07)
Complete date:
    YYYY-MM-DD (eg 1997-07-16)
Complete date plus hours and minutes:
    YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
Complete date plus hours, minutes and seconds:
    YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
Complete date plus hours, minutes, seconds and a decimal fraction of a second:
    YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)

where:
  YYYY = four-digit year
  MM   = two-digit month (01=January, etc.)
  DD   = two-digit day of month (01 through 31)
  hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
  mm   = two digits of minute (00 through 59)
  ss   = two digits of second (00 through 59)
  s    = one or more digits representing a decimal fraction of a second
  TZD  = time zone designator (Z or +hh:mm or -hh:mm)

@author Wouter Beek
@compart NOTE-datetime
@see http://www.w3.org/TR/NOTE-datetime
@version 2015/11-2015/12, 2016/02
*/

:- use_module(library(date_time/date_time)).
:- use_module(library(dcg)).





cdtf(date_time(Y,Mo,D,H,Mi,S,Off)) -->
  cYYYY(Y),
  (   {var(Mo)}, !
  ;   "-", cMM(Mo),
      (   {var(D)}, !
      ;   "-", cDD(D),
          (   {var(H)}, !
          ;   "T", chh(H),
              (   {var(Mi)}, !
              ;   ":", cmm(Mi),
                  (   {var(S)}, !
                  ;   ":", cs(S)
                  )
              ),
              cTZD(Off)
          )
      )
  ).

cYYYY(Y)  --> {between(0, 9999, Y)}, yearCanonicalFragmentMap(Y).
lYYYY(Y)  --> noDecimalMap(Y), {between(0, 9999, Y)}.

cMM(Mo)   --> unsTwoDigitCanonicalFragmentMap(Mo).
lMM(Month) --> dcg_integer(#(2, digit), Month).

cDD(D)    --> dayCanonicalFragmentMap(D).
lDD(D)    --> dayFragValue(D).

chh(H)    --> hourCanonicalFragmentMap(H).
lhh(H)    --> hourFragValue(H).

cmm(Mi)   --> minuteCanonicalFragmentMap(Mi).
lmm(Mi)   --> minuteFragValue(Mi).

cs(S)     --> secondCanonicalFragmentMap(S).
ls(S)     --> secondFragValue(S).

cTZD(0)   --> !, "Z".
cTZD(Off) -->
  ({Off < 0} -> "-" ; "+"),
  {H is abs(Off) // 60}, chh(H), ":",
  {Mi is abs(Mi) mod 60}, cmm(Mi).
lTZD(Off) -->
  ("-" -> {Sg = -1} ; "+" -> {Sg = 1}),
  lhh(H), ":", lmm(Mi),
  {Off is Sg * ((H * 60) + Mi)}.

ldtf(date_time(Y,Mo,D,H,Mi,S,Off)) -->
  lYYYY(Y),
  (   "-"
  ->  lMM(Mo),
      (   "-"
      ->  lDD(D),
          (   "T"
          -> lhh(H), ":", lmm(Mi), (":" -> ls(S) ; ""), lTZD(Off)
          ;  ""
          )
      ;   ""
      )
  ;   ""
  ).
