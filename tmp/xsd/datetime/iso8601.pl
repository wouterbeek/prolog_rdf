:- module(
  iso8601,
  [
    'date-week'//1 % +Week:between(1,53)
  ]
).

/** <module> ISO 8601

@author Wouter Beek
@compat ISO 8601
@see Interpretation of ISO 8601 according to RFC 3339.
@version 2015/07, 2015/11
*/

:- use_module(library(dcg/dcg_cardinal)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(xsd/datetime/xsd_datetime_fragments)).
:- use_module(library(xsd/number/xsd_number_aux)).





%! 'date-century'(+Century:between(00,99))//
% ```abnf
% date-century = 2DIGIT   ; 00-99
% ```

'date-century'(Ce) -->
  unsTwoDigitCanonicalFragmentMap(Ce).



%! 'date-decade'(?Decade:between(0,9))// is det.
% ```abnf
% date-decade = DIGIT   ; 0-9
% ```

'date-decade'(De) -->
  'DIGIT'(De).



%! 'date-fullyear'(+Year:between(0,9999))// is det.
% ```abnf
% date-fullyear = date-century date-year
% ```

'date-fullyear'(FY) -->
  {xsd_number_aux:(Ce is FY xsd_div 100)},
  'date-century'(Ce),
  {xsd_number_aux:(Y is FY xsd_mod 100)},
  'date-year'(Y).



%! 'date-mday'(+Day:between(1,31))// is det.
% ```abnf
% date-mday = 2DIGIT   ; 01-28, 01-29, 01-30, 01-31 based on month/year
% ```

'date-mday'(MoD) -->
  unsTwoDigitCanonicalFragmentMap(MoD).



%! 'date-month'(+Month:between(1,12))// is det.
% ```abnf
% date-month = 2DIGIT   ; 01-12
% ```

'date-month'(Mo) -->
  unsTwoDigitCanonicalFragmentMap(Mo).



%! 'date-subdecade'(?Subdecade:between(0,9))// is det.
% ```abnf
% date-subdecade = DIGIT   ; 0-9
% ```

'date-subdecade'(SDe) -->
  'DIGIT'(SDe).



%! 'date-wday'(?Day:between(1,7))// is det.
% ```abnf
% date-wday = DIGIT   ; 1-7   ; 1 is Monday, 7 is Sunday
% ```

'date-wday'(D) -->
  between_digit(1, 7, D).



%! 'date-week'(+Week:between(1,53))// is det.
% ```abnf
% date-week = 2DIGIT   ; 01-52, 01-53 based on year
% ```

'date-week'(W) -->
  unsTwoDigitCanonicalFragmentMap(W).



%! 'date-yday'(+Day:between(1,366))// is det.
% ```abnf
% date-yday = 3DIGIT   ; 001-365, 001-366 based on year
% ```

'date-yday'(D) -->
  {xsd_number_aux:(D1 is D xsd_div 100)},
  'DIGIT'(D1),
  {xsd_number_aux:(D23 is D xsd_mod 100)},
  unsTwoDigitCanonicalFragmentMap(D23).



%! 'date-year'(+Year:between(0,99))// is det.
% ```abnf
% date-year = date-decade date-subdecade
% ```

'date-year'(Y) -->
  {xsd_number_aux:(De is Y xsd_div 10)},
  'date-decade'(De),
  {xsd_number_aux:(SDe is Y xsd_mod 10)},
  'date-subdecade'(SDe).



%! 'dateopt-century'(?Centrury:between())// is det.
% ```abnf
% dateopt-century = "-" / date-century
% ```

'dateopt-century'(Ce) -->
  {var(Ce)}, !,
  "-".
'dateopt-century'(Ce) -->
  'date-century'(Ce).



%! 'datepart-fullyear'(+Year:between(0,9999))// is det.
% ```abnf
% datepart-fullyear = [date-century] date-year ["-"]
% ```

'datepart-fullyear'(FY) -->
  {xsd_number_aux:(Ce is FY xsd_div 100)},
  (   {Ce =:= 0}
  ;   'date-century'(Ce)
  ),
  {xsd_number_aux:(Y is FY xsd_mod 100)},
  'date-year'(Y),
  ("-" ; "").



%! 'datepart-ptyear'(+Year:between(0,9999))// is det.
% ```abnf
% datepart-ptyear = "-" [date-subdecade ["-"]]
% ```

'datepart-ptyear'(Y) -->
  {xsd_number_aux:(SDe is Y xsd_mod 10)},
  "-",
  (   'date-subdecade'(SDe),
      ("-" ; "")
  ;   ""
  ).



%! 'datepart-wkyear'(+Year:between(0,9999))// is det.
% ```abnf
% datepart-wkyear = datepart-ptyear / datepart-fullyear
% ```

'datepart-wkyear'(Y) --> 'datepart-ptyear'(Y).
'datepart-wkyear'(Y) --> 'datepart-fullyear'(Y).
