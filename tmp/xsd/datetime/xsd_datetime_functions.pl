  [
    xsd_datetime_compare/3 % -Order:oneof([incomparable,<,=,>])
                           % +Datetime1:datetime
                           % +Datetime2:datetime
  ]

%! xsd_datetime_compare(
%!   -Order:oneof([<,=,>]),
%!   +Datetime1:datetime,
%!   +Datetime2:datetime
%! ) is semidet.
% Fails only if the given values are incomparable.
%
% The ordering relation on the datetime value space.
%
% Datetime values are ordered according to their timeOnTimeline/2 values,
% except that if one value's =timezoneOffset= is absent
% and the other's is not,
% and using maximum and minimum =timezoneOffset= values
% for the one whose =timezoneOffset= is actually absent (i.e. -840 and 840)
% changes the resulting (strict) inequality,
% the original two values are incomparable.

xsd_datetime_compare(Order, datetime(Y1,Mo1,D1,H1,Mi1,S1,Off1), datetime(Y2,Mo2,D2,H2,Mi2,S2,Off2)):-
  (maplist(nonvar, [Off1,Off2]) ; maplist(var, [Off1,Off2])), !,
  timeOnTimeline(datetime(Y1,Mo1,D1,H1,Mi1,S1,Off1), N1),
  timeOnTimeline(datetime(Y2,Mo2,D2,H2,Mi2,S2,Off2), N2),
  compare(Order, N1, N2).
xsd_datetime_compare(Order, datetime(Y1,Mo1,D1,H1,Mi1,S1,_), datetime(Y2,Mo2,D2,H2,Mi2,S2,_)):-
  xsd_datetime_compare(Order, datetime(Y1,Mo1,D1,H1,Mi1,S1,-840), datetime(Y2,Mo2,D2,H2,Mi2,S2,Off2)),
  xsd_datetime_compare(Order, datetime(Y1,Mo1,D1,H1,Mi1,S1,840), datetime(Y2,Mo2,D2,H2,Mi2,S2,Off2)).
