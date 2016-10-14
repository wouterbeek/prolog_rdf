%! xsd_duration_compare(
%!   -Order:oneof([<,=,>]),
%!   +Duration1:compound,
%!   +Duration2:compound
%! ) is semidet.
% Fails only if the given values are incomparable.
%
% Equality of duration is defined in terms of equality of datetime;
% order for duration is defined in terms of the order of datetime.
% Specifically, the equality or order of two duration values
% is determined by adding each duration in the pair
% to each of the following four datetime values:
%   * `1696-09-01T00:00:00Z`
%   * `1697-02-01T00:00:00Z`
%   * `1903-03-01T00:00:00Z`
%   * `1903-07-01T00:00:00Z`
%
% If two duration values are ordered the same way when added to
% each of these four datetime values, they will retain the same order
% when added to any other datetime values.
% @tbd Where's the proof for this?

xsd_duration_compare(Order, Dur1, Dur2):-
  newDatetime(1696, 9, 1, 0, 0, 0, 0, DTa),
  newDatetime(1697, 2, 1, 0, 0, 0, 0, DTb),
  newDatetime(1903, 3, 1, 0, 0, 0, 0, DTc),
  newDatetime(1903, 7, 1, 0, 0, 0, 0, DTd),
  
  datetimePlusDuration(Dur1, DTa, Dur1a),
  datetimePlusDuration(Dur1, DTb, Dur1b),
  datetimePlusDuration(Dur1, DTc, Dur1c),
  datetimePlusDuration(Dur1, DTd, Dur1d),

  datetimePlusDuration(Dur2, DTa, Dur2a),
  datetimePlusDuration(Dur2, DTb, Dur2b),
  datetimePlusDuration(Dur2, DTc, Dur2c),
  datetimePlusDuration(Dur2, DTd, Dur2d),
  
  xsd_datetime_compare(Order, Dur1a, Dur2a),
  xsd_datetime_compare(Order, Dur1b, Dur2b),
  xsd_datetime_compare(Order, Dur1c, Dur2c),
  xsd_datetime_compare(Order, Dur1d, Dur2d).



%! xsd_duration_sum(
%!   +Durations:list(compound),
%!   -SummedDuration:compound
%! ) is det.

xsd_duration_sum(Ds, D):-
  foldl(xsd_duration_sum, Ds, duration(0,0), D).

%! xsd_duration_sum(
%!   +Duration1:compound,
%!   +Duration2:compound,
%!   -SummedDuration:compound
%! ) is det.

xsd_duration_sum(duration(Mo1,S1), duration(Mo2, S2), duration(Mo3,S3)):-
  Mo3 is Mo1 + Mo2,
  S3 is S1 + S2.
