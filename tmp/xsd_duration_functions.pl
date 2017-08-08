%! xsd_duration_compare(-Order:oneof([<,=,>]), +Duration1:compound,
%!                      +Duration2:compound) is semidet.
%
% Fails only if the given values are incomparable.
%
% Equality of duration is defined in terms of equality of datetime;
% order for duration is defined in terms of the order of datetime.
% Specifically, the equality or order of two duration values is
% determined by adding each duration in the pair to each of the
% following four datetime values:
%
%   * `1696-09-01T00:00:00Z`
%   * `1697-02-01T00:00:00Z`
%   * `1903-03-01T00:00:00Z`
%   * `1903-07-01T00:00:00Z`
%
% If two duration values are ordered the same way when added to each
% of these four datetime values, they will retain the same order when
% added to any other datetime values.
%
% @tbd Where's the proof for this?

xsd_duration_compare(Order, Duration1, Duration2):-
  newDatetime(1696, 9, 1, 0, 0, 0, 0, DTa),
  newDatetime(1697, 2, 1, 0, 0, 0, 0, DTb),
  newDatetime(1903, 3, 1, 0, 0, 0, 0, DTc),
  newDatetime(1903, 7, 1, 0, 0, 0, 0, DTd),
  
  datetimePlusDuration(Duration1, DTa, Duration1a),
  datetimePlusDuration(Duration1, DTb, Duration1b),
  datetimePlusDuration(Duration1, DTc, Duration1c),
  datetimePlusDuration(Duration1, DTd, Duration1d),

  datetimePlusDuration(Duration2, DTa, Duration2a),
  datetimePlusDuration(Duration2, DTb, Duration2b),
  datetimePlusDuration(Duration2, DTc, Duration2c),
  datetimePlusDuration(Duration2, DTd, Duration2d),
  
  xsd_datetime_compare(Order, Duration1a, Duration2a),
  xsd_datetime_compare(Order, Duration1b, Duration2b),
  xsd_datetime_compare(Order, Duration1c, Duration2c),
  xsd_datetime_compare(Order, Duration1d, Duration2d).



%! xsd_duration_sum(+Durations:list(compound), -Duration:compound) is det.

xsd_duration_sum(Durations, Duration):-
  foldl(xsd_duration_sum, Durations, duration(0,0), Duration).

%! xsd_duration_sum(+Duration1:compound, +Duration2:compound,
%!                  -Duration:compound) is det.

xsd_duration_sum(duration(Mo1,S1), duration(Mo2, S2), duration(Mo3,S3)):-
  Mo3 is Mo1 + Mo2,
  S3 is S1 + S2.
