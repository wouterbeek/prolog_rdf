    rdf_assert_now/3,      % +S, +P, +G
    rdf_assert_now/4,      % +S, +P, +D, +G
    rdf_dt/4               % ?S, ?P, -DataTime, ?G

%! rdf_assert_now(+S, +P, +G) is det.
%! rdf_assert_now(+S, +P, +D, +G) is det.
%
% The default date/time datatype is `xsd:dateTime`.

rdf_assert_now(S, P, G) :-
  rdf_assert_now(S, P, xsd:dateTime, G).


rdf_assert_now(S, P, D, G) :-
  now(Now),
  dt_to_rdf_date_time(Now, D, Term),
  rdf_assert(S, P, Term, G).



%! rdf_dt(?S, ?P, -Datetime:dt, ?G) is nondet.

rdf_dt(S, P, DT, G) :-
  rdf_triple(S, P, literal(type(D,Lex)), G),
  rdf11:xsd_date_time_type(D),
  xsd_time_string(DateTime, D, Lex),
  rdf_date_time_to_dt(DateTime, DT).
