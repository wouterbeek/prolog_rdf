:- module(
  rdf_date_time,
  [
    dt_to_rdf_date_time/3, % +Datetime1, +D, -Datetime2
    is_rdf_date_time/1,    % @Term
    rdf_assert_now/3,      % +S, +P, +G
    rdf_assert_now/4,      % +S, +P, +D, +G
    rdf_date_time_to_dt/2, % +Datetime1, -Datetime2
    rdf_dt/4               % ?S, ?P, -DataTime, ?G
  ]
).

/** <module> RDF date/time

Support for reading/writing date/time assertions in RDF.

@author Wouter Beek
@version 2017/08-2017/11
*/

:- use_module(library(date_time)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(sgml)).

:- multifile
    error:has_type/2.

error:has_type(rdf_date_time, date(Y,Mo,D)) :-
  error:has_type(date_time, date(Y,Mo,D)).
error:has_type(rdf_date_time, date_time(Y,Mo,D,H,Mi,S)) :-
  error:has_type(rdf_date_time, date(Y,Mo,D)),
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; (error:has_type(integer, S) ; error:has_type(float, S))).
error:has_type(rdf_date_time, month_day(Mo,D)) :-
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)),
  (var(D) -> true ; error:has_type(between(1,31), D)).
error:has_type(rdf_date_time, year_month(Y,Mo)) :-
  (var(Y) -> true ; error:has_type(integer, Y)),
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)).
error:has_type(rdf_date_time, time(H,Mi,S)) :-
  error:has_type(date_time, time(H,Mi,S)).

:- rdf_meta
   rdf_assert_now(o, r, r),
   rdf_assert_now(o, r, r, r),
   rdf_dt(r, r, -, r).





%! dt_to_rdf_date_time(+Datetime1:dt, +D:atom, -Datetime2:compound) is det.
%
% Converts date/time values to the representation supported by
% `semweb/rdf11'.

dt_to_rdf_date_time(dt(Y,Mo,D,_,_,_,_), xsd:date, date(Y,Mo,D)) :- !.
dt_to_rdf_date_time(dt(Y,Mo,D,H,Mi,S,_), xsd:dateTime, date_time(Y,Mo,D,H,Mi,S)) :- !.
dt_to_rdf_date_time(dt(_,_,D,_,_,_,_), xsd:gDay, D) :- !.
dt_to_rdf_date_time(dt(_,Mo,_,_,_,_,_), xsd:gMonth, Mo) :- !.
dt_to_rdf_date_time(dt(_,Mo,D,_,_,_,_), xsd:gMonthDay, month_day(Mo,D)) :- !.
dt_to_rdf_date_time(dt(Y,_,_,_,_,_,_), xsd:gYear, Y) :- !.
dt_to_rdf_date_time(dt(Y,Mo,_,_,_,_,_), xsd:gYearMonth, year_month(Y,Mo)) :- !.
dt_to_rdf_date_time(dt(_,_,_,H,Mi,S,_), xsd:time, time(H,Mi,S)).



%! is_rdf_date_time(@Term) is semidet.

is_rdf_date_time(Term) :-
  is_of_type(rdf_date_time, Term).



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



%! rdf_date_time_to_dt(+Datetime1:rdf_date_time, -Datetime2:dt) is det.
%
% Converts the five Semweb date/time representations to the one
% XSD-inspired 7-property model representation (type `dt`).
%
% Semweb uses the following five date/time representations (type
% `rdf_date_time`):
%
%   * date/3
%   * date_time/6
%   * month_day/2
%   * time/3
%   * year_month/2

rdf_date_time_to_dt(date(Y,Mo,D), dt(Y,Mo,D,_,_,_,0)) :- !.
rdf_date_time_to_dt(date_time(Y,Mo,D,H,Mi,S), dt(Y,Mo,D,H,Mi,S,0)) :- !.
rdf_date_time_to_dt(month_day(Mo,D), dt(_,Mo,D,_,_,_,0)) :- !.
rdf_date_time_to_dt(time(H,Mi,S), dt(_,_,_,H,Mi,S,0)) :- !.
rdf_date_time_to_dt(year_month(Y,Mo), dt(Y,Mo,_,_,_,_,0)).



%! rdf_dt(?S, ?P, -Datetime:dt, ?G) is nondet.

rdf_dt(S, P, DT, G) :-
  rdf(S, P, literal(type(D,Lex)), G),
  rdf11:xsd_date_time_type(D),
  xsd_time_string(DateTime, D, Lex),
  rdf_date_time_to_dt(DateTime, DT).
