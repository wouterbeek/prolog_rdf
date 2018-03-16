:- module(dbpedia, []).

/** <module> DBpedia

@author Wouter Beek
@version 2016/08, 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_term)).

:- multifile
    qh:qh_literal_hook//2,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3.

qh:qh_literal_hook(Val^^D, _) -->
  {rdf_subdatatype_ofs(D, [dbt:minute,dbt:second])}, !,
  html("~G"-[Val]).
qh:qh_literal_hook(Val^^D, _) -->
  {rdf_subdatatype_of(D, dbt:squareKilometre)}, !,
  {number_string(N, Val)},
  html([\number("~G", N)," km",sup("2")]).

rdf11:in_ground_type_hook(D1, Val, Lex) :-
  rdf_subdatatype_ofs(D1, [dbt:minute,dbt:second,dbt:squareKilometre]), !,
  rdf_equal(D2, xsd:float),
  rdf11:in_ground_type_hook(D2, Val, Lex).

rdf11:out_type_hook(D, Val, Lex) :-
  rdf_subdatatype_ofs(D, [dbt:minute,dbt:second,dbt:squareKilometre]), !,
  rdf_equal(D2, xsd:float),
  rdf11:out_type_hook(D2, Val, Lex).





% HELPERS %

%! rdf_subdatatype_ofs(+D, +Ds) is semidet.

rdf_subdatatype_ofs(D1, Ds) :-
  rdf_prefix_member(D0, Ds),
  rdf_global_id(D0, D2),
  rdf_subdatatype_of(D1, D2), !.
