:- module(dbpedia, []).

/** <module> DBpedia

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(http/html_write)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).


:- multifile
    qh:qh_literal_hook//2,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3.


qh:qh_literal_hook(Val^^D, _) -->
  {q_subdatatype_ofs(D, [dbt:minute,dbt:second])}, !,
  html("~G"-[Val]).


rdf11:in_ground_type_hook(D1, Val, Lex) :-
  q_subdatatype_ofs(D1, [dbt:minute,dbt:second]), !,
  rdf_equal(D2, xsd:float),
  rdf11:in_ground_type_hook(D2, Val, Lex).


rdf11:out_type_hook(D, Val, Lex) :-
  q_subdatatype_ofs(D, [dbt:minute,dbt:second]), !,
  rdf_equal(D2, xsd:float),
  rdf11:out_type_hook(D2, Val, Lex).



% HELPERS %

q_subdatatype_ofs(D1, Ds) :-
  q_memberchk(D2, Ds),
  q_subdatatype_of(D1, D2).
