:- use_module(library(debug_ext)).
:- use_module(library(owl/id_store)).
:- use_module(library(owl/owl_api)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

script1:-
  rdf_assert3(ex:a, rdfs:label, literal(type(xsd:string,a))),
  rdf_assert3(ex:b, rdfs:label, literal(type(xsd:string,b))),
  rdf_assert3(ex:a, owl:sameAs, ex:b),
  rdf_assert3(ex:a, owl:sameAs, ex:c),
  rdf_assert3(ex:d, owl:sameAs, ex:c),
  
  Opts = [indent(2)],
  format('~nOWL DB:~n'),
  rdf_print3(Opts),

  format('~nID STORE:~n'),
  print_id_store(Opts),

  format('~nRDF DB:~n'),
  rdf_print_graph(user, Opts).
:- script1.
