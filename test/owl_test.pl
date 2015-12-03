:- use_module(library(debug_ext)).
:- use_module(library(owl/id_store)).
:- use_module(library(owl/owl_api)).
:- use_module(library(rdf/rdf_print)).

script1:-
  rdf_assert(ex:a, rdfs:label, literal(type(xsd:string,a))),
  rdf_assert(ex:b, rdfs:label, literal(type(xsd:string,b))),
  rdf_assert(ex:a, owl:sameAs, ex:b),
  rdf_assert(ex:a, owl:sameAs, ex:c),
  rdf_assert(ex:d, owl:sameAs, ex:c),
  
  Opts = [indent(2)],
  format('~nOWL DB:~n'),
  rdf_print3(Opts),

  format('~nID STORE:~n'),
  print_store(Opts),

  format('~nRDF DB:~n'),
  rdf_print_graph(user, Opts).
:- script1.
