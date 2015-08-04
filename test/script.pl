:- ensure_loaded(test).

:- use_module(library(debug)).
:- use_module(library(mat/mat)).
:- use_module(library(owl/owl_build)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- debug(mat(rdfs(_))).
:- debug(mat(owl(cax(_)))).
:- debug(mat(owl(cls(_)))).
:- debug(mat(owl(scm(_)))).

:- rdf_register_prefix(ex, 'http://example.com/').



%! script1 is det.

script1:-
  G = script1,
  fresh_iri(ex, I),
  rdf_assert_instance(I, ex:'C', G),
  rdf_print_graph(G),
  rdf_assert_literal(I, ex:age, xsd:nonNegativeInteger, 2, G),
  rdf_assert_now(I, ex:registrationDate, G),
  rdf_print_graph(G).


%! script2 is det.
%
% ```turtle
% ex:japan rdf:type ex:JapanEraRegion .
% ex:JapanEraRegion owl:equivalentClass [ owl:intersectionOf
% ( [ rdf:type owl:Restriction ;
%     owl:onProperty ex:hasEra ;
%     owl:hasValue ex:stageOne
%   ]
%   [ rdf:type owl:Restriction ;
%     owl:onProperty ex:hasEra ;
%     owl:hasValue ex:stageTwo
%   ]
% )
% ```

script2:-
  G = script2,
  rdf_bnode(D),
  rdf_assert(ex:'JapanEraRegion', owl:equivalentClass, D, G),
  owl_assert_value_restriction(ex:hasEra, ex:stageOne, G, R1),
  owl_assert_value_restriction(ex:hasEra, ex:stageTwo, G, R2),
  rdf_assert_list([R1,R2], Rs, G),
  rdf_assert(D, owl:intersectionOf, Rs, G),
  rdf_assert_instance(ex:japan, ex:'JapanEraRegion', G),
  rdf_print_graph(G, [abbr_list(true),logic_sym(true)]),
  mat(G).
