:- module(
  shacl,
  [
    shacl_arc/2,                   % -Arc, ?Graph
    shacl_assert_class/1,          % +Entry
    shacl_assert_class/2,          % +Entry, +Graph
    shacl_class/2,                 % -Class, ?Graph
    shacl_property_class/3,        % +Property, -Class, ?Graph
    shacl_property_path/3,         % +Property, -Path, ?Graph
    shacl_property_path_class/3,   % +Class, -Pair, ?Graph
    shacl_property_path_datatype/3 % +Class, -Parg, ?Graph
  ]
).

/** <module> SHACL

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).

:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- maplist(rdf_assert_prefix, [owl,rdf,sh]).

:- rdf_meta
   shacl_arc(-, r),
   shacl_assert_class(+, r),
   shacl_class(-, r),
   shacl_property_class(r, -, r),
   shacl_property_path(r, -, r),
   shacl_property_path_class(r, -, r),
   shacl_property_path_datatype(r, -, r).





%! shacl_arc(-Arc:compound, ?G:rdf_graph) is nondet.

shacl_arc(arc(C,[P],D), G) :-
  rdf_equal(P, rdfs:subClassOf),
  rdf_triple(C, P, D, G).
shacl_arc(arc(C,Ps,D), G) :-
  shacl_property_path_class(C, Ps-D, G).



%! shacl_assert_class(+Entry:compound) is det.
%! shacl_assert_class(+Entry:compound, +G:iri) is det.

shacl_assert_class(Entry) :-
  rdf_default_graph(G),
  shacl_assert_class(Entry, G).


shacl_assert_class(C-Groups, G) :-
  atom_concat(C, 'Shape', Shape),
  rdf_assert_triple(Shape, rdf:type, sh:'NodeShape', G),
  rdf_assert_triple(Shape, sh:targetClass, C, G),
  rdf_assert_list_triple(Shape, sh:ignoredProperties, [rdf:type], G),
  maplist(shacl_assert_property_(C, G), Groups).

shacl_assert_property_(C, G, P-Os) :-
  rdf_create_bnode(BNode),
  rdf_assert_triple(C, sh:property, BNode, G),
  rdf_assert_triple(BNode, sh:path, P, G),
  (   Os = [O]
  ->  rdf_assert_triple(BNode, sh:class, O, G)
  ;   maplist(shacl_assert_object_(G), Os, BNodes),
      rdf_assert_list_triple(BNode, sh:or, BNodes, G)
  ).

shacl_assert_object_(G, O, BNode) :-
  rdf_create_bnode(BNode),
  rdf_assert_triple(BNode, sh:class, O, G).



%! shacl_class(-C:iri, ?G:rdf_graph) is nondet.

shacl_class(C, G) :-
  rdf_triple(C, owl:oneOf, _, G).
%shacl_class(C, G) :-
%  rdf_triple(_, rdf:type, C, G).
shacl_class(C, G) :-
  rdf_triple(C, rdfs:subClassOf, _, G).
shacl_class(C, G) :-
  rdf_triple(_, rdfs:subClassOf, C, G).
shacl_class(C, G) :-
  rdf_triple(Shape, rdf:type, sh:'NodeShape', G),
  rdf_triple(Shape, sh:targetClass, C, G).



%! shacl_property_class(+Property, -C, ?G:rdf_graph) is det.

shacl_property_class(Property, C, G) :-
  rdf_triple(Property, sh:class, C, G).
shacl_property_class(Property, C, G) :-
  rdf_triple_list_member(Property, sh:or, S, G),
  rdf_triple(S, sh:class, C, G).



%! shacl_property_path(+Property, -Ps:list, ?G:rdf_graph) is det.

shacl_property_path(Property, Ps, G) :-
  rdf_triple(Property, sh:path, Path, G),
  aggregate_all(set(P), rdf_list_member(P, Path, G), Ps0),
  (Ps0 == [] -> Ps = [Path] ; Ps = Ps0).



%! shacl_property_path_class(+C:iri, -Pair:pair(list(rdf_predicate),rdf_class),
%!                           ?G:rdf_graph) is nondet.

shacl_property_path_class(C, Ps-D, G) :-
  rdf_triple(Shape, sh:targetClass, C, G),
  rdf_triple(Shape, sh:property, Property, G),
  shacl_property_class(Property, D, G),
  shacl_property_path(Property, Ps, G).



%! shacl_property_path_datatype(+C:iri, -Pair:pair(list(rdf_predicate),rdf_datatype),
%!                              ?G:rdf_graph) is nondet.

shacl_property_path_datatype(C, Ps-D, G) :-
  rdf_triple(Shape, sh:targetClass, C, G),
  rdf_triple(Shape, sh:property, Property, G),
  shacl_property_path(Property, Ps, G),
  rdf_triple(Property, sh:datatype, D, G).
