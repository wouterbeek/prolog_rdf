TODO:
  * lang_equal/2
  * lang_matches/2
  * rdf_estimate_complexity/4,
  * rdf_has/[3,4]
  * rdf_match_label/3
  * rdf_predicate_property/2
  * rdf_reachable/[3,5]
  * rdf_set_predicate/2
  * rdf_statistics/1
  * rdf_update/[4,5]
  * rdfs_class_property/2
  * rdfs_individual_of/2
  * rdfs_member/2
  * rdfs_subclass_of/2
  * rdfs_subproperty_of/2



# Type system

```prolog
:- multifile(error:has_type/2).
error:has_type(rdf_bnode, B) :-
  rdf_is_bnode(B).
error:has_type(rdf_graph, G) :-
  (   G == default
  ;   error:has_type(iri, G)
  ).
error:has_type(rdf_literal, Lit) :-
  rdf_is_literal(Lit).
error:has_type(rdf_name, N) :-
  (   error:has_type(iri, N)
  ;   error:has_type(rdf_literal, N)
  ).
error:has_type(rdf_statement, Stmt) :-
  (   error:has_type(rdf_triple, Stmt)
  ;   error:has_type(rdf_quadruple, Stmt)
  ).
error:has_type(rdf_quadruple, T) :-
  T = rdf(S,P,O,G),
  error:has_type(rdf_term, S),
  error:has_type(iri, P),
  error:has_type(rdf_term, O),
  error:has_type(iri, G).
error:has_type(rdf_term, T) :-
  (   error:has_type(rdf_bnode, T)
  ;   error:has_type(rdf_literal, T)
  ;   error:has_type(iri, T)
  ).
error:has_type(rdf_triple, T) :-
  T = rdf(S,P,O),
  error:has_type(rdf_term, S),
  error:has_type(iri, P),
  error:has_type(rdf_term, O).
```
