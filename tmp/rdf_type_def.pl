:- multifile
    error:has_type/2.

error:has_type(rdf_bnode, B) :-
  rdf_is_bnode(B).
error:has_type(rdf_graph, G) :-
  (   G == default
  ;   error:has_type(iri, G)
  ).
error:has_type(rdf_literal, Lit) :-
  rdf_is_literal(Lit).
error:has_type(rdf_name, Name) :-
  (   error:has_type(iri, Name)
  ;   error:has_type(rdf_literal, Name)
  ).
error:has_type(rdf_tuple, Tuple) :-
  (   error:has_type(rdf_triple, Tuple)
  ;   error:has_type(rdf_quad, Tuple)
  ).
error:has_type(rdf_quad, Quad) :-
  Quad = rdf(S,P,O,G),
  error:has_type(rdf_term, S),
  error:has_type(iri, P),
  error:has_type(rdf_term, O),
  error:has_type(iri, G).
error:has_type(rdf_term, Term) :-
  (   error:has_type(rdf_bnode, Term)
  ;   error:has_type(rdf_literal, Term)
  ;   error:has_type(iri, Term)
  ).
error:has_type(rdf_triple, Triple) :-
  Triple = rdf(S,P,O),
  error:has_type(rdf_term, S),
  error:has_type(iri, P),
  error:has_type(rdf_term, O).
