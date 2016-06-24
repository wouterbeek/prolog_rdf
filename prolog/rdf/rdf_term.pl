:- module(
  rdf_term,
  [
    rdf_bnode/2,     % ?B, ?G
    rdf_datatype/1,  % ?D
    rdf_datatype/2,  % ?D, ?G
    rdf_iri/2,       % ?Iri, ?G
    rdf_literal/2,   % ?Lit, ?G
    rdf_lts/1,       % ?Lit
    rdf_lts/2,       % ?Lit, ?G
    rdf_name/2,      % ?Name, ?G
    rdf_node/2,      % ?Node, ?G
    rdf_object/2,    % ?O, ?G
    rdf_predicate/2, % ?P, ?G
    rdf_subject/2,   % ?S, ?G
    rdf_term/2       % ?Term, ?G
  ]
).

/** <module> RDF term

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(z/z_term)).

:- rdf_meta
   rdf_bnode(?, r),
   rdf_datatype(r),
   rdf_datatype(r, r),
   rdf_iri(r, r),
   rdf_legacy_literal(o, r, -, -),
   rdf_literal(o, r),
   rdf_literal(o, r, ?, ?),
   rdf_literal_datatype(o, r),
   rdf_literal_lex(o, ?),
   rdf_literal_val(o, ?),
   rdf_lts(o),
   rdf_lts(o, r),
   rdf_name(o, r),
   rdf_node(o, r),
   rdf_subject(r, r),
   rdf_object(o, r),
   rdf_predicate(r, r),
   rdf_term(o, r).





%! rdf_bnode(?B, ?G) is nondet.

rdf_bnode(B, G) :-
  z_bnode(mem, B, G).



%! rdf_datatype(?D) is nondet.

rdf_datatype(D) :-
  z_datatype(mem, D).



%! rdf_datatype(?D, ?G) is nondet.

rdf_datatype(D, G) :-
  z_datatype(mem, D, G).



%! rdf_iri(?Iri, ?G) is nondet.

rdf_iri(Iri, G) :-
  z_iri(mem, Iri, G).



%! rdf_literal(?Lit, ?G) is nondet.

rdf_literal(Lit, G) :-
  z_literal(mem, Lit, G).



%! rdf_lts(?Lit) is nondet.

rdf_lts(Lit) :-
  z_lts(mem, Lit).



%! rdf_lts(?Lit, ?G) is nondet.

rdf_lts(Lit, G) :-
  z_lts(mem, Lit, G).



%! rdf_name(?Name, ?G) is nondet.

rdf_name(Name, G) :-
  z_name(mem, Name, G).



%! rdf_node(?Node, ?G) is nondet.

rdf_node(Node, G) :-
  z_node(mem, Node, G).



%! rdf_object(?O, ?G) is nondet.

rdf_object(O, G) :-
  z_object(mem, O, G).



%! rdf_predicate(?P, ?G) is nondet.

rdf_predicate(P, G) :-
  z_predicate(mem, P, G).



%! rdf_subject(?S, ?G) is nondet.

rdf_subject(S, G) :-
  z_subject(mem, S, G).



%! rdf_term(?Term, ?G) is nondet.

rdf_term(P, G) :-
  z_term(mem, P, G).
