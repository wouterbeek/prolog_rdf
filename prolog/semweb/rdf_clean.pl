:- encoding(utf8).
:- module(
  rdf_clean,
  [
    rdf_clean_quad/3,   % +BaseIri, +SwiQuad, -Quad
    rdf_clean_triple/3, % +BaseIri, +SwiTriple, -Triple
    rdf_clean_tuple/3   % +BaseIri, +SwiTuple, -Tuple
  ]
).

/** <module> RDF cleaning

*/

:- use_module(library(semweb/rdf11), []).

:- use_module(library(hash_ext)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(uri_ext)).

:- rdf_meta
   rdf_clean_quad(+, t, -),
   rdf_clean_triple(+, t, -),
   rdf_clean_tuple(+, t, -),
   rdf_clean_lexical_form(r, +, -).





%! rdf_clean_bnode(+BaseIri:iri, +BNode:rdf_bnode, -Iri:iri) is det.
%
% Blank node cleaning results in Skolemization / a well-known IRI.
%
% BNodePrefix must uniquely denote the document scope in which the
% blank node occurs.  For this we use the BaseIri argument.

rdf_clean_bnode(BaseIri, BNode, Iri) :-
  % The SWI-Prolog RDF parsers create long blank node labels that do
  % not conform to serialization grammars (e.g.,
  % ‘_:http://www.gutenberg.org/feeds/catalog.rdf.bz2#_:Description2’).
  % We use MD5 hashes to (1) at least limit the maximum length a blank
  % node label can have, (2) ensure that the blank node label does not
  % violate serialization grammars, while (3) retaining the feature
  % that the same blank node in the source document receives the same
  % Skolemized well-known IRI.
  md5(BaseIri-BNode, Hash),
  well_known_iri([Hash], Iri).



%! rdf_clean_graph(+GraphName:rdf_graph_name,
%!                 -CleanGraphName:rdf_graph_name) is semidet.

rdf_clean_graph(G1, G3) :-
  rdf11:post_graph(G2, G1),
  (   G2 == user
  ->  rdf11:rdf_default_graph(G3)
  ;   rdf11:rdf_default_graph(G2)
  ->  G3 = G2
  ;   rdf_clean_iri(G2, G3)
  ).



%! rdf_clean_iri(+Iri:atom, -CleanIri:rdf_iri) is semidet.
%
% IRIs are assumed to have been made absolute by the RDF parser prior
% to cleaning (through option `base/1' or `base_iri/1').  If this is
% not the case, then perform the following prior to cleaning:
%
% ```
% rdf_base_iri(BaseIri),
% uri_resolve(Iri1, BaseIri, Iri2).
% ```
%
% @tbd There is no implementation for the IRI grammar yet, so we use a
% conversion from IRIs to URIs, together with an implementation of the
% URI grammar.

rdf_clean_iri(Iri, Iri) :-
  atom(Iri).



%! rdf_clean_lexical_form(+D:rdf_iri, +Lex:atom, -CleanLex:atom) is det.

% language-tagged string
rdf_clean_lexical_form(rdf:langString, Lex, _) :- !,
  throw(error(rdf_error(missing_language_tag,Lex),rdf_clean_lexical_form/3)).
% typed literal
rdf_clean_lexical_form(D, Lex1, Lex2) :-
  rdf_lexical_value(D, Lex1, Value),
  rdf_lexical_value(D, Lex2, Value),
  % Emit a warning if the lexical form is not canonical.
  (   Lex1 \== Lex2
  ->  print_message(
        warning,
        error(
          rdf_error(non_canonical_lexical_form,D,Lex1,Lex2),
          rdf_clean_lexical_form/3
        )
      )
  ;   true
  ).



%! rdf_clean_literal(+Literal:rdf_literal, -CleanLiteral:rdf_literal) is det.

% language-tagged string (rdf:langString)
rdf_clean_literal(literal(lang(LTag1,Lex)), literal(lang(LTag2,Lex))) :- !,
  downcase_atom(LTag1, LTag2),
  % Emit a warning if the language tag is not canonical.
  (   LTag1 \== LTag2
  ->  print_message(
        warning,
        error(
          rdf_error(non_canonical_language_tag,LTag1),
          rdf_clean_literal/2
        )
      )
  ;   true
  ).
% typed literal
rdf_clean_literal(literal(type(D1,Lex1)), literal(type(D2,Lex2))) :- !,
  rdf_clean_iri(D1, D2),
  rdf_clean_lexical_form(D2, Lex1, Lex2).
% simple literal (RDF 1.0): quickly clean this to a typed literal (RDF 1.1).
rdf_clean_literal(literal(Lex), literal(type(D,Lex))) :-
  rdf_equal(xsd:string, D).



%! rdf_clean_node(+BaseIri:iri, +Node:rdf_node, -CleanNode:rdf_node) is det.

rdf_clean_node(BaseIri, Node1, Node2) :-
  rdf_clean_nonliteral(BaseIri, Node1, Node2), !.
rdf_clean_node(_, Literal1, Literal2) :-
  rdf_clean_literal(Literal1, Literal2).



%! rdf_clean_nonliteral(+BaseIri:iri,
%!                      +NonLiteral:or([rdf_bnode,rdf_iri]),
%!                      -CleanNonLiteral:or([rdf_bnode,rdf_iri])) is semidet.

% blank node
rdf_clean_nonliteral(BaseIri, BNode, Iri) :-
  rdf_is_bnode(BNode), !,
  rdf_clean_bnode(BaseIri, BNode, Iri).
% IRI
rdf_clean_nonliteral(_, Iri1, Iri2) :-
  rdf_is_iri(Iri1), !,
  rdf_clean_iri(Iri1, Iri2).



%! rdf_clean_quad(+BaseIri:iri, +SwiQuad, -Quad:rdf_quad) is semidet.

rdf_clean_quad(BaseIri, rdf(S1,P1,O1,G1), tp(S2,P2,O2,G2)) :-
  rdf_clean_triple(BaseIri, rdf(S1,P1,O1), tp(S2,P2,O2)),
  rdf_clean_graph(G1, G2).



%! rdf_clean_triple(+BaseIri:iri, +SwiTriple, -Triple:rdf_triple) is semidet.

rdf_clean_triple(BaseIri, rdf(S1,P1,O1), tp(S2,P2,O2)) :-
  rdf_clean_nonliteral(BaseIri, S1, S2),
  rdf_clean_iri(P1, P2),
  rdf_clean_node(BaseIri, O1, O2).



%! rdf_clean_tuple(+BaseIri:iri, +SwiTuple, -Tuple:rdf_tuple) is semidet.

% triple
rdf_clean_tuple(BaseIri, rdf(S,P,O), Triple) :- !,
  rdf_clean_triple(BaseIri, rdf(S,P,O), Triple).
% quadruple
rdf_clean_tuple(BaseIri, Quad, CleanQuad) :-
  rdf_clean_quad(BaseIri, Quad, CleanQuad).
