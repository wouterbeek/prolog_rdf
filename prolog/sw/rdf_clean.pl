:- module(
  rdf_clean,
  [
    rdf_clean_quad/3,  % +BNodePrefix, +Quad, -CleanQuad
    rdf_clean_triple/3 % +BNodePrefix, +Triple, -CleanTriple
  ]
).

/** <module> RDF cleaning

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(hash_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11), []).
:- use_module(library(settings)).

:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).





%! rdf_clean_bnode(+BNodePrefix:iri, +BNode:atom, -Iri:atom) is det.
%
% Blank node cleaning results in Skolemization / a well-known IRI.
%
% BNodePrefix must uniquely denote the document scope in which the
% blank node occurs.

rdf_clean_bnode(BNodePrefix, BNode, Iri) :-
  % The RDF parsers create long blank node labels that do not conform
  % to serialization grammars (e.g.,
  % '_:http://www.gutenberg.org/feeds/catalog.rdf.bz2#_:Description2').
  % We use MD5 hashes to (1) at least limit the maximum length a blank
  % node label can have, (2) ensure that the blank node label does not
  % violate serialization grammars, while (3) retaining the feature
  % that the same blank node in the source document receives the same
  % Skolemized well-known IRI.
  md5(BNode, Hash),
  atom_concat(BNodePrefix, Hash, Iri).



%! rdf_clean_graph(+G:rdf_graph, -CleanG:rdf_graph) is det.

rdf_clean_graph(G1, G3) :-
  rdf11:post_graph(G2, G1),
  (rdf_default_graph(G2) -> G3 = G2 ; rdf_clean_iri(G2, G3)).



%! rdf_clean_iri(+Iri:atom, -CleanIri:atom) is det.
%
% IRIs are assumed to the made absolute by the respective RDF parsers
% (that all take either option `base/1' or option `base_uri/1').  If
% this is not the case, the perform the following prior to cleaning:
%
% ```
% setting(rdf_term:base_uri, BaseUri),
% uri_resolve(Iri1, BaseUri, Iri2).
% ```

rdf_clean_iri(Iri, Iri).



%! rdf_clean_lexical_form(+D:atom, +Lex:atom, -CleanLex:atom) is semidet.

% rdf:HTML
rdf_clean_lexical_form(rdf:'HTML', Lex1, Lex2) :-
  rdf11:write_xml_literal(html, Lex1, Lex2).
% rdf:XMLLiteral
rdf_clean_lexical_form(rdf:'XMLLiteral', Lex1, Lex2) :-
  rdf11:write_xml_literal(xml, Lex1, Lex2).
% xsd:decimal
rdf_clean_lexical_form(xsd:decimal, Lex1, Lex2) :-
  (   string_phrase(decimalLexicalMap(Val), Lex1)
  ->  atom_phrase(decimalCanonicalMap(Val), Lex2)
  ;   print_message(warning, invalid_decimal(Lex1)),
      fail
  ).
% rdf:langString
rdf_clean_lexical_form(rdf:langString, Lex, Lex).
% other datatype IRIs
rdf_clean_lexical_form(D, Lex1, Lex2) :-
  catch(rdf11:out_type(D, Value, Lex1), E, true),
  rdf11:in_type(D, Value, D, Lex2),
  (   var(E)
  ->  (   % Warning for a non-canonical lexical form.
          Lex1 \== Lex2
      ->  print_message(warning, non_canonical_lexical_form(D,Lex1,Lex2))
      ;   true
      )
  ;   % Warning+failure for an incorrect lexical form.
      print_message(warning, E),
      fail
  ).



%! rdf_clean_literal(+Literal:compound, -CleanLiteral:compound) is semidet.

% language-tagged strings (rdf:langString)
rdf_clean_literal(literal(lang(LTag1,Lex)), literal(lang(LTag2,Lex))) :- !,
  downcase_atom(LTag1, LTag2),
  % Warning for a non-canonical language tag.
  (   LTag1 \== LTag2
  ->  print_message(warning, non_canonical_language_tag(LTag1))
  ;   true
  ).
% typed literals
rdf_clean_literal(literal(type(D,Lex1)), literal(type(D,Lex2))) :- !,
  rdf_clean_lexical_form(D, Lex1, Lex2).
% simple literals
rdf_clean_literal(literal(Lex), Literal) :-
  rdf_equal(D, xsd:string),
  rdf_clean_literal(literal(type(D,Lex)), Literal).



%! rdf_clean_nonliteral(+BNodePrefix:iri, +NonLiteral:atom, -CleanNonLiteral:atom) is det.

% blank node
rdf_clean_nonliteral(BNodePrefix, BNode, Iri) :-
  rdf_is_bnode(BNode), !,
  rdf_clean_bnode(BNodePrefix, BNode, Iri).
% IRI
rdf_clean_nonliteral(_, Iri1, Iri2) :-
  rdf_is_iri(Iri1), !,
  rdf_clean_iri(Iri1, Iri2).



%! rdf_clean_quad(+BNodePrefix:iri, +Quad:compound, -CleanQuad:compound) is semidet.

rdf_clean_quad(BNodePrefix, rdf(S1,P1,O1,G1), rdf(S2,P2,O2,G2)) :-
  rdf_clean_triple(BNodePrefix, rdf(S1,P1,O1), rdf(S2,P2,O2)),
  rdf_clean_graph(G1, G2).



%! rdf_clean_term(+BNodePrefix:iri, +Term:rdf_term, -CleanTerm:rdf_term) is det.

rdf_clean_term(BNodePrefix, Term1, Term2) :-
  rdf_clean_nonliteral(BNodePrefix, Term1, Term2), !.
rdf_clean_term(_, Literal1, Literal2) :-
  rdf_clean_literal(Literal1, Literal2).



%! rdf_clean_triple(+BNodePrefix:iri, +Triple:compound, -CleanTriple:compound) is semidet.

rdf_clean_triple(BNodePrefix, rdf(S1,P1,O1), rdf(S2,P2,O2)) :-
  rdf_clean_nonliteral(BNodePrefix, S1, S2),
  rdf_clean_iri(P1, P2),
  rdf_clean_term(BNodePrefix, O1, O2).
