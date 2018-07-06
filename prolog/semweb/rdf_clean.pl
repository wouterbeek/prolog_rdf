:- module(
  rdf_clean,
  [
    rdf_clean_quad/3,   % +BNodePrefix, +Quad, -CleanQuad
    rdf_clean_triple/3, % +BNodePrefix, +Triple, -CleanTriple
    rdf_clean_tuple/3   % +BNodePrefix, +Tuple, -CleanTuple
  ]
).

/** <module> RDF cleaning

@author Wouter Beek
@tbd `rdf:HTML' and `rdf:XMLLiteral' do not have a lexical form, and
     do not have a canonical mapping.
@version 2017-2018
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



%! rdf_clean_graph(+G:rdf_graph, -CleanG:rdf_graph) is semidet.

rdf_clean_graph(G1, G3) :-
  rdf11:post_graph(G2, G1),
  (   G2 == user
  ->  rdf_default_graph(G3)
  ;   rdf_default_graph(G2)
  ->  G3 = G2
  ;   rdf_clean_iri(G2, G3)
  ).



%! rdf_clean_iri(+Iri:atom, -CleanIri:atom) is semidet.
%
% IRIs are assumed to have been made absolute by the RDF parser prior
% to cleaning (through option `base/1' or `base_uri/1').  If this is
% not the case, then perform the following prior to cleaning:
%
% ```
% setting(rdf_term:base_uri, BaseUri),
% uri_resolve(Iri1, BaseUri, Iri2).
% ```

rdf_clean_iri(Iri, Iri) :-
  is_iri(Iri).



%! rdf_clean_lexical_form(+D:atom, +Lex:atom, -CleanLex:atom) is det.
%
% @throw syntax_error
% @throw type_error
% @throw unimplemented_datatype_iri

% language-tagged string
rdf_clean_lexical_form(rdf:langString, Lex, _) :- !,
  syntax_error(missing_language_tag(Lex)).
% rdf:HTML
rdf_clean_lexical_form(rdf:'HTML', Dom, Dom) :- !.
% rdf:XMLLiteral
rdf_clean_lexical_form(rdf:'XMLLiteral', Dom, Dom) :- !.
% typed literal
rdf_clean_lexical_form(D, Lex1, Lex2) :-
  rdf_lexical_value(D, Lex1, Value),
  rdf_lexical_value(D, Lex2, Value),
  % Emit a warning if the lexical form is not canonical.
  (   Lex1 \== Lex2
  ->  print_message(warning, rdf(non_canonical_lexical_form(D,Lex1,Lex2)))
  ;   true
  ).



%! rdf_clean_literal(+Literal:compound, -CleanLiteral:compound) is det.

% language-tagged string (rdf:langString)
rdf_clean_literal(literal(lang(LTag1,Lex)), literal(lang(LTag2,Lex))) :- !,
  downcase_atom(LTag1, LTag2),
  % Emit a warning if the language tag is not canonical.
  (   LTag1 \== LTag2
  ->  print_message(warning, rdf(non_canonical_language_tag(LTag1)))
  ;   true
  ).
% typed literal
rdf_clean_literal(literal(type(D1,Lex1)), literal(type(D2,Lex2))) :- !,
  rdf_clean_iri(D1, D2),
  rdf_clean_lexical_form(D2, Lex1, Lex2).
% simple literal
rdf_clean_literal(literal(Lex), Literal) :-
  rdf_equal(D, xsd:string),
  rdf_clean_literal(literal(type(D,Lex)), Literal).



%! rdf_clean_nonliteral(+BNodePrefix:iri, +NonLiteral:atom, -CleanNonLiteral:atom) is semidet.

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
  catch(
    (
      rdf_clean_triple_(BNodePrefix, rdf(S1,P1,O1), rdf(S2,P2,O2)),
      rdf_clean_graph(G1, G2)
    ),
    E,
    (
      print_message(warning, E),
      fail
    )
  ).



%! rdf_clean_term(+BNodePrefix:iri, +Term:rdf_term, -CleanTerm:rdf_term) is det.

rdf_clean_term(BNodePrefix, Term1, Term2) :-
  rdf_clean_nonliteral(BNodePrefix, Term1, Term2), !.
rdf_clean_term(_, Literal1, Literal2) :-
  rdf_clean_literal(Literal1, Literal2).



%! rdf_clean_triple(+BNodePrefix:iri, +Triple:compound, -CleanTriple:compound) is semidet.

rdf_clean_triple(BNodePrefix, Triple1, Triple2) :-
  catch(
    rdf_clean_triple_(BNodePrefix, Triple1, Triple2),
    E,
    (
      print_message(warning, E),
      fail
    )
  ).

rdf_clean_triple_(BNodePrefix, rdf(S1,P1,O1), rdf(S2,P2,O2)) :-
  rdf_clean_nonliteral(BNodePrefix, S1, S2),
  rdf_clean_iri(P1, P2),
  rdf_clean_term(BNodePrefix, O1, O2).



%! rdf_clean_tuple(+BNodePrefix:iri, +Tuple:compound, -CleanTuple:compound) is semidet.

% triple
rdf_clean_tuple(BNodePrefix, rdf(S,P,O), Triple) :- !,
  rdf_clean_triple(BNodePrefix, rdf(S,P,O), Triple).
% quadruple
rdf_clean_tuple(BNodePrefix, Quad, CleanQuad) :-
  rdf_clean_quad(BNodePrefix, Quad, CleanQuad).
