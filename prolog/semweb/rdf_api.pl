:- module(
  rdf_api,
  [
    rdf_clean_quad/2,  % +Quad1, -Quad2
    rdf_literal/4,     % ?Literal, ?D, ?LTag, ?Lex
    rdf_query_term/2,  % +Term, -QueryTerm
    rdf_term_to_atom/2 % +Term, -Atom
  ]
).
:- reexport(library(semweb/rdf11)).

/** <module> RDF API

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_prefix), []).
:- use_module(library(xsd/xsd_number)).

:- rdf_meta
   rdf_literal(o, r, ?, ?),
   rdf_term_to_atom(o, -).





%! rdf_clean_bnode(+BNode:atom, -Iri:atom) is det.

rdf_clean_bnode(BNode, Iri) :-
  atomic_list_concat(L1, '_:', BNode),
  append(L2, [Local], L1),
  md5(L2, Hash),
  atomic_list_concat([Hash,Local], ':', BNodeLabel),
  rdf_global_id(bnode:BNodeLabel, Iri).



%! rdf_clean_graph(+G1, -G2) is det.

rdf_clean_graph(G1, G3) :-
  rdf11:post_graph(G2, G1),
  (G2 == user -> G3 = default ; rdf_clean_iri(G2, G3)).



%! rdf_clean_iri(+Uri1, -Uri2) is det.

rdf_clean_iri(Uri1, Uri2) :-
  uri_resolve(Uri1, 'https://example.org/', Uri2).



%! rdf_clean_object(+O1:term, -O2:term) is semidet.

rdf_clean_object(Literal1, Literal2) :-
  rdf_literal(Literal1, D, LTag1, Lex1), !,
  (   rdf_equal(rdf:'HTML', D)
  ->  rdf11:write_xml_literal(html, Lex1, Lex2)
  ;   rdf_equal(rdf:'XMLLiteral', D)
  ->  rdf11:write_xml_literal(xml, Lex1, Lex2)
  ;   rdf_equal(xsd:decimal, D)
  ->  (   string_phrase(decimalLexicalMap(Val), Lex1)
      ->  atom_phrase(decimalCanonicalMap(Val), Lex2)
      ;   print_message(warning, invalid_decimal(Lex1)),
          fail
      )
  ;   Lex2 = Lex1
  ),
  catch(
    (
      rdf11:post_object(Literal2, Literal1),
      rdf11:pre_object(Literal2, Literal3),
      rdf_literal(Literal3, D, LTag3, Lex3)
    ),
    E,
    true
  ),
  (   % Warn and fail for an incorrect lexical form.
      var(E)
  ->  (   % Warn for a non-canonical lexical form.
          Lex2 \== Lex3
      ->  print_message(warning, non_canonical_lexical_form(D,Lex2,Lex3))
      ;   true
      ),
      (   % Warn for a non-canonical language tag.
          ground(LTag1),
          LTag1 \== LTag3
      ->  print_message(warning, non_canonical_language_tag(LTag1))
      ;   true
      )
  ;   print_message(warning, E),
      fail
  ).
rdf_clean_object(BNode, Iri) :-
  rdf_is_bnode(BNode), !,
  rdf_clean_bnode(BNode, Iri).
rdf_clean_object(Iri1, Iri2) :-
  rdf_clean_iri(Iri1, Iri2).



%! rdf_clean_predicate(+P1:atom, -P2:atom) is det.

rdf_clean_predicate(P1, P2) :-
  rdf_clean_iri(P1, P2).



%! rdf_clean_quad(+Quad1:compound, -Quad2:compound) is semidet.

rdf_clean_quad(rdf(S1,P1,O1,G1), rdf(S2,P2,O2,G2)) :-
  rdf_clean_subject(S1, S2),
  rdf_clean_predicate(P1, P2),
  rdf_clean_object(O1, O2),
  rdf_clean_graph(G1, G2).



%! rdf_clean_subject(+S1:atom, -S2:atom) is det.

rdf_clean_subject(BNode, Iri) :-
  rdf_is_bnode(BNode), !,
  rdf_clean_bnode(BNode, Iri).
rdf_clean_subject(Iri1, Iri2) :-
  rdf_clean_iri(Iri1, Iri2).



%! rdf_literal(+Literal:compound, -D:atom, -LTag:atom, -Lex:atom) is det.
%! rdf_literal(-Literal:compound, +D:atom, +LTag:atom, +Lex:atom) is det.

rdf_literal(literal(lang(LTag,Lex)), rdf:langString, LTag, Lex) :-
  atom(LTag), !.
rdf_literal(literal(type(D,Lex)), D, _, Lex) :-
  atom(D), !.
rdf_literal(literal(Lex), _, _, Lex) :-
  atom(Lex).



%! rdf_query_term(+Term, -QueryTerm) is det.

rdf_query_term(Term, QueryTerm) :-
  Term =.. [Key,Value],
  ground(Value),
  rdf_term_to_atom(Value, Atom),
  QueryTerm =.. [Key,Atom].



%! rdf_term_to_atom(+Term, -Atom) is det.

rdf_term_to_atom(literal(type(D,Lex)), Atom) :- !,
  format(atom(Atom), '"~a"^^<~a>', [Lex,D]).
rdf_term_to_atom(literal(lang(LTag,Lex)), Atom) :- !,
  format(atom(Atom), '"~a"@~a', [Lex,LTag]).
rdf_term_to_atom(literal(Lex), Atom) :- !,
  rdf_term_to_atom(literal(type(xsd:string,Lex)), Atom).
rdf_term_to_atom(Atom, Atom) :-
  rdf_is_iri(Atom).
