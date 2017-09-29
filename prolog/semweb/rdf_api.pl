:- module(
  rdf_api,
  [
    rdf_clean_quad/2,            % +Quad1, -Quad2
    rdf_create_iri/3,            % +Prefix, +Path, -Iri
    rdf_create_well_known_iri/1, % -Iri
    rdf_is_skip_node/1,          % @Term
    rdf_is_well_known_iri/1,     % @Term
    rdf_literal/4,               % ?Literal, ?D, ?LTag, ?Lex
    rdf_load2/1,                 % +File
    rdf_query_term/2,            % +Term, -QueryTerm
    rdf_prefix_member/2,         % ?Elem, +L
    rdf_prefix_memberchk/2,      % ?Elem, +L
    rdf_save2/1,                 % +File
    rdf_save2/2,                 % +Type, +File
    rdf_save2/3,                 % +Type, +File, +G
    rdf_save2/4,                 % +Type, +File, +G, +Options
    rdf_save2/6,                 % +Type, +File, ?S, ?P, ?O, ?G
    rdf_save2/7,                 % +Type, +File, ?S, ?P, ?O, ?G, +Options
    rdf_term_to_atom/2,          % +Term, -Atom
    rdfs_range/3                 % ?P, ?C, ?G
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_load_db/1 as rdf_load_dump,
     rdf_save_db/1 as rdf_save_dump
   ]).
:- reexport(library(semweb/rdf11)).
:- reexport(library(semweb/rdf11_containers)).

/** <module> RDF API

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_prefix), []).
:- use_module(library(uri/uri_ext)).
:- use_module(library(uuid)).
:- use_module(library(xsd/xsd_number)).

:- rdf_register_prefix('_', 'https://example.org/.well-known/genid/').

:- rdf_meta
   rdf_clean_lexical_form(r, +, -),
   rdf_clean_literal(o, o),
   rdf_is_skip_node(r),
   rdf_is_well_known_iri(r),
   rdf_literal(o, r, ?, ?),
   rdf_prefix_member(t, t),
   rdf_prefix_memberchk(t, t),
   rdf_save(+, +, r),
   rdf_save(+, +, r, +),
   rdf_save(+, +, r, r, o, r),
   rdf_save(+, +, r, r, o, r, +),
   rdf_term_to_atom(o, -),
   rdfs_range(r, r, r).





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



%! rdf_clean_lexical_form(+D:atom, +Lex1:atom, -Lex2:atom) is semidet.

rdf_clean_lexical_form(rdf:'HTML', Lex1, Lex2) :-
  rdf11:write_xml_literal(html, Lex1, Lex2).
rdf_clean_lexical_form(rdf:'XMLLiteral', Lex1, Lex2) :-
  rdf11:write_xml_literal(xml, Lex1, Lex2).
rdf_clean_lexical_form(xsd:decimal, Lex1, Lex2) :-
  (   string_phrase(decimalLexicalMap(Val), Lex1)
  ->  atom_phrase(decimalCanonicalMap(Val), Lex2)
  ;   print_message(warning, invalid_decimal(Lex1)),
      fail
  ).
rdf_clean_lexical_form(D, Lex1, Lex2) :-
  catch(rdf11:out_type(D, Value, Lex1), E, true),
  rdf11:pre_ground_object(Value, literal(type(D,Lex2))),
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



%! rdf_clean_literal(+Literal1:compound, -Literal2:compound) is semidet.

% rdf:langString
rdf_clean_literal(literal(lang(LTag1,Lex)), literal(lang(LTag2,Lex))) :- !,
  downcase_atom(LTag1, LTag2),
  % Warning for a non-canonical language tag.
  (   LTag1 \== LTag2
  ->  print_message(warning, non_canonical_language_tag(LTag1))
  ;   true
  ).
rdf_clean_literal(literal(type(D,Lex1)), literal(type(D,Lex2))) :- !,
  rdf_clean_lexical_form(D, Lex1, Lex2).
rdf_clean_literal(literal(Lex), Literal) :-
  rdf_clean_literal(literal(type(xsd:string,Lex)), Literal).



%! rdf_clean_quad(+Quad1:compound, -Quad2:compound) is semidet.

rdf_clean_quad(rdf(S1,P1,O1,G1), rdf(S2,P2,O2,G2)) :-
  rdf_clean_nonliteral(S1, S2),
  rdf_clean_iri(P1, P2),
  rdf_clean_term(O1, O2),
  rdf_clean_graph(G1, G2).



%! rdf_clean_term(+Term1, -Term2) is det.

rdf_clean_term(Term1, Term2) :-
  rdf_clean_nonliteral(Term1, Term2), !.
rdf_clean_term(Literal1, Literal2) :-
  rdf_clean_literal(Literal1, Literal2).



%! rdf_clean_nonliteral(+Term1:atom, -Term2:atom) is det.

rdf_clean_nonliteral(BNode, Iri) :-
  rdf_is_bnode(BNode), !,
  rdf_clean_bnode(BNode, Iri).
rdf_clean_nonliteral(Iri1, Iri2) :-
  rdf_is_iri(Iri1), !,
  rdf_clean_iri(Iri1, Iri2).



%! rdf_create_iri(+Prefix:atom, +Segments:list(atom), -Iri:atom) is det.

rdf_create_iri(Prefix, Segments, Iri2) :-
  rdf_current_prefix(Prefix, Iri1),
  uri_comp_add(path, Iri1, Segments, Iri2).



%! rdf_create_well_known_iri(-Iri) is det.

rdf_create_well_known_iri(Iri) :-
  uuid(Id),
  rdf_global_id('_':Id, Iri).



%! rdf_literal(+Literal:compound, -D:atom, -LTag:atom, -Lex:atom) is det.
%! rdf_literal(-Literal:compound, +D:atom, +LTag:atom, +Lex:atom) is det.

rdf_literal(literal(lang(LTag,Lex)), rdf:langString, LTag, Lex) :-
  atom(LTag), !.
rdf_literal(literal(type(D,Lex)), D, _, Lex) :-
  atom(D), !.
rdf_literal(literal(Lex), _, _, Lex) :-
  atom(Lex).



%! rdf_is_skip_node(@Term) is semidet.

rdf_is_skip_node(Term) :-
  rdf_is_bnode(Term), !.
rdf_is_skip_node(Term) :-
  rdf_is_well_known_iri(Term).



%! rdf_is_well_known_iri(@Term) is semidet.

rdf_is_well_known_iri(Iri) :-
  uri_comps(Iri, uri(Scheme,Authority,['.well-known',genid|_],_,_)),
  ground(Scheme-Authority).



%! rdf_load2(+File:atom) is det.
%
% Loads RDF based in the file extension of File.

rdf_load2(File) :-
  file_name_extension(_, Extension, File),
  rdf_load_format_(Extension, Format),
  rdf_load(File, [format(Format)]).

rdf_load_format_(nq, nquads).
rdf_load_format_(nt, ntriples).
rdf_load_format_(rdf, xml).
rdf_load_format_(ttl, turtle).
rdf_load_format_(trig, trig).



%! rdf_prefix_member(?Elem, +L) is nondet.
%
% Calls member/2 under RDF prefix expansion.

rdf_prefix_member(Elem, L) :-
  member(Elem, L).



%! rdf_prefix_memberchk(?Elem, +L) is nondet.
%
% Calls memberchk/2 under RDF prefix expansion.

rdf_prefix_memberchk(Elem, L) :-
  memberchk(Elem, L).



%! rdf_query_term(+Term, -QueryTerm) is det.

rdf_query_term(Term, QueryTerm) :-
  Term =.. [Key,Value],
  ground(Value),
  rdf_term_to_atom(Value, Atom),
  QueryTerm =.. [Key,Atom].



%! rdf_save2(+File:atom) is det.
%! rdf_save2(+Type:oneof([quads,triples]), +File:atom) is det.
%! rdf_save2(+Type:oneof([quads,triples]), +File:atom, +G:atom) is det.
%! rdf_save2(+Type:oneof([quads,triples]), +File:atom, +G:atom,
%!           +Options:list(compound)) is det.
%! rdf_save2(+Type:oneof([quads,triples]), +File:atom, ?S, ?P, ?O, ?G) is det.
%! rdf_save2(+Type:oneof([quads,triples]), +File:atom, ?S, ?P, ?O, ?G,
%!           +Options:list(compound)) is det.
%
% Options are passed to call_to_file/3.

rdf_save2(File) :-
  rdf_save2(quads, File).


rdf_save2(Type, File) :-
  rdf_save2(Type, File, _).


rdf_save2(Type, File, G) :-
  rdf_save2(Type, File, G, []).


rdf_save2(Type, File, G, Options) :-
  rdf_save2(Type, File, _, _, _, G, Options).


rdf_save2(Type, File, S, P, O, G) :-
  rdf_save2(Type, File, S, P, O, G, []).


rdf_save2(Type, File, S, P, O, G, Options) :-
  call_to_file(File, rdf_save2_(Type, S, P, O, G), Options).

rdf_save2_(quads, S, P, O, G, Out, Metadata, Metadata) :- !,
  forall(rdf(S, P, O, G), write_nquad(Out, rdf(S,P,O,G))).
rdf_save2_(triples, S, P, O, G, Out, Metadata, Metadata) :-
  forall(rdf(S, P, O, G), write_ntriple(Out, rdf(S,P,O,G))).



%! rdf_term_to_atom(+Term, -Atom) is det.

rdf_term_to_atom(literal(type(D,Lex)), Atom) :- !,
  format(atom(Atom), '"~a"^^<~a>', [Lex,D]).
rdf_term_to_atom(literal(lang(LTag,Lex)), Atom) :- !,
  format(atom(Atom), '"~a"@~a', [Lex,LTag]).
rdf_term_to_atom(literal(Lex), Atom) :- !,
  rdf_term_to_atom(literal(type(xsd:string,Lex)), Atom).
rdf_term_to_atom(Atom, Atom) :-
  rdf_is_iri(Atom).



%! rdfs_range(?P, ?C, ?G) is nondet.

rdfs_range(P, C, G) :-
  rdf(P, rdfs:range, C, G).
rdfs_range(P, C, G) :-
  rdf(P, rdfs:subPropertyOf, Q, G),
  (P == Q -> print_message(warning, direct_cycle(P)) ; true),
  rdfs_range(Q, C, G).
