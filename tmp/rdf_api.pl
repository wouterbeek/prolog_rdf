:- module(
  rdf_api,
  [
    rdf2/3,                       % ?S, ?P, ?O
    rdf2/4,                       % ?S, ?P, ?O, ?G
    rdf_assert_list/2,            % +PrologList, -RdfList
    rdf_assert_list/3,            % +PrologList, -RdfList, +G
    rdf_assert_list/4,            % +S, +P, +PrologList, +G
    rdf_chk/4,                    % ?S, ?P, ?O, ?G
    rdf_clean_quad/2,             % +Quad1, -Quad2
    rdf_clean_triple/2,           % +Triple1, -Triple2
    rdf_create_graph/1,           % -G
    rdf_create_iri/3,             % +Prefix, +Path, -Iri
    rdf_deref_triple/2,           % +Uri, -Quads
    rdf_deref_triple/3,           % +Uri, -Quads, +Options
    rdf_label/2,                  % +Term, -Label
    rdf_label/3,                  % +Term, +P, -Label
    rdf_node/2,                   % ?Node, ?G
    rdf_query_term/2,             % +Term, -QueryTerm
    rdf_retractall/4,             % ?S, ?P, ?O, ?G
    rdf_retractall2/4,            % ?S, ?P, ?O, ?G
    rdf_subdatatype/2,            % ?D1, ?D2
    rdf_triple_term/2,            % +Triple, ?Term
    rdfs_instance/2,              % ?I, ?C
    rdfs_instance/3,              % ?I, ?C, ?G
    rdfs_range/2,                 % ?P, ?C
    rdfs_range/3,                 % ?P, ?C, ?G
    rdfs_subclass/2,              % ?C, ?D
    rdfs_subclass/3,              % ?C, ?D, ?G
    rdfs_subproperty/2,           % ?P, ?Q
    rdfs_subproperty/3            % ?P, ?Q, ?G
  ]).

/** <module> RDF API

@author Wouter Beek
@version 2017/09-2018/01
*/

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(call_ext)).
:- use_module(library(closure)).
:- use_module(library(dcg)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(http/http_header)).
:- use_module(library(lists)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin), []).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf11_containers)).
:- use_module(library(sgml)).
:- use_module(library(uri_ext)).
:- use_module(library(uuid)).
:- use_module(library(xml/xsd)).
:- use_module(library(xml/xsd_number)).

:- rdf_meta
   rdf2(r, r, o),
   rdf2(r, r, o, r),
   rdf_assert_list(+, -, r),
   rdf_assert_list(r, r, t, r),
   rdf_chk(r, r, o, r),
   rdf_clean_lexical_form(r, +, -),
   rdf_clean_literal(o, o),
   rdf_clean_quad(t, -),
   rdf_clean_triple(t, -),
   rdf_deref_triple(r, -),
   rdf_deref_triple(r, -, +),
   rdf_label(r, -),
   rdf_label(r, r, -),
   rdf_node(o, r),
   rdf_retractall(r, r, o, r),
   rdf_retractall2(r, r, o, r),
   rdf_subdatatype(r, r),
   rdf_triple_term(t, o),
   rdfs_instance(r, r),
   rdfs_instance(r, r, r),
   rdfs_range(r, r),
   rdfs_range(r, r, r),
   rdfs_subclass(r, r),
   rdfs_subclass(r, r, r),
   rdfs_subproperty(r, r),
   rdfs_subproperty(r, r, r).





%! rdf2(?S, ?P, ?O) is nondet.
%! rdf2(?S, ?P, ?O, ?G) is nondet.

rdf2(S, P, Term) :-
  rdf11:pre_object(Term, O),
  rdf_db:rdf(S, P, O),
  rdf11:post_object(Term, O).


rdf2(S, P, Term, G) :-
  rdf11:pre_object(Term, O),
  rdf(S, P, O, G),
  rdf11:post_object(Term, O).



%! rdf_chk(?S, ?P, ?O, ?G) is nondet.

rdf_chk(S, P, O, G) :-
  once(rdf(S, P, O, G)).



%! rdf_create_graph(-G:iri) is det.

rdf_create_graph(G) :-
  uuid(Id),
  rdf_global_id(ex:Id, G).



%! rdf_create_iri(+Alias:atom, +Segments:list(atom), -Iri:atom) is det.

rdf_create_iri(Alias, Segments, Iri2) :-
  rdf_prefix(Alias, Iri1),
  uri_comp_add(path, Iri1, Segments, Iri2).



%! rdf_deref_triple(+Uri:uri, -Triple:rdf_triple) is det.
%! rdf_deref_triple(+Uri:uri, -Triple:rdf_triple, +Options:list(compound)) is det.
%
% Options are passed to rdf_deref_uri/3.

rdf_deref_triple(Uri, Triple) :-
  rdf_deref_triple(Uri, Triple, []).


rdf_deref_triple(Uri, rdf(S,P,O), Options) :-
  setup_call_cleanup(
    rdf_create_graph(G),
    (
      rdf_deref_uri(Uri, rdf_deref_triples_(G), Options),
      rdf(S, P, O, G)
    ),
    rdf_retract_graph(G)
  ).

rdf_deref_triples_(G, Triples, _) :-
  maplist(rdf_deref_triple_(G), Triples).

rdf_deref_triple_(G, rdf(S,P,O)) :-
  rdf_assert(S, P, O, G).



%! rdf_label(+Term:rdf_term, -Label:string) is semidet.
%! rdf_label(+Term:rdf_term, +P:iri, -Label:string) is semidet.

rdf_label(Term, Label) :-
  rdf_label(Term, rdfs:label, Label).


rdf_label(Term, P, Label) :-
  aggregate_all(set(Literal), rdf(Term, P, Literal), Literals),
  Literals = [_|_], !,
  maplist(rdf_literal, _, LTags, _, Literals),
  include(ground, LTags, LRange),
  pairs_keys_values(Pairs, LRange, Literals),
  (   current_ltag(LRange, LTag)
  ->  memberchk(LTag-Literal, Pairs)
  ;   memberchk(Literal, Literals)
  ),
  rdf_literal_lexical_form(Literal, Lex),
  atom_string(Lex, Label).



%! rdf_query_term(+Term, -QueryTerm) is det.

rdf_query_term(Term, QueryTerm) :-
  Term =.. [Key,Value],
  ground(Value),
  rdf_atom_term(Atom, Value),
  QueryTerm =.. [Key,Atom].



%! rdf_retractall(?S, ?P, ?O, ?G) is det.

rdf_retractall(S, P, O, G) :-
  rdf11:pre_graph(G, G0),
  rdf_db:rdf_retractall(S, P, O, G0).



%! rdf_retractall2(?S, ?P, ?O, ?G) is det.

rdf_retractall2(S, P, Term, G) :-
  rdf11:pre_object(Term, O),
  rdf_retractall(S, P, O, G).



%! rdf_subdatatype(?D1:iri, ?D2:iri) is nondet.

rdf_subdatatype(D1, D2) :-
  xsd_subtype(D1, D2).
rdf_subdatatype(dbt:kilometer, xsd:double).



%! rdf_triple_term(+Triple:rdf_triple, +Term:rdf_term) is semidet.
%! rdf_triple_term(+Triple:rdf_triple, -Term:rdf_term) is nondet.

rdf_triple_term(rdf(S,_,_), S).
rdf_triple_term(rdf(_,P,_), P).
rdf_triple_term(rdf(_,_,O), O).



%! rdfs_instance(?I, ?C) is nondet.
%! rdfs_instance(?I, ?C, ?G) is nondet.

rdfs_instance(I, D) :-
  rdf(I, rdf:type, C),
  rdfs_subclass(C, D).


rdfs_instance(I, D, G) :-
  rdf(I, rdf:type, C, G),
  rdfs_subclass(C, D, G).



%! rdfs_range(?P, ?C) is nondet.
%! rdfs_range(?P, ?C, ?G) is nondet.

rdfs_range(P, C) :-
  ground(P), !,
  rdfs_subproperty(P, Q),
  rdf(Q, rdfs:range, C).
rdfs_range(P, C) :-
  rdf(Q, rdfs:range, C),
  rdfs_subproperty(P, Q).


rdfs_range(P, C, G) :-
  ground(P), !,
  rdfs_subproperty(P, Q, G),
  rdf(Q, rdfs:range, C, G).
rdfs_range(P, C, G) :-
  rdf(Q, rdfs:range, C, G),
  rdfs_subproperty(P, Q, G).



%! rdfs_subclass(?C, ?D) is nondet.
%! rdfs_subclass(?C, ?D, ?G) is nondet.

rdfs_subclass(C, D) :-
  path_closure0(rdfs_subclass_, C, D).

rdfs_subclass_(C, D) :-
  rdf(C, rdfs:subClassOf, D).


rdfs_subclass(C, D, G) :-
  path_closure0(rdfs_subclass_(G), C, D).

rdfs_subclass_(G, C, D) :-
  rdf(C, rdfs:subClassOf, D, G).



%! rdfs_subproperty(?P, ?Q) is nondet.
%! rdfs_subproperty(?P, ?Q, ?G) is nondet.

rdfs_subproperty(P, Q) :-
  path_closure0(rdfs_subproperty_, P, Q).

rdfs_subproperty_(P, Q) :-
  rdf(P, rdfs:subPropertyOf, Q).


rdfs_subproperty(P, Q, G) :-
  path_closure0(rdfs_subproperty_(G), P, Q).

rdfs_subproperty_(G, P, Q) :-
  rdf(P, rdfs:subPropertyOf, Q, G).
