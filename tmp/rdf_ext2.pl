:- module(
  rdf_ext,
  [
    hdt_call_on_file/2,              % +FileSpec, :Goal_1
    hdt_call_on_graph/2,             % +G, :Goal_1
    graph_file/2,                    % ?G, ?File
    rdf/5,                           % +M, ?S, ?P, ?O, ?G
    rdf_agent_image/4,               % +M, +Agent, -Image, ?G
    rdf_agent_name/4,                % +M, +Agent, -Name, ?G
    rdf_aggregate_all/3,             % +Templ, :Goal_0, -Result
    rdf_assert/1,                    % +Tuple
    rdf_assert/2,                    % +Triple, +G
    rdf_assert/5,                    % +M, +S, +P, +O, +G
    rdf_assert_action/5,             % +M, +ActionClass, +Actor, -Action, +G
    rdf_assert_agent/7,              % +M, +User, +Group, +Image, +GivenName,
                                     % +FamilyName, +G
    rdf_assert_objects/5,            % +M, +S, +P, +Os, +G
    rdf_bnode_iri/2,                 % +M, ?BNode
    rdf_bnode_iri/3,                 % +M, ?BNode, ?G
    rdf_cbd_quad/3,                  % +M, +Node,     -Quad
    rdf_cbd_quads/3,                 % +M, +Node,     -Quads
    rdf_cbd_triple/3,                % +M, +Node,     -Triple
    rdf_cbd_triple/4,                % +M, +Node, ?G, -Triple
    rdf_cbd_triples/3,               % +M, ?Node,     -Triples
    rdf_cbd_triples/4,               % +M, ?Node, ?G, -Triples
    rdf_chk_lexical_form/5,          % +M, ?S, ?P, -Lex, ?G
    rdf_clean_quad/2,                % +Quad1, -Quad2
    rdf_clean_tuple/2,               % +Tuple, -Quad
    rdf_creator/4,                   % +M, ?Resource, ?Agent, +G
    rdf_current_prefix/1,            % +Prefix
    rdf_endpoint_init/1,             % +Dict
    rdf_estimate/5,                  % +M, ?S, ?P, ?O, -Count
    rdf_estimate/6,                  % +M, ?S, ?P, ?O, ?G, -Count
    rdf_familyName/4,                % +M, +Agent, -FamilyName, ?G
    rdf_givenName/4,                 % +M, +Agent, -GivenName, ?G
    rdf_iri/2,                       % +M, ?Iri
    rdf_iri/3,                       % +M, ?Iri, ?G
    rdf_is_language_tagged_string/1, % @Term
    rdf_is_legacy_literal/1,         % @Term
    rdf_is_graph/1,                  % @Term
    rdf_list_memberchk/4,            % +M, ?L,  ?O,  ?G
    rdf_list_memberchk/5,            % +M, ?S,  ?P,  ?O, ?G
    rdf_literal/2,                   % +M, ?Literal
    rdf_literal/3,                   % +M, ?Literal, ?G
    rdf_literal/4,                   % ?Literal, ?D, ?Lexical, ?Lang
    rdf_literal_value/2,             % +Lit, ?Val
    rdf_name/2,                      % +M, ?Name
    rdf_name/3,                      % +M, ?Name, ?G
    rdf_node/2,                      % +M, ?Node
    rdf_node/3,                      % +M, ?Node, ?G
    rdf_object/2,                    % +M, ?O
    rdf_object/3,                    % +M, ?O, ?G
    rdf_predicate/2,                 % +M, ?P
    rdf_predicate/3,                 % +M, ?P, ?G
    rdf_pref_string/5,               % +M, ?S, ?P, -Lit, ?G
    rdf_pref_string/6,               % +M, ?S, ?P, +LRange, -Lit, ?G
    rdf_pref_string_lexical_form/5,  % +M, ?S, ?P, -Lex, ?G
    rdf_pref_string_lexical_form/6,  % +M, ?S, ?P, +LRange, -Lex, ?G
    rdf_query_term/2,                % +Term, -QueryTerm
    rdf_retractall/0,
    rdf_retractall/1,                % +Tuple
    rdf_root/2,                      % +M, ?Root
    rdf_root/3,                      % +M, ?Root, ?G
    rdf_scbd_quad/3,                 % +M, +Node,     -Quad
    rdf_scbd_quads/3,                % +M, +Node,     -Quads
    rdf_scbd_triple/3,               % +M, +Node,     -Triple
    rdf_scbd_triple/4,               % +M, +Node, ?G, -Triple
    rdf_scbd_triples/3,              % +M, ?Node,     -Triples
    rdf_scbd_triples/4,              % +M, ?Node, ?G, -Triples
    rdf_statistic/4,                 % +M, +Key, -Value, ?G
    rdf_subdatatype_of/2,            % ?Sub, ?Super
    rdf_subject/2,                   % +M, ?S
    rdf_subject/3,                   % +M, ?S, ?G
    rdf_term/2,                      % +M, ?Term
    rdf_term/3,                      % +M, ?Term, ?G
    rdf_tuple_quad/3,                % +Tuple, +G, -Quad
    rdf_tuple_triple/2               % +Tuple, ?Triple
  ]
).

/** <module> RDF extensions

@author Wouter Beek
@version 2017/04-2017/09
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(default)).
:- use_module(library(dict)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(lists)).
:- use_module(library(nb_set)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_date_time)).
:- use_module(library(semweb/rdf_guess)).
:- use_module(library(solution_sequences)).
:- use_module(library(uri_ext)).
:- use_module(library(xml/xsd)).
:- use_module(library(yall)).

:- debug(clean_tuple).

:- meta_predicate
    hdt_call_on_file(+, 1),
    hdt_call_on_graph(?, 1),
    rdf_aggregate_all(+, 0, -).

:- multifile
    http:map_exception_to_http_status_hook/4,
    user:message_hook/3.

http:map_exception_to_http_status_hook(
  error(existence_error(hdt_graph,G),_),
  not_found(G),
  [connection(close)],
  []
).

user:message_hook(non_canonical_lexical_form('http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML',_,_), _, _).
user:message_hook(non_canonical_lexical_form('http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',_,_), _, _).
user:message_hook(non_canonical_lexical_form('http://www.w3.org/2001/XMLSchema#double',_,_), _, _).
user:message_hook(non_canonical_lexical_form('http://www.w3.org/2001/XMLSchema#float',_,_), _, _).

:- rdf_meta
   graph_file(r, -),
   ntriples_to_nquads(+, t, +),
   rdf(+, r, r, o, r),
   rdf_agent_image(+, r, -, r),
   rdf_agent_name(+, r, -, r),
   rdf_aggregate_all(+, t, -),
   rdf_assert(t),
   rdf_assert(t, r),
   rdf_assert(+, r, r, o, r),
   rdf_assert_action(+, r, r, -, r),
   rdf_assert_agent(+, r, r, +, +, +, r),
   rdf_assert_objects(+, r, r, t, r),
   rdf_cbd_quad(?, o, -),
   rdf_cbd_quads(?, o, -),
   rdf_cbd_triple(?, o, -),
   rdf_cbd_triple(?, o, r, -),
   rdf_cbd_triples(?, o, -),
   rdf_cbd_triples(?, o, r, -),
   rdf_chk_lexical_form(+, r, r, -, r),
   rdf_creator(+, r, r, r),
   rdf_estimate(+, r, r, o, -),
   rdf_estimate(+, r, r, o, -, r),
   rdf_familyName(+, r, -, r),
   rdf_givenName(+, r, -, r),
   rdf_iri(?, r),
   rdf_iri(?, r, r),
   rdf_is_legacy_literal(o),
   rdf_is_graph(r),
   rdf_is_language_tagged_string(o),
   rdf_is_real_iri(r),
   rdf_list_memberchk(+, r, o, r),
   rdf_list_memberchk(+, r, r, o, r),
   rdf_literal(?, o),
   rdf_literal(?, o, r),
   rdf_literal(o, r, ?, ?),
   rdf_literal_value(o, ?),
   rdf_name(?, o),
   rdf_name(?, o, r),
   rdf_node(?, o),
   rdf_node(?, o, r),
   rdf_object(?, o),
   rdf_object(?, o, r),
   rdf_predicate(?, r),
   rdf_predicate(?, r, r),
   rdf_pref_string(+, r, r, -, r),
   rdf_pref_string(+, r, r, +, -, r),
   rdf_pref_string_lexical_form(+, r, r, -, r),
   rdf_pref_string_lexical_form(+, r, r, +, -, r),
   rdf_retractall(t),
   rdf_root(+, r),
   rdf_root(+, r, r),
   rdf_scbd_quad(?, o, -),
   rdf_scbd_quads(?, o, -),
   rdf_scbd_triple(?, o, -),
   rdf_scbd_triple(?, o, r, -),
   rdf_scbd_triples(?, o, -),
   rdf_scbd_triples(?, o, r, -),
   rdf_statistic(+, +, -, r),
   rdf_subdatatype_of(r, r),
   rdf_subject(?, r),
   rdf_subject(?, r, r),
   rdf_term(?, o),
   rdf_term(?, o, r),
   rdf_tuple_quad(t, r, t),
   rdf_tuple_triple(t, t).





%! graph_file(+G, -File) is det.

graph_file(G, File) :-
  rdf_prefix_iri(graph:Local, G),
  absolute_file_name(data(Local), File, [access(write),extensions([nt])]).



%! hdt_call_on_file(+FileSpec:term, :Goal_1) is det.

hdt_call_on_file(FileSpec, Goal_1) :-
  absolute_file_name(FileSpec, File, [access(read)]),
  setup_call_cleanup(
    hdt_open(File, [handle(Hdt)]),
    call(Goal_1, Hdt),
    hdt_close(Hdt)
  ).



%! hdt_call_on_graph(+G:atom, :Goal_1) is det.

hdt_call_on_graph(G, Goal_1) :-
  hdt_graph(Hdt, G),
  call(Goal_1, Hdt).



%! rdf(+M, ?S, ?P, ?O, ?G) is nondet.

rdf(hdt, S, P, O, G) :-
  hdt(S, P, O, G).
rdf(trp, S, P, O, G) :-
  rdf(S, P, O, G).



%! rdf_agent_gravatar(+M, +Agent, -Uri, +G) is det.

rdf_agent_gravatar(M, Agent, Uri, G) :-
  rdf_chk(M, Agent, foaf:mbox, EMail^^xsd:anyURI, G),
  downcase_atom(EMail, CanonicalEMail),
  md5(CanonicalEMail, Hash),
  uri_comps(Uri, uri(http,'www.gravatar.com',[avatar,Hash],_,_)).



%! rdf_agent_image(+M, +Agent, -Image, ?G) is det.

rdf_agent_image(M, Agent, Image, G) :-
  rdf_chk(M, Agent, foaf:depiction, Image^^xsd:anyURI, G), !.
rdf_agent_image(M, Agent, Image, G) :-
  rdf_agent_gravatar(M, Agent, Image, G).



%! rdf_agent_name(+M, +Agent, -Name, ?G) is det.

rdf_agent_name(M, Agent, Name, G) :-
  rdf_givenName(M, Agent, GivenName0, G),
  rdf_familyName(M, Agent, FamilyName0, G), !,
  maplist(rdf_literal, [GivenName0,FamilyName0], _, _, Names),
  atomics_to_string(Names, " ", Name).
rdf_agent_name(M, Agent, Name, G) :-
  rdf_pref_string(M, Agent, foaf:name, Name0, G),
  rdf_literal(Name0, _, _, Name).



%! rdf_aggregate_all(+Templ, :Goal_0, -Result) is det.
%
% @see aggregate_all/3 with RDF prefix expansion applied.

rdf_aggregate_all(Templ, Goal_0, Result) :-
  aggregate_all(Templ, Goal_0, Result).



%! rdf_assert(+Tuple) is det.
%! rdf_assert(+Triple, +G) is det.
%! rdf_assert(+M, +S, +P, +O, +G) is det.

rdf_assert(rdf(S,P,O)) :- !,
  rdf_assert(S, P, O).
rdf_assert(rdf(S,P,O,G)) :-
  rdf_assert(S, P, O, G).


rdf_assert(rdf(S,P,O), G) :-
  rdf_assert(S, P, O, G).


rdf_assert(stream(Out), S, P, O, G) :-
  ground(Out),
  (   is_uri(G)
  ->  write_nquad(Out, S, P, O, G)
  ;   write_ntriple(Out, S, P, O)
  ).
rdf_assert(trp, S, P, O, G) :-
  rdf_assert(S, P, O, G).



%! rdf_assert_action(+M, +ActionClass, +Actor, -Action, +G) is det.

rdf_assert_action(M, ActionClass, Actor, Action, G):-
  fresh_uri(Action, uri(_,_,[action],_,_)),
  rdf_assert(M, Action, rdf:type, ActionClass, G),
  rdf_assert_now(M, Action, prov:atTime, G),
  rdf_assert(M, Action, prov:wasAssociatedWith, Actor, G).



%! rdf_assert_agent(+M, +User, +Group, +Image, +GivenName, +FamilyName,
%!                  +G) is det.

rdf_assert_agent(M, User, Group, Image, GivenName, FamilyName, G) :-
  rdf_assert(M, User, rdf:type, Group, G),
  rdf_assert(M, User, foaf:depiction, Image^^xsd:anyURI, G),
  rdf_assert(M, User, foaf:familyName, FamilyName@nl, G),
  rdf_assert(M, User, foaf:givenName, GivenName@nl, G).



%! rdf_assert_objects(+M, +S, +P, +Os, +G) is det.

rdf_assert_objects(M, S, P, Os, G) :-
  maplist({M,S,P,G}/[O]>>rdf_assert(M, S, P, O, G), Os).



%! rdf_bnode_iri(+M, ?BNode) is nondet.
%! rdf_bnode_iri(+M, ?BNode, ?G) is nondet.

rdf_bnode_iri(M, BNode) :-
  rdf_iri(M, BNode),
  rdf_is_bnode_iri(BNode).


rdf_bnode_iri(M, BNode, G) :-
  rdf_iri(M, BNode, G),
  rdf_is_bnode_iri(BNode).



%! rdf_cbd_quad(+M, +Node, -Quad) is nondet.
%! rdf_cbd_quad(+M, +Node, -Quad) is nondet.

rdf_cbd_quad(M, Node, rdf(S,P,O,G)) :-
  rdf_cbd_triple(M, Node, G, rdf(S,P,O)).


rdf_cbd_quads(M, Node, Quads) :-
  rdf_subject(M, Node),
  aggregate_all(set(Quad), rdf_cbd_quad(M, Node, Quad), Quads).



%! rdf_cbd_triple(+M, +Node, -Triple) is nondet.
%! rdf_cbd_triple(+M, +Node, ?G, -Triple) is nondet.

rdf_cbd_triple(M, Node, Triple) :-
  rdf_cbd_triple(M, Node, _, Triple).


rdf_cbd_triple(M, Node, G, Triple) :-
  rdf_cbd_triple1(M, Node, [Node], G, Triple).

rdf_cbd_triple1(M, S, Hist1, G, Triple) :-
  rdf_is_subject(S),
  rdf(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode_iri(O),
      node_history1(Hist1, O, Hist2),
      rdf_cbd_triple1(M, O, Hist2, G, Triple)
  ;   rdf_reification(M, S, P, O, G, Stmt),
      node_history1(Hist1, Stmt, Hist2),
      rdf_cbd_triple1(M, Stmt, Hist2, G, Triple)
  ).

rdf_cbd_inv_triple1(M, O, Hist1, G, Triple) :-
  rdf(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode_iri(S),
      node_history1(Hist1, S, Hist2),
      rdf_cbd_inv_triple1(M, S, Hist2, G, Triple)
  ;   rdf_reification(M, S, P, O, G, Stmt),
      node_history1(Hist1, Stmt, Hist2),
      rdf_scbd_triple1(M, Stmt, Hist2, G, Triple)
  ).

node_history1(Hist, O, _) :-
  memberchk(O, Hist), !,
  fail.
node_history1(Hist1, O, Hist2) :-
  ord_add_element(Hist1, O, Hist2).



%! rdf_cbd_triples(+M, ?Node, -Triples) is det.
%! rdf_cbd_triples(+M, ?Node, ?G, -Triples) is det.

rdf_cbd_triples(M, Node, Triples) :-
  rdf_cbd_triples(M, Node, _, Triples).


rdf_cbd_triples(M, Node, G, Triples) :-
  rdf_subject(M, Node, G),
  aggregate_all(set(Triple), rdf_cbd_triple(M, Node, G, Triple), Triples).



%! rdf_chk_lexical_form(+M, ?S, ?P, -Lex, ?G) is nondet.

rdf_chk_lexical_form(M, S, P, Lex, G) :-
  once((
    rdf(M, S, P, Lit, G),
    rdf_is_literal(Lit)
  )),
  rdf_literal(Lit, _, _, Lex).



%! rdf_creator(+M, ?Resource, ?Agent, +G) is nondet.
%
% @tbd Generatize to rdf_api as “direct resource or resource in list”.

% An RDF list of multiple creators.
rdf_creator(M, Resource, Agent, G) :-
  rdf_list_member(M, Resource, dc:creator, Agent, G).
% A single creator.
rdf_creator(M, Resource, Agent, G) :-
  rdf(M, Resource, dc:creator, Agent, G),
  \+ rdf_list_member(M, Agent, _, G).



%! rdf_current_prefix(+Prefix:atom) is semidet.

rdf_current_prefix(Prefix) :-
  rdf_current_prefix(Prefix, _).



%! rdf_endpoint_init(+Dict) is det.

rdf_endpoint_init(Dict) :-
  dict_get(graphs, Dict, [], Dicts),
  maplist(rdf_graph_init_, Dicts).

rdf_graph_init_(Dict) :-
  hdt_open(Dict.file, [graph(Dict.name)]).



%! rdf_estimate(+M, ?S, ?P, ?O, -Count:nonneg) is det.
%! rdf_estimate(+M, ?S, ?P, ?O, -Count:nonneg, ?G) is det.
%
% @tbd TRP support for rdf_estimate/6.

rdf_estimate(_, S, _, _, 0) :-
  ground(S),
  rdf_is_literal(S), !.
rdf_estimate(_, S, P, O, 1) :-
  ground(rdf(S,P,O)), !.
rdf_estimate(hdt, S, P, O, Count) :- !,
  aggregate_all(
    sum(Count),
    hdt_count(S, P, O, Count),
    Count
  ).
rdf_estimate(trp, S, P, O, Count) :-
  rdf_estimate_complexity(S, P, O, Count).


rdf_estimate(_, S, _, _, 0, _) :-
  ground(S),
  rdf_is_literal(S), !.
rdf_estimate(_, S, P, O, 1, _) :-
  ground(rdf(S,P,O)), !.
rdf_estimate(hdt, S, P, O, Count, G) :-
  hdt_count(S, P, O, Count, G).



%! rdf_familyName(+M, +Agent, -FamilyName, ?G) is det.

rdf_familyName(M, Agent, FamilyName, G) :-
  rdf_pref_string(M, Agent, foaf:familyName, FamilyName, G).



%! rdf_givenName(+M, +Agent, -GivenName, ?G) is det.

rdf_givenName(M, Agent, GivenName, G) :-
  rdf_pref_string(M, Agent, foaf:givenName, GivenName, G).



%! rdf_iri(+M, ?Iri) is nondet.
%! rdf_iri(+M, ?Iri, ?G) is nondet.

rdf_iri(hdt, Iri) :-
  distinct(Iri, rdf_iri(hdt, Iri, _)).
rdf_iri(trp, Iri) :-
  rdf_iri(Iri).


rdf_iri(hdt, Iri, G) :-
  hdt_term(name, Iri, G),
  rdf_is_iri(Iri).
rdf_iri(trp, Iri, G) :-
  rdf_iri(Iri),
  distinct(G, rdf_term(trp, Iri, G)).



%!  rdf_is_graph(@Term) is semidet.

rdf_is_graph(G) :-
  rdf_default_graph(G), !.
rdf_is_graph(G) :-
  rdf_is_iri(G).



%! rdf_is_language_tagged_string(@Term) is semidet.

rdf_is_language_tagged_string(Term) :-
  ground(Term),
  Term = _@_.



%!  rdf_is_legacy_literal(@Term) is semidet.

rdf_is_legacy_literal(literal(type(_,_))) :- !.
rdf_is_legacy_literal(literal(lang(_,_))) :- !.
rdf_is_legacy_literal(literal(_)).



%! rdf_language_tagged_string(+M, ?S, ?P, -Lit) is nondet.
%! rdf_language_tagged_string(+M, ?S, ?P, -Lit, ?G) is nondet.
%! rdf_language_tagged_string(+M, ?S, ?P, +LRange, -Lit, ?G) is nondet.
%
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.  Notice that results for
% each of the prioritized languages are given in arbitrary order.

rdf_language_tagged_string(M, S, P, Lit) :-
  rdf_language_tagged_string(M, S, P, Lit, _).


rdf_language_tagged_string(M, S, P, Lit, G) :-
  current_lrange(LRange),
  rdf_language_tagged_string(M, S, P, LRange, Lit, G).


rdf_language_tagged_string(M, S, P, LRange, Lit, G) :-
  rdf(M, S, P, Lex@LTag, G),
  basic_filtering(LRange, LTag),
  Lit = Lex@LTag.



%! rdf_list_memberchk(+M, ?L, ?O, ?G) is semidet.
%! rdf_list_memberchk(+M, ?S, ?P, ?O, ?G) is semidet.

rdf_list_memberchk(M, L, O, G) :-
  once(rdf_list_member(M, L, O, G)).


rdf_list_memberchk(M, S, P, O, G) :-
  once(rdf_list_member(M, S, P, O, G)).



%! rdf_literal(+M, ?Literal) is nondet.
%! rdf_literal(+M, ?Literal, ?G) is nondet.
%! rdf_literal(+Lit, -D, -Lex, -LTag) is det.
%! rdf_literal(-Lit, +D, +Lex, -LTag) is det.
%! rdf_literal(-Lit, +D, +Lex, +LTag) is det.

rdf_literal(M, Literal) :-
  rdf_object(M, Literal),
  rdf_is_literal(Literal).


rdf_literal(M, Literal, G) :-
  rdf_object(M, Literal, G),
  rdf_is_literal(Literal).



%! rdf_literal_value(+Literal, +Value) is semidet.
%! rdf_literal_value(+Literal, -Value) is nondet.

rdf_literal_value(Val^^_, Val).
rdf_literal_value(Val@_, Val).



%! rdf_name(+M, ?Name) is nondet.
%! rdf_name(+M, ?Name, ?G) is nondet.

rdf_name(hdt, Name) :-
  distinct(Name, hdt_term(name, Name)).
rdf_name(trp, Name) :-
  rdf_name(Name).


rdf_name(hdt, Name, G) :-
  (ground(Name) -> rdf_is_subject(Name) ; true),
  hdt_term(subject, Name, G).
rdf_name(hdt, Name, G) :-
  (ground(Name) -> rdf_is_predicate(Name) ; true),
  hdt_term(predicate, Name, G),
  \+ hdt_term(subject, Name, G).
rdf_name(hdt, Name, G) :-
  hdt_term(object, Name, G),
  \+ hdt_term(subject, Name, G),
  \+ hdt_term(predicate, Name, G).
rdf_name(trp, Name, G) :-
  rdf_term(trp, Name, G),
  \+ rdf_is_bnode_iri(Name).



%! rdf_node(+M, ?Node) is nondet.
%! rdf_node(+M, ?Node, ?G) is nondet.

rdf_node(hdt, Node) :-
  distinct(Node, hdt_term(node, Node)).
rdf_node(trp, Node) :-
  rdf_node(Node).


rdf_node(hdt, Node, G) :-
  hdt_term(node, Node, G).
rdf_node(trp, S, G) :-
  rdf_subject(trp, S, G).
rdf_node(trp, O, G) :-
  rdf_object(trp, O, G),
  % Make sure there are no duplicates.
  \+ rdf_subject(trp, O, G).



%! rdf_number_of(+M, +Witness, ?S, ?P, ?O, ?G, -NumWitnesses) is semidet.
%
% Master predicate that calculates how many instances match a
% 〈S,P,O,G〉 pattern WRT a given Witness (e.g., `S' or `rdf(S,P,O)').

rdf_number_of(M, Witness, S, P, O, G, NumWitnesses) :-
  aggregate_all(count, distinct(Witness, rdf(M, S, P, O, G)), NumWitnesses).



%! rdf_object(+M, ?O) is nondet.
%! rdf_object(+M, ?O, ?G) is nondet.

rdf_object(hdt, O) :-
  distinct(O, hdt_term(object, O)).
rdf_object(trp, O) :-
  rdf_object(O).


rdf_object(hdt, O, G) :-
  hdt_term(object, O, G).
rdf_object(trp, O, G) :-
  % [O] In memory we can pre-enumerate (pre-check is idle).
  (var(O) -> rdf_object(O) ; true),
  distinct(G, rdf(_, _, O, G)).



%! rdf_predicate(+M, ?P) is nondet.
%! rdf_predicate(+M, ?P, ?G) is nondet.

rdf_predicate(hdt, P) :-
  distinct(P, hdt_term(predicate, P)).
rdf_predicate(trp, P) :-
  rdf_predicate(P).


rdf_predicate(hdt, P, G) :-
  (var(P) -> true ; rdf_is_predicate(P)),
  hdt_term(predicate, P, G).
rdf_predicate(trp, P, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (var(P) -> rdf_predicate(P) ; rdf_is_iri(P)),
  distinct(G, rdf(_, P, _, G)).



%! rdf_pref_string(+M, ?S, ?P, -Lit, ?G) is nondet.
%! rdf_pref_string(+M, ?S, ?P, +LanguageRange, -Lit, ?G) is nondet.
%
% Returns, in the following order:
%
%   1. Language-tagged strings that match the given language priority
%      list; returning results for higher priority language earlier.
%
%   2. Language-tagged strings that do not match the given language
%      priority list.
%
%   3. XSD strings.

rdf_pref_string(M, S, P, Lit, G) :-
  current_lrange(LRange),
  rdf_pref_string(M, S, P, LRange, Lit, G).


% Matching language-tagged strings.
rdf_pref_string(M, S, P, LRange, Lit, G) :-
  rdf_language_tagged_string(M, S, P, LRange, Lit, G).
% Non-matching language-tagged strings.
rdf_pref_string(M, S, P, LRange, Lit, G) :-
  rdf(M, S, P, Lex@LTag, G),
  % Avoid duplicates.
  \+ basic_filtering(LRange, LTag),
  Lit = Lex@LTag.
% Plain XSD strings.
rdf_pref_string(M, S, P, _, Value^^Datatype, G) :-
  % @bug RDF prefix expansion does not work here.
  rdf_equal(Datatype, xsd:string),
  rdf(M, S, P, Value^^Datatype, G).

%! basic_filtering(+Ranges:list, +LTag) is semidet.
%
% Succeeds if the LanguagePriorityList matches the LanguageTag
% according to the basic filtering algorithm described in RFC 4647,
% i.e., if the former is a case-insensitive prefix of the latter,
% while also treating the `*` sign as a wildcard.
%
% @compat RFC 4647

% Allow language priority lists of length 1 to be specified as atoms.
basic_filtering(Ranges, Tag):-
  % NONDET
  member(Range, Ranges),
  atomic_list_concat(Subtags1, -, Range),
  atomic_list_concat(Subtags2, -, Tag),
  basic_filtering1(Subtags1, Subtags2), !.

basic_filtering1(_, []).
basic_filtering1([H1|T1], [H2|T2]):-
  subtag_match(H1, H2),
  basic_filtering1(T1, T2).

%! subtag_match(+RangeSubtag, +Subtag) is semidet.
%
% Two subtags match if either they are the same when compared
% case-insensitively or the language range's subtag is the wildcard
% `*`

subtag_match(*, _):- !.
subtag_match(X1, X2):-
  downcase_atom(X1, X),
  downcase_atom(X2, X).



%! rdf_pref_string_lexical_form(+M, ?S, ?P, -Lex, ?G) is nondet.
%! rdf_pref_string_lexical_form(+M, ?S, ?P, +LRange, -Lex, ?G) is nondet.
%
% Like rdf_pref_string/[4-6], but returns only the lexical form.

rdf_pref_string_lexical_form(M, S, P, Lex, G) :-
  current_lrange(LRange),
  rdf_pref_string_lexical_form(M, S, P, LRange, Lex, G).


rdf_pref_string_lexical_form(M, S, P, LRange, Lex, G) :-
  rdf_pref_string(M, S, P, LRange, Lit, G),
  rdf_literal(Lit, _, _, Lex).



%! rdf_query_term(+Term, -QueryTerm) is det.

rdf_query_term(Term, QueryTerm) :-
  Term =.. [Key,Value],
  ground(Value),
  rdf_term_to_atom(Value, Atom),
  QueryTerm =.. [Key,Atom].



%! rdf_retractall is det.
%! rdf_retractall(+Tuple:compound) is det.

rdf_retractall :-
  rdf_retractall(_, _, _, _).


rdf_retractall(rdf(S,P,O)) :- !,
  rdf_retractall(S, P, O, _).
rdf_retractall(rdf(S,P,O,G)) :-
  rdf_retractall(S, P, O, G).



%! rdf_root(+M, ?Root) is nondet.
%! rdf_root(+M, ?Root, ?G) is nondet.

rdf_root(M, Root) :-
  rdf_root(M, Root, _).


rdf_root(M, Root, G) :-
  rdf_subject(M, Root, G),
  \+ rdf(M, _, _, Root, G).



%! rdf_scbd_quad(+M, +Node, -Quad) is nondet.
%! rdf_scbd_quads(+M, +Node, -Quads) is nondet.

rdf_scbd_quad(M, Node, rdf(S,P,O,G)) :-
  rdf_scbd_triple(M, Node, G, rdf(S,P,O)).


rdf_scbd_quads(M, Node, Quads) :-
  rdf_subject(M, Node),
  aggregate_all(set(Quad), rdf_scbd_quad(M, Node, Quad), Quads).



%! rdf_scbd_triple(+M, +Node, -Triple) is nondet.
%! rdf_scbd_triple(+M, +Node, ?G, -Triple) is nondet.

rdf_scbd_triple(M, Node, Triple) :-
  rdf_scbd_triple(M, Node, _, Triple).


rdf_scbd_triple(M, Node, G, Triple) :-
  rdf_scbd_triple1(M, Node, [Node], G, Triple).

rdf_scbd_triple1(M, O, Hist, G, Triple) :-
  rdf_cbd_inv_triple1(M, O, Hist, G, Triple).
rdf_scbd_triple1(M, S, Hist, G, Triple) :-
  rdf_cbd_triple1(M, S, Hist, G, Triple).



%! rdf_scbd_triples(+M, ?Node, -Triples) is det.
%! rdf_scbd_triples(+M, ?Node, ?G, -Triples) is det.

rdf_scbd_triples(M, Node, Triples) :-
  rdf_scbd_triples(M, Node, _, Triples).


rdf_scbd_triples(M, Node, G, Triples) :-
  rdf_term(M, Node, G),
  aggregate_all(set(Triple), rdf_scbd_triple(M, Node, G, Triple), Triples).



%! rdf_statistic(+M, +Key, -Value, +G) is det.
%
% @arg Key The following keys are supported:
%
%          * nodes(-nonneg)
%
%            The number of unique RDF nodes.
%
%          * objects(-nonneg)
%
%            The number of unique objects.
%
%          * predicates(-nonneg)
%
%            The number of unique predicates.
%
%          * subjects(-nonneg)
%
%            The number of unique subjects.
%
%          * terms(-nonneg)
%
%            The number of unique terms.
%
%          * triples(-nonneg)
%
%            The number of unique triples.

rdf_statistic(hdt, Role, N, G) :-
  hdt_term_count(Role, N, G).
rdf_statistic(trp, nodes, N, G) :-
  aggregate_all(count, rdf_node(trp, _, G), N).
rdf_statistic(trp, objects, N, G) :-
  aggregate_all(count, rdf_object(trp, _, G), N).
rdf_statistic(trp, predicates, N, G) :-
  aggregate_all(count, rdf_predicate(trp, _, G), N).
rdf_statistic(trp, subjects, N, G) :-
  aggregate_all(count, rdf_subject(trp, _, G), N).
rdf_statistic(trp, terms, N, G) :-
  aggregate_all(count, rdf_term(trp, _, G), N).
rdf_statistic(trp, triples, N, G) :-
  rdf_graph_property(G, triples(N)).



%! rdf_subdatatype_of(?Sub, ?Super) is nondet.

rdf_subdatatype_of(X, Y) :-
  xsd_subtype_of(X, Y).



%! rdf_subject(+M, ?S) is nondet.
%! rdf_subject(+M, ?S, ?G) is nondet.

rdf_subject(hdt, S) :-
  distinct(S, hdt_term(subject, S)).
rdf_subject(trp, S) :-
  rdf_subject(S).


rdf_subject(hdt, S, G) :-
  (var(S) -> true ; rdf_is_subject(S)),
  hdt_term(subject, S, G).
rdf_subject(trp, S, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (var(S) -> rdf_subject(S) ; rdf_is_subject(S)),
  (var(G) -> distinct(G, rdf(S, _, _, G)) ; once(rdf(S, _, _, G))).



%! rdf_term(+M, ?Term) is nondet.
%! rdf_term(+M, ?Term, ?G) is nondet.

rdf_term(hdt, Term) :-
  distinct(Term, hdt_term(term, Term)).
rdf_term(trp, Term) :-
  rdf_term(Term).


rdf_term(hdt, Name, G) :-
  rdf_term(name, Name, G).
rdf_term(hdt, BNode, G) :-
  rdf_bnode_iri(hdt, BNode, G).
rdf_term(trp, P, G) :-
  rdf_predicate(trp, P, G).
rdf_term(trp, Node, G) :-
  rdf_node(trp, Node, G),
  % Ensure there are no duplicates.
  \+ rdf_predicate(trp, Node, G).



%! rdf_tuple_quad(+Tuple, +G, -Quad) is det.

rdf_tuple_quad(rdf(S,P,O), G, rdf(S,P,O,G)) :- !.
rdf_tuple_quad(Quad, _, Quad).



%! rdf_tuple_triple(+Tuple, ?Triple) is det.

rdf_tuple_triple(rdf(S,P,O), rdf(S,P,O)) :- !.
rdf_tuple_triple(rdf(S,P,O,_), rdf(S,P,O)).
