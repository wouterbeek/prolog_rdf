:- module(
  qb,
  [
  % RDF
    qb/2,              % +M, +Quad
    qb/3,              % +M, +Triple, +G
    qb/5,              % +M, +S, +P, +O, +G
   %qb_alias/2,        % +Alias, +Prefix
   %qb_bnode/1,        % -B
    qb_bnode_prefix/1, % -BPrefix
    qb_deref/2,        % +M, +Iri
    qb_deref/3,        % +M, +Iri, +G
    qb_instance/4,     % +M, +I, ?C, +G
    qb_instances/4,    % +M, +I, +Cs, +G
    qb_iri/2,          % +Alias, -Iri
    qb_iri/3,          % +Alias, +SubPaths, -Iri
    qb_now/4,          % +M, +S, +P, +G
    qb_now/5,          % +M, +S, +P, +D, +G
    qb_objects/5,      % +M, +S, +P, +Os, +G
    qb_reification/4,  % +M, +Triple, +G, ?Stmt
    qb_rev/5,          % +M, +O, +P, +S, +G
    qu/7,              % +M1, +M2, +S, +P, +O, +G, +Action
  % RDFS
    qb_class/6,        % +M, +C, ?D, ?Lbl, ?Comm, +G
    qb_comment/4,      % +M, +S, +Comm, +G
    qb_domain/4,       % +M, +P, +C, +G
    qb_isDefinedBy/3,  % +M, +S, +G
    qb_isDefinedBy/4,  % +M, +S, ?Iri, +G
    qb_label/4,        % +M, +S, +O, +G
    qb_property/5,     % +M, +C, +P, +D, +G
    qb_range/4,        % +M, +P, +C, +G
    qb_rm/2,           % +M, +Quad
    qb_rm/3,           % +M, +Triple, +G
    qb_rm/5,           % +M, +S, +P, +O, +G
    qb_seeAlso/4,      % +M, +S, +Iri, +G
    qb_subclass/4,     % +M, +C, +D, +G
    qb_subproperty/4,  % +M, +P, +Q, +G
  % OWL
    qb_class/6,               % +M, +C, ?D, ?Lbl, ?Comm, +G
    qb_data_property/9,       % +M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts
    qb_disjointWith/4,        % +M, +C, +D, +G
    qb_equivalent_class/4,    % +M, +C, +D, +G
    qb_functional_property/3, % +M, +P, +G
    qb_identity/4,            % +M, +S, +O, +G
    qb_intersection_of/4,     % +M, +C, +Ds, +G
    qb_named_individual/6,    % +M, +I, +C, ?Lbl, ?Comm, +G
    qb_object_property/9,     % +M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts
    qb_ontology/3,            % +M, +Ontology, +G
    qb_value_restriction/5,   % +M, +P, +Val, +G, -Restriction
  % DOMAIN-SPECIFIC
    qb_action/5, % +M, +ActionC, +Actor, -Action, +G
    qb_mail/4,   % +M, +Agent, +Mail, +G
    qb_user/7    % +M, +User, +C, +Img, +GivenName, +FamilyName, +G
  ]
).
:- reexport(library(semweb/rdf11), [
     rdf_create_bnode/1 as qb_bnode,
     rdf_register_prefix/2 as qb_alias
   ]).

/** <module> Quine build API

@author Wouter Beek
@version 2016/06-2016/08
*/

:- use_module(library(debug)).
:- use_module(library(default)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(q/q_list)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).
:- use_module(library(uuid)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- qb_alias(prov, 'http://www.w3.org/ns/prov#').

:- rdf_meta
   % RDF
   qb(+, t),
   qb(+, t, r),
   qb(+, r, r, o, r),
   qb_deref(+, r),
   qb_deref(+, r, r),
   qb_instance(+, r, r, r),
   qb_instances(+, r, t, r),
   qb_now(+, o, r, r),
   qb_now(+, o, r, r, r),
   qb_objects(+, r, r, t, r),
   qb_reification(+, t, r, r),
   qb_rev(+, o, r, r, r),
   qb_rm(+, t),
   qb_rm(+, t, r),
   qb_rm(+, r, r, o, r),
   qu(+, +, r, r, o, r, t),
   % RDFS
   qb_class(+, r, t, ?, ?, r),
   qb_comment(+, r, +, r),
   qb_domain(+, r, r, r),
   qb_isDefinedBy(+, r, r),
   qb_isDefinedBy(+, r, r, r),
   qb_label(+, r, o, r),
   qb_property(+, r, r, r, r),
   qb_range(+, r, r, r),
   qb_seeAlso(+, r, +, r),
   qb_subclass(+, r, t, r),
   qb_subproperty(+, r, t, r),
   % OWL
   qb_data_property(+, r, t, ?, ?, r, r, r, +),
   qb_disjointWith(+, o, o, r),
   qb_equivalent_class(+, r, r, r),
   qb_functional_property(+, r, r),
   qb_identity(+, r, r, r),
   qb_intersection_of(+, r, t, r),
   qb_named_individual(+, r, t, ?, ?, r),
   qb_object_property(+, r, t, ?, ?, r, r, r, +),
   qb_ontology(+, r, r),
   qb_value_restriction(+, r, r, r, -),
   % DOMAIN-SPECIFIC
   qb_action(+, r, r, -, r),
   qb_mail(+, r, +, r),
   qb_user(+, r, r, +, +, +, r).





% RDF %

%! qb(+M, +Quad) is det.
%! qb(+M, +Triple, +G) is det.
%! qb(+M, +S, +P, +O, +G) is det.

qb(M, rdf(S,P,O,G)) :-
  qb(M, S, P, O, G).


qb(M, rdf(S,P,O), G) :-
  qb(M, S, P, O, G).


qb(hdt, S, P, O, G) :- !,
  q_store_call(gen_ntuple(S, P, O), G).
qb(rdf, S, P, O, G) :- !,
  rdf_assert(S, P, O, G).



%! qb_deref(+M, +Iri) is det.
%! qb_deref(+M, +Iri, +G) is det.

qb_deref(M, Iri) :-
  q_view_graph(rdf, Iri), !,
  q_load(M, Iri).
qb_deref(M, Iri) :-
  qb_deref(M, Iri, Iri).


qb_deref(M, Iri, G) :-
  q_deref(Iri, rdf(S,P,O,_)),
  S == Iri,
  qb(M, S, P, O, G),
  fail.
qb_deref(_, Iri, _) :-
  debug(qb(qb_deref), "Dereferenced: ~a", [Iri]).



%! qb_bnode_prefix(-BPrefix) is det.
%
% BPrefix is a universally unique blank node label prefix.

qb_bnode_prefix(BPrefix) :-
  uuid(Uuid),
  atomic_list_concat(['_',Uuid,''], :, BPrefix).



%! qb_instance(+M, +I, ?C, +G) is det.

qb_instance(M, I, C, G) :-
  q_defval(C, rdfs:'Resource'),
  qb(M, I, rdf:type, C, G).



%! qb_instances(+M, +I, +Cs, +G) is det.

qb_instances(M, I, Cs, G) :-
  maplist({M,I,G}/[C]>>qb_instance(M, I, C, G), Cs).



%! qb_iri(+Alias, -Iri) is det.
%! qb_iri(+Alias, +SubPaths, -Iri) is det.
%
% Succeeds with a fresh IRI within the RDF namespace denoted by Alias
% and the given SubPaths.
%
% IRI freshness is guaranteed by the UUID that is used as the path
% suffix.

qb_iri(Alias, Iri) :-
  qb_iri(Alias, [], Iri).


qb_iri(Alias, SubPaths0, Iri) :-
  uuid(Id),
  append(SubPaths0, [Id], SubPaths),
  atomic_list_concat(SubPaths, /, LocalName),
  rdf_global_id(Alias:LocalName, Iri).



%! qb_now(+M, +S, +P, +G) is det.
%! qb_now(+M, +S, +P, +D, +G) is det.
%
% The default date/time datatype is `xsd:dateTime`.

qb_now(M, S, P, G) :-
  qb_now(M, S, P, xsd:dateTime, G).


qb_now(M, S, P, D, G) :-
  get_time(Now),
  qb(M, S, P, Now^^D, G).



%! qb_objects(+M, +S, +P, +Os, +G) is det.

qb_objects(M, S, P, Os, G) :-
  maplist({M,S,P,G}/[O]>>qb(M, S, P, O, G), Os).



%! qb_reification(+M, +Triple, +G, ?Stmt) is det.
%
% If Stmt is not given a fresh blank node is created.

qb_reification(M, rdf(S,P,O), G, Stmt) :-
  defgoal(qb_bnode, Stmt),
  qb_instance(M, Stmt, rdf:'Stmt', G),
  qb(M, Stmt, rdf:subject, S, G),
  qb(M, Stmt, rdf:predicate, P, G),
  qb(M, Stmt, rdf:object, O, G),
  qb(M, S, P, O, G).



%! qb_rev(+M, +O, +P, +S, +G) is det.

qb_rev(M, O, P, S, G) :-
  qb(M, S, P, O, G).



%! qb_rm(+M, +Quad) is det.
%! qb_rm(+M, +Triple, +G) is det.
%! qb_rm(+M, +S, +P, +O, +G) is det.

qb_rm(M, rdf(S,P,O,G)) :-
  qb_rm(M, S, P, O, G).


qb_rm(M, rdf(S,P,O), G) :-
  qb_rm(M, S, P, O, G).


qb_rm(rdf, S, P, O, G) :- !,
  rdf_retractall(S, P, O, G).



%! qu(+M1, +M2, +S, +P, +O, +G, +Action) is det.

qu(rdf, rdf, S, P, O, G, Action) :- !,
  rdf_update(S, P, O, G, Action).





% RDFS %

%! qb_class(+M, +C, ?D, ?Lbl, ?Comment, +G) is det.

qb_class(M, C, D, Lbl, Comm, G) :-
  (var(D) -> true ; qb_subclass(M, C, D, G)),
  (var(Lbl) -> true ; qb_label(M, C, Lbl, G)),
  (var(Comm) -> true ; qb_comment(M, C, Comm, G)).



%! qb_comment(+M, +S, +Comment, +G) is det.

qb_comment(M, S, Comment, G) :-
  qb(M, S, rdfs:comment, Comment, G).



%! qb_domain(+M, +P, +C, +G) is det.

qb_domain(M, P, D, G) :-
  qb(M, P, rdfs:domain, D, G).



%! qb_isDefinedBy(+M, +S, +G) is det.
%! qb_isDefinedBy(+M, +S, ?Iri, +G) is det.
%
% If Iri is uninstantiated, the IRI denoted by the registered RDF
% prefix of Term, if any, is used.

qb_isDefinedBy(M, S, G) :-
  q_iri_prefix(S, Prefix),
  qb_isDefinedBy(M, S, Prefix, G).


qb_isDefinedBy(M, S, Iri, G) :-
  (var(Iri) -> q_iri_prefix(S, Iri) ; true),
  qb(M, S, rdfs:isDefinedBy, Iri^^xsd:anyURI, G).



%! qb_label(+M, +S, +O, +G) is det.

qb_label(M, S, O, G) :-
  qb(M, S, rdfs:label, O, G).



%! qb_property(+M, +C, +P, +D, +G) is det.

qb_property(M, C, P, D, G) :-
  qb_domain(M, P, C, G),
  qb_range(M, P, D, G).



%! qb_range(+M, +P, +C, +G) is det.

qb_range(M, P, C, G) :-
  qb(M, P, rdfs:range, C, G).



%! qb_seeAlso(+M, +S, +Iri, +G) is det.

qb_seeAlso(M, S, Iri, G) :-
  qb(M, S, rdfs:seeAlso, Iri, G).



%! qb_subclass(+M, +C, +D, +G) is det.
% If D is uninstantiated it defaults to `rdfs:Resource`.

qb_subclass(M, C, D, G) :-
  qb(M, C, rdfs:subClassOf, D, G).



%! qb_subproperty(+M, +P, +Q, +G) is det.
%
% Creates a new property that is a subproperty of the given parent
% property.

qb_subproperty(M, P, Q, G) :-
  qb(M, P, rdfs:subPropertyOf, Q, G).





% OWL %

%! qb_data_property(+M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts) is det.
%
% The following options are supported:
%
%   * functional(+boolean)

qb_data_property(M, P, Q, Lbl, Comm, Dom, Ran, G, Opts) :-
  qb_instance(M, P, owl:'DataProperty', G),
  qb_property0(M, P, Q, Lbl, Comm, Dom, Ran, G, Opts).



%! qb_disjointWith(+M, +C, +D, +G) is det.

qb_disjointWith(M, C, D, G) :-
  qb(M, C, owl:disjointWith, D, G).



%! qb_equivalent_class(+M, +C, +D, +G) is det.

qb_equivalent_class(M, C, D, G) :-
  qb(M, C, owl:equivalentClass, D, G).



%! qb_functional_property(+M, +P, +G) is det.

qb_functional_property(M, P, G) :-
  qb_instance(M, P, owl:'FunctionalProperty', G).



%! qb_identity(+M, +S, +O, +G) is det.

qb_identity(M, S, O, G) :-
  qb(M, S, owl:sameAs, O, G).



%! qb_intersection_of(+M, ?C, +Ds, +G) is det.

qb_intersection_of(M, C, Ds, G) :-
  defgoal(qb_bnode, C),
  qb_instance(M, C, owl:'Class', G),
  qb_list(M, Ds, Ds0, G),
  qb(M, C, owl:intersectionOf, Ds0, G).



%! qb_named_individual(+M, +I, +C, ?Lbl, ?Comm, +G) is det.

qb_named_individual(M, I, C, Lbl, Comm, G) :-
  qb_instance(M, I, owl:'NamedIndividual', G),
  (var(C) -> true ; qb_instance(M, I, C, G)),
  (var(Lbl) -> true ; qb_label(M, I, Lbl, G)),
  (var(Comm) -> true ; qb_comment(M, I, Comm, G)),
  qb_isDefinedBy(M, I, G).



%! qb_object_property(+M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts) is det.
%
% The following options are supported:
%
%   * functional(+boolean)

qb_object_property(M, P, Parent, Lbl, Comm, D, R, G, Opts) :-
  qb_instance(M, P, owl:'ObjectProperty', G),
  qb_property0(M, P, Parent, Lbl, Comm, D, R, G, Opts).



%! qb_ontology(+M, +Ontology, +G) is det.

qb_ontology(M, Onto, G) :-
  qb_instance(M, Onto, owl:'Ontology', G).



%! qb_value_restriction(+M, +P, +Val, +G, -Restriction) is det.

qb_value_restriction(M, P, V, G, R) :-
  qb_bnode(R),
  qb_instance(M, R, owl:'Restriction', G),
  qb(M, R, owl:onProperty, P, G),
  qb(M, R, owl:hasValue, V, G).





% DOMAIN-SPECIFIC %

%! qb_action(+M, +ActionC, +Actor, -Action, +G) is det.

qb_action(M, ActionC, Actor, Action, G):-
  qb_iri(vzm, [action], Action),
  qb_instance(M, Action, ActionC, G),
  qb_now(M, Action, prov:atTime, G),
  qb(M, Action, prov:wasAssociatedWith, Actor, G).



%! qb_mail(+M, +Agent, +Mail, +G) is det.

qb_mail(M, Agent, Mail, G) :-
  atomic_list_concat([mailto,Mail], :, Iri),
  qb(M, Agent, foaf:mbox, Iri^^xsd:anyURI, G).



%! qb_user(+M, +User, +C, +Img, +GivenName, +FamilyName, +G) is det.

qb_user(M, User, C, Img, GivenName, FamilyName, G) :-
  qb_instance(M, User, C, G),
  qb(M, User, foaf:depiction, Img^^xsd:anyURI, G),
  qb(M, User, foaf:familyName, FamilyName@nl, G),
  qb(M, User, foaf:givenName, GivenName@nl, G).





% HELPERS %

%! qb_property0(+M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts) is det.

qb_property0(M, P, Q, Lbl, Comm, Dom, Ran, G, Opts) :-
  (option(functional(true), Opts) -> qb_functional_property(M, P, G) ; true),
  (var(Q) -> true ; qb_subproperty(M, P, Q, G)),
  (var(Lbl) -> true ; qb_label(M, P, Lbl, G)),
  (var(Comm) -> true ; qb_comment(M, P, Comm, G)),
  (var(Dom) -> true ; qb_domain(M, P, Dom, G)),
  (var(Ran) -> true ; qb_range(M, P, Ran, G)),
  qb_isDefinedBy(M, P, G).
