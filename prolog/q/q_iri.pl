:- module(
  q_iri,
  [
    q_abox_iri/2,  % ?Concept, ?Iri
    q_abox_iri/3,  % ?Concept, ?Refs, ?Iri
    q_abox_iri/4,  % ?Scheme, ?Auth, ?Concept, ?Iri
    q_abox_iri/5,  % ?Scheme, ?Auth, ?Concept, ?Refs, ?Iri
    q_graph_iri/2, % +Name, -G
    q_init_ns/0,
    q_tbox_iri/2,  % ?Term, ?Iri
    q_tbox_iri/4   % ?Scheme, ?Auth, ?Term, ?Iri
  ]
).

/** <module> Quine IRIs

@author Wouter Beek
@version 2016/08, 2016/10
*/

:- use_module(library(default)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf11)).





%! q_abox_iri(+Concept, -Iri) is det.
%! q_abox_iri(-Concept, +Iri) is det.
%! q_abox_iri(+Concept, +Refs, -Iri) is det.
%! q_abox_iri(-Concept, -Refs, +Iri) is det.
%! q_abox_iri(+Scheme, +Auth, +Concept, -Iri) is det.
%! q_abox_iri(-Scheme, -Auth, -Concept, +Iri) is det.
%! q_abox_iri(+Scheme, +Auth, +Concept, +Refs, -Iri) is det.
%! q_abox_iri(-Scheme, -Auth, -Concept, -Refs, +Iri) is det.

q_abox_iri(Concept, Iri) :-
  q_abox_iri(Concept, _, Iri).


q_abox_iri(Concept, Refs, Iri) :-
  q_abox_iri(_, _, Concept, Refs, Iri).


q_abox_iri(Scheme, Auth, Concept, Iri) :-
  q_abox_iri(Scheme, Auth, Concept, _, Iri).


q_abox_iri(Scheme, Auth, Concept, Refs2, Iri) :-
  ground(Iri), !,
  uri_components(Iri, uri_components(Scheme,Auth,Path,_,_)),
  atomic_list_concat(['',Concept|Refs1], /, Path),
  q_abox_iri_refs_out(Refs1, Refs2).
q_abox_iri(Scheme, Auth, Concept, Refs1, Iri) :-
  iri_prefix(SchemeDef, AuthDef),
  defval(SchemeDef, Scheme),
  defval(AuthDef, Auth),
  q_abox_iri_refs_in(Refs1, Refs2),
  atomic_list_concat(['',Concept|Refs2], /, Path),
  uri_components(Iri, uri_components(Scheme,Auth,Path,_,_)).


q_abox_iri_refs_in(VAR, [Ref]) :-
  var(VAR), !,
  uuid(Ref).
q_abox_iri_refs_in(Refs, Refs) :-
  is_list(Refs), !.
q_abox_iri_refs_in(Ref, [Ref]).


q_abox_iri_refs_out([Ref], Ref) :- !.
q_abox_iri_refs_out(Refs, Refs).



%! q_graph_iri(+Name, -G) is det.

q_graph_iri(Name, G) :-
  q_abox_iri(graph, Name, G).



q_init_ns :-
  iri_prefix(Scheme, Auth),
  uri_components(Prefix1, uri_components(Scheme,Auth,'/',_,_)),
  qb_alias(ns, Prefix1),
  uri_components(Prefix2, uri_components(Scheme,Auth,'/def',_,'')),
  qb_alias(nsdef, Prefix2),
  uri_components(Prefix3, uri_components(Scheme,Auth,'/doc/',_,_)),
  qb_alias(nsdoc, Prefix3),
  uri_components(Prefix4, uri_components(Scheme,Auth,'/id/',_,_)),
  qb_alias(nsid, Prefix4),
  forall(
    q_name(Name),
    (
      atomic_list_concat(['',Name,''], /, Path),
      uri_components(Prefix5, uri_components(Scheme,Auth,Path,_,_)),
      qb_alias(Name, Prefix5)
    )
  ).



%! q_tbox_iri(+Term, -Iri) is det.
%! q_tbox_iri(-Term, +Iri) is det.
%! q_tbox_iri(+Scheme, +Auth, +Term, -Iri) is det.
%! q_tbox_iri(-Scheme, -Auth, -Term, +Iri) is det.

q_tbox_iri(Term, Iri) :-
  nonvar(Iri), !,
  q_tbox_iri(_, _, Term, Iri).
q_tbox_iri(Term, Iri) :-
  iri_prefix(Scheme, Auth),
  q_tbox_iri(Scheme, Auth, Term, Iri).


q_tbox_iri(Scheme, Auth, Term, Iri) :-
  nonvar(Iri), !,
  uri_components(Iri, uri_components(Scheme,Auth,Path,_,Term)),
  atomic_list_concat(['',def], /, Path).
q_tbox_iri(Scheme, Auth, Term, Iri) :-
  atomic_list_concat(['',def], /, Path),
  uri_components(Iri, uri_components(Scheme,Auth,Path,_,Term)).
