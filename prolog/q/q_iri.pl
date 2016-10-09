:- module(
  q_iri,
  [
    q_abox_iri/2, % ?Concept, ?Iri
    q_abox_iri/3, % ?Concept, ?Refs, ?Iri
    q_abox_iri/5, % ?Scheme, ?Auth, ?Concept, ?Refs, ?Iri
    q_init_ns/0,
    q_tbox_iri/2, % ?Term, ?Iri
    q_tbox_iri/4  % ?Scheme, ?Auth, ?Term, ?Iri
  ]
).

/** <module> Quine IRIs

@author Wouter Beek
@version 2016/08, 2016/10
*/

:- use_module(library(iri/iri_ext)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_term)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf11)).





%! q_abox_iri(+Concept, -Iri) is det.
%! q_abox_iri(-Concept, +Iri) is det.
%! q_abox_iri(+Concept, +Refs, -Iri) is det.
%! q_abox_iri(-Concept, -Refs, +Iri) is det.
%! q_abox_iri(+Scheme, +Auth, +Concept, +Refs, -Iri) is det.
%! q_abox_iri(-Scheme, -Auth, -Concept, -Refs, +Iri) is det.

q_abox_iri(Concept, Iri) :-
  uuid(Ref),
  q_abox_iri(Concept, [Ref], Iri).


q_abox_iri(Concept, Refs, Iri) :-
  nonvar(Iri), !,
  q_abox_iri(_, _, Concept, Refs, Iri).
q_abox_iri(Concept, Refs, Iri) :-
  iri_prefix(Scheme, Auth),
  q_abox_iri(Scheme, Auth, Concept, Refs, Iri).


q_abox_iri(Scheme, Auth, Concept, Refs, Iri) :-
  nonvar(Iri), !,
  uri_components(Iri, uri_components(Scheme,Auth,Path,_,_)),
  atomic_list_concat(['',id,Concept|Refs], /, Path).
q_abox_iri(Scheme, Auth, Concept, Refs, Iri) :-
  atomic_list_concat(['',id,Concept|Refs], /, Path),
  uri_components(Iri, uri_components(Scheme,Auth,Path,_,_)).



q_init_ns :-
  iri_prefix(Scheme, Auth),
  uri_components(Prefix1, uri_components(Scheme,Auth,'/data/',_,_)),
  qb_alias(data, Prefix1),
  uri_components(Prefix2, uri_components(Scheme,Auth,'/meta/',_,_)),
  qb_alias(meta, Prefix2),
  uri_components(Prefix3, uri_components(Scheme,Auth,'/',_,_)),
  qb_alias(ns, Prefix3),
  uri_components(Prefix4, uri_components(Scheme,Auth,'/def',_,'')),
  qb_alias(nsdef, Prefix4),
  uri_components(Prefix5, uri_components(Scheme,Auth,'/doc/',_,_)),
  qb_alias(nsdoc, Prefix5),
  uri_components(Prefix6, uri_components(Scheme,Auth,'/id/',_,_)),
  qb_alias(nsid, Prefix6).



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
