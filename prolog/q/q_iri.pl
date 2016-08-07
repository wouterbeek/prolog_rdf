:- module(
  q_iri,
  [
    q_abox_iri/4, % ?Domain, ?Concept, ?Ref, ?Iri
    q_init_ns/0,
    q_tbox_iri/3  % ?Domain, ?Term, ?Iri
  ]
).

/** <module> Quine IRIs

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(q/qb)).
:- use_module(library(settings)).
:- use_module(library(uri)).





%! q_abox_iri(+Domain, +Concept, +Ref, -Iri) is det.
%! q_abox_iri(-Domain, -Concept, -Ref, +Iri) is det.

q_abox_iri(Domain, Concept, Ref, Iri) :-
  nonvar(Iri), !,
  uri_components(Iri, uri_components(http,Domain,Path,_,_)),
  atomic_list_concat(['',id,Concept,Ref], /, Path).
q_abox_iri(Domain, Concept, Ref, Iri) :-
  atomic_list_concat(['',id,Concept,Ref], /, Path),
  uri_components(Iri, uri_components(http,Domain,Path,_,_)).



q_init_ns :-
  setting(iri:data_scheme, Scheme),
  setting(iri:data_auth, Host),
  uri_components(Prefix1, uri_components(Scheme,Host,'/',_,_)),
  qb_alias(ns, Prefix1),
  uri_components(Prefix2, uri_components(Scheme,Host,'/def',_,'')),
  qb_alias(nsdef, Prefix2),
  uri_components(Prefix3, uri_components(Scheme,Host,'/doc/',_,_)),
  qb_alias(nsdoc, Prefix3),
  uri_components(Prefix4, uri_components(Scheme,Host,'/id/',_,_)),
  qb_alias(nsid, Prefix4).



%! q_tbox_iri(+Domain, +Term, -Iri) is det.
%! q_tbox_iri(-Domain, -Term, +Iri) is det.

q_tbox_iri(Domain, Term, Iri) :-
  nonvar(Iri), !,
  uri_components(Iri, uri_components(http,Domain,Path,_,Term)),
  atomic_list_concat(['',def], /, Path).
q_tbox_iri(Domain, Term, Iri) :-
  atomic_list_concat(['',def], /, Path),
  uri_components(Iri, uri_components(http,Domain,Path,_,Term)).
