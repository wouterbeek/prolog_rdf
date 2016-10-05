:- module(
  q_iri,
  [
    q_abox_iri/1,       % -Iri
    q_abox_iri/2,       % +Refs, -Iri
    q_abox_iri/3,       % +Concept, +Refs, -Iri
    q_abox_iri/5,       % ?Scheme, ?Host, ?Concept, ?Refs, ?Iri
    q_init_ns/0,
    q_tbox_iri/2,       % +Term, -Iri
    q_tbox_iri/4        % ?Scheme, ?Host, ?Term, ?Iri
  ]
).

/** <module> Quine IRIs

@author Wouter Beek
@version 2016/08, 2016/11
*/

:- use_module(library(iri/iri_ext), []).
:- use_module(library(q/qb)).
:- use_module(library(q/q_term)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- rdf_meta
   q_abox_iri(?, ?, ?, ?, r),
   q_tbox_iri(?, ?, ?, r).





%! q_abox_iri(-Iri) is det.
%! q_abox_iri(+Refs, -Iri) is det.
%! q_abox_iri(+Concept, +Refs, -Iri) is det.
%! q_abox_iri(+Scheme, +Host, +Concept, +Refs, -Iri) is det.
%! q_abox_iri(-Scheme, -Host, -Concept, -Refs, +Iri) is det.

q_abox_iri(Iri) :-
  uuid(Ref),
  q_abox_iri([Ref], Iri).


q_abox_iri(Refs, Iri) :-
  q_abox_iri('$concept', Refs, Iri).


q_abox_iri(Concept, Refs, Iri) :-
  setting(iri:data_scheme, Scheme),
  setting(iri:data_auth, Host),
  q_abox_iri(Scheme, Host, Concept, Refs, Iri).


q_abox_iri(Scheme, Host, Concept, Refs, Iri) :-
  nonvar(Iri), !,
  uri_components(Iri, uri_components(Scheme,Host,Path,_,_)),
  atomic_list_concat(['',id,Concept|Refs], /, Path).
q_abox_iri(Scheme, Host, Concept, Refs, Iri) :-
  atomic_list_concat(['',id,Concept|Refs], /, Path),
  uri_components(Iri, uri_components(Scheme,Host,Path,_,_)).



q_init_ns :-
  setting(iri:data_scheme, Scheme),
  setting(iri:data_auth, Host),
  uri_components(Prefix1, uri_components(Scheme,Host,'/data/',_,_)),
  qb_alias(data, Prefix1),
  uri_components(Prefix2, uri_components(Scheme,Host,'/meta/',_,_)),
  qb_alias(meta, Prefix2),
  uri_components(Prefix3, uri_components(Scheme,Host,'/',_,_)),
  qb_alias(ns, Prefix3),
  uri_components(Prefix4, uri_components(Scheme,Host,'/def',_,'')),
  qb_alias(nsdef, Prefix4),
  uri_components(Prefix5, uri_components(Scheme,Host,'/doc/',_,_)),
  qb_alias(nsdoc, Prefix5),
  uri_components(Prefix6, uri_components(Scheme,Host,'/id/',_,_)),
  gtrace,
  qb_alias(nsid, Prefix6).



%! q_tbox_iri(+Term, -Iri) is det.
%! q_tbox_iri(+Scheme, +Host, +Term, -Iri) is det.
%! q_tbox_iri(-Scheme, -Host, -Term, +Iri) is det.

q_tbox_iri(Term, Iri) :-
  setting(iri:data_scheme, Scheme),
  setting(iri:data_auth, Host),
  q_tbox_iri(Scheme, Host, Term, Iri).


q_tbox_iri(Scheme, Host, Term, Iri) :-
  nonvar(Iri), !,
  uri_components(Iri, uri_components(Scheme,Host,Path,_,Term)),
  atomic_list_concat(['',def], /, Path).
q_tbox_iri(Scheme, Host, Term, Iri) :-
  atomic_list_concat(['',def], /, Path),
  uri_components(Iri, uri_components(Scheme,Host,Path,_,Term)).
