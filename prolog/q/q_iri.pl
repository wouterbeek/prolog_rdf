:- module(
  q_iri,
  [
    q_abox_iri/2,        % ?Concept, ?Iri
    q_abox_iri/3,        % ?Concept, ?Refs, ?Iri
    q_abox_iri/4,        % ?Scheme, ?Auth, ?Concept, ?Iri
    q_abox_iri/5,        % ?Scheme, ?Auth, ?Concept, ?Refs, ?Iri
    q_dataset_iri/2,     % ?Refs, ?D
    q_graph_iri/2,       % +Refs, -G
    q_is_external_iri/1, % +Iri
    q_is_internal_iri/1, % +Iri
    q_string_to_local/2, % +Str, -Local
    q_tbox_iri/2,        % ?Term, ?Iri
    q_tbox_iri/4         % ?Scheme, ?Auth, ?Term, ?Iri
  ]
).

/** <module> Quine IRIs

@author Wouter Beek
@version 2016/08, 2016/10, 2016/12
*/

:- use_module(library(dcg/basics)).
:- use_module(library(default)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf11)).

:- initialization(q_init_ns).





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



%! q_dataset_iri(+Refs, -D) is det.
%! q_dataset_iri(-Refs, +D) is det.

q_dataset_iri(Refs, D) :-
  q_abox_iri(dataset, Refs, D).



%! q_graph_iri(+Refs, -G) is det.

q_graph_iri(Refs, G) :-
  q_abox_iri(graph, Refs, G).



q_init_ns :-
  iri_prefix(Scheme, Auth),
  uri_components(Prefix1, uri_components(Scheme,Auth,'/',_,_)),
  qb_alias(ns, Prefix1),
  uri_components(Prefix2, uri_components(Scheme,Auth,'/.well-known/genid/',_,_)),
  qb_alias(bnode, Prefix2),
  uri_components(Prefix3, uri_components(Scheme,Auth,'/dataset/',_,_)),
  qb_alias(dataset, Prefix3),
  uri_components(Prefix4, uri_components(Scheme,Auth,'/def',_,'')),
  qb_alias(nsdef, Prefix4),
  uri_components(Prefix5, uri_components(Scheme,Auth,'/doc/',_,_)),
  qb_alias(nsdoc, Prefix5),
  uri_components(Prefix6, uri_components(Scheme,Auth,'/graph/',_,_)),
  qb_alias(graph, Prefix6),
  uri_components(Prefix7, uri_components(Scheme,Auth,'/id/',_,_)),
  qb_alias(nsid, Prefix7).



%! q_is_external_iri(+Iri) is semidet.

q_is_external_iri(Iri) :-
  uri_components(Iri, uri_components(Scheme,Auth,_,_,_)),
  iri_prefix(Scheme, Auth).



%! q_is_internal_iri(+Iri) is semidet.

q_is_internal_iri(Iri) :-
  uri_components(Iri, uri_components(Scheme,Auth,_,_,_)),
  \+ iri_prefix(Scheme, Auth).



%! q_string_to_local(+Str, -Local) is det.

q_string_to_local(Str, Local) :-
  string_codes(Str, Cs1),
  phrase(string_to_local, Cs1, Cs2),
  atom_codes(Local, Cs2).

% Remove blanks and use CamelCase.
string_to_local, [C2] -->
  'blank+',
  [C1], !,
  {code_type(C1, to_lower(C2))},
  string_to_local.
% Skup blanks at the end.
string_to_local -->
  'blank+', !,
  string_to_local.
% Other characters are lowercase.
string_to_local, [C2] -->
  [C1], !,
  {code_type(C1, to_upper(C2))},
  string_to_local.
string_to_local --> "".

'blank+' --> blank, 'blank*'.
'blank*' --> blank, 'blank*'.
'blank*' --> "".



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
