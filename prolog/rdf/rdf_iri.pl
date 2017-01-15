:- module(
  rdf_iri,
  [
    rdf_abox_iri/2,        % ?Concept, ?Iri
    rdf_abox_iri/3,        % ?Concept, ?Segments, ?Iri
    rdf_abox_iri/4,        % ?Scheme, ?Auth, ?Concept, ?Iri
    rdf_abox_iri/5,        % ?Scheme, ?Auth, ?Concept, ?Segments, ?Iri
    rdf_dataset_iri/2,     % ?Segments, ?D
    rdf_graph_iri/2,       % +Segments, -G
    rdf_init_ns/0,
    rdf_is_external_iri/1, % +Iri
    rdf_is_internal_iri/1, % +Iri
    rdf_string_to_local/2, % +Str, -Local
    rdf_tbox_iri/2,        % ?Term, ?Iri
    rdf_tbox_iri/3,        % ?Term, ?Subterms, ?Iri
    rdf_tbox_iri/4,        % ?Scheme, ?Auth, ?Term, ?Iri
    rdf_tbox_iri/5         % ?Scheme, ?Auth, ?Term, ?Subterms, ?Iri
  ]
).

/** <module> Quine IRIs

@author Wouter Beek
@version 2016/08-2017/01
*/

:- use_module(library(dcg/basics)).
:- use_module(library(default)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri/uri_ext)).





%! rdf_abox_iri(+Concept, -Iri) is det.
%! rdf_abox_iri(-Concept, +Iri) is det.
%! rdf_abox_iri(+Concept, +Segments, -Iri) is det.
%! rdf_abox_iri(-Concept, -Segments, +Iri) is det.
%! rdf_abox_iri(+Scheme, +Auth, +Concept, -Iri) is det.
%! rdf_abox_iri(-Scheme, -Auth, -Concept, +Iri) is det.
%! rdf_abox_iri(+Scheme, +Auth, +Concept, +Segments, -Iri) is det.
%! rdf_abox_iri(-Scheme, -Auth, -Concept, -Segments, +Iri) is det.
%
% The following options are supported for Segments when Iri is
% uninstantated:
%
%   * `var`
%
%     Generate a UUID.
%
%   * `list(atom)
%
%     The segments of the IRI path.
%
%   * `atom`
%
%     A single-segment IRI path.

rdf_abox_iri(Concept, Iri) :-
  rdf_abox_iri(Concept, _, Iri).


rdf_abox_iri(Concept, Segments, Iri) :-
  rdf_abox_iri(_, _, Concept, Segments, Iri).


rdf_abox_iri(Scheme, Auth, Concept, Iri) :-
  rdf_abox_iri(Scheme, Auth, Concept, _, Iri).


rdf_abox_iri(Scheme, Auth, Concept, Segments, Iri) :-
  rdf_iri(Scheme, Auth, id, Concept, Segments, Iri).



%! rdf_dataset_iri(+Segments, -D) is det.
%! rdf_dataset_iri(-Segments, +D) is det.

rdf_dataset_iri(Segments, D) :-
  rdf_abox_iri(dataset, Segments, D).



%! rdf_graph_iri(+Segments, -G) is det.

rdf_graph_iri(Segments, G) :-
  rdf_abox_iri(graph, Segments, G).



rdf_init_ns :-
  iri_prefix(Scheme, Auth),
  uri_comps(Prefix1, uri(Scheme,Auth,[],_,_)),
  rdf_create_alias(ns, Prefix1),
  uri_comps(Prefix2, uri(Scheme,Auth,['.well-known','genid/'],_,_)),
  rdf_create_alias(bnode, Prefix2),
  uri_comps(Prefix3, uri(Scheme,Auth,['dataset/'],_,_)),
  rdf_create_alias(dataset, Prefix3),
  uri_comps(Prefix4, uri(Scheme,Auth,['def/'],_,_)),
  rdf_create_alias(nsdef, Prefix4),
  uri_comps(Prefix5, uri(Scheme,Auth,['doc/'],_,_)),
  rdf_create_alias(nsdoc, Prefix5),
  uri_comps(Prefix6, uri(Scheme,Auth,['graph/'],_,_)),
  rdf_create_alias(graph, Prefix6),
  uri_comps(Prefix7, uri(Scheme,Auth,['id/'],_,_)),
  rdf_create_alias(nsid, Prefix7).



%! rdf_is_external_iri(+Iri) is semidet.

rdf_is_external_iri(Iri) :-
  uri_comps(Iri, uri(Scheme,Auth,_,_,_)),
  iri_prefix(Scheme, Auth).



%! rdf_is_internal_iri(+Iri) is semidet.

rdf_is_internal_iri(Iri) :-
  uri_comps(Iri, uri(Scheme,Auth,_,_,_)),
  \+ iri_prefix(Scheme, Auth).



%! rdf_string_to_local(+Str, -Local) is det.

rdf_string_to_local(Str, Local) :-
  string_codes(Str, Cs1),
  phrase(string_to_local, Cs1, Cs2),
  atom_codes(Local, Cs2).

% Remove blanks and use CamelCase.
string_to_local, [C2] -->
  'blank+',
  [C1], !,
  {code_type(C1, to_lower(C2))},
  string_to_local.
% Skip blanks at the end.
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



%! rdf_tbox_iri(+Term, -Iri) is det.
%! rdf_tbox_iri(-Term, +Iri) is det.
%! rdf_tbox_iri(+Scheme, +Auth, +Term, -Iri) is det.
%! rdf_tbox_iri(-Scheme, -Auth, -Term, +Iri) is det.

rdf_tbox_iri(Concept, Iri) :-
  rdf_tbox_iri(Concept, _, Iri).


rdf_tbox_iri(Concept, Segments, Iri) :-
  rdf_tbox_iri(_, _, Concept, Segments, Iri).


rdf_tbox_iri(Scheme, Auth, Concept, Iri) :-
  rdf_tbox_iri(Scheme, Auth, Concept, _, Iri).


rdf_tbox_iri(Scheme, Auth, Concept, Segments, Iri) :-
  rdf_iri(Scheme, Auth, def, Concept, Segments, Iri).





% HELPERS %

rdf_iri(Scheme, Auth, Kind, Concept, Segments2, Iri) :-
  ground(Iri), !,
  uri_comps(Iri, uri(Scheme,Auth,[Kind,Concept|Segments1],_,_)),
  rdf_iri_refs_out(Segments1, Segments2).
rdf_iri(Scheme, Auth, Kind, Concept, Segments1, Iri) :-
  iri_prefix(SchemeDef, AuthDef),
  defval(SchemeDef, Scheme),
  defval(AuthDef, Auth),
  rdf_iri_refs_in(Segments1, Segments2),
  uri_comps(Iri, uri(Scheme,Auth,[Kind,Concept|Segments2],_,_)).

rdf_iri_refs_in(VAR, [Segment]) :-
  var(VAR), !,
  uuid(Segment).
rdf_iri_refs_in(Segments, Segments) :-
  is_list(Segments), !.
rdf_iri_refs_in(Segment, [Segment]).

rdf_iri_refs_out([Segment], Segment) :- !.
rdf_iri_refs_out(Segments, Segments).
