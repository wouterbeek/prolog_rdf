:- module(
  ll_index,
  [
    ll_iri_doc/2,    % +Iri, -Doc
    ll_iri_docs/2,   % +Iri, -Docs
    ll_prefix_doc/2, % +Prefix, -Doc
    ll_prefix_docs/2 % +Prefix, -Docs
  ]
).

/** <module> LOD Laundromat indices

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(json_ext)).
:- use_module(library(lists)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- qb_alias(llr, 'http://lodlaundromat.org/resource/').

:- rdf_meta
   ll_iri_doc(r, -),
   ll_iri_docs(r, -),
   ll_prefix_doc(r, -),
   ll_prefix_docs(r, -).





%! ll_iri_doc(+Iri, -Doc) is nondet.
%! ll_iri_docs(+Iri, -Docs) is nondet.

ll_iri_doc(Iri, Doc) :-
  ll_iri_docs(Iri, Docs),
  member(Doc, Docs).


ll_iri_docs(Iri, Docs) :-
  uri_query_components(Query, [limit(0),uri(Iri)]),
  uri_components(
    RequestIri,
    uri_components(http,'index.lodlaundromat.org','/r2d/',Query,_)
  ),
  json_read_any(RequestIri, DocNames1),
  maplist(atom_string, DocNames2, DocNames1),
  maplist(doc_name0, Docs, DocNames2).



%! ll_prefix_doc(+Prefix, -Doc) is nondet.
%! ll_prefix_docs(+Prefix, -Docs) is nondet.

ll_prefix_doc(Prefix, Doc) :-
  ll_prefix_docs(Prefix, Docs),
  member(Doc, Docs).


ll_prefix_docs(Prefix, Docs) :-
  uri_query_components(Query, [limit(0),uri(Prefix)]),
  uri_components(
    RequestIri,
    uri_components(http,'index.lodlaundromat.org','/ns2d/',Query,_)
  ),
  json_read_any(RequestIri, DocNames1),
  maplist(atom_string, DocNames2, DocNames1),
  maplist(doc_name0, Docs, DocNames2).





% HELPERS %

doc_name0(Doc, Name) :-
  rdf_global_id(llr:Name, Doc).
