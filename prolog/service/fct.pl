:- module(
  fct,
  [
    fct_label/2 % +Search, -Result
  ]
).

/** <module> FCT

@author Wouter Beek
@see http://dbpedia.org/fct/
@version 2016/07
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(yall)).

:- setting(
     endpoint_host,
     atom,
     'dbpedia.org',
     "The host name of the FCT endpoint."
   ).
:- setting(
     endpoint_path,
     list(atom),
     [services,rdf,'iriautocomplete.get'],
     "The path of the FCT endpoint."
   ).
:- setting(
     endpoint_scheme,
     oneof([http,https]),
     http,
     "The scheme of the FCT endpoint."
   ).





%! fct_label(+Search, -Result) is nondet.

fct_label(Search, Result) :-
  setting(fct:endpoint_scheme, Scheme),
  setting(fct:endpoint_host, Host),
  setting(fct:endpoint_path, PathComps),
  atomic_list_concat([''|PathComps], /, Path),
  uri_query_components(Query, [label(Search)]),
  uri_components(Iri, uri_components(Scheme,Host,Path,Query,_)),
  % json_read_any/[2,3] cannot be used here because the `Accept`
  % header must be `*` in order to retrieve JSON.
  setup_call_cleanup(
    http_open(Iri, In, [request_header('Accept'='*')]),
    json_read_dict(In, Results),
    close(In)
  ),
  member(Result0, Results.results),
  atom_string(Result, Result0),
  is_http_iri(Result).
