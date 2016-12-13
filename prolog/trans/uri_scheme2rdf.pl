:- module(uri_scheme2rdf, []).

/** <module> RDF representation of URI schemes

@author Wouter Beek
@version 2015/03, 2016/05, 2016/11
*/

:- use_module(library(lists)).
:- use_module(library(os/io)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- qb_alias('uri-scheme-o', 'http://urischeme.org/ontology/').

:- rdf_meta
   init_uri_scheme(r).

:- initialization(init_uri_scheme).





%! init_uri_scheme is det.
%! init_uri_scheme(+G) is det.

init_uri_scheme :-
  init_uri_scheme(uri_scheme:'').


init_uri_scheme(G) :-
  absolute_file_name(
    G,
    File,
    [access(read),extensions([ttl]),file_errors(fail)]
  ), !,
  rdf_load_file(File, [graph(G),format(turtle)]).
init_uri_scheme(G) :-
  qb_class(
    trp,
    'uri-scheme-o':'UriScheme',
    _,
    "URI Scheme"@en,
    "The class of IANA-registered URI Schemes."@en,
    G
  ),
  maplist(init_uri_scheme(G), ['uri-schemes-1','uri-schemes-2']).


init_uri_scheme(G, Cat) :-
  atomic_list_concat(
    ['http://www.iana.org/assignments/uri-schemes/',Cat,'.csv'],
    Iri
  ),
  absolute_file_name(G, File, [access(write),extensions([ttl])]),
  csv2rdf(
    Iri,
    File,
    _{host: 'urischeme.org'},
    [rdf_media_type(application/turtle)]
  ).
