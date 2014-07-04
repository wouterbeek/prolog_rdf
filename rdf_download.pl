:- module(
  rdf_download,
  [
    rdf_download/3, % +Url:url
                    % ?File:atom
                    % +Options:list(nvpair)
    rdf_download/4 % :Goal
                   % +Url:url
                   % ?File:atom
                   % +Options:list(nvpair)
  ]
).

/** <module> RDF download

Additional support for downloading RDF,
e.g. freshness lifetime.

@author Wouter Beek
@version 2014/05, 2014/07
*/

:- use_module(library(option)).
:- use_module(library(predicate_options)).
:- use_module(library(semweb/rdf_db)).

:- use_module(http(http_download)).

:- use_module(plRdf(rdf_graph)).
:- use_module(plRdf_ser(rdf_serial)).

:- meta_predicate(rdf_download(4,+,?,+)).

:- predicate_options(rdf_download/3, 3, [
     pass_to(rdf_download/4, 4)
   ]).
:- predicate_options(rdf_download/4, 4, [
     graph(+atom),
     freshness_lifetime(+float),
     pass_to(rdf_load_any/2, 2)
   ]).



%! rdf_download(+Url:url, ?File:atom, +Options:list(nvpair)) is det.

rdf_download(Url, File, Options):-
  rdf_download(rdf_download_default, Url, File, Options).

%! rdf_download(
%!   :Goal,
%!   +Url:url,
%!   ?File:atom,
%!   +Options:list(nvpair)
%! ) is det.
% Options are passed on to rdf_load/2 and download_to_file/3.

rdf_download(_, Url, File, Options):-
  option(graph(Graph), Options),
  is_rdf_graph(Graph), !,
  option(freshness_lifetime(FreshnessLifetime), Options, inf),
  (
    is_fresh_rdf_graph(Graph, FreshnessLifetime)
  ->
    true
  ;
    rdf_unload_graph(Graph),
    rdf_download(Url, File, Options)
  ).
rdf_download(_, _, File, Options):-
  access_file(File, read), !,
  rdf_load_any(File, Options).
rdf_download(Goal, Url, File, Options):-
  call(Goal, Url, File, Options).

rdf_download_default(Url, File, Options):-
  download_to_file(Url, File, Options),
  rdf_load_any(File, Options).

