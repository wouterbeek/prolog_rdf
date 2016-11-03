:- module(q_download, []).

/** <module> Quine: Download API

@author Wouter Beek
@version 2016/11
*/

:- use_module(library(default)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/rest)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(settings)).

:- http_handler(cp(download), download_handler, []).

:- multifile
    http_param/1,
    media_type/1.

http_param(graph).

media_type(text/turtle).

:- setting(
     backend,
     oneof([hdt,trp]),
     hdt,
     "The backend that is used to populate the Geo API with."
   ).





download_handler(Req) :-
  rest_method(Req, [get], download_method).


download_method(Req, get, MTs) :-
  http_parameters(
    Req,
    [
      %backend(M, [oneof([hdt,trp])]),
      graph(G, [atom])
    ]
  ),
  setting(backend, M0),
  defval(M0, M),
  http_output(Req, Out),
  rest_media_type(Req, get, MTs, download_media_type(Out,M,G)).


download_media_type(Out, M, G, get, application/turtle) :-
  gtrace,
  set_stream(Out, type(text)),
  rdf_write_to_sink_legacy(Out, M, [graph(G),rdf_format(turtle)]).
