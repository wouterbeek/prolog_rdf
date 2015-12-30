:- module(
  rdf_deref,
  [
    rdf_deref/1, % +Source
    rdf_deref/2, % +Source, -Graph
    rdf_deref/3, % +Subject:iri, -Predicate, -Object
    rdf_deref/4, % +Source
                 % -Subject:or([bnode,iri])
                 % -Predicate:iri
                 % -Object:rdf_term
    rdf_derefs/0,
    rdf_derefs/1 % +Options:list(compound)
  ]
).

/** <module> RDF: Synchronization

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(http/http_info)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(uri)).

:- debug(rdf_deref).

:- rdf_meta(rdf_deref(r)).
:- rdf_meta(rdf_deref(r,-)).
:- rdf_meta(rdf_deref(r,-,-)).
:- rdf_meta(rdf_deref(r,-,-,-)).

:- predicate_options(rdf_derefs/1, 1, [
     excluded_authorities(+list(atom))
   ]).





%! rdf_deref(+Source) is det.
% Wrapper around rdf_deref/2 that does not return the graph.

rdf_deref(Source):-
  rdf_deref(Source, _).


%! rdf_deref(+Source, -Graph:rdf_graph) is det.

rdf_deref(Source, G):-
  iri_normalized(Source, G),
  rdf_load_file(Source, [graph(G)]),
  if_debug(rdf_deref, rdf_print_graph(G)).


%! rdf_deref(+Subject:iri, -Predicate:iri, -Object:rdf_term) is nondet.

rdf_deref(S, P, O):-
  rdf_deref(S, S, P, O).


%! rdf_deref(
%!   +Source,
%!   -Subject:or([bnode,iri]),
%!   -Predicate:iri,
%!   -Object:rdf_term
%! ) is nondet.

rdf_deref(Source, S, P, O):-
  rdf_call_on_graph(Source, rdf_petty_min(S, P, O)).



%! rdf_derefs is det.
% Wrapper around rdf_derefs/1 with default options.

rdf_derefs:-
  rdf_derefs([]).


%! rdf_derefs(+Options:list(compound)) is det.
% The following options are supported:
%   * excluded_authorities(+list(atom))
%     Default is `[]'.

rdf_derefs(Opts):-
  option(excluded_authorities(ExclAuths), Opts, []),
  gtrace,
  % NONDET.
  rdf_iri(X),
  uri_components(X, uri_components(Scheme,Auth,_,_,_)),
  http_scheme(Scheme),
  (memberchk(Auth, ExclAuths) -> true ; rdf_deref(X)),
  fail.
rdf_derefs(_).
