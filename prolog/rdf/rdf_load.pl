:- module(
  rdf_load,
  [
    rdf_load_any/1, % +Spec
    rdf_load_any/2 % +Spec
                   % +Options:list(compound)
  ]
).

/** <module> RDF load

Support for loading RDF data.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(option)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).





%! rdf_load_any(+Spec) is det.

rdf_load_any(Spec):-
  rdf_load_any(Spec, []).

%! rdf_load_any(+Spec, +Options:list(compound)) is det.
% Options are passed to rdf_load/2.

rdf_load_any(Spec, Opts0):-
  % Set the graph name.
  option(graph(G), Opts0, _VAR),
  (   var(G),
      atom(Spec)
  ->  G = Spec
  ;   true
  ),
  merge_options([graph(G)], Opts0, Opts),
  rdf_stream(Spec, rdf_load0(Opts), Opts).

rdf_load0(Opts0, Format, Read):-
  merge_options([format(Format)], Opts0, Opts),
  rdf_load(Read, Opts).
