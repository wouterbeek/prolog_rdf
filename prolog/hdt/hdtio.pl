:- module(
  hdtio,
  [
    hdt_load_or_write/2, % +G, :Goal_1
    hdt_load_or_write/3  % +G, :Goal_1, +Opts
  ]
).

/** <module> HDT I/O

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(hdt/hdt_ext)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    hdt_load_or_write(+, 1),
    hdt_load_or_write(+, 1, +).

:- rdf_meta
   hdt_load_or_write(r, :),
   hdt_load_or_write(r, :, +).





%! hdt_load_or_write(+G, :Goal_1) is det.
%! hdt_load_or_write(+G, :Goal_1, +Opts) is det.

hdt_load_or_write(G, Goal_1) :-
  hdt_load_or_write(G, Goal_1, []).


hdt_load_or_write(G, _, Opts) :-
  hdt_load(G, Opts), !.
hdt_load_or_write(G, Goal_1, Opts) :-
  hdt_graph_to_file(G, [nt,gz], NTriplesFile),
  call_to_stream(NTriplesFile, [Out,M,M]>>call(Goal_1, Out), Opts).
