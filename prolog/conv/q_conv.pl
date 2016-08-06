:- module(
  q_conv,
  [
    q_conv/0,
    q_conv/1,         % +Name
    q_conv_options/2, % +Opts1, -Opts2
    q_transform/2     % +G, :Goal_3
  ]
).

/** <module> Quine conversion generics

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [rdf_load/2]).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(uri)).

:- meta_predicate
    q_transform(+, 3).

:- multifile
    q_conv_hook/1.

:- rdf_meta
   q_transform(+, t).





%! q_conv is det.
%! q_conv(+Name) is det.

q_conv :-
  q_conv(Name),
  debug(q(q_conv), "~a", [Name]),
  fail.
q_conv.


q_conv(Name) :-
  nonvar(Name), !,
  once(q_conv_hook(Name)).
q_conv(Name) :-
  q_conv_hook(Name).



%! q_conv_options(+Opts1, -Opts2) is det.

q_conv_options(Opts1, Opts2) :-
  q_alias_prefix(ns, Prefix),
  uri_components(Prefix, Comps),
  uri_data(authority, Comps, Domain),
  merge_dicts(_{concept: resource, domain: Domain}, Opts1, Opts2).



%! q_transform(+G, :Goal_3) is det.

q_transform(G, Goal_3) :-
  q_graph:q_graph_to_file(store, G, ntriples, File),
  setup_call_cleanup(
    rdf_load(File, [format(ntriples),graph(G)]),
    (
      call(Goal_3, rdf, rdf, G),
      q_save(rdf, G)
    ),
    rdf_unload_graph(G)
  ).
