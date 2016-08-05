:- module(
  q_conv,
  [
    q_conv_options/2, % +Opts1, -Opts2
    q_transform/2     % +G, :Goal_3
  ]
).

/** <module> Quine conversion generics

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(dict_ext)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- meta_predicate
    q_transform(+, 3).

:- rdf_meta
   q_transform(+, t).





%! q_conv_options(+Opts1, -Opts2) is det.

q_conv_options(Opts1, Opts2) :-
  q_alias_prefix(ns, Prefix),
  uri_components(Prefix, Comps),
  uri_data(authority, Comps, Domain),
  merge_dicts(_{concept: resource, domain: Domain}, Opts1, Opts2).



%! q_transform(+G, :Goal_3) is det.

q_transform(G, Goal_3) :-
  q_load(rdf, G),
  call(Goal_3, rdf, rdf, G),
  q_save(rdf, G),
  q_unload(rdf, G).
