:- module(
  jsonld_generics,
  [
    jsonld_abbreviate_iri/3, % +Context, +Full, -Compact
    jsonld_expand_iri/3      % +Context, +Compact, -Full
  ]
).

/** <module> JSON-LD generics

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).





%! jsonld_abbreviate_iri(+Context, +Full, -Compact) is det.

jsonld_abbreviate_iri(Context, Full, Compact) :-
  context_to_map(Context, Map),
  is_iri(Full),
  member(Alias-Prefix, Map),
  atom_concat(Prefix, Local, Full), !,
  atomic_list_concat([Alias,Local], :, Compact).



%! jsonld_expand_iri(+Context, +Compact, -Full) is det.

jsonld_expand_iri(Context, Compact, Full) :-
  atomic_list_concat([Alias,Local], :, Compact),
  \+ atom_prefix(Local, '//'),
  context_to_map(Context, Map),
  memberchk(Alias-Prefix, Map), !,
  atomic_concat(Prefix, Local, Full).

context_to_map(Context, Map) :- get_dict(map, Context, Map), !.
context_to_map(Context, Map) :- dict_pairs(Context, Map).
