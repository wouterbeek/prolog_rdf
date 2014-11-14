:- module(
  jsonld_build,
  [
    jsonld_dict/2 % +POPairs:list(compound)
                  % -Dict:dict
  ]
).

/** <module> JSON-LD: Build

Builds JSON-LD structures.

@author Wouter Beek
@version 2014/10
*/

:- use_module(library(apply)).



jsonld_dict(POPairs, Dict):-
  maplist(pairify, POPairs, Pairs),
  dict_pairs(Dict, json, Pairs).

pairify(popair(Property,Object), [Property-Dict]):-
  dict_create(Dict, json, ["@id"-Object]).
