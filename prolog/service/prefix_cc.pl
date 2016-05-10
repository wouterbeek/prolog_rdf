:- module(
  prefix_cc,
  [
    prefix_cc/2 % ?Alias, ?Prefix
  ]
).

/** <module> Prefix.cc

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(lists)).
:- use_module(library(persistency)).

:- initialization(db_attach('prefix_cc.db', [])).

:- persistent
   prefix_cc(alias:atom, iri:atom).





assert_prefix_cc(Alias-Prefix) :-
  assert_prefix_cc(Alias, Prefix).



init_prefix_cc :-
  json_download('http://prefix.cc/popular/all.file.json', D),
  dict_pairs(D, Pairs),
  maplist(assert_prefix_cc, Pairs).
