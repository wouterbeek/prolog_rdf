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

:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(library(persistency)).

:- initialization(db_attach('prefix_cc.db', [])).

:- persistent
   prefix(alias:atom, iri:atom).





init_prefix_cc :-
  forall(prefix_cc(Alias, Prefix), assert_prefix_cc(Alias, Prefix)).



prefix_cc(Alias, Prefix) :-
  csv_read_file(File, Rows),
  member(row(Alias, Prefix), Rows).
