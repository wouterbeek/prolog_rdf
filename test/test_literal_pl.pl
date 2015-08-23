:- module(test_literal_pl, [run_tests/0]).

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- rdf_register_prefix(ex, 'http://www.example.org/').

run_tests:-
  % Atom
  rdf_assert_literal_pl(ex:s, ex:p, '1000'),
  % date/3
  rdf_assert_literal_pl(ex:s, ex:p, date(1000,10,1)),
  % date/9
  rdf_assert_literal_pl(ex:s, ex:p, date(1000,10,1,10,11,11.111,111,-,true)),
  % time/3
  rdf_assert_literal_pl(ex:s, ex:p, time(11,11,11.111)),
  % float
  rdf_assert_literal_pl(ex:s, ex:p, 1.11111111),
  % HTML DOM
  rdf_assert_literal_pl(ex:s, ex:p, [element(html,[],[element(p,[],[monkey])])]),
  % integer
  rdf_assert_literal_pl(ex:s, ex:p, 1),
  % Pair denoting a language-tagged string.
  rdf_assert_literal_pl(ex:s, ex:p, [en,'US']-one),
  % rational
  rdf_assert_literal_pl(ex:s, ex:p, 111111111 rdiv 100000000),
  % string
  rdf_assert_literal_pl(ex:s, ex:p, "1"),
  % XML DOM
  rdf_assert_literal_pl(ex:s, ex:p, [element(something,[],[element(inner,[],[])])]),

  rdf_print_graph(user, [ellip_lit(inf),ellip_iri(inf)]).
