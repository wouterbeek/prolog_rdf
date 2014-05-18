:- module(rdf_mt_test, []).

/** <module> RDF_MT_TEST

Tests for RDFS model theory.

@author Wouter Beek
@version 2013/08
*/

:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_mt(rdf_mt_build)).
:- use_module(rdf_mt(rdf_mt_i)).
:- use_module(rdf_mt(rdf_mt_print)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ex, 'http://www.example.com/').

:- begin_tests(rdf_mt).

test(
  rdf_mt_true,
  [
    cleanup(clean_test(G,M)),
    forall(test_name(true, G, M)),
    setup(build_test(G,M))
  ]
):-
  mt_i(G, M, A),
  rdf_mt_print_model(user_output, G, M, A).

test(
  rdf_mt_false,
  [
    cleanup(clean_test(G,M)),
    fail,
    forall(test_name(false, G, M)),
    setup(build_test(G,M))
  ]
):-
  mt_i(G, M, A),
  rdf_mt_print_model(user_output, G, M, A).

% @tbd This is in option setup/1 of test/2. Still red...
build_test(G, M):-
  build_test_syntax(G),
  build_test_model(G, M),
  build_test_map(G, M).

build_test_map(G, M):-
  (G = rdf_mt_graph_1, ! ; G = rdf_mt_graph_3),
  M = rdf_mt_model_1,
  rdf_add_i_s(G, ex:a, M, 1),
  rdf_add_i_s(G, ex:b, M, 1),
  rdf_add_i_s(G, ex:c, M, 2),
  rdf_add_i_l(G, literal(type(ex:b,whatever)), M, 2).
build_test_map(G, M):-
  G = rdf_mt_graph_2,
  M = rdf_mt_model_1,
  rdf_add_i_s(G, ex:a, M, 1),
  rdf_add_i_s(G, ex:b, M, 1),
  rdf_add_i_s(G, ex:c, M, 2).

build_test_model(G, M):-
  rdf_add_model(M),
  % Resources.
  rdf_add_resource(M, 1),
  rdf_add_resource(M, 2),
  rdf_add_plain_literals(G, M),

  % Properties.
  rdf_add_property(M, 1),
  rdf_add_i_ext(M, 1, 1, 2),
  rdf_add_i_ext(M, 1, 2, 1).

build_test_syntax(G):-
  G = rdf_mt_graph_1,
  rdf_assert(ex:a, ex:b, ex:c, G),
  rdf_assert(ex:c, ex:a, ex:a, G),
  rdf_assert(ex:c, ex:b, ex:a, G),
  rdf_assert(ex:a, ex:b, literal(type(ex:b,whatever)), G).
build_test_syntax(G):-
  G = rdf_mt_graph_2,
  rdf_assert(ex:a, ex:c, ex:b, G),
  rdf_assert(ex:a, ex:b, ex:b, G),
  rdf_assert(ex:c, ex:a, ex:c, G),
  rdf_assert(ex:a, ex:b, literal(whatever), G).
build_test_syntax(G):-
  G = rdf_mt_graph_3,
  rdf_bnode(X),
  rdf_assert(   X, ex:b, ex:c, G),
  rdf_assert(ex:c, ex:a, ex:a, G),
  rdf_assert(ex:c, ex:b, ex:a, G),
  rdf_assert(ex:a, ex:b, literal(type(ex:b,whatever)), G).

clean_test(G, M):-
  rdf_unload_model(M),
  rdf_unload_graph_debug(G).

%! test_name(?Succeeds:boolean, ?Graph:atom, ?Model:atom) is nondet.
%
% ## `rdf_mt_graph_1` & `rdf_mt_model_1`
%
% The first example from Hayes2004 (true):
% ~~~
% <ex:a> <ex:b> <ex:c> .
% <ex:c> <ex:a> <ex:a> .
% <ex:c> <ex:b> <ex:a> .
% <ex:a> <ex:b> "whatever"^^<ex:b> .
% ~~~
%
% ## `rdf_mt_graph_2` & `rdf_mt_model_1`
%
% The second example from Hayes2004 (false):
% ~~~
% <ex:a> <ex:c> <ex:b> .
% <ex:a> <ex:b> <ex:b> .
% <ex:c> <ex:a> <ex:c> .
% <ex:a> <ex:b> "whatever" .
% ~~~
%
% ## `rdf_mt_graph_3` & `rdf_mt_model_1`
%
% Variant with blank nodes (true):
% ~~~
%   _:x  <ex:c> <ex:b> .
% <ex:a> <ex:b> <ex:b> .
% <ex:c> <ex:a> <ex:c> .
% <ex:a> <ex:b> "whatever"^^<ex:b> .
% ~~~

test_name(true,  rdf_mt_graph_1, rdf_mt_model_1).
test_name(false, rdf_mt_graph_2, rdf_mt_model_1).
test_name(true,  rdf_mt_graph_3, rdf_mt_model_1).

:- end_tests(rdf_mt).

