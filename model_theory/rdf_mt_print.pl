:- module(
  rdf_mt_print,
  [
    rdf_mt_print_model/3, % +Out
                          % +Graph:atom
                          % +Model:atom
    rdf_mt_print_model/4 % +Out
                         % +Graph:atom
                         % +Model:atom
                         % +Assignments:list(pair(bnode,resource))
  ]
).

/** <module> RDF_MT_PRINT

Semantic model printing.

Sytax-to-semantics map printing.

@author Wouter Beek
@version 2013/08, 2014/01
*/

:- use_module(dcg(dcg_collection)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_mt(rdf_mt)).



% GRAPH %

rdf_mt_print_graph(G):-
  format('GRAPH ~w\n', [G]),
  rdf_graph:rdf_graph_to_triples(G, Ts),
  forall(
    member(T, Ts),
    (tab, dcg_with_output_to(current_output, rdf_triple_name(T)), nl)
  ).



% MAP %

rdf_mt_print_asssignments(_G, _M, []):- !.
rdf_mt_print_asssignments(G, M, A):-
  format('ASSIGNMENTS ~w -> ~w\n', [G,M]),
  forall(
    member([BNode,Resource], A),
    format('\t~w\t->\t~w\n', [BNode,Resource])
  ).

rdf_mt_print_i_l(G, M):-
  \+ i_l(G, _TypedLiteral, M, _Resource), !.
rdf_mt_print_i_l(G, M):-
  format('IL : ~w -> ~w\n', [G, M]),
  forall(
    i_l(G, TypedLiteral, M, Resource),
    (
      TypedLiteral = literal(type(Type,Value)),
      format('\t"~w"^^~w\t->\t~w\n', [Value,Type,Resource])
    )
  ).

rdf_mt_print_i_s(G, M):-
  \+ i_s(G, _IRI, M, _Resource), !.
rdf_mt_print_i_s(G, M):-
  format('IS : ~w -> ~w\n', [G, M]),
  forall(
    i_s(G, IRI, M, Resource),
    format('\t~w\t->\t~w\n', [IRI,Resource])
  ).

rdf_mt_print_lv(G, M):-
  \+ lv(G, M, _PlainLiteral), !.
rdf_mt_print_lv(G, M):-
  format('LV ~w ~w:\n', [G,M]),
  forall(
    lv(G, M, PlainLiteral),
    format('\t~w\n', [PlainLiteral])
  ).

rdf_mt_print_map(G, M):-
  rdf_mt_print_i_l(G, M),
  rdf_mt_print_i_s(G, M),
  rdf_mt_print_lv(G, M).

rdf_mt_print_map(G, M, A):-
  rdf_mt_print_map(G, M),
  rdf_mt_print_asssignments(G, M, A).



% MODEL %

rdf_mt_print_model(M):-
  rdf_mt_print_resources(M),
  rdf_mt_print_properties(M),
  rdf_mt_print_i_ext(M).

rdf_mt_print_model(Out, G, M):-
  with_output_to(Out, rdf_mt_print_model_(G, M)).

rdf_mt_print_model(Out, G, M, A):-
  with_output_to(Out, rdf_mt_print_model_(G, M, A)),
  flush_output(Out).

rdf_mt_print_model_(G, M):-
  nl, write('***'), nl,
  rdf_mt_print_graph(G),
  rdf_mt_print_model(M),
  rdf_mt_print_map(G, M).

rdf_mt_print_model_(G, M, A):-
  nl, write('***'), nl,
  rdf_mt_print_graph(G),
  rdf_mt_print_model(M),
  rdf_mt_print_map(G, M, A).

rdf_mt_print_properties(M):-
  \+ property(M, _P), !.
rdf_mt_print_properties(M):-
  format(user_output, 'PROPERTIES ~w:\n', [M]),
  forall(
    property(M, P),
    format('\t~w\n', [P])
  ).

rdf_mt_print_resources(M):-
  \+ resource(M, _R), !.
rdf_mt_print_resources(M):-
  format(user_output, 'RESOURCES ~w:\n', [M]),
  forall(
    resource(M, R),
    format('\t~w\n', [R])
  ).

rdf_mt_print_i_ext(M):-
  \+ i_ext(M, _Property, _Resource1, _Resource2), !.
rdf_mt_print_i_ext(M):-
  format(user_output, 'IEXT ~w:\n', [M]),
  forall(
    (
      property(M, Property),
      findall(
        Resource1-Resource2,
        i_ext(M, Property, Resource1, Resource2),
        ResourcePairs
      )
    ),
    (
      format('\t~w -> ', [Property]),
      dcg_with_output_to(current_output, set(pair(ascii), ResourcePairs)),
      nl
    )
  ).

