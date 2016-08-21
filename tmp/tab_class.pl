:- module(
  tab_class,
  [
    tab_class//1,  % +C
    tab_classes//1 % +G
  ]
).

/** <module> Tabular browser for RDF classes

Generates HTML tables for overviews of RDFS classes.

@author Wouter Beek
@version 2015/08, 2015/12, 2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(http/html_write)).
:- use_module(library(list_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdfs), [rdfs_individual_of/2]).
:- use_module(library(stat/rdfs_stat)).
:- use_module(library(tab/tab_generics)).





%! tab_class(+C)// is det.

tab_class(C) -->
  {
    aggregate_all(set([I]), rdfs_individual_of(I, C), Rows),
    length(Rows, Len)
  },
  html([
    \bs_table(
      html([
        "Overview of the ",
        \html_thousands(Len),
        " instances of RDFS class ",
        \qh_class(C),
        "."
      ]),
      \html_table_header_row(["Instance"]),
      \html_maplist(qh_term_row, Rows)
    ),
    \tab_node_triples(C, _)
  ]).



%! tab_classes(+G)// is det.
% Generates an HTML overview of the class-denoting terms in Graph.

tab_classes(G) -->
  {
    aggregate_all(
      set(C),
      (rdfs_individual_of(C, rdfs:'Class'), rdf_term(C, G)),
      Cs
    ),
    maplist(rdfs_number_of_instances, Cs, Ns),
    pairs_keys_values(Pairs, Ns, Cs),
    desc_pairs(Pairs, DescPairs),
    list_truncate(DescPairs, 100, TopPairs),
    maplist(tab_classes_row0, TopPairs, TopRows)
  },
  bs_table(
    html(["Overview of classes in RDF graph ",\qh_graph(G),"."]),
    html_table_header_row(["Class","Members"]),
    html_maplist(qh_class_count_row, TopRows)
  ).
tab_classes_row0(N-C, [C,N]).





% HELPERS %

qh_class_count_row([C,N]) -->
  html(tr([td(\qh_class(C)),td(\html_thousands(N))])).

qh_term_row([T]) -->
  html(tr(td(\qh_term(T)))).
