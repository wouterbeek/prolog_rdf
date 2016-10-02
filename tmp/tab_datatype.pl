:- module(
  tab_datatype,
  [
    tab_datatype//1, % +D
    tab_datatypes//1 % ?G
  ]
).

/** <module> Tabular browser for RDF datatypes

Generated HTML overviews of singular and of multiple datatype IRIs.

@author Wouter Beek
@version 2015/08, 2015/12, 2016/03, 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/zh)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_prefix), []).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).





%! tab_datatype(+D)// is det.

% Datatype `rdf:langString` has no interesting value (= lexical form)
% but does have a language tag.
tab_datatype(D) -->
  {
    rdf_equal(rdf:langString, D), !,
    aggregate_all(
      set(N-Lex@LTag),
      (rdf_literal(Lex@LTag), aggregate_all(count, rdf(_, _, Lex@LTag), N)),
      Pairs
    )
  },
  rdf_datatype_table(D, Pairs, "Language tag").
% All other datatypes.
tab_datatype(D) -->
  {
    aggregate_all(
      set(N-Lex^^D),
      (rdf_literal(Lex^^D), aggregate_all(count, rdf(_, _, Lex^^D), N)),
      Pairs
    )
  },
  rdf_datatype_table(D, Pairs, "Value").


rdf_datatype_table(D, Pairs, ColumnHeader) -->
  {
    desc_pairs(Pairs, DescPairs),
    findall([H|T], member(H-T, DescPairs), Rows)
  },
  table(
    html(["Overview of datatype IRI ",\zh_datatype(D),"."]),
    \table_header_row(["Number of literals","Lexical from","Datatype IRI",ColumnHeader]),
    \html_maplist(table_data_row, Rows)
  ).



%! tab_datatypes(?G)// is det.
%
% Generates an HTML enumeration of datatypes in the given graph (if
% given), sorted by the number of occurrences (as a proxy for
% relevance).

tab_datatypes(G) -->
  {
    % First collect all datatype IRIs.
    aggregate_all(set(D), rdf_datatype_iri(D, G), Ds),
    
    % Then look up the number of triples in which a typed literal
    % with a specific datatype IRI occurs.
    aggregate_all(
      set(NTs-D),
      (
        member(D, Ds),
        aggregate_all(count, rdf(_, _, _^^D, _), NTs)
      ),
      Pairs
    ),
    desc_pairs(Pairs, DescPairs),
    findnsols(100, [NTs,D], member(NTs-D, DescPairs), Rows)
  },
  (   {Rows == []}
  ->  html([])
  ;   table(
        html(["Overview of datatype IRIs in graph ",\zh_graph(G),"."]),
        table_header_row(["Number of literals","Datatype IRI"]),
        html_maplist(table_data_row, Rows)
      )
  ).
