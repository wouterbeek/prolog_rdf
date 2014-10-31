:- module(void_tabular, []).

/** <module> VoID: Tabular

Tabular browser for VoID data.

@author Wouter Beek
@version 2014/03, 2014/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plServer(web_modules)).

:- use_module(plRdf(rdf_dataset)).

:- use_module(plTabular(rdf_html_table)).

:- dynamic(http:location/3).
:- multifile(http:location/3).

http:location(void, root(void), []).

:- http_handler(void(tabular), void_tabular, []).

:- multifile(user:web_module/2).

user:web_module('VoID Tabular', void_tabular).



void_tabular(_Request):-
  aggregate_all(
    set(RdfDataset),
    rdf_dataset(RdfDataset),
    RdfDatasets
  ),
  maplist(rdf_dataset_rows, RdfDatasets, Rowss),
  append(Rowss, Rows),
  reply_html_page(
    plServer_style,
    title('VoID Tabular'),
    html(
      \rdf_html_table(
        html('Overview of currently loaded RDF datasets'),
        Rows,
        []
      )
    )
  ).


rdf_dataset_rows(
  RdfDataset,
  [[[rowspan=NumberOfGraphs]-Node,DefaultGraph,NumberOfTriples1]|Rows]
):-
  rdf_default_graph(RdfDataset, DefaultGraph),
  rdf_statistics(triples_by_graph(DefaultGraph,NumberOfTriples1)),
  findall(
    NamedGraph-NumberOfTriples2,
    (
      rdf_named_graph(RdfDataset, NamedGraph),
      rdf_statistics(triples_by_graph(NamedGraph,NumberOfTriples2))
    ),
    Pairs
  ),
  pairs_values(Pairs, NumberOfTripless),
  length(NumberOfTripless, NumberOfTriples),
  findall([X,Y], member(X-Y, Pairs), Rows),
  length(Rows, NumberOfNamedGraphs),
  NumberOfGraphs is NumberOfNamedGraphs + 1,
  format(
    atom(Node),
    '~:d graphs with ~:d triples',
    [NumberOfGraphs,NumberOfTriples]
  ).

