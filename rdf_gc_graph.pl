:- module(
  rdf_gc_graph,
  [
    rdf_graph_exclude_from_gc/1, % +Graph:atom
    rdf_graph_touch/1 % +Graph:atom
  ]
).

/** <module> RDF graph garbage collector

@author Wouter Beek
@version 2014/02, 2014/04
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(list_ext)).
:- use_module(generics(thread_ext)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(web_modules)).

http:location(rdf, root(rdf), []).
:- http_handler(rdf(gc_graph), rdf_gc_graph, []).

%! rdf_graph_exlcuded_from_gc(?Graph:atom) is nondet.

:- dynamic(rdf_graph_exlcuded_from_gc/1).

%! rdf_graph(?FirstTouch:float, ?LastTouch:float, ?Graph:atom) is nondet.

:- dynamic(rdf_graph/3).

user:web_module('RDF GC Graph', rdf_gc_graph).

:- initialization(init_rdf_gc_graph).



rdf_gc_graph(_Request):-
  reply_html_page(
    app_style,
    title('RDF garbage collect graphs'),
    \rdf_core_graphs
  ).

rdf_core_graphs -->
  {
    findall(
      First-Graph,
      rdf_graph(First, _, Graph),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    list_truncate(Pairs2, 100, Pairs3),
    findall(
      [Graph,First2],
      (
        member(First1-Graph, Pairs3),
        format_time(atom(First2), '%FT%T', First1)
      ),
      Rows
    )
  },
  html(
    \rdf_html_table(
      [header_row(true)],
      html('The core graphs'),
      [['Graph','Added']|Rows]
    )
  ).



rdf_graph_exclude_from_gc(Graph):-
  rdf_graph_exlcuded_from_gc(Graph), !.
rdf_graph_exclude_from_gc(Graph):-
  with_mutex(
    rdf_gc_graph,
    assert(rdf_graph_exlcuded_from_gc(Graph))
  ).


% Can't touch this!
rdf_graph_touch(Graph):-
  rdf_graph_exlcuded_from_gc(Graph).
rdf_graph_touch(Graph):-
  with_mutex(
    rdf_gc_graph,
    rdf_graph_touch_sync(Graph)
  ).

rdf_graph_touch_sync(Graph):-
  retract(rdf_graph(First, _, Graph)), !,
  get_time(Last),
  assert(rdf_graph(First, Last, Graph)).
rdf_graph_touch_sync(Graph):-
  get_time(Now),
  assert(rdf_graph(Now, Now, Graph)).



rdf_gc_graph:-
  rdf_statistics(triples(Triples)),
  rdf_gc_triples_by_graph(Triples).

rdf_gc_triples_by_graph(Triples):-
  % 10,000,000
  Triples =< 10000000, !.
rdf_gc_triples_by_graph(_):-
  findall(
    Time-Graph,
    rdf_graph(_, Time, Graph),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, [Graph|_]),
  
  % Remove from administration.
  retract(rdf_graph(_, Last, Graph)),
  
  % Assemble information to be displayed in debug message.
  duration(Last, Duration),
  rdf_statistics(triples_by_graph(Graph,Triples)),
  
  % Unload the graph and all of its contents.
  rdf_unload_graph_debug(Graph),
  
  % Display the debug message.
  format(
    user_output,
    '[-~:d triples] [~w dure] Unloaded graph ~w\n.',
    [Triples,Duration,Graph]
  ),
  flush_output(user_output),
  
  % See whether there is more work to do.
  rdf_gc_graph.

duration(Timestamp, Duration):-
  get_time(Now),
  Delta is Now - Timestamp,
  stamp_date_time(Delta, date(Y1,MM1,D1,H1,M1,S1,_,_,_), 'UTC'),
  Y2 is Y1 - 1970,
  MM2 is MM1 - 1,
  D2 is D1 - 1,
  format_time(atom(Duration), '%FT%T', date(Y2,MM2,D2,H1,M1,S1,0,-,-)).


init_rdf_gc_graph:-
  % Run 5 seconds.
  intermittent_thread(rdf_gc_graph, fail, 5, _, []).

