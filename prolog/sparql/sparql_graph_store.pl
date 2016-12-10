:- module(
  sparql_graph_store,
  [
    sparql_get_graph/3,             % +Iri, ?G, -Triples
    sparql_get_graph/4,             % +Iri, ?G, -Triples, +Opts
    sparql_post_graph_file/3,       % +Iri, ?G, +File
    sparql_post_graph_file/4,       % +Iri, ?G, +File,  +Opts
    sparql_post_graph_statements/3, % +Iri, ?G, +Triples
    sparql_post_graph_statements/4  % +Iri, ?G, +Triples, +Opts
  ]
).

/** <module> SPARQL Graph Store

Partial implementation of the SPARQL Graph Store.

@author Wouter Beek
@compat http://www.w3.org/TR/sparql11-http-rdf-update/
@version 2015/08, 2016/01, 2016/03-2016/04
*/

:- use_module(library(debug)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(http/http_io)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(os/io)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(sparql/sparql_ext)).
:- use_module(library(yall)).





%! sparql_get_graph(+Iri, ?G, -Quads) is det.
%! sparql_get_graph(+Iri, ?G, -Quads, +Opts) is det.

sparql_get_graph(Iri, G, Quads) :-
  sparql_get_graph(Iri, G, Quads, []).


sparql_get_graph(Iri1, G, Triples, Opts) :-
  uri_components(Iri1, uri_components(Scheme,Auth,Path1,_,_)),
  sparql_path(http, Path1, Path2, Opts),
  sparql_graph_param(G, Param, Opts),
  uri_query_components(Query, [Param]),
  uri_components(Iri2, uri_components(Scheme,Auth,Path2,Query,_)),
  rdf_load_triples(Iri2, Triples, Opts).



%! sparql_post_graph_file(+Iri, ?G, +File) is det.
%! sparql_post_graph_file(+Iri, ?G, +File, +Opts) is det.

sparql_post_graph_file(Iri, G, File) :-
  sparql_post_graph_file(Iri, G, File, []).


sparql_post_graph_file(Iri, G, File0, Opts) :-
  absolute_file_name(File0, File, [access(read)]),
  rdf_guess_media_type(File, MT),
  sparql_post_graph_data(Iri, G, file(MT,File), Opts).



%! sparql_post_graph_statements(+Iri, ?G, +Triples) is det.
%! sparql_post_graph_statements(+Iri, ?G, +Triples, +Opts) is det.

sparql_post_graph_statements(Iri, G, Triples) :-
  sparql_post_graph_statements(Iri, G, Triples, []).


sparql_post_graph_statements(Iri, G, Triples, Opts) :-
  call_to_ntriples(codes(Cs), gen_ntuples(Triples)),
  debug(sparql(graph_store), "~s", [Cs]),
  sparql_post_graph_data(Iri, G, codes(application/'n-triples',Cs), Opts).





% HELPERS %

%! sparql_graph_param(+G, -Param, +Opts) is det.

% Default graph.
sparql_graph_param(G, Key, Opts) :-
  var(G), !,
  sparql_option_name(http, default_graph, Key, Opts).
% Named graph.
sparql_graph_param(G, Key-G, Opts) :-
  sparql_option_name(http, named_graph, Key, Opts).



%! sparql_post_graph_data(+Iri, ?G, +Data:compound, +Opts) is det.

sparql_post_graph_data(Iri1, G, Data1, Opts) :-
  uri_components(Iri1, uri_components(Scheme,Auth,Path1,_,_)),
  sparql_path(http, Path1, Path2, Opts),
  sparql_graph_param(G, Param, Opts),
  uri_query_components(Query, [Param]),
  uri_components(Iri2, uri_components(Scheme,Auth,Path2,Query,_)),
  fix_media_type(Data1, Data2),
  http_post(
    Iri2,
    Data2,
    {Reply}/[In,Meta,Meta]>>read_stream_to_string(In, Reply),
    Opts
  ), !,
  debug(sparql(graph_store), "~s", [Reply]).


fix_media_type(codes(Type/Subtype,Cs), codes(A,Cs)) :- !,
  atomic_concat(Type, Subtype, A).
fix_media_type(file(Type/Subtype,File), file(A,File)) :-
  atomic_concat(Type, Subtype, A).
