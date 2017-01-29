:- module(
  sparql_update_client,
  [
    sparql_build_delete_where//3, % +Prefixes, +Bgps,    +Opts
    sparql_delete_data/3,         % +Iri,      +Tuples,  +Opts
    sparql_delete_where/3,        % +Iri,      +Body,    +Opts
    sparql_drop_graph/3,          % +Iri,      +G,       +Opts
    sparql_insert_data/3,         % +Iri,      +Triples, +Opts
    sparql_update_request/3       % +Iri,      +Body,    +Opts
  ]
).

/** <module> SPARQL Update client

API for performing SPARQL Update requests.

@author Wouter Beek
@version 2015/08, 2015/12, 2016/03-2016/04, 2016/07, 2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(http/http_io)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/io)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(sparql/sparql_build)).
:- use_module(library(sparql/sparql_ext)).
:- use_module(library(yall)).

:- rdf_meta
   sparql_drop_graph(+, r, +).





%! sparql_build_delete_where(+Prefixes, +Bgps, +Opts)// is det.

sparql_build_delete_where(Prefixes, Bgps, Opts) -->
  sparql_build(delete, Prefixes, [], Bgps, Opts).



%! sparql_delete_data(+Iri, +Tuples, +Opts) is det.

sparql_delete_data(Iri, Tuples, Opts) :-
  sparql_delete_insert_data(Iri, delete, Tuples, Opts).



%! sparql_delete_insert_data(+Iri, +Mode, +Tuples, +Opts) is det.
% Mode is either `delete` or `insert`.

sparql_delete_insert_data(Iri, Mode, Tuples, Opts) :-
  % Make sure that the default graphs option is instantiated.
  option(default_graphs(DGs), Opts, []),
  merge_options([default_graphs(DGs)], Opts, DGOpts),

  % Triples in the default graph have no graph element (length 3 lists).
  % Triples in a named graph have the named graph as the 4th element,
  % also called quadruples.
  partition([Entry]>>(Entry=rdf(_,_,_)), Tuples, Triples, Quads),

  % Throw an instantiation error in case no default graph is given,
  % but there is triple.
  (Triples \== [], DGs == [] -> instantiation_error(DGs) ; true),

  % Send SPARQL Update requests for all default graphs.
  sparql_update_request(
    Iri,
    sparql_delete_insert_data_body(Mode, Triples, Opts),
    DGOpts
  ),

  % For each named graph we want to perform one SPARQL Update request.
  % However, the quadruples that mention a specific graph
  % need not occur consecutively.
  findall(NG-rdf(S,P,O), member(rdf(S,P,O,NG), Quads), Pairs),
  group_pairs_by_key(Pairs, NG-Triples),
  merge_options([named_graphs([NG])], DGOpts, NGOpts),
  with_output_to(codes(Body),
    sparql_delete_insert_data_body(Mode, Triples, NGOpts)
  ),
  sparql_update_request(Iri, Body, NGOpts).


%! sparql_delete_insert_data_body(+Mode, +Triples, +Opts) is det.
% Mode is either `delete` or `insert`.

sparql_delete_insert_data_body(Mode, Triples, Opts1) :-
  % We want to retrieve the number of triples inserted/deleted
  % for debugging purposes.
  merge_options(Opts1, [triples(NumTriples)], Opts2),

  % Start of content block.
  % This is the only mode-dependent part.
  upcase_atom(Mode, UpcaseMode),
  format(current_output, "~a DATA {", [UpcaseMode]),

  % Write triple block contents.
  rdf_call_to_ntriples(current_output, rdf_write_ntuples(Triples, Opts2)),

  % End of content block.
  format(user_output, "}", []),

  % Debug message.
  debug(sparql(update), "SPARQL ~a DATA: ~D triples.", [UpcaseMode,NumTriples]).



%! sparql_delete_where(+Iri, +Body, +Opts) is det.

sparql_delete_where(Iri, Body, Opts) :-
  sparql_update_request(Iri, Body, Opts).



%! sparql_drop_graph(+Iri, +G, +Opts) is det.
% Options are passed to sparql_update_request/3.

sparql_drop_graph(Iri, G, Opts) :-
  format(atom(Body), "DROP SILENT GRAPH <~a>~n", [G]),
  sparql_update_request(Iri, Body, Opts).



%! sparql_insert_data(+Iri, +Tuples, +Opts) is det.

sparql_insert_data(Iri, Tuples, Opts) :-
  sparql_delete_insert_data(Iri, insert, Tuples, Opts).



%! sparql_update_impl(-Impl, +Opts) is det.

sparql_update_impl(Impl, Opts) :-
  option(manufacturer(Manufacturer), Opts),
  sparql_update_impl0(Manufacturer, Impl).

sparql_update_impl0(cliopatria, direct).
sparql_update_impl0(virtuoso, url_encoded).



%! sparql_update_request(+Iri, +Body, +Opts) is det.
% According to the standard, the query can be send using
% either URL encoded or direct (default).

sparql_update_request(Iri, Body, Opts) :-
  sparql_update_impl(url_encoded, Opts), !,
  sparql_update_request_url_encoded(Iri, Body, Opts).
sparql_update_request(Iri, Body, Opts) :-
  sparql_update_request_direct(Iri, Body, Opts).



%! sparql_update_request_direct(+Iri, +Body, +Opts) is det.
% Perform a SPARQL Update request using the direct method.

sparql_update_request_direct(Iri1, Body, Opts1) :-
  uri_components(Iri1, uri_components(Scheme,Auth,Path1,_,_)),
  sparql_path(update, Path1, Path2, Opts1),
  % The graph parameters (default graph, named graphs)
  % are sent as the search part of the POST URL.
  sparql_graph_params(update, Opts1, GParams, Opts2),
  uri_query_components(Query, GParams),
  uri_components(Iri2, uri_components(Scheme,Auth,Path2,Query,_)),
  % Send the SPARQL Update request.
  http_post(
    Iri2,
    codes('application/sparql-update', Body),
    {Reply}/[In,Meta,Meta]>>read_stream_to_string(In, Reply),
    Opts2
  ),
  debug(sparql(update), "~s", [Reply]).



%! sparql_update_request_url_encoded(+Iri, +Body, +Opts) is det.
% Perform a SPARQL Update request using the direct method.

sparql_update_request_url_encoded(Iri1, Body, Opts1) :-
  uri_components(Iri1, uri_components(Scheme,Auth,Path1,_,_)),
  sparql_path(update, Path1, Path2, Opts1),
  sparql_graph_params(update, Opts1, GParams, Opts2),
  uri_query_components(Query, GParams),
  uri_components(Iri2, uri_components(Scheme,Auth,Path2,Query,_)),

  % Both the graph parameters and the query are URI encoded.
  phrase(params(GParams), Body),
  phrase(iri_query_enc, Body, EncBody),
  debug(sparql(update), "~s", [EncBody]),

  % The IRI encoded content is sent in the body.
  % This is crazy, but this is what the standard says!
  http_post(
    Iri2,
    codes('application/x-www-form-urlencoded',EncBody),
    {Reply}/[In,Meta,Meta]>>read_stream_to_string(In, Reply),
    Opts2
  ),
  debug(sparql(reply), "~s", [Reply]).





% HELPERS %

param(Param) -->
  {Param =.. [Key,Val]},
  atom(Key), "=", atom(Val).


params([H|T]) --> !,
  "query=", param(H),
  sep_params(T).
params([]) --> [].


sep_params([H|T]) --> !,
  "&", param(H),
  sep_params(T).
sep_params([]) --> [].
