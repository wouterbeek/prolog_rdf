:- module(
  sparql_ext,
  [
    sparql_authentication/3, % +User, +Password, -Opt
    sparql_endpoint/3,       % +Endpoint, -Iri, -Opts
    sparql_graph_params/4,   % +Mode, +Opts, -GraphParams, -RestOpts
    sparql_option_name/4,    % +Mode, +Name1, -Name2, +Opts
    sparql_path/4            % +Mode, +Path1, -Path2, +Opts
  ]
).

/** <module> SPARQL extensions

@author Wouter Beek
@version 2015/08, 2015/12, 2016/03-2016/04
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option_ext)).
:- use_module(library(yall)).





%! sparql_authentication(+User, +Password, -Opt) is det.

sparql_authentication(User, Password, Opt) :-
  atomic_list_concat([User,Password], :, Plain),
  base64(Plain, Enc),
  atomic_list_concat(['Basic',Enc], ' ', Auth),
  Opt = request_header('Authorization'=Auth).



%! sparql_endpoint(+Endpoint, -Iri, -Opts) is multi.

sparql_endpoint(dbpedia, Iri, [menufacturer(virtuoso)]) :-
  member(Iri, ['http://dbpedia.org/','http://live.dbpedia.org']).



%! sparql_graph_params(+Mode, +Opts, -GraphParams, -RestOpts) is det.
% Mode is either `http`, `query` or `update`.
%
% The following options are processed to generate the GraphParameters:
%   * default_graphs(-list(atom))
%   * named_graphs(-list(atom))

sparql_graph_params(Mode, Opts1, Params3, Opts3):-
  sparql_graph_params(default_graph, Mode, Opts1, Params1, Opts2),
  sparql_graph_params(named_graph, Mode, Opts2, Params2, Opts3),
  merge_options(Params1, Params2, Params3).


sparql_graph_params(Name1, Mode, Opts, Params, RestOpts):-
  sparql_option_name(Mode, Name1, Name2, Opts),
  option_components(Opt, Name1, Vals),
  select_option(Opt, Opts, RestOpts, []),
  maplist(
    {Name2}/[Val,Param]>>option_components(Param, Name2, Val),
    Vals,
    Params
  ).



%! sparql_option_name(+Mode, +Name1, -Name2, +Opts) is det.
% @arg Kind is `default_graph` or `named_graph`.
% @arg Manufacturer may be instantiated or not.
% @arg Mode is `http`, `query` or `update`.
% @arg Name The name of the default graphs or named graphs option
%      for the given mode as used by the given manufacturer, if any.

sparql_option_name(Mode, Name1, Name2, Opts) :-
  option(manufacturer(Manufacturer), Opts),
  sparql_option_name0(Mode, Manufacturer, Name1, Name2), !.
sparql_option_name(Mode, Name1, Name2, _) :-
  sparql_option_name0(Mode, Name1, Name2).

sparql_option_name0(http, default_graph, default).
sparql_option_name0(http, named_graph, graph).
sparql_option_name0(query, default_graph, 'default-graph-uri').
sparql_option_name0(query, named_graph, 'named-graph-uri').
sparql_option_name0(update, default_graph, 'using-graph-uri').
sparql_option_name0(update, named_graph, 'using-named-graph-uri').

sparql_option_name0(http, virtuoso, default_graph, graph).
sparql_option_name0(http, virtuoso, named_graph, 'graph-uri').



%! sparql_path(+Mode, +Path1, -Path2, +Opts) is det.
% @arg Mode is either `http`, `query` or `update`.
% @arg Manufacturer is either `cliopatria` or `virtuoso`.

sparql_path(Mode, _, Path, Opts) :-
  option(manufacturer(Manufacturer), Opts),
  sparql_path0(Mode, Manufacturer, Path), !.
sparql_path(_, Path, Path, _).

sparql_path0(http,   virtuoso,   '/sparql-graph-crud').
sparql_path0(query,  cliopatria, '/sparql/').
sparql_path0(query,  virtuoso,   '/sparql').
sparql_path0(update, cliopatria, '/sparql/update').
sparql_path0(update, virtuoso,   '/update').
