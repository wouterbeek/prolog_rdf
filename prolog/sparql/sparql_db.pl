:- module(
  sparql_db,
  [
    sparql_endpoint_by_resource/2, % +Resource:iri
                                   % -Endpoint:atom
    sparql_endpoint_location/2, % +Endpoint:atom
                                % -Location:atom
    sparql_endpoint_location/3, % +Endpoint:atom
                                % ?Mode:oneof([http,query,update])
                                % -Location:atom
    sparql_location_by_resource/3, % +Resource:iri
                                   % +Mode:oneof([http,query,update])
                                   % -Location:atom
%%%%
    sparql_endpoint/1, % ?Endpoint:atom
    sparql_endpoint_option/3, % +Endpoint:atom
                              % ?Name:compound
                              % ?Value:compound
    sparql_register_endpoint/3, % +Endpoint:atom
                                % +Locations:list(uri)
                                % +Manufacturer:atom
    sparql_remove_endpoint/1 % +SparqlRemote:atom
  ]
).

/** <module> SPARQL database

Persistent store of SPARQL-related information.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(base64)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(service_db)).
:- use_module(library(typecheck)).
:- use_module(library(uri_ext)).

%! sparql_endpoint(?Endpoint:atom) is nondet.
% Currently registered SPARQL endpoints.

:- dynamic(sparql_endpoint/1).

%! sparql_endpoint_option0(
%!   ?Endpoint:atom,
%!   ?Key:compound,
%!   ?Value:compound
%! ) is nondet.
% A special endpoint option is `manufacturer`,
% which sets the default options for a given manufacturer.
% Endpoint options can override manufacturer options.
%
% Another special option is `location`,
% which uses a compound term to denote a location for the given endpoint.
% An endpoint has at least one location.

:- dynamic(sparql_endpoint_option0/3).
:- multifile(sparql_endpoint_option0/3).

%! sparql_manufacturer_option0(
%!   ?Endpoint:atom,
%!   ?Key:compound,
%!   ?Value:compound
%! ) is nondet.
% Manufacturer options are generic settings that endpoints
% from a specific manufacturer have.
%
% Individual endpoints can override these settings.

:- dynamic(sparql_manufacturer_option0/3).





%! sparql_endpoint_by_resource(+Resource:iri, -Endpoint:atom) is nondet.
% Endpoints that are associated with the prefix of the given resource.

sparql_endpoint_by_resource(Resource, Endpoint):-
  uri_component(Resource, host, Host),
  sparql_endpoint_option(Endpoint, location, Location),
  uri_component(Location, host, Host).



%! sparql_endpoint_mode(+Mode:atom) is semidet.
%! sparql_endpoint_mode(-Mode:atom) is multi.
% The supported SPARQL modes.

sparql_endpoint_mode(http).
sparql_endpoint_mode(query).
sparql_endpoint_mode(update).



%! sparql_endpoint_option(
%!   +Endpoint:atom,
%!   +Key:compound,
%!   -Value:compound
%! ) is det.
% Option lookup for SPARQL endpoints.
%
% Since manufacturers of SPARQL endpoints deviate from the SParql standards
% in idiosyncratic ways, the values of several optionsare relative to
% an endpoint's manufacturer.

% The authentication option is special,
% since it requires access to the service database.
sparql_endpoint_option(Endpoint, authentication(Mode), Value):-
  % Retrieve a user registration for the given SPARQL endpoint.
  service(Endpoint, login(User,Password)),

  % ClioPatria uses HTTP authentication  for SPARQL Update requests.
  sparql_endpoint_option0(Endpoint, manufacturer, cliopatria),
  Mode == update,

  atomic_list_concat([User,Password], :, Plain),
  base64(Plain, Encoded),
  atomic_list_concat(['Basic',Encoded], ' ', Authentication),
  Value = request_header('Authorization'=Authentication).
% Options that are set on the manufacturer level.
sparql_endpoint_option(Endpoint, Name, Value):-
  % Optimization: locations are always set for specific endpoints,
  % never generically, i.e. for endpoint manufacturers.
  Name \== location,

  sparql_endpoint_option0(Endpoint, manufacturer, Manufacturer),
  sparql_manufacturer_option0(Manufacturer, Name, Value),

  % Individual endpoints can override manufacturer settings!
  \+ sparql_endpoint_option0(Endpoint, Name, _).
% Options that are set at the endpoint-specific level.
sparql_endpoint_option(Endpoint, Name, Value):-
  sparql_endpoint_option0(Endpoint, Name, Value).

sparql_manufacturer_option0(cliopatria, method(update), direct).
sparql_manufacturer_option0(cliopatria, path_suffix(query), '/sparql/').
sparql_manufacturer_option0(cliopatria, path_suffix(update), '/sparql/update').
sparql_manufacturer_option0(virtuoso, default_graph(http), graph).
sparql_manufacturer_option0(virtuoso, method(update), url_encoded).
sparql_manufacturer_option0(virtuoso, named_graph(http), 'graph-uri').
sparql_manufacturer_option0(virtuoso, path_suffix(http), '/sparql-graph-crud').
sparql_manufacturer_option0(virtuoso, path_suffix(query), '/sparql').
sparql_manufacturer_option0(virtuoso, path_suffix(update), '/update').



%! sparql_endpoint_location(+Endpoint:atom, -Uri:atom) is nondet.

sparql_endpoint_location(Endpoint, Uri):-
  sparql_endpoint_location(Endpoint, _, Uri).


%! sparql_endpoint_location(
%!   +Endpoint:atom,
%!   ?Mode:oneof([http,query,update]),
%!   -Uri:atom
%! ) is nondet.
% Returns the URL locations that are associated with the given endpoint+mode.
%
% @throws existence_error If the given SPARQL endpoint
%         is not available at any location.

sparql_endpoint_location(Uri, Mode, Uri):-
  is_uri(Uri), !,
  \+ sparql_endpoint_option(Uri, Mode, _).
sparql_endpoint_location(Endpoint, Mode, Uri):-
  % NONDET: There may be multiple locations registered with an endpoint.
  sparql_endpoint_option(Endpoint, location, Base),

  (   % Slight optimization by first looking for the mode part,
      % since every registration will have a location,
      % but not all registrations will contain every mode.
      sparql_endpoint_option(Endpoint, path_suffix(Mode), PathSuffix)
  ->  % Build the mode-specific location.
      uri_normalized(PathSuffix, Base, Uri)
  ;   Uri = Base
  ).



%! sparql_location_by_resource(
%!   +Resource:iri,
%!   +Mode:oneof([http,query,update]),
%!   -Uri:atom
%! ) is nondet.

sparql_location_by_resource(Resource, Mode, Uri):-
  % Find an endpoint that is associated with
  % (the prefix of) a given resource.
  sparql_endpoint_by_resource(Resource, Endpoint),

  % Find a location -- possibly different from the given resource! --
  % for the given endpoint.
  sparql_endpoint_location(Endpoint, Mode, Uri).



%! sparql_register_endpoint(
%!   +Endpoint:atom,
%!   +Locations:list(uri),
%!   +Manufacturer:atom
%! ) is det.

sparql_register_endpoint(Endpoint, Locations, Manufacturer):-
  assert(sparql_endpoint(Endpoint)),
  forall(
    member(Location, Locations),
    assert(sparql_endpoint_option0(Endpoint, location, Location))
  ),
  assert(sparql_endpoint_option0(Endpoint, manufacturer, Manufacturer)).



%! sparql_remove_endpoint(+Endpoint:atom) is det.
% Removes all modes for the given SPARQL endpoint registration.

sparql_remove_endpoint(Endpoint):-
  retractall(sparql_endpoint(Endpoint)),
  retractall(sparql_endpoint_option0(Endpoint, _, _)).
sparql_remove_endpoint(Endpoint):-
  existence_error(sparql_endpoint, Endpoint).
