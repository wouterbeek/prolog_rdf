:- module(
  rdf_mt_build,
  [
% MAP
    rdf_add_i_l/4, % +Model:atom
                   % +TypedLiteral:compound
                   % +Graph:atom
                   % +Resource
    rdf_add_i_s/4, % +Model:atom
                   % +IRI:iri
                   % +Graph:atom
                   % +PropertyOrResource
    rdf_unload_map/2, % ?Graph:atom
                      % ?Model:atom
% MODEL
    rdf_add_i_ext/4, % +Model:atom
                     % +Predicate
                     % +Resource1
                     % +Resource2
    rdf_add_model/1, % +Model:atom
    rdf_add_plain_literals/2, % +Model:atom
                              % +Graph:atom
    rdf_add_property/2, % +Model:atom
                        % +Property
    rdf_add_resource/2, % +Model:atom
                        % +Resource
    rdf_unload_model/1 % ?Model:atom
  ]
).

/** <module> RDF_MT_BUILD

Build semantic models and syntax-to-semantics maps.

## Building a model

  * rdf_add_i_ext/4
  * rdf_add_model/1
  * rdf_add_plain_literals/2
  * rdf_add_property/2
  * rdf_add_resource/2
  * rdf_unload_model/1

## Building a map from graph into model

  * rdf_add_i_l/4
  * rdf_add_i_s/4
  * rdf_unload_map/2

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_module(generics(db_ext)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(rdf_term(rdf_plain_literal)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_term(rdf_typed_literal)).
:- use_module(rdf_mt(rdf_mt)).

:- rdf_meta(rdf_add_i_l(+,r,+,+)).
:- rdf_meta(rdf_add_i_s(+,r,+,+)).



% MAP %

%! rdf_add_i_l(
%!   +Graph:atom,
%!   +TypedLiteral:compound,
%!   +Model:atom,
%!   +Resource
%! ) is det.

% Function *IL* maps typed literal values onto resources.
rdf_add_i_l(G, TypedLiteral1, M, Resource):-
  % Type checks.
  % We assume that the resource is added in advance.
  rdf_graph(G),
  rdf_typed_literal(G, TypedLiteral1),
  model(M),
  resource(M, Resource), !,
  rdf_global_object(TypedLiteral1, TypedLiteral2),
  db_add_novel(rdf_mt:i_l(G, TypedLiteral2, M, Resource)).

%! rdf_add_i_s(
%!   +Graph:atom,
%!   +IRI:iri,
%!   +Model:atom,
%!   +ResourceOrProperty
%! ) is det.

rdf_add_i_s(G, IRI, M, X):-
  rdf_graph(G),
  rdf_iri(IRI, G),
  model(M),
  (resource(M, X) ; property(M, X)), !,
  db_add_novel(rdf_mt:i_s(G, IRI, M, X)).

%! rdf_unload_map(+Graph:atom, +Model:atom) is det.

rdf_unload_map(G, M):-
  retractall(rdf_mt:i_l(G,_,M,_)),
  retractall(rdf_mt:i_s(G,_,M,_)).



% MODEL %

%! rdf_add_i_ext(Model:atom, Property, Resource1, Resource2) is det.

rdf_add_i_ext(M, P, R1, R2):-
  model(M),
  property(M, P),
  resource(M, R1),
  resource(M, R2), !,
  db_add_novel(rdf_mt:i_ext(M, P, R1, R2)).

%! rdf_add_model(+Model:atom) is det.

rdf_add_model(M):-
  db_add_novel(rdf_mt:model(M)).

%! rdf_add_plain_literals(+Model:atom, +Graph:atom) is det.
% Adds the plain literals that occur in the given graph to the model.

rdf_add_plain_literals(G, M):-
  rdf_graph(G),
  model(M), !,
  forall(
    rdf_plain_literal(Lit, G),
    % $LV \subseteq IR$ can be determined using syntax.
    rdf_add_resource(M, Lit)
  ).

%! rdf_add_property(Model:atom, +Property) is det.

rdf_add_property(M, P):-
  model(M), !,
  db_add_novel(rdf_mt:property(M, P)).

%! rdf_add_resource(+Model:atom, +Resource) is det.

rdf_add_resource(M, R):-
  model(M), !,
  db_add_novel(rdf_mt:resource(M, R)).

%! rdf_unload_model(+Model:atom) is det.

rdf_unload_model(M):-
  retractall(model(M)), !,
  rdf_unload_map(_, M),
  retractall(rdf_mt:i_ext(M,_,_,_)),
  retractall(rdf_mt:property(M,_)),
  retractall(rdf_mt:resource(M,_)).

