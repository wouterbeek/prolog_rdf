:- module(
  json_to_rdf,
  [
    json_to_rdf/5 % +Graph:atom
                  % +Module:atom
                  % +Prefix:atom
                  % +Json:dict
                  % ?Resource:iri
  ]
).

/** <module> JSON to RDF

Automated JSON to RDF conversion.

This requires a Prolog module whose name is also registered as
 the XML namespace that is used for the RDF vocabulary.

@author Wouter Beek
@version 2014/01-2014/03, 2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plDcg(dcg_atom)). % DCG rule.
:- use_module(plDcg(dcg_generics)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(term/rdf_datatype)).
:- use_module(plRdf(term/rdf_list)).





%! find_matching_legend(
%!   +Dict:dict,
%!   +Module:atom,
%!   -MatchingLegend:atom
%! ) is det.

find_matching_legend(Dict, Module, MatchingLegend):-
  dict_pairs(Dict, json, Pairs1),
  pairs_keys(Pairs1, Keys1),
  list_to_set(Keys1, Keyset1),
  aggregate(
    max(Length, Legend),
    (
      Module:legend(Legend, Pairs2),
      pairs_keys(Pairs2, Keys2),
      list_to_set(Keys2, Keyset2),
      intersection(Keyset1, Keyset2, Shared),
      length(Shared, Length)
    ),
    max(_, MatchingLegend)
  ), !.



%! create_resource(
%!   +Prefix:atom,
%!   +Legend:atom,
%!   +Graph:atom,
%!   ?Resource:iri
%! ) is det.

create_resource(Prefix, Legend, Graph, Resource):-
  % Create the class-denoting RDF term based on the legend name.
  once(dcg_phrase(atom_capitalize, Legend, ClassName)),
  atomic_list_concat([ontology,ClassName], ClassLocalName),
  rdf_global_id(Prefix:ClassLocalName, Class),

  % Create the instance.
  rdf_create_next_resource(Prefix, [Legend], Class, Graph, Resource).



%! json_to_rdf(
%!   +Graph:atom,
%!   +Module:atom,
%!   +Prefix:atom,
%!   +Json:dict,
%!   ?Resource:iri
%! ) is det.
% Automated conversion from JSON to RDF,
%  based on registered legends.
%
% # Conversion table
%
% | **JSON**   | **Prolog**    | **RDF**       |
% |:----------:|:-------------:|:-------------:|
% | Object     | Compound term | Resource      |
% | Array      | List          |               |
% | String     | Atom          | `xsd:string`  |
% |            | Atom          | Resource      |
% | Number     | Number        | `xsd:float`   |
% |            |               | `xsd:integer` |
% | `false`    | `@(false)`    | `xsd:boolean` |
% | `true`     | `@(true)`     | `xsd:boolean` |
% | `null`     | `@(null)`     | skip          |
%
% # Arguments
%
% @arg Graph The atomic name of the RDF graph in which results are asserted.
% @arg LegendModule The atomic name of the Prolog module that contains
%      the legens to which JSON terms have to conform.
% @arg Prefix ???
% @arg Json A compound term representing a JSON term.
%      This will be converted to RDF.
% @arg Resource An IRI denoting the RDF version of the JSON term.

% Multiple JSON objects need to be converted.
json_to_rdf(Graph, Module, Prefix, Dicts, Resources):-
  is_list(Dicts), !,
  maplist(json_to_rdf(Graph, Module, Prefix), Dicts, Resources).

json_to_rdf(Graph, Module, Prefix, Dict, Resource):-
  % Make sure the RDF prefix has been registered.
  (   rdf_current_prefix(Prefix, _)
  ->  true
  ;   existence_error(rdf_prefix, Prefix)
  ),

  % Find the legend to which this JSON object matches most closely.
  find_matching_legend(Dict, Module, Legend),
  json_to_rdf(Graph, Module, Prefix, Legend, Dict, Resource).

% Now we have a legend based on which we do the conversion.
json_to_rdf(Graph, Module, Prefix, Legend, Dict, Resource):-
  % Class and individual.
  (   var(Resource)
  ->  create_resource(Prefix, Legend, Graph, Resource)
  ;   true
  ),

  % Assert all predications of Resource.
  dict_pairs(Dict, json, Pairs),
  Module:legend(Legend, Specs),
  maplist(
    assert_json_property(Graph, Module, Prefix, Specs, Resource),
    Pairs
  ).


%! assert_json_property(
%!   +Graph:atom,
%!   +Module:atom,
%!   +Prefix:atom,
%!   +ArgumentSpecifications:list(pair),
%!   +Resource:iri,
%!   +NVPair:pair(atom,term)
%! ) is det.
% Make sure a property with the given name exists.
% Also retrieve the type the value should adhere to.

assert_json_property(Graph, Module, Prefix, Specs, Resource, Name-Value):-
  memberchk(Name-Type, Specs), !,
  assert_json_property(Graph, Module, Prefix, Resource, Name, Type, Value).
% Unrecognized JSON key / RDF property.
assert_json_property(Graph, Module, Prefix, Specs, Resource, Name-Value):-
  gtrace, %DEB
  assert_json_property(Graph, Module, Prefix, Specs, Resource, Name-Value).

% The value must match at least one of the given types.
assert_json_property(Graph, Module, Prefix, Resource, Name, or(Types), Value):-
  % NONDET.
  member(Type, Types),
  assert_json_property(Graph, Module, Prefix, Resource, Name, Type, Value), !.
% We do not have an RDF equivalent for the JSON null value,
% so we do not assert pairs with a null value in RDF.
assert_json_property(_, _, _, _, _, _, Value):-
  Value = @(null), !.
% We do not believe that empty values -- i.e. the empty atom --
% are very usefull, so we do not assert pairs with this value.
assert_json_property(_, _, _, _, _, _, ''):- !.
% We have a specific type that is always skipped, appropriately called `skip`.
assert_json_property(_, _, _, _, _, skip, _):- !.
% There are two ways to realize legend types / create resources:
% 1. JSON terms (always).
assert_json_property(Graph, Module, Prefix, Individual1, Name, Legend/_, Value):-
  is_dict(Value), !,
  json_to_rdf(Graph, Module, Prefix, Legend, Value, Individual2),
  rdf_global_id(Prefix:Name, Predicate),
  rdf_assert(Individual1, Predicate, Individual2, Graph).
% There are two ways to realize legend types / create resources:
% 2. JSON strings (sometimes).
assert_json_property(Graph, _, Prefix, Resource1, Name, Legend/_, Value):-
  atom(Value), !,
  create_resource(Prefix, Legend, Graph, Resource2),
  rdfs_assert_label(Resource2, Value, Graph),
  rdf_global_id(Prefix:Name, Predicate),
  rdf_assert(Resource1, Predicate, Resource2, Graph).
% A JSON object occurs for which the legend is not yet known.
assert_json_property(Graph, Module, Prefix, Individual1, Name, Type, Value):-
  Type \= _/_,
  is_dict(Value), !,
  json_to_rdf(Graph, Module, Prefix, Value, Individual2),
  rdf_global_id(Prefix:Name, Predicate),
  rdf_assert(Individual1, Predicate, Individual2, Graph).
% List: link every element individually.
assert_json_property(Graph, Module, Prefix, Resource, Name, list(Type), Values):-
  is_list(Values), !,
  maplist(
    assert_json_property(Graph, Module, Prefix, Resource, Name, Type),
    Values
  ).
% RDF list: mimic the list in RDF and link to the list.
assert_json_property(Graph, _, Prefix, Resource, Name, rdf_list(Type), Values):-
  is_list(Values), !,
  rdf_global_id(Prefix:Name, Predicate),
  rdf_global_id(xsd:Type, Datatype),
  rdf_assert_list(Values, RdfList, Graph, [datatype(Datatype)]),
  rdf_assert(Resource, Predicate, RdfList, Graph).
% Typed literals.
assert_json_property(Graph, _, Prefix, Resource, Name, Datatype0, Value0):-
  rdf_global_id(Datatype0, Datatype),
  rdf_global_id(Prefix:Name, Predicate),
  rdf_lexical_map(Datatype, Value0, Value),
  rdf_assert_typed_literal(Resource, Predicate, Value, Datatype, Graph).
