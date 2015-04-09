:- module(
  json_to_rdf,
  [
    json_to_rdf/6 % +Graph:atom
                  % +Module:atom
                  % +SchemaPrefix:atom
                  % +DataPrefix:atom
                  % +Json:dict
                  % ?Resource:iri
  ]
).

/** <module> JSON to RDF

Automated JSON to RDF conversion.

This requires a Prolog module whose name is also registered as
 the XML namespace that is used for the RDF vocabulary.

@author Wouter Beek
@version 2014/01-2014/03, 2014/12-2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_atom)). % DCG rule.
:- use_module(plc(dcg/dcg_generics)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(syntax/turtle_convert)).
:- use_module(plRdf(term/rdf_datatype)).
:- use_module(plRdf(term/rdf_list)).





%! find_matching_legend(
%!   +Dict:dict,
%!   +Module:atom,
%!   -MatchingLegend:atom
%! ) is det.

find_matching_legend(Dict, Mod, MatchingLegend):-
  dict_pairs(Dict, json, Pairs1),
  pairs_keys(Pairs1, Keys1),
  list_to_set(Keys1, Keyset1),
  aggregate_all(
    max(Length, Legend),
    (
      Mod:legend(Legend, Properties),
      maplist(property_name, Properties, Keys2),
      list_to_set(Keys2, Keyset2),
      intersection(Keyset1, Keyset2, Shared),
      length(Shared, Length)
    ),
    max(_, MatchingLegend)
  ), !.

property_name(property(Name, _), Name).



%! create_resource(
%!   +Prefix:atom,
%!   +Legend:atom,
%!   +Graph:atom,
%!   ?Resource:iri
%! ) is det.

create_resource(SPrefix, DPrefix, Legend, G, Resource):-
  % Create the class-denoting RDF term based on the legend name.
  once(atom_phrase(atom_capitalize, Legend, ClassName)),
  rdf_global_id(SPrefix:ClassName, Class),

  % Create the instance.
  rdf_create_next_resource(DPrefix, [Legend], Class, G, Resource).



%! json_to_rdf(
%!   +Graph:atom,
%!   +Module:atom,
%!   +SchemaPrefix:atom,
%!   +DataPrefix:atom,
%!   +Json:dict,
%!   ?Subject:iri
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
% @arg SchemaPrefix
% @arg DataPrefix
% @arg Json A compound term representing a JSON term.
%      This will be converted to RDF.
% @arg Resource An IRI denoting the RDF version of the JSON term.

json_to_rdf(G, Mod, SPrefix, DPrefix, Dicts, Resources):-
  is_list(Dicts), !,
  maplist(json_to_rdf(G, Mod, SPrefix, DPrefix), Dicts, Resources).
json_to_rdf(G, Mod, SPrefix, DPrefix, Dict, Resource):-
  % Make sure the RDF prefix has been registered.
  maplist(exists_rdf_prefix, [SPrefix,DPrefix]),
  % Find the legend to which this JSON object matches most closely.
  find_matching_legend(Dict, Mod, Legend),
  json_to_rdf(G, Mod, SPrefix, DPrefix, Legend, Dict, Resource).

exists_rdf_prefix(Prefix):-
  rdf_current_prefix(Prefix, _), !.
exists_rdf_prefix(Prefix):-
  existence_error(rdf_prefix, Prefix).

% Now we have a legend based on which we do the conversion.
json_to_rdf(G, Mod, SPrefix, DPrefix, Legend, Dict, S):-
  (   var(S)
  ->  create_resource(SPrefix, DPrefix, Legend, G, S)
  ;   true
  ),

  % Assert all predications (P-O) of S.
  dict_pairs(Dict, json, Pairs1),
  Mod:legend(Legend, Specs),
  findall(
    P-O,
    (
      member(P-Value, Pairs1),
      assert_json_property(G, Mod, SPrefix, DPrefix, Specs, P, Value, O)
    ),
    Pairs2
  ),
  forall(
    member(P-O, Pairs2),
    assert_triples0(SPrefix, G, S, P, O)
  ).

assert_triples0(_, _, _, _, O):-
  var(O), !.
assert_triples0(SchemaPrefix, G, S, P, Os):-
  is_list(Os), !,
  maplist(assert_triples0(SchemaPrefix, G, S, P), Os).
assert_triples0(SchemaPrefix, G, S, P0, O):-
  atom_to_pn_local(P0, PnLocal),
  rdf_global_id(SchemaPrefix:PnLocal, P),
  rdf_assert(S, P, O, G).



%! assert_json_property(
%!   +Graph:atom,
%!   +Module:atom,
%!   +SchemaPrefix:atom,
%!   +DataPrefix:atom,
%!   +ArgumentSpecifications:list(compound),
%!   +Predicate:iri,
%!   +Value,
%!   -Object:rdf_term
%! ) is semidet.
% Make sure a property with the given name exists.
% Also retrieve the type the value should adhere to.

assert_json_property(G, Mod, SPrefix, DPrefix, Specs, Property, Value, O):-
  memberchk(property(Property, Datatype), Specs), !,
  assert_json_property(G, Mod, SPrefix, DPrefix, Datatype, Value, O).
% Unrecognized JSON key / RDF property.
assert_json_property(G, Mod, SPrefix, DPrefix, Specs, Property, Value, O):-
  gtrace, %DEB
  assert_json_property(G, Mod, SPrefix, DPrefix, Specs, Property, Value, O).

%%%%% We do not believe that empty values -- i.e. the empty string --
%%%%% are very usefull, so we do not assert pairs with this value.
%%%%assert_json_property(_, _, _, _, _, "", _):- !, fail.
% List: link every element individually.
assert_json_property(G, Mod, SPrefix, DPrefix, list(Type), Values, Os):-
  is_list(Values), !,
  maplist(
    assert_json_property(G, Mod, SPrefix, DPrefix, Type),
    Values,
    Os
  ).
% The value must match at least one of the given types.
assert_json_property(G, Mod, SPrefix, DPrefix, or(Types), Value, O):-
  % NONDET.
  member(Type, Types),
  assert_json_property(G, Mod, SPrefix, DPrefix, Type, Value, O), !.
% RDF list: mimic the list in RDF and link to the list.
assert_json_property(G, Mod, SPrefix, DPrefix, rdf_list(Type), Values, O):-
  is_list(Values), !,
  findall(
    O,
    (
      member(Value, Values),
      assert_json_property(G, Mod, SPrefix, DPrefix, Type, Value, O)
    ),
    Os
  ),
  rdf_assert_list(Os, O, G, []).
% We have a specific type that is always skipped,
% appropriately called `skip`.
assert_json_property(_, _, _, _, skip, _, _):- !, fail.
% There are two ways to realize legend types / create resources:
% 1. JSON terms (always).
assert_json_property(G, Mod, SPrefix, DPrefix, Legend/_, Value, O):-
  is_dict(Value), !,
  json_to_rdf(G, Mod, SPrefix, DPrefix, Legend, Value, O).
% There are two ways to realize legend types / create resources:
% 2. JSON strings (sometimes).
assert_json_property(G, _, SPrefix, DPrefix, Legend/_, Value, O):-
  % Remember that SWI dictionaries contain SWI strings, not atoms.
  string(Value), !,
  create_resource(SPrefix, DPrefix, Legend, G, O),
  rdfs_assert_label(O, Value, G).
% We do not have an RDF equivalent for the JSON null value,
% so we do not assert pairs with a null value in RDF.
assert_json_property(_, _, _, _, _, Value, _):-
  Value = @(null), !,
  fail.
% A JSON object occurs for which the legend is not yet known.
assert_json_property(G, Mod, SPrefix, DPrefix, Type, Value, O):-
  Type \= _/_,
  is_dict(Value), !,
  json_to_rdf(G, Mod, SPrefix, DPrefix, Value, O).
% Typed literals.
assert_json_property(_, _, _, _, Datatype0, Value1, O):-
  rdf_global_id(Datatype0, Datatype),
  % Remember that SWI dictionaries contain SWI strings, not atoms.
  atom_string(Value2, Value1),
  rdf_lexical_map(Datatype, Value2, Value3),
  rdf_canonical_map(Datatype, Value3, LexicalForm),
  O = literal(type(Datatype,LexicalForm)).
