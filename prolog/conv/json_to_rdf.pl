:- module(
  json_to_rdf,
  [
    json_to_rdf/5, % +Graph, +Module, +Prefix, +Json, ?Resource
    json_to_rdf/6 % +Graph:rdf_graph
                  % +Module:atom
                  % +SchemaPrefix:atom
                  % +DataPrefix:atom
                  % +Json:dict
                  % ?Resources:or([rdf_term,list(rdf_term)])
  ]
).

/** <module> JSON to RDF

Automated JSON to RDF conversion.

This requires a Prolog module whose name is also registered as
the RDF prefix that is used for the RDF vocabulary.

@author Wouter Beek
@version 2015/08, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/turtle_conv)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdfs/rdfs_build)).





%! find_matching_legend(+Dict:dict, +Module:atom, -Legend:atom) is det.

find_matching_legend(D, Mod, Match):-
  dict_pairs(D, json, Pairs1),
  pairs_keys(Pairs1, Keys1),
  list_to_set(Keys1, Keyset1),
  aggregate_all(
    max(Score,Legend),
    (
      Mod:legend(Legend, Properties),
      maplist(property_name, Properties, Keys2),
      list_to_set(Keys2, Keyset2),
      intersection(Keyset1, Keyset2, SharedKeys),
      length(SharedKeys, Score)
    ),
    max(_, Match)
  ), !.

property_name(property(Name, _), Name).



%! create_resource(
%!   +SchemaPrefix:atom,
%!   +DataPrefix:atom,
%!   +Legend:atom,
%!   +Graph:rdf_graph,
%!   ?Resource:iri
%! ) is det.

create_resource(SPrefix, DPrefix, Legend, G, R):-
  % Create the class-denoting RDF term based on the legend name.
  once(atom_phrase(atom_capitalize, Legend, CName)),
  rdf_global_id(SPrefix:CName, C),

  % Create the instance.
  fresh_iri(DPrefix, [Legend], R),
  rdf_assert_instance(R, C, G).



%! json_to_rdf(
%!   +Graph:atom,
%!   +Module:atom,
%!   +Prefix:atom,
%!   +Json:dict,
%!   ?Resource:iri
%! ) is det.
% Wrapper around json_to_rdf/6 whose schema and data prefix are the same.

json_to_rdf(G, Mod, Prefix, Json, R):-
  json_to_rdf(G, Mod, Prefix, Prefix, Json, R).

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

json_to_rdf(G, Mod, SPrefix, DPrefix, Ds, Rs):-
  is_list(Ds), !,
  maplist(json_to_rdf(G, Mod, SPrefix, DPrefix), Ds, Rs).
json_to_rdf(G, Mod, SPrefix, DPrefix, D, R):-
  % Make sure the RDF prefix has been registered.
  maplist(exists_rdf_prefix, [SPrefix,DPrefix]),
  % Find the legend to which this JSON object matches most closely.
  find_matching_legend(D, Mod, Legend),
  json_to_rdf(G, Mod, SPrefix, DPrefix, Legend, D, R).

exists_rdf_prefix(Prefix):-
  rdf_current_prefix(Prefix, _), !.
exists_rdf_prefix(Prefix):-
  existence_error(rdf_prefix, Prefix).

% Now we have a legend based on which we do the conversion.
json_to_rdf(G, Mod, SPrefix, DPrefix, Legend, D, S):-
  (var(S) -> create_resource(SPrefix, DPrefix, Legend, G, S) ; true),

  % Assert all predications (P-O) of S.
  dict_pairs(D, json, Pairs1),
  Mod:legend(Legend, Specs),
  findall(
    P-O,
    (
      member(P-Val, Pairs1),
      assert_json_property(G, Mod, SPrefix, DPrefix, Specs, P, Val, O)
    ),
    Pairs2
  ),
  forall(member(P-O, Pairs2), assert_triples0(SPrefix, G, S, P, O)).

assert_triples0(_, _, _, _, O):-
  var(O), !.
assert_triples0(SPrefix, G, S, P, Os):-
  is_list(Os), !,
  maplist(assert_triples0(SPrefix, G, S, P), Os).
assert_triples0(SPrefix, G, S, P0, O):-
  to_pn_local(P0, PnLocal),
  rdf_global_id(SPrefix:PnLocal, P),
  user:rdf_assert(S, P, O, G).



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

assert_json_property(G, Mod, SPrefix, DPrefix, Specs, P, Val, O):-
  (   memberchk(property(P, Datatype), Specs)
  ->  assert_json_property(G, Mod, SPrefix, DPrefix, Datatype, Val, O)
  ;   format(user_error, 'Unrecognized JSON key <-> RDF property: ~w~n', [P])
  ).

%%%%% We do not believe that empty values -- i.e. the empty string --
%%%%% are very usefull, so we do not assert pairs with this value.
%%%%assert_json_property(_, _, _, _, _, "", _):- !, fail.
% List: link every element individually.
assert_json_property(G, Mod, SPrefix, DPrefix, list(Type), Vals, Os):-
  is_list(Vals), !,
  maplist(assert_json_property(G, Mod, SPrefix, DPrefix, Type), Vals, Os).
% The value must match at least one of the given types.
assert_json_property(G, Mod, SPrefix, DPrefix, or(Types), Val, O):-
  % NONDET.
  member(Type, Types),
  assert_json_property(G, Mod, SPrefix, DPrefix, Type, Val, O), !.
% RDF IRI (JSON-LD notation).
assert_json_property(_, _, _, _, '@id', Val, O):- !,
  atom_string(O, Val).
% RDF list: mimic the list in RDF and link to the list.
assert_json_property(G, Mod, SPrefix, DPrefix, rdf_list(Type), Vals, O):-
  is_list(Vals), !,
  findall(
    O,
    (
      member(Val, Vals),
      assert_json_property(G, Mod, SPrefix, DPrefix, Type, Val, O)
    ),
    Os
  ),
  rdf_assert_list(Os, O, G).
% We have a specific type that is always skipped,
% appropriately called `skip`.
assert_json_property(_, _, _, _, skip, _, _):- !, fail.
% There are two ways to realize legend types / create resources:
% 1. JSON terms (always).
assert_json_property(G, Mod, SPrefix, DPrefix, Legend/_, Val, O):-
  is_dict(Val), !,
  json_to_rdf(G, Mod, SPrefix, DPrefix, Legend, Val, O).
% There are two ways to realize legend types / create resources:
% 2. JSON strings (sometimes).
assert_json_property(G, _, SPrefix, DPrefix, Legend/_, Val, O):-
  % Remember that SWI dictionaries contain SWI strings, not atoms.
  string(Val), !,
  create_resource(SPrefix, DPrefix, Legend, G, O),
  rdfs_assert_label(O, Val, G).
% We do not have an RDF equivalent for the JSON null value,
% so we do not assert pairs with a null value in RDF.
assert_json_property(_, _, _, _, _, Val, _):-
  Val = @(null), !,
  fail.
% A JSON object occurs for which the legend is not yet known.
assert_json_property(G, Mod, SPrefix, DPrefix, Type, Val, O):-
  Type \= _/_,
  is_dict(Val), !,
  json_to_rdf(G, Mod, SPrefix, DPrefix, Val, O).
% Typed literals.
assert_json_property(_, _, _, _, D0, Lex0, Lit):-
  rdf_global_id(D0, D),
  % Remember that SWI dictionaries contain SWI strings, not atoms.
  atom_string(Lex, Lex0),
  rdf_lexical_map(D, Lex, Val),
  rdf_canonical_map(D, Val, Lit).
