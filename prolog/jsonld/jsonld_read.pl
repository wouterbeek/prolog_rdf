:- module(
  jsonld_read,
  [
    jsonld_tuple/2,              % +D, -Tuple
    jsonld_tuple/3,              % +D, -Tuple,  +Opts
    jsonld_tuple_with_context/3, % +Context, +D, -Tuple
    jsonld_tuples/2,             % +D, -Tuples
    jsonld_tuples/3              % +D, -Tuples, +Opts
  ]
).

/** <module> JSON-LD read

@author Wouter Beek
@version 2016/01-2016/03, 2016/05
*/

:- use_module(library(dict_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(typecheck)).

:- rdf_meta
   tuple_term(+, r, r, o, -).

:- dynamic
    jsonld_read:jsonld_tuple_hook/7.

:- multifile
    jsonld_read:jsonld_tuple_hook/7.





%! jsonld_tuple(+D, -Tuple) is det.
%! jsonld_tuple(+D, -Tuple, +Opts) is det.

jsonld_tuple(D, Tuple) :-
  jsonld_tuple(D, Tuple, []).


jsonld_tuple(D, Tuple, Opts) :-
  % The base IRI that was set outside of the JSON-LD context
  % takes precedence over the one that is set inside the JSON-LD context.
  (   option(base_iri(BaseIri), Opts)
  ->  Context = _{'@base': BaseIri}
  ;   Context = _{}
  ),
  jsonld_tuple_with_context(Context, D, Tuple).



%! jsonld_tuple_with_context(+Context, +D, -Tuple) is nondet.

jsonld_tuple_with_context(Context, D, Tuple) :-
  jsonld_tuple(Context, D, Tuple, _).



% Case 1: An array of dictionaries.
jsonld_tuple(Context, Array, Tuple, S) :-
  is_list(Array), !,
  member(Obj, Array),
  jsonld_tuple(Context, Obj, Tuple, S).
% Case 2: A dictionary.
jsonld_tuple(Context1, Obj, Tuple, S) :-
  jsonld_context_and_data(Obj, Context2, Data),
  merge_contexts(Context1, Context2, Context3),
  jsonld_tuple_goto(Context3, Data, Tuple, S).



%! jsonld_tuples(+D, -Tuples) is det.
%! jsonld_tuples(+D, -Tuples, +Opts) is det.

jsonld_tuples(D, Tuples) :-
  jsonld_tuples(D, Tuples, []).


jsonld_tuples(D, Tuples, Opts) :-
  aggregate_all(set(Tuple), jsonld_tuple(D, Tuple, Opts), Tuples).



% GOTO point for recursive structures (see below).
jsonld_tuple_goto(Context1, Data1, Tuple, S) :-
  selectchk('@context'-Context2, Data1, Data2), !,
  merge_contexts(Context1, Context2, Context3),
  jsonld_tuple_goto(Context3, Data2, Tuple, S).
% No more data to process.
jsonld_tuple_goto(_, [], _, _) :- !,
  fail.
jsonld_tuple_goto(Context, Data1, Tuple, S) :-
  jsonld_to_subject(Context, Data1, S, Data2),
  % NONDET
  member(Key-Value, Data2),
  jsonld_to_predicate(Context, Key, P, ODef, LTag),
  jsonld_tuple(Context, S, P, ODef, LTag, Value, Tuple).



%! jsonld_dict_tuple(+Context, +S, +P, +Dict, -Tuple) is nondet.

jsonld_dict_tuple(Context, S1, P, D, Tuple) :-
  dict_pairs(D, Pairs),
  findall(Tuple, jsonld_tuple_goto(Context, Pairs, Tuple, _), Tuples),
  (   % The triple connecting the parent object to the child object.
      Tuples = [rdf(S2,_,_)|_],
      Tuple = rdf(S1,P,S2)
  ;   % The triples that constitute the child object.
      % NONDET
      member(Tuple, Tuples)
  ).



%! jsonld_to_list_triple(+Context, +S, +P, +ODef, _LTag,  +L, -Tuple) is nondet.

jsonld_to_list_triple(Context, S, P, _, _, [], Tuple) :- !,
  rdf_equal(rdf:nil, Nil),
  tuple_term(Context, S, P, Nil, Tuple).
jsonld_to_list_triple(Context, S, P, ODef, LTag, L, Tuple) :-
  rdf_create_bnode(B),
  (   tuple_term(Context, S, P, B, Tuple)
  ;   jsonld_to_list_triple(Context, B, ODef, LTag, L, Tuple)
  ).

jsonld_to_list_triple(Context, S, ODef, LTag, [H], Tuple) :- !,
  (   rdf_equal(rdf:first, First),
      jsonld_tuple(Context, S, First, ODef, LTag, H, Tuple)
  ;   rdf_equal(rdf:rest, Rest),
      rdf_equal(rdf:nil, Nil),
      tuple_term(Context, S, Rest, Nil, Tuple)
  ).
jsonld_to_list_triple(Context, S, ODef, LTag, [H|T], Tuple) :-
  (   rdf_equal(rdf:first, First),
      jsonld_tuple(Context, S, First, ODef, LTag, H, Tuple)
  ;   rdf_equal(rdf:rest, Rest),
      rdf_create_bnode(B),
      (   tuple_term(Context, S, Rest, B, Tuple)
      ;   jsonld_to_list_triple(Context, B, ODef, LTag, T, Tuple)
      )
  ).



%! jsonld_to_predicate(+Context, +Key, -P, -ODef, -LTag) is det.
% Extract the RDF predicate term encoded by the given JSON-LD key.
% Also returns the datatype and language specification, if present.

jsonld_to_predicate(Context, P1, P5, ODef2, LTag) :-
  jsonld_predicate(Context, P1, P2, Keys),
  (   P2 == '@type'
  ->  rdf_equal(rdf:type, P5),
      ODef2 = '@id'
  ;   member(Key, Keys),
      atom(Key),
      get_dict(Key, Context, ODef1),
      is_dict(ODef1),
      (   get_dict('@type', ODef1, D)
      ->  jsonld_expand_term(Context, D, ODef2),
          P5 = P2
      ;   get_dict('@container', ODef1, D)
      ->  atom_string(ODef2, D),
          P5 = P2
      ;   get_dict('@reverse', ODef1, P3)
      ->  jsonld_to_predicate(Context, P3, P4, ODef2, LTag),
          P5 = '@reverse'(P4)
      ), !
  ;   P5 = P2
  ),
  jsonld_language_mapping(Context, Keys, LTag).


jsonld_predicate(Context, P1, Q, [P1|T]) :-
  jsonld_expand_term(Context, P1, P2),
  (   P1 == P2
  ->  Q = P2,
      T = []
  ;   jsonld_predicate(Context, P2, Q, T)
  ).


%! jsonld_to_subject(+Context, +Data1:list(pair), -S, -Data2:list(pair)) is det.
% Extract or create the subject term from the JSON-LD data.

% Case 1: An explicit subject term.
jsonld_to_subject(Context, L1, S2, L2) :-
  selectchk('@id'-S1, L1, L2), !,
  jsonld_expand_term(Context, S1, S2), !.
% Case 2: No explicit subject term.  Create a fresh blank node.
jsonld_to_subject(_, L, S, L) :-
  rdf_create_bnode(S).


%! jsonld_tuple(+Context, +S, +P, +ODef, +LTag, +Value, -Tuple) is det.

% Implementation-specific hooks.
jsonld_tuple(Context, S, P, ODef, LTag, Val, Tuple) :-
  jsonld_read:jsonld_tuple_hook(Context, S, P, ODef, LTag, Val, Tuple), !.
% Null value.
jsonld_tuple(_, _, _, _, _, null, _) :- !,
  fail.
% Reverse property.
jsonld_tuple(Context, O, '@reverse'(P), _, _, V, Tuple) :-
  ground(P), !,
  findall(S-STuple, jsonld_tuple(Context, V, STuple, S), Pairs),
  group_pairs_by_key(Pairs, Groups),
  Groups = [S-STuples],
  (   member(Tuple, STuples)
  ;   tuple_term(Context, S, P, O, Tuple)
  ).
% Default graph.
jsonld_tuple(Context, B, '@graph', _, _, Array, Tuple) :-
  rdf_is_bnode(B), !,
  jsonld_tuple(Context, Array, Tuple, _).
% Named graph.
jsonld_tuple(Context1, S, '@graph', _, _, Array, Tuple) :- !,
  put_dict('@graph', Context1, S, Context2),
  jsonld_tuple(Context2, Array, Tuple, _).
% Blank node.
jsonld_tuple(Context, S, P, _, _, _{'@id': O1}, Tuple) :-
  jsonld_is_bnode(O1), !,
  atom_string(O2, O1),
  tuple_term(Context, S, P, O2, Tuple).
% Container.
jsonld_tuple(Context, S, P, ODef, LTag, Os, Tuple) :-
  (ODef == '@list' ; ODef == '@set'), !,
  is_list(Os),
  % NONDET
  jsonld_to_list_triple(Context, S, P, _, LTag, Os, Tuple).
% Abbreviated object list.
jsonld_tuple(Context, S, P, ODef, LTag, Os, Tuple) :-
  is_list(Os), !,
  % NONDET
  member(O, Os),
  jsonld_tuple(Context, S, P, ODef, LTag, O, Tuple).
% Object is an RDF container.
jsonld_tuple(Context, S, P, ODef, LTag, D, Tuple) :-
  (D = _{'@list': L} ; D = _{'@set': L}), !,
  % NONDET
  jsonld_to_list_triple(Context, S, P, ODef, LTag, L, Tuple).
% Object is an RDF language-tagged string.
jsonld_tuple(Context, S, P, _, _, _{'@language': LTag, '@value': Lex}, Tuple) :- !,
  tuple_term(Context, S, P, Lex@LTag, Tuple).
% Object is an RDF literal with explicitly supplied RDF datatype.
jsonld_tuple(Context, S, P, _, _, _{'@type': D1, '@value': V}, Tuple) :- !,
  jsonld_expand_term(Context, D1, D2),
  tuple_term(Context, S, P, V^^D2, Tuple).
% Object is an IRI.
jsonld_tuple(Context, S, P, _, _, _{'@id': O1}, Tuple) :- !,
  jsonld_expand_term(Context, O1, O2),
  tuple_term(Context, S, P, O2, Tuple).
% Object is a string without explicitly supplied datatype.
jsonld_tuple(Context, S, P, _, _, _{'@value': Lex}, Tuple) :- !,
  rdf_equal(xsd:string, D),
  tuple_term(Context, S, P, Lex^D, Tuple).
% Object is a JSON-LD object (nesting).
jsonld_tuple(Context, S1, P, _, _, O1, Tuple) :-
  is_dict(O1), !,
  dict_pairs(O1, Data1),
  findall(Tuple, jsonld_tuple_goto(Context, Data1, Tuple, _), Tuples),
  (   % The triple connecting to the parent object to the child object.
      Tuples = [rdf(S2,_,_)|_],
      Tuple = rdf(S1,P,S2)
  ;   % The triples that constitute the child object.
      % NONDET
      member(Tuple, Tuples)
  ).
% Object is an IRI; try to expand it.
jsonld_tuple(Context, S, P, ODef, _, O1, Tuple) :-
  ODef == '@id', !,
  jsonld_expand_term(Context, O1, O2),
  tuple_term(Context, S, P, O2, Tuple).
% Explicitly typed literal.
jsonld_tuple(Context, S, P, D, _, Lex, Tuple) :-
  ground(D), !,
  tuple_term(Context, S, P, Lex^^D, Tuple).
% Language-tagged string ‘rdf:langString’.
jsonld_tuple(Context, S, P, _, LTag, V, Tuple) :-
  (nonvar(LTag) ; get_dict('@language', Context, LTag)),
  LTag \== null, !,
  tuple_term(Context, S, P, V@LTag, Tuple).
% Boolean strings default to ‘xsd:boolean’.
jsonld_tuple(Context, S, P, _, _, V, Tuple) :-
  memberchk(V, [false,true]), !,
  rdf_equal(xsd:boolean, D),
  tuple_term(Context, S, P, V^^D, Tuple).
% Integers default to ‘xsd:integer’.
jsonld_tuple(Context, S, P, _, _, V, Tuple) :-
  integer(V), !,
  rdf_equal(xsd:integer, D),
  tuple_term(Context, S, P, V^^D, Tuple).
% Decimal numbers default to ‘xsd:double’.
jsonld_tuple(Context, S, P, _, _, V, Tuple) :-
  float(V), !,
  rdf_equal(xsd:double, D),
  tuple_term(Context, S, P, V^^D, Tuple).
% Strings default to ‘xsd:string’.
jsonld_tuple(Context, S, P, _, _, V, Tuple) :-
  rdf_equal(xsd:string, D),
  tuple_term(Context, S, P, V^^D, Tuple).





% HELPERS %

%! jsonld_context_and_data(+D, -Context, -Data:list(pair)) is det.

jsonld_context_and_data(D, Context6, Data) :-
  dict_pairs(D, Pairs1),
  (   selectchk('@context'-Context1, Pairs1, Data)
  ->  % Splits context from data and parses the context into:
      %   1. a fully qualitied base IRI
      %   2. a prefix map
      %   3. a default natural language
      %   4. a vocabulary IRI
      %
      % The context can be supplied either externally or internally.
      (   is_http_iri(Context1)
      ->  json_download(Context1, Context2)
      ;   Context2 = Context1
      ),
      % Extract the base IRI, if any.
      (   del_dict('@base', Context2, BaseIri, Context3)
      ->  true
      ;   Context3 = Context2
      ),
      % Extract the default language tag, if any.
      (   del_dict('@language', Context3, LTag, Context4)
      ->  true
      ;   Context4 = Context3
      ),
      % Extract the default vocabular, if any.
      (   del_dict('@vocab', Context4, Voc1, Context5)
      ->  jsonld_expand_term(Context5, Voc1, Voc2)
      ;   Context5 = Context4
      ),
      dict_pairs(Context5, Map)
  ;   Map = [],
      Data = Pairs1
  ),
  % Return the extracted context properties into a dictionary
  % since that is easy to pass around.
  rdf_equal(rdf:type, Type),
  include(
    pair_with_nonvar_value,
    ['@base'-BaseIri,'@language'-LTag,'@type'-Type,'@vocab'-Voc2|Map],
    Pairs2
  ),
  dict_pairs(Context6, Pairs2).

pair_with_nonvar_value(_-V) :- nonvar(V).



tuple_term(Context, S, P, O, rdf(S,P,O,G)) :-
  get_dict('@graph', Context, G), !.
tuple_term(_, S, P, O, rdf(S,P,O)).



%! jsonld_language_mapping(+Context, +Keys, -LTag) is det.

jsonld_language_mapping(_, [], _) :- !.
jsonld_language_mapping(Context, [Key|_], LTag) :-
  get_dict(map, Context, Map),
  memberchk(Key-ODef, Map),
  is_dict(ODef),
  get_dict('@language', ODef, LTag), !.
jsonld_language_mapping(Context, [_|Keys], LTag) :-
  jsonld_language_mapping(Context, Keys, LTag).



%! merge_contexts(+Context1, +Context2, -Context3) is det.

merge_contexts(Context1, Context2, Context3) :-
  merge_dict(Context1, Context2, Context3).
