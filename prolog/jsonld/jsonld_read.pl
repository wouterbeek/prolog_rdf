:- module(
  jsonld_read,
  [
    jsonld_to_triple/2, % +D, -T
    jsonld_to_triple/3  % +D, -T, +Opts
  ]
).

/** <module> JSON-LD read

@author Wouter Beek
@version 2016/01-2016/03
*/

:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_download)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_bnode_name)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(typecheck)).

%! bnode_map(?Name, ?B) is nondet.

:- thread_local
   bnode_map/2.

:- predicate_options(jsonld_load/2, 2, [
     pass_to(open_any2/5, 5)
   ]).
:- predicate_options(jsonld_to_triple/3, 3, [
     pass_to(jsonld_to_context_and_data/4, 4)
   ]).
:- predicate_options(jsonld_to_context_and_data/4, 4, [
     base_iri(+atom)
   ]).





%! jsonld_to_triple(+D, -T) is det.
%! jsonld_to_triple(+D, -T, +Opts) is det.

jsonld_to_triple(D, T) :-
  jsonld_to_triple(D, T, []).


% Case 1: An array of dictionaries.
jsonld_to_triple(Ds, T, Opts) :-
  is_list(Ds), !,
  member(D, Ds),
  jsonld_to_triple(D, T, Opts).
% Case 2: A dictionary.
jsonld_to_triple(D, T, Opts) :-
  jsonld_to_context_and_data(D, Context, Data, Opts),
  jsonld_to_triple_goto(Context, Data, T).

% GOTO point for recursive structures (see below).
jsonld_to_triple_goto(Context, Data1, T) :-
  jsonld_to_subject(Context, Data1, S, Data2),
  % NONDET
  member(Pair, Data2),
  jsonld_to_triple(Context, S, Pair, T).

%! jsonld_to_triple(+Context, +S, +Pair, -T) is nondet.

jsonld_to_triple(Context, S, Key-Value, T) :-
  jsonld_to_predicate(Context, Key, P, ODef, LTag),
  jsonld_to_triple(Context, S, P, ODef, LTag, Value, T).



%! jsonld_to_bnode(+BNodeName:atom, -BNode:bnode) is det.

jsonld_to_bnode(B1, B2) :-
  atomic_list_concat(['_',_], :, B1), !,
  (   bnode_map(B1, B2)
  ->  true
  ;   rdf_create_bnode(B2),
      assert(bnode_map(B1, B2))
  ).



%! jsonld_dict_to_triple(+Context, +S, +P, +Dict, -T) is nondet.

jsonld_dict_to_triple(Context, S1, P, D, T) :-
  dict_pairs(D, Pairs),
  findall(T, jsonld_to_triple_goto(Context, Pairs, T), Ts),
  (   % The triple connecting to the parent object to the child object.
      Ts = [rdf(S2,_,_)|_], T = rdf(S1,P,S2)
  ;   % The triples that constitute the child object.
      % NONDET
      member(T, Ts)
  ).



%! jsonld_to_list_triple(+Context, +S, +P, +ODef, _LTag,  +L, -T) is nondet.

jsonld_to_list_triple(_, _, _, _, _, [], _) :- !, fail.
jsonld_to_list_triple(Context, S, P, _, _, Ds, T) :-
  maplist(is_dict, Ds), !,
  % NONDET
  member(D, Ds),
  jsonld_dict_to_triple(Context, S, P, D, T).
jsonld_to_list_triple(Context, S, P, ODef, LTag, L, T) :-
  rdf_create_bnode(B),
  (   statement_term(S, P, B, T)
  ;   jsonld_to_list_triple0(Context, B, ODef, LTag, L, T)
  ).

jsonld_to_list_triple0(Context, B, ODef, LTag, [H], T):- !,
  (   rdf_equal(rdf:first, First),
      jsonld_to_triple(Context, B, First, ODef, LTag, H, T)
  ;   rdf_equal(rdf:rest, Rest),
      rdf_equal(rdf:nil, Nil),
      jsonld_to_triple(Context, B, Rest, '@id', LTag, Nil, T)
  ).
jsonld_to_list_triple0(Context, B1, ODef, LTag, [H1|_], T):-
  rdf_equal(rdf:first, First),
  jsonld_to_triple(Context, B1, First, ODef, LTag, H1, T).
jsonld_to_list_triple0(Context, B1, ODef, LTag, [_,H|L], T):-
  rdf_create_bnode(B2),
  (   rdf_equal(rdf:rest, Rest),
      jsonld_to_triple(Context, B1, Rest, ODef, LTag, _{'@id': B2}, T)
  ;   jsonld_to_list_triple0(Context, B2, ODef, LTag, [H|L], T)
  ).


%! jsonld_to_predicate(+Context, +Key, -P, -D, -LTag) is det.
% Extract the RDF predicate term encoded by the given JSON-LD key.
% Also returns the datatype and language specification, if present.

% Case 1: The predicate is ‘rdf:type’.
%         The object definition is ‘@id’.
%         There is no language tag.
jsonld_to_predicate(_, '@type', P, '@id', _) :- !,
  rdf_equal(rdf:type, P).
% Case 2: Other predicates.
jsonld_to_predicate(Context, P, Q, D, LTag) :-
  jsonld_predicate(Context, P, Q, Keys),
  jsonld_datatype_mapping(Context, Keys, D),
  jsonld_language_mapping(Context, Keys, LTag).


%% Case 1: Trivial translation due to SWI7 dict syntax.
%jsonld_predicate(Context, P1, Q, Keys) :-
%  string(P1), !,
%  atom_string(P2, P1),
%  jsonld_predicate(Context, P2, Q, Keys).
% Case 2: JSON-LD keyword.
jsonld_predicate(_, P, P, []) :-
  jsonld_keyword(P), !.
% Case 3: JSON-LD IRI expansion.
jsonld_predicate(Context, P1, Q, [P1|T]) :-
  jsonld_expand_iri(Context, P1, P2),
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
  (   jsonld_is_bnode(S1)
  ->  jsonld_to_bnode(S1, S2)
  ;   jsonld_expand_iri(Context, S1, S2)
  ), !.
% Case 2: No explicit subject term.  Create a fresh blank node.
jsonld_to_subject(_, L, S, L) :-
  rdf_create_bnode(S).


%! jsonld_to_triple(+Context, +S, +P, +ODef, +LTag, +Value, -T) is det.

% RDF blank node.
jsonld_to_triple(_, S, P, _, _, _{'@id': O1}, T) :-
  jsonld_is_bnode(O1), !,
  jsonld_to_bnode(O1, O2),
  statement_term(S, P, O2, T).
% RDF container.
jsonld_to_triple(Context, S, P, ODef, LTag, Os, T) :-
  (ODef == '@list' ; ODef == '@set'), !,
  is_list(Os),
  % NONDET
  jsonld_to_list_triple(Context, S, P, ODef, LTag, Os, T).
% Abbreviated object list.
jsonld_to_triple(Context, S, P, ODef, LTag, Os, T) :-
  is_list(Os), !,
  % NONDET
  member(O, Os),
  jsonld_to_triple(Context, S, P, ODef, LTag, O, T).
% Object is an RDF container.
jsonld_to_triple(Context, S, P, ODef, LTag, _{'@list': L}, T) :- !,
  % NONDET
  jsonld_to_list_triple(Context, S, P, ODef, LTag, L, T).
jsonld_to_triple(Context, S, P, ODef, LTag, _{'@set': L}, T) :- !,
  % NONDET
  jsonld_to_list_triple(Context, S, P, ODef, LTag, L, T).
% Object is an RDF language-tagged string.
jsonld_to_triple(_, S, P, _, _, _{'@language': LTag, '@value': Lex}, T) :- !,
  statement_term(S, P, Lex@LTag, T).
% Object is an RDF literal with explicitly supplied RDF datatype.
jsonld_to_triple(Context, S, P, _, _, _{'@type': D1, '@value': V}, T) :- !,
  jsonld_expand_iri(Context, D1, D2),
  statement_term(S, P, V^^D2, T).
% Object is an IRI.
jsonld_to_triple(Context, S, P, _, _, _{'@id': O1}, T) :- !,
  jsonld_expand_iri(Context, O1, O2),
  statement_term(S, P, O2, T).
% Object is an XSD string without explicitly supplied RDF datatype.
jsonld_to_triple(_, S, P, _, _, _{'@value': Lex}, T) :- !,
  rdf_equal(xsd:string, D),
  statement_term(S, P, Lex^D, T).
% Object is a JSON-LD object (nesting).
jsonld_to_triple(Context, S1, P, _, _, O1, T) :-
  is_dict(O1), !,
  dict_pairs(O1, Data1),
  findall(T, jsonld_to_triple_goto(Context, Data1, T), Ts),
  (   % The triple connecting to the parent object to the child object.
      Ts = [rdf(S2,_,_)|_],
      T = rdf(S1,P,S2)
  ;   % The triples that constitute the child object.
      % NONDET
      member(T, Ts)
  ).
% Object is an IRI; try to expand it.
jsonld_to_triple(Context, S, P, ODef, _, O1, T) :-
  ODef == '@id', !,
  jsonld_expand_iri(Context, O1, O2),
  statement_term(S, P, O2, T).
% Explicitly typed literal.
jsonld_to_triple(_, S, P, D, _, Lex, T) :-
  ground(D), !,
  statement_term(S, P, Lex^^D, T).
% Language-tagged string ‘rdf:langString’.
jsonld_to_triple(Context, S, P, _, LTag, Lex, T) :-
  (nonvar(LTag) ; get_dict('@language', Context, LTag)),
  LTag \== null, !,
  statement_term(S, P, Lex@LTag, T).
% String ‘xsd:string’.
jsonld_to_triple(_, S, P, _, _, Lex, T) :-
  rdf_equal(xsd:string, D),
  statement_term(S, P, Lex^^D, T).





% HELPERS %

%! jsonld_datatype_mapping(+Context, +Keys, -D) is det.
% Returns the datatype for the given Key

jsonld_datatype_mapping(_, [], _) :- !.
jsonld_datatype_mapping(Context, [Key|_], D2) :-
  get_dict(Key, Context, ODef),
  is_dict(ODef),
  (   get_dict('@type', ODef, D1)
  ->  jsonld_expand_iri(Context, D1, D2)
  ;   get_dict('@container', ODef, D2)
  ->  true
  ), !.
jsonld_datatype_mapping(Context, [_|Keys], D) :-
  jsonld_datatype_mapping(Context, Keys, D).



%! jsonld_language_mapping(+Context, +Keys, -LTag) is det.

jsonld_language_mapping(_, [], _) :- !.
jsonld_language_mapping(Context, [Key|_], LTag) :-
  get_dict(map, Context, Map),
  memberchk(Key-ODef, Map),
  is_dict(ODef),
  get_dict('@language', ODef, LTag), !.
jsonld_language_mapping(Context, [_|Keys], LTag) :-
  jsonld_language_mapping(Context, Keys, LTag).



%! jsonld_to_context_and_data(
%!   +JsonLd,
%!   -Context,
%!   -Data:list(pair),
%!   +Opts
%! ) is det.

jsonld_to_context_and_data(D, Context6, Data, Opts) :-
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
      (   del_dict('@base', Context2, BaseIri1, Context3)
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
      ->  jsonld_expand_iri(Context5, Voc1, Voc2)
      ;   Context5 = Context4
      ),
      dict_pairs(Context5, Map)
  ;   Map = [],
      Data = Pairs1
  ),
  % The base IRI that was set outside of the JSON-LD context
  % takes precedence over the one that is set inside the JSON-LD context.
  option(base_iri(BaseIri2), Opts, BaseIri1),
  % Return the extracted context properties into a dictionary
  % since that is easy to pass around.
  include(
    pair_with_nonvar_value,
    ['@base'-BaseIri2,'@language'-LTag,'@vocab'-Voc2|Map],
    Pairs2
  ),
  dict_pairs(Context6, Pairs2).

pair_with_nonvar_value(_-V) :- nonvar(V).

statement_term(S, P, O, rdf(S,P,O)).
