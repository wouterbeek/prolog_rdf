:- module(
  jsonld_read,
  [
    jsonld_statement/2, % +Json, -Stmt
    jsonld_statement/3  % +Json, -Stmt, +Opts
  ]
).

/** <module> JSON-LD read

@author Wouter Beek
@version 2016/01-2016/03
*/

:- use_module(library(dict_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(typecheck)).





%! jsonld_statement(+Json, -Stmt) is det.

jsonld_statement(Json, Stmt) :-
  jsonld_statement(Json, Stmt, []).

jsonld_statement(Json, Stmt, Opts) :-
  % The base IRI that was set outside of the JSON-LD context
  % takes precedence over the one that is set inside the JSON-LD context.
  (   option(base_iri(BaseIri), Opts)
  ->  Context = _{'base': BaseIri}
  ;   Context = _{}
  ),
  jsonld_statement0(Context, Json, Stmt).

% Case 1: An array of dictionaries.
jsonld_statement0(Context, Array, Stmt) :-
  is_list(Array), !,
  member(Obj, Array),
  jsonld_statement0(Context, Obj, Stmt).
% Case 2: A dictionary.
jsonld_statement0(Context1, Obj, Stmt) :-
  jsonld_context_and_data(Obj, Context2, Data),
  merge_contexts(Context1, Context2, Context3),
  jsonld_statement_goto(Context3, Data, Stmt).

% GOTO point for recursive structures (see below).
jsonld_statement_goto(Context, Data1, Stmt) :-
  jsonld_to_subject(Context, Data1, S, Data2),
  % NONDET
  member(Key-Value, Data2),
  jsonld_to_predicate(Context, Key, P, ODef, LTag),
  jsonld_statement(Context, S, P, ODef, LTag, Value, Stmt).



%! jsonld_dict_statement(+Context, +S, +P, +Dict, -Stmt) is nondet.

jsonld_dict_statement(Context, S1, P, D, Stmt) :-
  dict_pairs(D, Pairs),
  findall(Stmt, jsonld_statement_goto(Context, Pairs, Stmt), Stmts),
  (   % The triple connecting the parent object to the child object.
      Stmts = [rdf(S2,_,_)|_],
      Stmt = rdf(S1,P,S2)
  ;   % The triples that constitute the child object.
      % NONDET
      member(Stmt, Stmts)
  ).



%! jsonld_to_list_triple(+Context, +S, +P, +ODef, _LTag,  +L, -Stmt) is nondet.

jsonld_to_list_triple(Context, S, P, _, _, [], Stmt) :- !,
  rdf_equal(rdf:nil, Nil),
  statement_term(Context, S, P, Nil, Stmt).
jsonld_to_list_triple(Context, S, P, ODef, LTag, L, Stmt) :-
  rdf_create_bnode(B),
  (   statement_term(Context, S, P, B, Stmt)
  ;   jsonld_to_list_triple(Context, B, ODef, LTag, L, Stmt)
  ).

jsonld_to_list_triple(Context, S, ODef, LTag, [H], Stmt) :- !,
  (   rdf_equal(rdf:first, First),
      jsonld_statement(Context, S, First, ODef, LTag, H, Stmt)
  ;   rdf_equal(rdf:rest, Rest),
      rdf_equal(rdf:nil, Nil),
      statement_term(Context, S, Rest, Nil, Stmt)
  ).
jsonld_to_list_triple(Context, S, ODef, LTag, [H|T], Stmt) :-
  (   rdf_equal(rdf:first, First),
      jsonld_statement(Context, S, First, ODef, LTag, H, Stmt)
  ;   rdf_equal(rdf:rest, Rest),
      rdf_create_bnode(B),
      (   statement_term(Context, S, Rest, B, Stmt)
      ;   jsonld_to_list_triple(Context, B, ODef, LTag, T, Stmt)
      )
  ).



%! jsonld_to_predicate(+Context, +Key, -P, -ODef, -LTag) is det.
% Extract the RDF predicate term encoded by the given JSON-LD key.
% Also returns the datatype and language specification, if present.

% Case 1: The predicate is ‘rdf:type’.
%         The object definition is ‘@id’.
%         There is no language tag.
jsonld_to_predicate(_, '@type', P, '@id', _) :- !,
  rdf_equal(rdf:type, P).
% Case 2: Other predicates.
jsonld_to_predicate(Context, P, Q, ODef, LTag) :-
  jsonld_predicate(Context, P, Q, Keys),
  jsonld_datatype_mapping(Context, Keys, ODef),
  jsonld_language_mapping(Context, Keys, LTag).


% Case 1: JSON-LD keyword.
jsonld_predicate(_, P, P, []) :-
  jsonld_keyword(P), !.
% Case 2: JSON-LD IRI expansion.
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


%! jsonld_statement(+Context, +S, +P, +ODef, +LTag, +Value, -Stmt) is det.

% Named graph.
jsonld_statement(Context1, S, '@graph', _, _, Array, Stmt) :- !,
  put_dict('@graph', Context1, S, Context2),
  jsonld_statement0(Context2, Array, Stmt).
% Blank node.
jsonld_statement(Context, S, P, _, _, _{'@id': O}, Stmt) :-
  jsonld_is_bnode(O), !,
  statement_term(Context, S, P, O, Stmt).
% Container.
jsonld_statement(Context, S, P, ODef, LTag, Os, Stmt) :-
  (ODef == '@list' ; ODef == '@set'), !,
  is_list(Os),
  % NONDET
  jsonld_to_list_triple(Context, S, P, _, LTag, Os, Stmt).
% Abbreviated object list.
jsonld_statement(Context, S, P, ODef, LTag, Os, Stmt) :-
  is_list(Os), !,
  % NONDET
  member(O, Os),
  jsonld_statement(Context, S, P, ODef, LTag, O, Stmt).
% Object is an RDF container.
jsonld_statement(Context, S, P, ODef, LTag, _{'@list': L}, Stmt) :- !,
  % NONDET
  jsonld_to_list_triple(Context, S, P, ODef, LTag, L, Stmt).
jsonld_statement(Context, S, P, ODef, LTag, _{'@set': L}, Stmt) :- !,
  % NONDET
  jsonld_to_list_triple(Context, S, P, ODef, LTag, L, Stmt).
% Object is an RDF language-tagged string.
jsonld_statement(Context, S, P, _, _, _{'@language': LTag, '@value': Lex}, Stmt) :- !,
  statement_term(Context, S, P, Lex@LTag, Stmt).
% Object is an RDF literal with explicitly supplied RDF datatype.
jsonld_statement(Context, S, P, _, _, _{'@type': D1, '@value': V}, Stmt) :- !,
  jsonld_expand_term(Context, D1, D2),
  statement_term(Context, S, P, V^^D2, Stmt).
% Object is an IRI.
jsonld_statement(Context, S, P, _, _, _{'@id': O1}, Stmt) :- !,
  jsonld_expand_term(Context, O1, O2),
  statement_term(Context, S, P, O2, Stmt).
% Object is a string without explicitly supplied datatype.
jsonld_statement(Context, S, P, _, _, _{'@value': Lex}, Stmt) :- !,
  rdf_equal(xsd:string, D),
  statement_term(Context, S, P, Lex^D, Stmt).
% Object is a JSON-LD object (nesting).
jsonld_statement(Context, S1, P, _, _, O1, Stmt) :-
  is_dict(O1), !,
  dict_pairs(O1, Data1),
  findall(Stmt, jsonld_statement_goto(Context, Data1, Stmt), Stmts),
  (   % The triple connecting to the parent object to the child object.
      Stmts = [rdf(S2,_,_)|_],
      Stmt = rdf(S1,P,S2)
  ;   % The triples that constitute the child object.
      % NONDET
      member(Stmt, Stmts)
  ).
% Object is an IRI; try to expand it.
jsonld_statement(Context, S, P, ODef, _, O1, Stmt) :-
  ODef == '@id', !,
  jsonld_expand_term(Context, O1, O2),
  statement_term(Context, S, P, O2, Stmt).
% Explicitly typed literal.
jsonld_statement(Context, S, P, D, _, Lex, Stmt) :-
  ground(D), !,
  statement_term(Context, S, P, Lex^^D, Stmt).
% Language-tagged string ‘rdf:langString’.
jsonld_statement(Context, S, P, _, LTag, V, Stmt) :-
  (nonvar(LTag) ; get_dict('@language', Context, LTag)),
  LTag \== null, !,
  statement_term(Context, S, P, V@LTag, Stmt).
% Boolean strings default to ‘xsd:boolean’.
jsonld_statement(Context, S, P, _, _, V, Stmt) :-
  memberchk(V, [false,true]), !,
  rdf_equal(xsd:boolean, D),
  statement_term(Context, S, P, V^^D, Stmt).
% Integers default to ‘xsd:integer’.
jsonld_statement(Context, S, P, _, _, V, Stmt) :-
  integer(V), !,
  rdf_equal(xsd:integer, D),
  statement_term(Context, S, P, V^^D, Stmt).
% Decimal numbers default to ‘xsd:double’.
jsonld_statement(Context, S, P, _, _, V, Stmt) :-
  float(V), !,
  rdf_equal(xsd:double, D),
  statement_term(Context, S, P, V^^D, Stmt).
% Strings default to ‘xsd:string’.
jsonld_statement(Context, S, P, _, _, V, Stmt) :-
  rdf_equal(xsd:string, D),
  statement_term(Context, S, P, V^^D, Stmt).





% HELPERS %

%! jsonld_context_and_data(+JsonLd, -Context, -Data:list(pair)) is det.

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
  include(
    pair_with_nonvar_value,
    ['@base'-BaseIri,'@language'-LTag,'@vocab'-Voc2|Map],
    Pairs2
  ),
  dict_pairs(Context6, Pairs2).

pair_with_nonvar_value(_-V) :- nonvar(V).



statement_term(Context, S, P, O, rdf(S,P,O,G)) :-
  get_dict('@graph', Context, G), !.
statement_term(_, S, P, O, rdf(S,P,O)).



%! jsonld_datatype_mapping(+Context, +Keys, -D) is det.
% Returns the datatype for the given Key

jsonld_datatype_mapping(_, [], _) :- !.
jsonld_datatype_mapping(Context, [Key|_], D2) :-
  get_dict(Key, Context, ODef),
  is_dict(ODef),
  (   get_dict('@type', ODef, D1)
  ->  jsonld_expand_term(Context, D1, D2)
  ;   get_dict('@container', ODef, D1),
      atom_string(D2, D1)
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



%! merge_contexts(+Context1, +Context2, -Context3) is det.

merge_contexts(Context1, Context2, Context3) :-
  merge_dict(Context1, Context2, Context3).
