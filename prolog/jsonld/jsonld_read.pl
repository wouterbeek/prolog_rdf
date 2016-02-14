:- module(
  jsonld_read,
  [
    jsonld_to_triple/2, % +JsonLd, -Triple
    jsonld_to_triple/3  % +JsonLd, -Triple, +Opts
  ]
).

/** <module> JSON-LD read

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_download)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_bnode_name)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(typecheck)).

%! bnode_map(?Name, ?B) is nondet.

:- thread_local
   bnode_map/2.

:- predicate_options(jsonld_to_triple/3, 3, [
     pass_to(jsonld_to_context_and_data/4, 4)
   ]).
:- predicate_options(jsonld_to_context_and_data/4, 4, [
     base_iri(+atom)
   ]).





%! jsonld_to_triple(+Jsonld, -Triple) is det.
%! jsonld_to_triple(+Jsonld, -Triple, +Opts) is det.

jsonld_to_triple(D, Triple) :-
  flag(something, _, 0),
  jsonld_to_triple(D, Triple, []).

jsonld_to_triple(Ds, Triple, Opts) :-
  is_list(Ds), !,
  member(D, Ds),
  jsonld_to_triple(D, Triple, Opts).
jsonld_to_triple(D, Triple, Opts) :-
  jsonld_to_context_and_data(D, Context, Data, Opts),
  jsonld_to_triple_goto(Context, Data, Triple).

jsonld_to_triple_goto(Context, Data1, Triple) :-
  jsonld_to_subject(Context, Data1, S, Data2),
  % NONDET
  member(Pair, Data2),
  jsonld_to_triple(Context, S, Pair, Triple).


%! jsonld_to_triple(+Context, +S, +Pair, -Triple) is nondet.

jsonld_to_triple(Context, S, Key-Value, Triple) :-
  jsonld_to_predicate(Context, Key, P, ODef, LTag),
  jsonld_to_triple(Context, S, P, ODef, LTag, Value, Triple).



%! jsonld_to_bnode(+BNodeName:atom, -BNode:bnode) is det.

jsonld_to_bnode(B1, B2) :-
  atomic_list_concat(['_',_], :, B1), !,
  (bnode_map(B1, B2) -> true ; rdf_bnode(B2), assert(bnode_map(B1, B2))).



%! jsonld_dict_to_triple(+Context, +S, +P, +Dicts, -Triple) is nondet.

jsonld_dict_to_triple(Context, S1, P, D, Triple) :-
  dict_pairs(D, Pairs),
  findall(Triple, jsonld_to_triple_goto(Context, Pairs, Triple), Triples),
  (   % The triple connecting to the parent object to the child object.
      Triples = [rdf(S2,_,_)|_],
      Triple = rdf(S1,P,S2)
  ;   % The triples that constitute the child object.
      % NONDET
      member(Triple, Triples)
  ).



%! jsonld_to_list_triple(+Context, +S, +P, +List, -Triple) is nondet.

jsonld_to_list_triple(Context, S, P, L, T) :-
  L \== [],
  (   maplist(is_dict, L)
  ->  % NONDET
      member(D, L),
      jsonld_dict_to_triple(Context, S, P, D, T)
  ;   rdf_bnode(B),
      (statement_term(S, P, B, T) ; jsonld_to_list_triple0(Context, B, L, T))
  ).

jsonld_to_list_triple0(Context, B, [X1], T):- !,
  jsonld_term(n, Context, X1, X, _, _),
  (   rdf_equal(rdf:first, P), statement_term(B, P, X, T)
  ;   rdf_equal(rdf:rest, P), rdf_equal(rdf:nil, O), statement_term(B, P, O, T)
  ).
jsonld_to_list_triple0(Context, B1, [X1|_], T):-
  jsonld_term(n, Context, X1, X2, _, _),
  rdf_equal(rdf:first, P), statement_term(B1, P, X2, T).
jsonld_to_list_triple0(Context, B1, [_,X2|L], T):-
  rdf_bnode(B2),
  (   rdf_equal(rdf:rest, P), statement_term(B1, P, B2, T)
  ;   jsonld_to_list_triple0(Context, B2, [X2|L], T)
  ).



%! jsonld_to_predicate(+Context, +Key, -P, -ODef, -LTag) is det.
% Extracts the RDF predicate term encoded by the given JSON-LD key.
% Also returns the datatype and language specification, if present.

jsonld_to_predicate(_, '@type', P, '@id', _) :- !,
  rdf_equal(rdf:type, P).
jsonld_to_predicate(Context, P1, P2, ODef, Lang) :-
  jsonld_term(p, Context, P1, P2, ODef, Lang).



%! jsonld_to_subject(+Context, +Data1:list(pair), -S, -Data2:list(compound)) is det.
% Extracts the subject term from the JSON-LD data.

% An explicit subject term.
jsonld_to_subject(Context, L1, S2, L2) :-
  selectchk('@id'-S1, L1, L2), !,
  (jsonld_to_bnode(S1, S2) ; jsonld_term(n, Context, S1, S2, _, _)), !.
% No explicit subject term: create fresh blank node.
jsonld_to_subject(_, L, S, L) :- rdf_create_bnode(S).



%! jsonld_to_triple(+Context, +S, +P, +ODef, +LTag, +Value, -Triple) is det.

% RDF blank node.
jsonld_to_triple(_, S, P, _, _, _{'@id': O1}, T) :-
  jsonld_to_bnode(O1, O2), !,
  statement_term(S, P, O2, T).
% RDF container.
jsonld_to_triple(Context, S, P, ODef, _, Os, T) :-
  (ODef == "@list" ; ODef == "@set"), !,
  is_list(Os),
  % NONDET
  jsonld_to_list_triple(Context, S, P, Os, T).
% Abbreviated object list.
jsonld_to_triple(Context, S, P, ODef, L, Os, T) :-
  is_list(Os), !,
  % NONDET
  member(O, Os),
  jsonld_to_triple(Context, S, P, ODef, L, O, T).
% Object is an RDF container.
jsonld_to_triple(Context, S, P, _, _, _{'@list': L}, T) :- !,
  % NONDET
  jsonld_to_list_triple(Context, S, P, L, T).
jsonld_to_triple(Context, S, P, _, _, _{'@set': L}, T) :- !,
  % NONDET
  jsonld_to_list_triple(Context, S, P, L, T).
% Object is an RDF language-tagged string.
jsonld_to_triple(_, S, P, _, _, _{'@language':Lang,'@value':Lex}, T) :- !,
  statement_term(S, P, Lex@Lang, T).
% Object is an RDF literal with explicitly supplied RDF datatype.
jsonld_to_triple(_, S, P, _, _, _{'@type':D,'@value':Lex}, T) :- !,
  statement_term(S, P, Lex^^D, T).
% Object is an IRI.
jsonld_to_triple(Context, S, P, _, _, _{'@id':O0}, T) :- !,
  jsonld_term(n, Context, O0, O, _, _),
  statement_term(S, P, O, T).
% Object is an XSD string without explicitly supplied RDF datatype.
jsonld_to_triple(_, S, P, _, _, _{'@value':Lex}, T) :- !,
  rdf_equal(xsd:string, D), statement_term(S, P, Lex^D, T).
% Object is a JSON-LD object (nesting).
jsonld_to_triple(Context, S, P, _, _, O1, T) :-
  is_dict(O1), !,
  dict_pairs(O1, Data1),
  jsonld_to_subject(Context, Data1, O2, Data2),
  (   statement_term(S, P, O2, T)
  ;   % NONDET
      member(Pair, Data2),
      jsonld_to_triple(Context, O2, Pair, T)
  ).
% Object is a compact IRI.
jsonld_to_triple(Context, S, P, _, _, O1, T) :-
  jsonld_expand_iri(Context, O1, O2), !,
  statement_term(S, P, O2, T).
% ???
jsonld_to_triple(Context, S, P, ODef, _, O1, T) :-
  ODef == '@id', !,
  jsonld_term(n, Context, O1, O2, _, _),
  statement_term(S, P, O2, T).
% ???
jsonld_to_triple(_, S, P, D, _, Lex, T) :-
  ground(D), !,
  statement_term(S, P, Lex^^D, T).
% ???
jsonld_to_triple(Context, S, P, _, LTag, Lex, T) :-
  (   (nonvar(LTag) ; get_dict('@language', Context, LTag)),
      LTag \== null
  ->  statement_term(S, P, Lex@LTag, T)
  ;   rdf_equal(xsd:string, D), statement_term(S, P, Lex^^D, T)
  ).





% HELPERS %

%! jsonld_term(+Mode:oneof([n,p]), +Context, +ODef, -Term, -D, -LTag) is det.

jsonld_term(Mode, Context, ODef, Term, D, LTag) :-
  jsonld_term0(Mode, Context, ODef, Term, Keys),
  jsonld_datatype_mapping(Context, Keys, D),
  jsonld_language_mapping(Context, Keys, LTag).

% IRIs are fine.
jsonld_term0(p, _, X, X, [X]) :- is_http_iri(X), !.
% JSON-LD keywords are fine.
jsonld_term0(p, _, X, X, []) :- jsonld_keyword(X), !.
% Blank nodes are fine.
jsonld_term0(n, _, X, X, []) :- rdf_is_bnode(X), !.
% RDF literals are fine.
jsonld_term0(n, _, X, X, []) :- rdf_is_literal(X), !.
% Names that can be expanded by the JSON-LD context.
jsonld_term0(Mode, Context, X1, Y, [X1|T]) :-
  jsonld_expand_iri(Context, X1, X2), !,
  jsonld_term0(Mode, Context, X2, Y, T).
% Names that cannot be expanded belong to a vocabulary IRI, if present.
% This excludes names that are explicitly mapped to `null’ by the context.
jsonld_term0(Mode, Context, X1, Y, [X1|T]) :-
  \+ is_http_iri(X1),
  has_dict_key(Context, '@vocab'),
  \+ (get_dict(map, Context, Map), memberchk(X1-null, Map)), !,
  atomic_concat(Context.'@vocab', X1, X2),
  jsonld_term0(Mode, Context, X2, Y, T).
% Resolve relative IRIs WRT the base IRI.
jsonld_term0(n, Context, X, Y, []) :-
  (get_dict('@base', Context, BaseIri) -> uri_resolve(X, BaseIri, Y) ; Y = X).



%! jsonld_datatype_mapping(+Context, +Keys, -D) is det.
% Returns the datatype for the given Key

jsonld_datatype_mapping(_, [], _) :- !.
jsonld_datatype_mapping(Context, [Key|_], D2) :-
  get_dict(map, Context, Map),
  memberchk(Key-ODef, Map),
  is_dict(ODef),
  (   get_dict('@type', ODef, D1)
  ->  jsonld_term(p, Context, D1, D2, _, _)
  ;   get_dict('@container', ODef, D2)
  ->  true
  ), !.
jsonld_datatype_mapping(Context, [_|Keys], D) :-
  jsonld_datatype_mapping(Context, Keys, D).



%! jsonld_keyword(+Key) is semidet.
%! jsonld_keyword(-Key) is multi.

jsonld_keyword('@base').
jsonld_keyword('@context').
jsonld_keyword('@id').
jsonld_keyword('@type').
jsonld_keyword('@value').



%! jsonld_language_mapping(+Context, +Keys, -LTag) is det.

jsonld_language_mapping(_, [], _) :- !.
jsonld_language_mapping(Context, [Key|_], LTag) :-
  get_dict(map, Context, Map),
  memberchk(Key-ODef, Map),
  is_dict(ODef),
  get_dict('@language', ODef, LTag), !.
jsonld_language_mapping(Context, [_|Keys], LTag) :-
  jsonld_language_mapping(Context, Keys, LTag).



%! jsonld_to_base_iri(+Pairs1, -BaseIri, -Pairs2) is det.
% Extracts the base IRI from a JSON-LD context, if any.

jsonld_to_base_iri(Pairs1, BaseIri, Pairs2) :-
  selectchk('@base'-BaseIri, Pairs1, Pairs2), !.
jsonld_to_base_iri(Pairs, _, Pairs).



%! jsonld_to_context_and_data(+JsonLd, -Context, -Data:list(pair), +Opts) is det.

jsonld_to_context_and_data(D, Context3, Data, Opts) :-
  dict_pairs(D, Pairs),
  (   selectchk('@context'-Context1, Pairs, Data)
  ->  % Splits context from data and parses the context into:
      %   1. a fully qualitied base IRI
      %   2. a prefix map
      %   3. a default natural language
      %   4. a vocabulary IRI
      %
      % The context can be supplied either externally or internally.
      (is_http_iri(Context1) -> json_download(Context1, Context2) ; Context2 = Context1),
      dict_pairs(Context2, Pairs1),
      jsonld_to_base_iri(Pairs1, BaseIri1, Pairs2),
      jsonld_to_ltag(Pairs2, LTag, Pairs3),
      jsonld_to_vocab(Pairs3, Voc, Map)
  ;   Data = Pairs
  ),
  % The base IRI that was set outside of the JSON-LD context
  % takes precedence over the one that is set inside the JSON-LD context.
  option(base_iri(BaseIri2), Opts, BaseIri1),
  % Return the extracted context properties into a dictionary
  % since that is easy to pass around.
  include(pair_with_nonvar_value, ['@base'-BaseIri2,'@language'-LTag,map-Map,'@vocab'-Voc], L2),
  dict_pairs(Context3, L2).



%! jsonld_to_ltag(+Pairs1, -LTag, -Pairs2) is det.
% Extracts the default natural language from a JSON-LD context, if any.

jsonld_to_ltag(Pairs1, LTag, Pairs2) :-
  selectchk('@language'-LTag, Pairs1, Pairs2), !.
jsonld_to_ltag(Pairs, _, Pairs).



%! jsonld_to_vocab(+Pairs1, -Vocabulary, -Map) is det.
% Extracts the vocabulary IRI from a JSON-LD context, if any.

jsonld_to_vocab(Pairs, Voc, Map) :-
  selectchk('@vocab'-Voc0, Pairs, Map), !,
  jsonld_term(p, _{map:Map}, Voc0, Voc, _, _).
jsonld_to_vocab(Pairs, _, Pairs).

pair_with_nonvar_value(_-V) :- nonvar(V).

statement_term(S, P, O, rdf(S,P,O)).
