:- module(
  sparql_parser,
  [
    sparql_parse/5 % +BaseIri, +Dataset, +Query, -State, -Algebra
  ]
).

/** <module> SPARQL parser

Parses a SPARQL query string into the corresponding algebraic
expression.

@author Wouter Beek
@version 2017/05, 2017/08
*/

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dlist)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(sgml)).

:- meta_predicate
    'STRING_LITERAL_'(//, -, ?, ?).

:- multifile
    http:bad_request_error//2,
    prolog:message//1.

:- rdf_meta
   'NumericLiteralNegative'(o, ?, ?),
   'NumericLiteralPositive'(o, ?, ?),
   'NumericLiteralUnsigned'(o, ?, ?).





%! sparql_parse(+BaseIri:atom, +Dataset:compound, +Query:string, -State:dict,
%!              -Algebra:compound) is det.
%
% `VariableMap' can include hidden variables, i.e., ones that do not
% appear in the projection.

sparql_parse(BaseIri, dataset(DefaultGraphs,NamedGraphs), Query, State2, Algebra2) :-
  atom_codes(Query, Codes1),
  phrase(replace_codepoint_escape_sequences, Codes1, Codes2),
  empty_assoc(VariableMap1),
  State1 = _{
    active_graph: default,
    base_iri: BaseIri,
    default_graphs: DefaultGraphs,
    named_graphs: NamedGraphs,
    prefixes: [],
    variable_map: VariableMap1
  },
  once(phrase(sparql_parse0(State1, Form, Algebra1), Codes2)),
  _{prefixes: Prefixes2, variable_map: VariableMap2} :< State1,
  replace_vars(VariableMap2, Algebra1, Algebra2),
  assoc_to_list(VariableMap2, VariableMap3),
  transpose_pairs(VariableMap3, InvVariableMap),
  State2 = _{prefixes: Prefixes2, form: Form, variable_map: InvVariableMap}.

replace_vars(_, X, X) :-
  var(X), !.
replace_vars(VariableMap, var(VarName), X) :-
  get_assoc(VarName, VariableMap, X), !.
replace_vars(_, var(X), X) :- !.
replace_vars(VariableMap, L1, L2) :-
  is_list(L1), !,
  maplist(replace_vars(VariableMap), L1, L2).
replace_vars(VariableMap, Comp1, Comp2) :-
  compound(Comp1), !,
  Comp1 =.. [Pred|Args1],
  maplist(replace_vars(VariableMap), Args1, Args2),
  Comp2 =.. [Pred|Args2].
replace_vars(_, Term, Term).

sparql_parse0(State, Form, Algebra, In, Out) :-
  catch(
    'QueryUnit'(State, Form, Algebra, In, Out),
    E,
    add_error_location(E, In)
  ).





% CODEPOINT ESCAPE SEQUENCES %

%! replace_codepoint_escape_sequences// is det.
%
% @compat SPARQL 1.1 Query, Section 19.2.

replace_codepoint_escape_sequences, [C] -->
  codepoint_escape_sequence(C), !,
  replace_codepoint_escape_sequences.
replace_codepoint_escape_sequences, [C] -->
  [C], !,
  replace_codepoint_escape_sequences.
replace_codepoint_escape_sequences --> "".



%! codepoint_escape_sequence(-C)// .
%
% SPARQL alows the following two ways of escaping Unicode codes:
%
% ```bnf
% '\u' HEX HEX HEX HEX
% ```
%
% A Unicode code point in the range U+0 to U+FFFF inclusive
% corresponding to the encoded hexadecimal value.
%
% ```bnf
% '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ```
%
% A Unicode code point in the range U+0 to U+10FFFF inclusive
% corresponding to the encoded hexadecimal value.

codepoint_escape_sequence(C) -->
  "\\u", !,
  (   xdigit(D1), xdigit(D2), xdigit(D3), xdigit(D4)
  ->  {C is D1<<12 + D2<<8 + D3<<4 + D4}
  ;   {type_error(codepoint_escape_Sequence, C)}
  ).
codepoint_escape_sequence(C) -->
  "\\U",
  (   xdigit(D1), xdigit(D2), xdigit(D3), xdigit(D4),
      xdigit(D5), xdigit(D6), xdigit(D7), xdigit(D8)
  ->  {C is D1<<28 + D2<<24 + D3<<20 + D4<<16 + D5<<12 + D6<<8 + D7<<4 + D8}
  ;   {type_error(codepoint_escape_Sequence, C)}
  ).





% GRAMMAR: NON-TERMINALS %

%! 'AdditiveExpression'(+State, -E)// is det.
%
% ```ebnf
% [116] AdditiveExpression ::= MultiplicativeExpression
%                              (  '+' MultiplicativeExpression
%                              |  '-' MultiplicativeExpression
%                              |  (   NumericLiteralPositive
%                                 |   NumericLiteralNegative
%                                 )
%                                 (   ( '*' UnaryExpression )
%                                 |   ( '/' UnaryExpression )
%                                 )*
%                              )*
% ```

'AdditiveExpression'(State, E2) -->
  'MultiplicativeExpression'(State, E1),
  'AdditiveExpression0*'(State, E1, E2).

'AdditiveExpression0*'(State, E1, E3) -->
  'AdditiveExpression0'(State, E1, E2), !,
  'AdditiveExpression0*'(State, E2, E3).
'AdditiveExpression0*'(_, E, E) --> "".

'AdditiveExpression0'(State, E1, Function) -->
  (   "+"
  ->  {Functor = 'op:numeric-add'}
  ;   "-"
  ->  {Functor = 'op:numeric-subtract'}
  ), !,
  skip_ws,
  must_see('MultiplicativeExpression'(State, E2)),
  {Function =.. [Functor,E1,E2]}.
'AdditiveExpression0'(State, E1, E4) -->
  ('NumericLiteralPositive'(E2) ; 'NumericLiteralNegative'(E2)), !,
  (   "*"
  ->  skip_ws,
      'UnaryExpression'(State, E3),
      % @tbd op:numeric-multiply
      {E4 = E1*E2*E3}
  ;   "/"
  ->  skip_ws,
      'UnaryExpression'(State, E3),
      % @tbd op:numeric-divide
      {E4 = E1/E2/E3}
  ).



%! 'Aggregate'(+State, -Aggr)// is det.
%
% ```ebnf
% [127] Aggregate ::= 'COUNT' '(' 'DISTINCT'? ( '*' | Expression ) ')'
%                   | 'SUM' '(' 'DISTINCT'? Expression ')'
%                   | 'MIN' '(' 'DISTINCT'? Expression ')'
%                   | 'MAX' '(' 'DISTINCT'? Expression ')'
%                   | 'AVG' '(' 'DISTINCT'? Expression ')'
%                   | 'SAMPLE' '(' 'DISTINCT'? Expression ')'
%                   | 'GROUP_CONCAT' '(' 'DISTINCT'?  Expression
%                     ( ';' 'SEPARATOR' '=' String )? ')'
% ```

% COUNT
'Aggregate'(State, Count) -->
  keyword(`count`), !,
  must_see_code(0'(),
  'DISTINCT?'(Count, E),
  ("*" -> skip_ws, {E = *} ; 'Expression'(State, E)),
  must_see_code(0')).
% SUM
% MIN
% MAX
% AVG
% SAMPLE
'Aggregate'(State, Aggr) -->
  ( keyword(`sum`) -> {AggrOp = sum}
  ; keyword(`min`) -> {AggrOp = min}
  ; keyword(`max`) -> {AggrOp = max}
  ; keyword(`avg`) -> {AggrOp = avg}
  ; keyword(`sample`) -> {AggrOp = sample}
  ), !,
  must_see_code(0'(),
  {Aggr =.. [AggrOp,AggrArg]},
  'DISTINCT?'(AggrArg, AggrE),
  'Expression'(State, AggrE),
  must_see_code(0')).
% GROUP_CONCAT
'Aggregate'(State, group_concat(E1,literal(Sep))) -->
  keyword(`group_concat`), !,
  must_see_code(0'(),
  'DISTINCT?'(E1, E2),
  'Expression'(State, E2),
  (   ";"
  ->  skip_ws,
      must_see(keyword(`separator`)),
      must_see_code(0'=),
      'String'(Sep)
  ;   {Sep = ' '}      % default sep is a single space
  ),
  must_see_code(0')).

aggregate_keyword(avg).
aggregate_keyword(max).
aggregate_keyword(min).
aggregate_keyword(sample).
aggregate_keyword(sum).

is_aggregate(avg(_)).
is_aggregate(count(_)).
is_aggregate(group_concat(_,_)).
is_aggregate(max(_)).
is_aggregate(min(_)).
is_aggregate(sample(_)).
is_aggregate(sum(_)).



%! 'ArgList'(+State, -Args:list)// is det.
%
% ```ebnf
% [71] ArgList ::= NIL
%                | '(' 'DISTINCT'? Expression ( ',' Expression )* ')'
% ```

'ArgList'(_, []) -->
  'NIL'(_), !, skip_ws.
'ArgList'(State, Distinct) -->
  "(", skip_ws,
  'DISTINCT?'(Distinct, Es),
  'Expression+'(State, Es),
  must_see_code(0')).



%! 'AskQuery'(+State, -P1, -P2, -P3)// is det.
%
% ```bnf
% [12] AskQuery ::= 'ASK' DatasetClause* WhereClause SolutionModifier
% ```

'AskQuery'(State, P2, P3, P4) -->
  keyword(`ask`), !,
  'DatasetClause*'(State),
  'WhereClause'(State, P1),
  'SolutionModifier'(State, P1, ask, P2, P3, P4).



%! 'BaseDecl'(+State)// is det.
%
%  Match "base <URI>".
%
% ```bnf
% [5] BaseDecl ::= 'BASE' IRIREF
% ```
%
% @tbd Should a new base IRI be resolved relative to the previous base
%      IRI?

'BaseDecl'(State) -->
  keyword(`base`),
  must_see('IRIREF'(State, Iri)),
  {nb_set_dict(base_iri, State, Iri)}.



%! 'Bind'(+State, -Binding:compound)// is det.
%
% ```ebnf
% [60] Bind ::= 'BIND' '(' Expression 'AS' Var ')'
% ```

'Bind'(State, 'BIND'(Var,E)) -->
  keyword(`bind`), !,
  must_see_code(0'(),
  must_see('Expression'(State, E)),
  must_see(keyword(`as`)),
  must_see('Var'(State, Var)),
  must_see_code(0')).



%! 'BlankNode'(-BNode)// is det.
%
% Anonymous blank nodes are returned as bnode(_).
%
% ```ebnf
% [138] BlankNode ::= BLANK_NODE_LABEL | ANON
% ```

'BlankNode'(BNode) -->
  'BLANK_NODE_LABEL'(BNode), !,
  skip_ws.
'BlankNode'(BNode) -->
  'ANON'(BNode),
  skip_ws.



%! 'BlankNodePropertyList'(+State, -Node, -Triples)// is det.
%
% ‘[p o]’ encodes triple ‘b p o’, where ‘b’ is a new blank node.  ‘b’
% can be used in the subject or object notation of another triple: ‘[p
% o] q v’ or ‘s p [q o]’.
%
% ‘[p o]’ is equivalent to ‘[] p o’.
%
% ```ebnf
% [99] BlankNodePropertyList ::= '[' PropertyListNotEmpty ']'
% ```

'BlankNodePropertyList'(State, Node, Triples) -->
  "[",
  {rdf_create_bnode_iri(Node)},
  skip_ws,
  'PropertyListNotEmpty'(State, Node, Triples),
  must_see_code(0']),
  skip_ws.



%! 'BlankNodePropertyListPath'(+State, -Node, -Triples)// is det.
%
% ```ebnf
% [101] BlankNodePropertyListPath ::= '[' PropertyListPathNotEmpty ']'
% ```

'BlankNodePropertyListPath'(State, Node, Triples) -->
  "[",
  {rdf_create_bnode_iri(Node)},
  skip_ws,
  'PropertyListPathNotEmpty'(State, Node, Triples),
  must_see_code(0']),
  skip_ws.



%! 'BooleanLiteral'(-Lit)// is det.
%
% ```ebnf
% [134] BooleanLiteral ::= 'true' | 'false'
% ```

'BooleanLiteral'(true^^xsd:boolean) -->
  keyword(`true`), !.
'BooleanLiteral'(false^^xsd:boolean) -->
  keyword(`false`).



%! 'BrackettedExpression'(+State, -E)// is det.
%
% ```ebnf
% [120] BrackettedExpression ::= '(' Expression ')'
% ```

'BrackettedExpression'(State, E) -->
  "(",
  skip_ws,
  must_see('Expression'(State, E)),
  must_see_code(0')).



%! 'BuiltInCall'(+State, -Function)// is det.
%
% ```ebnf
% [121] BuiltInCall ::= Aggregate
%                     | 'STR' '(' Expression ')'
%                     | 'LANG' '(' Expression ')'
%                     | 'LANGMATCHES' '(' Expression ',' Expression ')'
%                     | 'DATATYPE' '(' Expression ')'
%                     | 'BOUND' '(' Var ')'
%                     | 'IRI' '(' Expression ')'
%                     | 'URI' '(' Expression ')'
%                     | 'BNODE' ( '(' Expression ')' | NIL )
%                     | 'RAND' NIL
%                     | 'ABS' '(' Expression ')'
%                     | 'CEIL' '(' Expression ')'
%                     | 'FLOOR' '(' Expression ')'
%                     | 'ROUND' '(' Expression ')'
%                     | 'CONCAT' ExpressionList
%                     | SubstringExpression
%                     | 'STRLEN' '(' Expression ')'
%                     | StrReplaceExpression
%                     | 'UCASE' '(' Expression ')'
%                     | 'LCASE' '(' Expression ')'
%                     | 'ENCODE_FOR_URI' '(' Expression ')'
%                     | 'CONTAINS' '(' Expression ',' Expression ')'
%                     | 'STRSTARTS' '(' Expression ',' Expression ')'
%                     | 'STRENDS' '(' Expression ',' Expression ')'
%                     | 'STRBEFORE' '(' Expression ',' Expression ')'
%                     | 'STRAFTER' '(' Expression ',' Expression ')'
%                     | 'YEAR' '(' Expression ')'
%                     | 'MONTH' '(' Expression ')'
%                     | 'DAY' '(' Expression ')'
%                     | 'HOURS' '(' Expression ')'
%                     | 'MINUTES' '(' Expression ')'
%                     | 'SECONDS' '(' Expression ')'
%                     | 'TIMEZONE' '(' Expression ')'
%                     | 'TZ' '(' Expression ')'
%                     | 'NOW' NIL
%                     | 'UUID' NIL
%                     | 'STRUUID' NIL
%                     | 'MD5' '(' Expression ')'
%                     | 'SHA1' '(' Expression ')'
%                     | 'SHA256' '(' Expression ')'
%                     | 'SHA384' '(' Expression ')'
%                     | 'SHA512' '(' Expression ')'
%                     | 'COALESCE' ExpressionList
%                     | 'IF' '(' Expression ',' Expression ',' Expression ')'
%                     | 'STRLANG' '(' Expression ',' Expression ')'
%                     | 'STRDT' '(' Expression ',' Expression ')'
%                     | 'sameTerm' '(' Expression ',' Expression ')'
%                     | 'isIRI' '(' Expression ')'
%                     | 'isURI' '(' Expression ')'
%                     | 'isBLANK' '(' Expression ')'
%                     | 'isLITERAL' '(' Expression ')'
%                     | 'isNUMERIC' '(' Expression ')'
%                     | RegexExpression
%                     | ExistsFunc
%                     | NotExistsFunc
% ```

% Aggregate
'BuiltInCall'(State, Function) -->
  'Aggregate'(State, Function), !.
% Unary functions: STR, LANG, DATATYPE, IRI, URI, ABS, CEIL, FLOOR,
%                  ROUND, STRLEN, UCASE, LCASE, ENCODE_FOR_URI, YEAR,
%                  MONTH, DAY, HOURS, MINUTES, SECONDS, TIMEZONE, TZ,
%                  MD5, SHA1, SHA256, SHA384, SHA512, isIRI, isURI,
%                  isBLANK, isLITERAL, isNUMERIC
'BuiltInCall'(State, Function) -->
  ( keyword(`str`) -> {Functor = 'STR'}
  ; keyword(`lang`) -> {Functor = 'LANG'}
  ; keyword(`datatype`) -> {Functor = 'DATATYPE'}
  ; keyword(`iri`) -> {Functor = 'IRI'}
  ; keyword(`uri`) -> {Functor = 'URI'}
  ; keyword(`abs`) -> {rdf_equal(fn:'numeric-abs', Functor)}
  ; keyword(`ceil`) -> {rdf_equal(fn:'numeric-ceil', Functor)}
  ; keyword(`floor`) -> {rdf_equal(fn:'numeric-floor', Functor)}
  ; keyword(`round`) -> {rdf_equal(fn:'numeric-round', Functor)}
  ; keyword(`strlen`) -> {rdf_equal(fn:'string-length', Functor)}
  ; keyword(`ucase`) -> {rdf_equal(fn:'upper-case', Functor)}
  ; keyword(`lcase`) -> {rdf_equal(fn:'lower-case', Functor)}
  ; keyword(`encode_for_uri`) -> {rdf_equal(fn:'encode-for-uri', Functor)}
  ; keyword(`year`) -> {rdf_equal(fn:'year-from-dateTime', Functor)}
  ; keyword(`month`) -> {rdf_equal(fn:'month-from-dateTime', Functor)}
  ; keyword(`day`) -> {rdf_equal(fn:'day-from-dateTime', Functor)}
  ; keyword(`hours`) -> {rdf_equal(fn:'hours-from-dateTime', Functor)}
  ; keyword(`minutes`) -> {rdf_equal(fn:'minutes-from-dateTime', Functor)}
  ; keyword(`seconds`) -> {rdf_equal(fn:'seconds-from-dateTime', Functor)}
  ; keyword(`timezone`) -> {rdf_equal(fn:'timezone-from-dateTime', Functor)}
  ; keyword(`tz`) -> {Functor = tz}
  ; keyword(`md5`) -> {Functor = 'MD5'}
  ; keyword(`sha1`) -> {Functor = 'SHA1'}
  ; keyword(`sha256`) -> {Functor = 'SHA256'}
  ; keyword(`sha384`) -> {Functor = 'SHA384'}
  ; keyword(`sha512`) -> {Functor = 'SHA512'}
  ; keyword(`isiri`) -> {Functor = isIri}
  ; keyword(`isuri`) -> {Functor = isUri}
  ; keyword(`isblank`) -> {Functor = isBlank}
  ; keyword(`isliteral`) -> {Functor = isLiteral}
  ; keyword(`isnumeric`) -> {Functor = isNumeric}
  ), !,
  must_see_code(0'(),
  must_see('Expression'(State, E)),
  must_see_code(0')),
  {Function =.. [Functor,E]}.
% Binary functions: LANGMATCHES, CONTAINS, STRSTARTS, STRENDS,
%                   STRBEFORE, STRAFTER, STRLANG, STRDT, sameTerm
'BuiltInCall'(State, Function) -->
  ( keyword(`langmatches`) -> {Functor = langMatches}
  ; keyword(`contains`) -> {rdf_equal(fn:contains, Functor)}
  ; keyword(`strstarts`) -> {rdf_equal(fn:'starts-with', Functor)}
  ; keyword(`strends`) -> {rdf_equal(fn:'ends-with', Functor)}
  ; keyword(`strbefore`) -> {rdf_equal(fn:'substring-before', Functor)}
  ; keyword(`strafter`) -> {rdf_equal(fn:'substring-after', Functor)}
  ; keyword(`strlang`) -> {Functor = 'STRLANG'}
  ; keyword(`strdt`) -> {Functor = 'STRDT'}
  ; keyword(`sameterm`) -> {Functor = sameTerm}
  ), !,
  must_see_code(0'(),
  must_see('Expression'(State, E1)),
  must_see_code(0',),
  must_see('Expression'(State, E2)),
  must_see_code(0')),
  {Function =.. [Functor,E1,E2]}.
% BNODE
'BuiltInCall'(State, Function) -->
  keyword(`bnode`), !,
  (   must_see_code(0'(),
      must_see('Expression'(State, E)),
      must_see_code(0'))
  ->  {Function = 'BNODE'(E)}
  ;   must_see('NIL'(_))
  ->  {Function = 'BNODE'}
  ).
% BOUND
'BuiltInCall'(State, bound(Var)) -->
  keyword(`bound`), !,
  must_see_code(0'(),
  must_see('Var'(State, Var)),
  must_see_code(0')).
% 0-ary functions: RAND, NOW, UUID, STRUUID
'BuiltInCall'(_, Functor) -->
  (   keyword(`rand`) -> {Functor = rand}
  ;   keyword(`now`) -> {Functor = now}
  ;   keyword(`uuid`) -> {Functor = 'UUID'}
  ;   keyword(`struuid`) -> {Functor = 'STRUUID'}
  ), !,
  must_see('NIL'(_)).
% N-arg functions: CONCAT, COALESCE
'BuiltInCall'(State, Function) -->
  (   keyword(`concat`) -> {rdf_equal(fn:concat, Functor)}
  ;   keyword(`coalesce`) -> {Functor = 'COALESCE'}
  ), !,
  must_see('ExpressionList'(State, Es)),
  {Function =.. [Functor,Es]}.
% SubstringExpression
'BuiltInCall'(State, Substr) -->
  'SubstringExpression'(State, Substr), !.
% StrReplaceExpression
'BuiltInCall'(State, Replace) -->
  'StrReplaceExpression'(State, Replace), !.
% Tertiary functions: IF
'BuiltInCall'(State, 'IF'(E1,E2,E3)) -->
  keyword(`if`), !,
  must_see_code(0'(),
  must_see('Expression'(State, E1)),
  must_see_code(0',),
  must_see('Expression'(State, E2)),
  must_see_code(0',),
  must_see('Expression'(State, E3)),
  must_see_code(0')).
% RegexExpression
'BuiltInCall'(State, Function) -->
  'RegexExpression'(State, Function), !.
% ExistsFunc
'BuiltInCall'(State, Function) -->
  'ExistsFunc'(State, Function), !.
% NotExistsFunc
'BuiltInCall'(State, Function) -->
  'NotExistsFunc'(State, Function), !.



%! 'Collection'(+State, -Node, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [102] Collection ::= '(' GraphNode+ ')'
% ```

'Collection'(State, Node, Triples) -->
  "(", skip_ws,
  'GraphNode+'(State, Node, Triples),
  must_see_code(0')).



%! 'CollectionPath'(+State, -Node, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [103] CollectionPath ::= '(' GraphNodePath+ ')'
% ```

'CollectionPath'(State, Node, Triples) -->
  "(",
  skip_ws,
  'GraphNodePath+'(State, Node, Triples),
  must_see_code(0')).



%! 'ConditionalAndExpression'(+State, -E)// is det.
%
% ```ebnf
% [112] ConditionalAndExpression ::= ValueLogical
%                                    ( '&&' ValueLogical )*
% ```

'ConditionalAndExpression'(State, E3) -->
  'ValueLogical'(State, E1),
  (   "&&"
  ->  skip_ws,
      must_see('ConditionalAndExpression'(State, E2)),
      {E3 = 'logical-and'(E1,E2)}
  ;   {E3 = E1}
  ).



%! 'ConditionalOrExpression'(+State, -E)// is det.
%
% ```ebnf
% [111] ConditionalOrExpression ::= ConditionalAndExpression
%                                   ( '||' ConditionalAndExpression )*
% ```

'ConditionalOrExpression'(State, E3) -->
  'ConditionalAndExpression'(State, E1),
  (   "||"
  ->  skip_ws,
      must_see('ConditionalOrExpression'(State, E2)),
      {E3 = 'logical-or'(E1,E2)}
  ;   {E3 = E1}
  ).



%! 'Constraint'(+State, -Constraint:compound)// is det.
%
% ```ebnf
% [69] Constraint ::= BrackettedExpression
%                   | BuiltInCall
%                   | FunctionCall
% ```

'Constraint'(State, E) -->
  'BrackettedExpression'(State, E).
'Constraint'(State, Function) -->
  'BuiltInCall'(State, Function).
'Constraint'(State, Function) -->
  'FunctionCall'(State, Function).



%! 'ConstructQuery'(+State, -P1, -P2, -P3)// is det.
%
% ```bnf
% [10] ConstructQuery ::= 'CONSTRUCT'
%                         ( ConstructTemplate
%                           DatasetClause*
%                           WhereClause
%                           SolutionModifier
%                         | DatasetClause*
%                           'WHERE' '{' TriplesTemplate? '}'
%                           SolutionModifier
%                         )
% ```

'ConstructQuery'(State, P2, P3, 'Construct'(Templ,P4)) -->
  keyword(`construct`), !,
  (   'ConstructTemplate'(State, Templ)
  ->  'DatasetClause*'(State),
      'WhereClause'(State, P1),
      'SolutionModifier'(State, P1, construct, P2, P3, P4)
  ;   'DatasetClause*'(State),
      must_see(keyword(`where`)),
      must_see_code(0'{),
      ('TriplesTemplate'(State, Triples) -> skip_ws ; {Triples = T-T}),
      {dlist_to_list(Triples, P1)},
      must_see_code(0'}),
      'SolutionModifier'(State, P1, construct, P2, P3, P4)
  ).



%! 'ConstructTemplate'(+State, -Triples:dlist(compound))// is det.
%
% ```bnf
% [73] ConstructTemplate ::= '{' ConstructTriples? '}'
% ```

'ConstructTemplate'(State, G2) -->
  "{",
  skip_ws,
  ('ConstructTriples'(State, G1) -> {translate_group(G1, G2)} ; {G2 = []}),
  must_see_code(0'}).



%! 'ConstructTriples'(+State, -Triples:dlist(compound))// is det.
%
% ```bnf
% [74] ConstructTriples ::= TriplesSameSubject
%                           ( '.' ConstructTriples? )?
% ```

'ConstructTriples'(State, G3) -->
  'TriplesSameSubject'(State, G1), !,
  (   "."
  ->  skip_ws,
      (   'ConstructTriples'(State, G2)
      ->  {dappend(G1, G2, G3)}
      ;   {G3 = G1})
  ;   {G3 = G1}
  ).



%! 'DataBlock'(+State, -Data:compound)// is det.
%
% ‘Data’ is either of the following:
%
%    * ‘var_in(Var,Vals)’
%
%    * ‘vars_in(Vars,Vals)’
%
% ```ebnf
% [62] DataBlock ::= InlineDataOneVar | InlineDataFull
% ```

'DataBlock'(State, VarIn) -->
  'InlineDataOneVar'(State, VarIn), !.
'DataBlock'(State, VarsIn) -->
  'InlineDataFull'(State, VarsIn).



%! 'DataBlockValue'(+State, -Term)// is det.
%
% ```ebnf
% [65] DataBlockValue ::= iri
%                       | RDFLiteral
%                       | NumericLiteral
%                       | BooleanLiteral
%                       | 'UNDEF'
% ```

'DataBlockValue'(State, Iri) -->
  iri(State, Iri), !.
'DataBlockValue'(State, Lit) -->
  'RDFLiteral'(State, Lit), !.
'DataBlockValue'(_, Lit) -->
  'NumericLiteral'(Lit), !.
'DataBlockValue'(_, Lit) -->
  'BooleanLiteral'(Lit), !.
% UNDEF acts as a Prolog variable.
'DataBlockValue'(_, _) -->
  keyword(`undef`).

'DataBlockValue*'(State, Terms) -->
  'DataBlockValue*'(State, Terms, []).

'DataBlockValue*'(State, [Term|Terms0], Terms) -->
  'DataBlockValue'(State, Term), !,
  'DataBlockValue*'(State, Terms0, Terms).
'DataBlockValue*'(_, Terms, Terms) --> "".



%! 'DatasetClause'(+State:dict, -Graph:compound)// is det.
%
% ```bnf
% [13] DatasetClause ::= 'FROM' ( DefaultGraphClause | NamedGraphClause )
% ```


'DatasetClause*'(State) -->
  dataset_clauses(State, [], Ds, [], Ns),
  {set_dataset(State, Ds, Ns)}.

dataset_clauses(State, Ds1, Ds2, Ns1, Ns2) -->
  keyword(`from`), !,
  (   'DefaultGraphClause'(State, D)
  ->  {ord_add_element(Ds1, D, Ds2), Ns2 = Ns1}
  ;   'NamedGraphClause'(State, N)
  ->  {Ds2 = Ds1, ord_add_element(Ns1, N, Ns2)}
  ).
dataset_clauses(_, Ds, Ds, Ns, Ns) --> "".

% The dataset is not set by the SPARQL 1.1 Protocol.
set_dataset(State, Ds, Ns) :-
  _{default_graphs: [], named_graphs: []} :< State, !,
  (   Ds == [], Ns == []
  ->  % We use the default default graph.  The dataset is set
      % by using the SPARQL 1.1 Protocol.  This takes precedence over
      % `FROM' and `FROM NAMED' clauses in the query.
      rdf_equal(graph:default, D),
      nb_set_dict(default_graphs, State, [D])
  ;   % The dataset is set by `FROM' and/or `FROM NAMED' clauses.
      nb_set_dict(default_graphs, State, Ds),
      nb_set_dict(named_graphs, State, Ns)
  ).
% The dataset is set by the SPARQL 1.1 Protocol: do not change it.
set_dataset(_, _, _).



%! 'DefaultGraphClause'(+State, -DefaultGraph)// is det.
%
% ```bnf
% [14] DefaultGraphClause ::= SourceSelector
% ```
%
% @tbd Allow more than one default graph.

'DefaultGraphClause'(State, DefaultGraph) -->
  'SourceSelector'(State, DefaultGraph).



%! 'DescribeQuery'(+State, -P1, -P2, -P3)// is det.
%
% ```bnf
% [11] DescribeQuery ::= 'DESCRIBE'
%                        ( VarOrIri+ | '*' )
%                        DatasetClause*
%                        WhereClause?
%                        SolutionModifier
% ```

'DescribeQuery'(State, P2, P3, P4) -->
  keyword(`describe`), !,
  (   +('VarOrIri'(State), Terms)
  ->  {Proj = Terms}
  ;   "*"
  ->  {Proj = *},
      skip_ws
  ),
  'DatasetClause*'(State),
  ('WhereClause'(State, P1) -> "" ; {P1 = 'Z'}),
  'SolutionModifier'(State, P1, describe(Proj), P2, P3, P4).



%! 'ExistsFunc'(+State, -Exists:compound)// is det.
%
% Replaces ‘EXISTS{P}’ with ‘exists(translate(P))’ (§18.2.2.2).
%
% ```ebnf
% [125] ExistsFunc ::= 'EXISTS' GroupGraphPattern
% ```

'ExistsFunc'(State, exists(G)) -->
  keyword(`exists`),
  % @note G has already been translated.
  'GroupGraphPattern'(State, G).



%! 'Expression'(+State, -E)// is det.
%
% ```ebnf
% [110] Expression ::= ConditionalOrExpression
% ```

'Expression'(State, E) -->
  'ConditionalOrExpression'(State, E),
  skip_ws.

'Expression+'(State, [E|Es]) -->
  must_see('Expression'(State, E)),
  'Expression*'(State, Es).

'Expression*'(State, [E|Es]) -->
  ",", !,
  skip_ws,
  must_see('Expression'(State, E)),
  'Expression*'(State, Es).
'Expression*'(_, []) --> "".



%! 'ExpressionList'(+State, -Es:list)// is det.
%
% ```ebnf
% [72] ExpressionList ::= NIL
%                       | '(' Expression ( ',' Expression  )* ')'
% ```

'ExpressionList'(_, []) -->
  'NIL'(_), !.
'ExpressionList'(State, Es) -->
  "(",
  skip_ws,
  'Expression+'(State, Es),
  must_see_code(0')).



%! 'Filter'(+State, -Filter:compound)// is det.
%
% ```ebnf
% [68] Filter ::= 'FILTER' Constraint
% ```

'Filter'(State, 'FILTER'(Constraint)) -->
  keyword(`filter`),
  must_see('Constraint'(State, Constraint)).



%! 'FunctionCall'(+State, -Function:compound)// is det.
%
% ```ebnf
% [70] FunctionCall ::= iri ArgList
% ```

'FunctionCall'(State, 'FUNCTION'(Functor,Args)) -->
  iri(State, Functor),
  'ArgList'(State, Args).



%! 'GraphGraphPattern'(+State, -Pattern)// is det.
%
% If the form is GRAPH IRI GroupGraphPattern
%     The result is Graph(IRI, Translate(GroupGraphPattern))
% If the form is GRAPH Var GroupGraphPattern
%     The result is Graph(Var, Translate(GroupGraphPattern))
%
% ```ebnf
% [58] GraphGraphPattern ::= 'GRAPH' VarOrIri GroupGraphPattern
% ```

'GraphGraphPattern'(State, Pattern) -->
  keyword(`graph`), !,
  'VarOrIri'(State, NamedGraph),
  {
    get_dict(named_graphs, State, NamedGraphs),
    (memberchk(NamedGraph, NamedGraphs) -> true ; existence_error(named_graph, NamedGraph)),
    get_dict(active_graph, State, AG),
    nb_set_dict(active_graph, State, NamedGraph)
  },
  'GroupGraphPattern'(State, Pattern),
  {nb_set_dict(active_graph, State, AG)}.



%! 'GraphNode'(+State, -Node, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [104] GraphNode ::= VarOrTerm | TriplesNode
% ```

'GraphNode'(State, Node, T-T) -->
  'VarOrTerm'(State, Node), !.
'GraphNode'(State, Node, Triples) -->
  'TriplesNode'(State, Node, Triples).

'GraphNode+'(State, Node, Triples) -->
  must_see('GraphNode'(State, Node, Triples1)),
  'GraphNode*'(State, Triples2),
  {dappend(Triples1, Triples2, Triples)}.

'GraphNode*'(State, Triples) -->
  'GraphNode'(State, _, Triples1),
  'GraphNode*'(State, Triples2),
  {dappend(Triples1, Triples2, Triples)}.
'GraphNode*'(_, T-T) --> "".



%! 'GraphNodePath'(+State, -Node, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [105] GraphNodePath ::= VarOrTerm | TriplesNodePath
% ```

'GraphNodePath'(State, Node, T-T) -->
  'VarOrTerm'(State, Node), !.
'GraphNodePath'(State, Node, Triples) -->
  'TriplesNodePath'(State, Node, Triples).

'GraphNodePath+'(State, Node, Triples) -->
  must_see('GraphNodePath'(State, Node, Triples1)),
  'GraphNodePath*'(State, Triples2),
  {dappend(Triples1, Triples2, Triples)}.

'GraphNodePath*'(State, Triples) -->
  'GraphNodePath'(State, _, Triples1), !,
  'GraphNodePath*'(State, Triples2),
  {dappend(Triples1, Triples2, Triples)}.
'GraphNodePath*'(_, T-T) --> "".



%! 'GraphPatternNotTriples'(+State, -Pattern:compound)// is det.
%
% ```ebnf
% [56] GraphPatternNotTriples ::= GroupOrUnionGraphPattern
%                               | OptionalGraphPattern
%                               | MinusGraphPattern
%                               | GraphGraphPattern
%                               | ServiceGraphPattern
%                               | Filter
%                               | Bind
%                               | InlineData
% ```

'GraphPatternNotTriples'(State, Union) -->
  'GroupOrUnionGraphPattern'(State, Union), !.
'GraphPatternNotTriples'(State, Optional) -->
  'OptionalGraphPattern'(State, Optional), !.
'GraphPatternNotTriples'(State, Minus) -->
  'MinusGraphPattern'(State, Minus), !.
'GraphPatternNotTriples'(State, GraphGraphPattern) -->
  'GraphGraphPattern'(State, GraphGraphPattern), !.
'GraphPatternNotTriples'(State, Service) -->
  'ServiceGraphPattern'(State, Service), !.
'GraphPatternNotTriples'(State, Filter) -->
  'Filter'(State, Filter), !.
'GraphPatternNotTriples'(State, Binding) -->
  'Bind'(State, Binding), !.
'GraphPatternNotTriples'(State, VarOrVarsIn) -->
  'InlineData'(State, VarOrVarsIn).



%! 'GraphTerm'(+State, -Term)// is det.
%
% ```ebnf
% [109] GraphTerm ::= iri
%                   | RDFLiteral
%                   | NumericLiteral
%                   | BooleanLiteral
%                   | BlankNode
%                   | NIL
% ```

'GraphTerm'(State, Iri) -->
  iri(State, Iri), !.
'GraphTerm'(State, Lit) -->
  'RDFLiteral'(State, Lit), !.
'GraphTerm'(_, Lit) -->
  'NumericLiteral'(Lit), !.
'GraphTerm'(_, Lit) -->
  'BooleanLiteral'(Lit), !.
'GraphTerm'(_, BNode) -->
  'BlankNode'(BNode).
'GraphTerm'(_, Iri) -->
  'NIL'(Iri),
  skip_ws.



%! 'GroupClause'(+State, -Es)// is det.
%
% ```ebnf
% [19] GroupClause ::= 'GROUP' 'BY' GroupCondition+
% ```

'GroupClause'(State, Es) -->
  keyword(`group`),
  must_see(keyword(`by`)),
  +('GroupCondition'(State), Es).



%! 'GroupCondition'(+State, -E)// is det.
%
% ```ebnf
% [20] GroupCondition ::= BuiltInCall
%                       | FunctionCall
%                       | '(' Expression ( 'AS' Var )? ')'
%                       | Var
% ```
%
% Processes ‘'(' Expression ( 'AS' Var )? ')'’ into one of:
%   * ‘binding(Var,E)’
%   * ‘E’

'GroupCondition'(State, E) -->
  'BuiltInCall'(State, E), !.
'GroupCondition'(State, Function) -->
  'FunctionCall'(State, Function), !.
'GroupCondition'(State, E2) -->
  "(", !,
  skip_ws,
  must_see('Expression'(State, E1)),
  (   keyword(`as`)
  ->  must_see('Var'(State, Var)),
      {E2 = binding(Var,E1)}
  ;   {E2 = E1}
  ),
  must_see_code(0')).
'GroupCondition'(State, Var) -->
  'Var'(State, Var).



%! 'GroupGraphPattern'(+State, -Pattern:compound)// is det.
%
% ```ebnf
% [53] GroupGraphPattern ::= '{' ( SubSelect | GroupGraphPatternSub ) '}'
% ```
%
% If FS is not empty
%   Let G := output of preceding step
%   Let X := Conjunction of expressions in FS
%   G := Filter(X, G)
%   End

'GroupGraphPattern'(State, G3) -->
  "{", skip_ws,
  (   'SubSelect'(State, G3)
  ->  ""
  ;   % Split filter clauses from the rest of the group pattern.
      'GroupGraphPatternSub'(State, G1, FS1)
  ->  {
        translate_group(G1, G2),
        translate_filter(FS1, FS2),
        (FS2 == [] -> G3 = G2 ; G3 = 'Filter'(FS2,G2))
      }
  ),
  must_see_code(0'}).



%! translate_filter(+FS1, -FS2) is det.

translate_filter('logical-and'(X,'logical-and'(Y,Z)), FS) :- !,
  concat_into_list(X, Y, XY),
  translate_filter('logical-and'(XY,Z), FS).
translate_filter('logical-and'(X,Y), FS) :- !,
  concat_into_list(X, Y, XY1),
  dlist_to_list(XY1, XY2),
  translate_filter('logical-and'(XY2), FS).
translate_filter('logical-or'(X,'logical-or'(Y,Z)), FS) :- !,
  concat_into_list(X, Y, XY),
  translate_filter('logical-or'(XY,Z), FS).
translate_filter('logical-or'(X,Y), FS) :- !,
  concat_into_list(X, Y, XY1),
  dlist_to_list(XY1, XY2),
  translate_filter('logical-or'(XY2), FS).
translate_filter(X, X).

concat_into_list(L1-H1, X, L3) :- !,
  dappend(L1-H1, [X|H2]-H2, L3).
concat_into_list(X1, X2, L3) :-
  dappend([X1|H1]-H1, [X2|H2]-H2, L3).



%! translate_group(+G1, -G2) is det.
%
% Let FS := the empty set
% Let G := the empty pattern, a basic graph pattern which is the empty set.
% For each element E in the GroupGraphPattern
%   If E is of the form OPTIONAL{P}
%     Let A := Translate(P)
%     If A is of the form Filter(F, A2)
%       G := LeftJoin(G, A2, F)
%     Else
%       G := LeftJoin(G, A, true)
%       End
%     End
%   If E is of the form MINUS{P}
%     G := Minus(G, Translate(P))
%     End
%   If E is of the form BIND(expr AS var)
%     G := Extend(G, var, expr)
%     End
%   If E is any other form
%     Let A := Translate(E)
%     G := Join(G, A)
%     End
%  End

translate_group(G1, G4) :-
  dlist_to_list(G1, G2),
  translate_group(G2, 'Z', G3),
  simplify(G3, G4).

% §18.2.2.5 After translating property paths, any adjacent triple
% patterns are collected together to form a basic graph pattern
% BGP(triples).
translate_group(['BGP'(Triples1),'BGP'(Triples2)|T], G1, G2) :- !,
  append(Triples1, Triples2, Triples3),
  translate_group(['BGP'(Triples3)|T], G1, G2).
translate_group(['OPTIONAL'('Filter'(F,A))|T], G1, G2) :- !,
  translate_group(T, 'LeftJoin'(G1,A,F), G2).
translate_group(['OPTIONAL'(A)|T], G1, G2) :- !,
  translate_group(T, 'LeftJoin'(G1,A,true), G2).
translate_group(['MINUS'(A)|T], G1, G2) :- !,
  translate_group(T, 'Minus'(G1,A), G2).
translate_group(['BIND'(Var,E)|T], G1, G2) :- !,
  translate_group(T, 'Extend'(G1,Var,E), G2).
translate_group([A|T], G1, G2) :- !,
  translate_group(T, 'Join'(G1,A), G2).
translate_group([], G, G).



%! simplify(+G1, -G2) is det.
%
% Some groups of one graph pattern become join(Z, A), where Z is the
% empty basic graph pattern (which is the empty set).  These can be
% replaced by A.  The empty graph pattern Z is the identity for join:
%
%   * Replace join(Z, A) by A
%
%   * Replace join(A, Z) by A

simplify('Join'('Z',A1), A2) :- !,
  simplify(A1, A2).
simplify('Join'(A1,'Z'), A2) :- !,
  simplify(A1, A2).
simplify(L, L) :-
  is_list(L), !.
simplify(X-Y, A2) :- !,
  dlist_to_list(X-Y, A2).
simplify(A1, A2) :-
  compound(A1), !,
  A1 =.. [Pred|Args1],
  maplist(simplify, Args1, Args2),
  A2 =.. [Pred|Args2].
simplify(A, A).



%! 'GroupGraphPatternSub'(+State, -Triples, -FS)// is det.
%
% Collects filters independently of other group expressions
% (§18.2.2.2).  Notice that filters apply to the whole group graph
% pattern in which they appear, independent of their position within
% the group graph pattern.
%
% ```ebnf
% [54] GroupGraphPatternSub ::= TriplesBlock?
%                               ( GraphPatternNotTriples '.'?  TriplesBlock? )*
% ```

'GroupGraphPatternSub'(State, Triples, FS) -->
  ('TriplesBlock'(State, Triples1) -> "" ; {Triples1 = T-T}),
  'GroupGraphPatternSub_*'(State, Triples2, FS0),
  {
    dappend(Triples1, Triples2, Triples),
    ord_union(FS0, FS)
  }.

'GroupGraphPatternSub_*'(State, G, FS1) -->
  'GraphPatternNotTriples'(State, G1), !,
  {(  G1 = 'FILTER'(E)
  ->  G2 = T1-T1,
      FS1 = [E|FS2]
  ;   G2 = [G1|T1]-T1,
      FS1 = FS2
  )},
  ("." -> skip_ws ; ""),
  ('TriplesBlock'(State, Triples1) -> "" ; {Triples1 = T2-T2}),
  'GroupGraphPatternSub_*'(State, Triples2, FS2),
  {dappend([G2,Triples1,Triples2], G)}.
'GroupGraphPatternSub_*'(_, T-T, []) --> "".



%! 'GroupOrUnionGraphPattern'(+State, -Union:compound)// is det.
%
% ```
% Let A := undefined
% For each element G in the GroupOrUnionGraphPattern
%     If A is undefined
%         A := Translate(G)
%     Else
%         A := Union(A, Translate(G))
%     End
% The result is A
% ```
%
% ```ebnf
% [67] GroupOrUnionGraphPattern ::= GroupGraphPattern
%                                   ( 'UNION' GroupGraphPattern )*
% ```

'GroupOrUnionGraphPattern'(State, G2) -->
  'GroupGraphPattern'(State, G1),
  'GroupOrUnionGraphPattern_*'(State, G1, G2).

'GroupOrUnionGraphPattern_*'(State, G1, G3) -->
  keyword(`union`), !,
  'GroupGraphPattern'(State, G2),
  'GroupOrUnionGraphPattern_*'(State, 'Union'(G1,G2), G3).
'GroupOrUnionGraphPattern_*'(_, G, G) --> "".



%! 'HavingClause'(+State, +P1, -P2)// is det.
%
% ```ebnf
% [21] HavingClause ::= 'HAVING' HavingCondition+
% ```

'HavingClause'(State, P1, P2) -->
  keyword(`having`),
  must_see('HavingCondition'(State, E)),
  'HavingCondition*'(State, 'Filter'(E,P1), P2).

'HavingCondition*'(State, P1, P2) -->
  'HavingCondition'(State, E), !,
  'HavingCondition*'(State, 'Filter'(E,P1), P2).
'HavingCondition*'(_, P, P) --> "".



%! 'HavingCondition'(+State, -E)// is det.
%
% ```ebnf
% [22] HavingCondition ::= Constraint
% ```

'HavingCondition'(State, E) -->
  'Constraint'(State, E).



%! 'InlineData'(+State, -Data:compound)// is det.
%
% ```ebnf
% [61] InlineData ::= 'VALUES' DataBlock
% ```
%
% @tbd:
%
% The result is a multiset of solution mappings 'data'.
%
% data is formed by forming a solution mapping from the variable in
% the corresponding position in list of variables (or single
% variable), omitting a binding if the BindingValue is the word UNDEF.

'InlineData'(State, Data) -->
  keyword(`values`),
  'DataBlock'(State, Data).



%! 'InlineDataFull'(+State, -VarsIn:compound)// is det.
%
% ```ebnf
% [64] InlineDataFull ::= ( NIL | '(' Var* ')' )
%                         '{' ( '(' DataBlockValue* ')' | NIL )* '}'
% ```
%
% The number of variables in the ‘Vars’ list must be the same as the
% number of values in the ‘Vals’ list.

'InlineDataFull'(State, vars_in(Vars,Terms)) -->
  (   'NIL'(_)
  ->  skip_ws,
      {Vars = []}
  ;   "("
  ->  skip_ws,
      'Var*'(State, Vars),
      must_see_code(0'))
  ),
  must_see_code(0'{),
  'InlineDataFull_*'(State, Terms),
  must_see_code(0'}).

'InlineDataFull_*'(State, Terms) -->
  'InlineDataFull_*'(State, Terms, []).

'InlineDataFull_*'(State, Terms1, Terms3) -->
  'InlineDataFull_'(State, Terms1, Terms2), !,
  'InlineDataFull_*'(State, Terms2, Terms3).
'InlineDataFull_*'(_, Terms, Terms) --> "".

'InlineDataFull_'(State, Terms1, Terms2) -->
  "(", !,
  skip_ws,
  'DataBlockValue*'(State, Terms1, Terms2),
  must_see_code(0')).
'InlineDataFull_'(_, Terms, Terms) -->
  'NIL'(_),
  skip_ws.



%! 'InlineDataOneVar'(+State, -VarIn:compound)// is det.
%
% ```ebnf
% [63] InlineDataOneVar ::= Var '{' DataBlockValue* '}'
% ```

'InlineDataOneVar'(State, var_in(Var,Terms)) -->
  'Var'(State, Var),
  must_see_code(0'{),
  'DataBlockValue*'(State, Terms),
  must_see_code(0'}).



%! iri(+State, -Iri)// is det.
%
% ```ebnf
% [136] iri ::= IRIREF | PrefixedName
% ```

iri(State, Iri) -->
  'IRIREF'(State, Iri), !,
  skip_ws.
iri(State, Iri) -->
  'PrefixedName'(State, Iri).



%! iriOrFunction(+State, -Function)// is det.
%
% ```ebnf
% [128] iriOrFunction ::= iri ArgList?
% ```

iriOrFunction(State, Function) -->
  iri(State, Functor),
  ('ArgList'(State, Args) -> "" ; {Args = []}),
  {Function =.. [Functor|Args]}.



%! 'LimitClause'(-Len:nonneg)// is det.
%
% ```ebnf
% [26] LimitClause ::= 'LIMIT' INTEGER
% ```

'LimitClause'(Len) -->
  keyword(`limit`),
  'INTEGER'(Len),
  skip_ws.



%! 'LimitOffsetClauses'(+P1, -P2)// is det.
%
% ```ebnf
% [25] LimitOffsetClauses ::= LimitClause OffsetClause?
%                           | OffsetClause LimitClause?
% ```
%
% ```algebra
% M := Slice(M, start, length),
%      where ‘start’ defaults to 0
%      and ‘length’ defaults to ‘size(M)-start’
% ```

'LimitOffsetClauses'(P, 'Slice'(P,Start,Len)) -->
  'LimitClause'(Len), !,
  ('OffsetClause'(Start) -> "" ; {Start = 0}).
'LimitOffsetClauses'(P, 'Slice'(P,Start,Len)) -->
  'OffsetClause'(Start), !,
  ('LimitClause'(Len) -> "" ; {Len = ∞}).
'LimitOffsetClauses'(P, 'Slice'(P,0,∞)) --> "".



%! 'MinusGraphPattern'(+State, -Minus:compound)// is det.
%
% ```ebnf
% [66] MinusGraphPattern ::= 'MINUS' GroupGraphPattern
% ```

'MinusGraphPattern'(State, 'MINUS'(G)) -->
  keyword(`minus`),
  'GroupGraphPattern'(State, G).



%! 'MultiplicativeExpression'(+State, -E)// is det.
%
% ```ebnf
% [117] MultiplicativeExpression ::= UnaryExpression
%                                    (  '*' UnaryExpression
%                                    |  '/' UnaryExpression
%                                    )*
% ```

'MultiplicativeExpression'(State, E2) -->
  'UnaryExpression'(State, E1),
  'MultiplicativeExpression_*'(State, E1, E2).

'MultiplicativeExpression_*'(State, E1, E4) -->
  (   "*"
  ->  {Functor = 'op:numeric-multiply'}
  ;   "/"
  ->  {Functor = 'op:numeric-divide'}
  ), !,
  skip_ws,
  must_see('UnaryExpression'(State, E2)),
  {E3 =.. [Functor,E1,E2]},
  'MultiplicativeExpression_*'(State, E3, E4).
'MultiplicativeExpression_*'(_, E, E) --> "".



%! 'NamedGraphClause'(+State, -NamedGraph)// is det.
%
% ```ebnf
% [15] NamedGraphClause ::= 'NAMED' SourceSelector
% ```

'NamedGraphClause'(State, NamedGraph) -->
  keyword(`named`),
  'SourceSelector'(State, NamedGraph).



%! 'NotExistsFunc'(+State, +G, -NotExists:compound)// is det.
%
% Replaces ‘NOT EXISTS{P}’ with ‘fn:not(exists(translate(P)))’
% (§18.2.2.2).
%
% ```ebnf
% [126] NotExistsFunc ::= 'NOT' 'EXISTS' GroupGraphPattern
% ```

'NotExistsFunc'(State, Function) -->
  keyword(`not`),
  keyword(`exists`),
  % @note G has already been translated.
  'GroupGraphPattern'(State, G),
  {
    rdf_equal(fn:not, Functor),
    Function =.. [Functor,exists(G)]
  }.



%! 'NumericExpression'(+State, -E)// is det.
%
% ```ebnf
% [115] NumericExpression ::= AdditiveExpression
% ```

'NumericExpression'(State, E) -->
  'AdditiveExpression'(State, E).



%! 'NumericLiteral'(-Lit)// is det.
%
% Match a literal value and return it as a term Val^^D.
%
% Where datatype D is one of xsd:double, xsd:decimal or xsd:integer
% and Val is the interpreted value.
%
% ```ebnf
% [130] NumericLiteral ::= NumericLiteralUnsigned
%                        | NumericLiteralPositive
%                        | NumericLiteralNegative
% ```

'NumericLiteral'(Lit) --> 'NumericLiteralUnsigned'(Lit), !.
'NumericLiteral'(Lit) --> 'NumericLiteralPositive'(Lit), !.
'NumericLiteral'(Lit) --> 'NumericLiteralNegative'(Lit).



%! 'NumericLiteralNegative'(-Lit)// is det.
%
% ```ebnf
% [133] NumericLiteralNegative ::= INTEGER_NEGATIVE
%                                | DECIMAL_NEGATIVE
%                                | DOUBLE_NEGATIVE
% ```

'NumericLiteralNegative'(Lit) -->
  'INTEGER_NEGATIVE'(Val), !,
  {rdf_global_object(Val^^xsd:integer, Lit)},
  skip_ws.
'NumericLiteralNegative'(Lit) -->
  'DECIMAL_NEGATIVE'(Val), !,
  {rdf_global_object(Val^^xsd:decimal, Lit)},
  skip_ws.
'NumericLiteralNegative'(Lit) -->
  'DOUBLE_NEGATIVE'(Val),
  {rdf_global_object(Val^^xsd:double, Lit)},
  skip_ws.



%! 'NumericLiteralPositive'(-Lit)// is det.
%
% ```ebnf
% [132] NumericLiteralPositive ::= INTEGER_POSITIVE
%                                | DECIMAL_POSITIVE
%                                | DOUBLE_POSITIVE
% ```

'NumericLiteralPositive'(Lit) -->
  'INTEGER_POSITIVE'(Val), !,
  {rdf_global_object(Val^^xsd:integer, Lit)},
  skip_ws.
'NumericLiteralPositive'(Lit) -->
  'DECIMAL_POSITIVE'(Val), !,
  {rdf_global_object(Val^^xsd:decimal, Lit)},
  skip_ws.
'NumericLiteralPositive'(Lit) -->
  'DOUBLE_POSITIVE'(Val),
  {rdf_global_object(Val^^xsd:double, Lit)},
  skip_ws.



%! 'NumericLiteralUnsigned'(-Lit)// is det.
%
% ```ebnf
% [131] NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
% ```

'NumericLiteralUnsigned'(Lit) -->
  'INTEGER'(Val), !,
  {rdf_global_object(Val^^xsd:integer, Lit)},
  skip_ws.
'NumericLiteralUnsigned'(Lit) -->
  'DECIMAL'(Val), !,
  {rdf_global_object(Val^^xsd:decimal, Lit)},
  skip_ws.
'NumericLiteralUnsigned'(Lit) -->
  'DOUBLE'(Val),
  {rdf_global_object(Val^^xsd:double, Lit)},
  skip_ws.



%! 'Object'(+State, -O, -Triples:dlist(compound))// is det.
%
% Triples are non-empty if blank node list notation or collection
% notation is used.
%
% ```ebnf
% [80] Object ::= GraphNode
% ```

'Object'(State, O, Triples) -->
  'GraphNode'(State, O, Triples).



%! 'ObjectList'(+State, +S, +Path, -Triples:dlist(compound))// is det.
%
% Parses a list of object terms, implementing object list notation
% (‘,’).
%
% ```ebnf
% [79] ObjectList ::= Object ( ',' Object )*
% ```

'ObjectList'(State, S, Path, Triples5) -->
  'Object'(State, O, Triples1),
  {
    path_triples(State, S, Path, O, Triples2),
    dappend(Triples1, Triples2, Triples3)
  },
  (   ","
  ->  skip_ws,
      must_see('ObjectList'(State, S, Path, Triples4)),
      {dappend(Triples3, Triples4, Triples5)}
  ;   {Triples5 = Triples3}
  ).



%! 'ObjectListPath'(+State, +S, +Path, -Triples)// is det.
%
% ```ebnf
% [86] ObjectListPath ::= ObjectPath ( ',' ObjectPath )*
% ```

'ObjectListPath'(State, S, Path, Triples) -->
  'ObjectPath'(State, O, Triples1),
  {
    path_triples(State, S, Path, O, Triples2),
    dappend(Triples1, Triples2, Triples3)
  },
  (   ","
  ->  skip_ws,
      must_see('ObjectListPath'(State, S, Path, Triples4)),
      {dappend(Triples3, Triples4, Triples)}
  ;   {Triples = Triples3}
  ).



%! 'ObjectPath'(+State, -O, -Triples)// is det.
%
% ```ebnf
% [87] ObjectPath ::= GraphNodePath
% ```

'ObjectPath'(State, O, Triples) -->
  'GraphNodePath'(State, O, Triples).



%! 'OffsetClause'(-Offset:nonneg)// is det.
%
% ```ebnf
% [27] OffsetClause ::= 'OFFSET' INTEGER
% ```

'OffsetClause'(Offset) -->
  keyword(`offset`),
  'INTEGER'(Offset),
  skip_ws.



%! 'OptionalGraphPattern'(+State, -Optional:compound)// is det.
%
% ```
% If E is of the form OPTIONAL{P}
%   Let A := Translate(P)
%   If A is of the form Filter(F, A2)
%     G := LeftJoin(G, A2, F)
%   Else
%     G := LeftJoin(G, A, true)
%     End
%   End
% ```
%
% ```ebnf
% [57] OptionalGraphPattern ::= 'OPTIONAL' GroupGraphPattern
% ```

'OptionalGraphPattern'(State, 'OPTIONAL'(G)) -->
  keyword(`optional`),
  'GroupGraphPattern'(State, G).



%! 'OrderClause'(+State, +P1, -P2)// is det.
%
% ```ebnf
% [23] OrderClause ::= 'ORDER' 'BY' OrderCondition+
% ```
%
% ```algebra
% M := OrderBy(M, list of order comparators)
% ```

'OrderClause'(State, M, 'OrderBy'(M,[H|T])) -->
  keyword(`order`),
  must_see(keyword(`by`)),
  must_see('OrderCondition'(State, H)),
  *('OrderCondition'(State), T).



%! 'OrderCondition'(+State:dict, -Comparator:compound)// is det.
%
% Order is either of the following:
%
%   * ‘ascending(E)’
%
%   * ‘descending(E)’
%
% ```ebnf
% [24] OrderCondition ::= ( ( 'ASC' | 'DESC' BrackettedExpression )
%                       | ( Constraint | Var )
% ```

'OrderCondition'(State, ascending(E)) -->
  keyword(`asc`), !,
  'BrackettedExpression'(State, E).
'OrderCondition'(State, descending(E)) -->
  keyword(`desc`), !,
  'BrackettedExpression'(State, E).
'OrderCondition'(State, ascending(Constraint)) -->
  'Constraint'(State, Constraint), !.
'OrderCondition'(State, ascending(Var)) -->
  'Var'(State, Var).



%! 'Path'(+State, -Path)// is det.
%
% ```ebnf
% [88] Path ::= PathAlternative
% ```

'Path'(State, Path) -->
  'PathAlternative'(State, Path).



%! 'PathAlternative'(+State, -PathExpression)// is det.
%
% ```ebnf
% [89] PathAlternative ::= PathSequence ( '|' PathSequence )*
% ```

'PathAlternative'(State, PE) -->
  'PathSequence'(State, PE1),
  (   "|"
  ->  skip_ws,
      'PathAlternative'(State, PE2),
      {PE = alt(PE1,PE2)}
  ;   {PE = PE1}
  ).



%! 'PathElt'(+State, -Path)// is det.
%
% ```ebnf
% [91] PathElt ::= PathPrimary PathMod?
% ```

'PathElt'(State, Path2) -->
  'PathPrimary'(State, Path1),
  'PathMod'(Path1, Path2).



%! 'PathEltOrInverse'(+State, -PathExpression)// .
%
% ```ebnf
% [92] PathEltOrInverse ::= PathElt | '^' PathElt
% ```

'PathEltOrInverse'(State, PE2) -->
  (   "^"
  ->  skip_ws,
      'PathElt'(State, PE1),
      {(PE1 = link(Iri) -> PE2 = inv(Iri) ; PE2 = inv(PE1))}
  ;   'PathElt'(State, PE2)
  ).



%! 'PathMod'(+Path1, -Path2)// is det.
%
% ```ebnf
% [93] PathMod ::= '?' | '*' | '+'
% ```

% AST: p?, algebra: ZeroOrOnePath(p)
'PathMod'(Path, 'ZeroOrOnePath'(Path)) -->
  "?",
  % Distinguishes between the following two cases:
  %   1. ‘:p? :o’   (path modifier)
  %   2. ‘:p  ?o’   (variable)
  \+ 'VARNAME_1'(_), !,
  skip_ws.
% AST: p*, algebra: ZeroOrMorePath(p)
'PathMod'(Path, 'ZeroOrMorePath'(Path)) -->
  "*", !,
  skip_ws.
% AST: p+, algebra: OneOrMorePath(p)
'PathMod'(Path, 'OneOrMorePath'(Path)) -->
  "+", !,
  skip_ws.
'PathMod'(Path, Path) --> "".



%! 'PathNegatedPropertySet'(+State, -Iris, -InvIris)// is det.
%
% ```ebnf
% [95] PathNegatedPropertySet ::= PathOneInPropertySet
%                               | '('
%                                 (   PathOneInPropertySet
%                                     ( '|' PathOneInPropertySet )*
%                                 )?
%                                 ')'
% ```
%
% @bug This allows empty bracket pairs in the grammar.
%
% Parses a Negated Property Set (NPS).  The PredicatePaths and
% InversePaths are grouped separately for easy postprocessing.

'PathNegatedPropertySet'(State, Iris, InvIris) -->
  'PathOneInPropertySet'(State, Iris, [], InvIris, []), !.
'PathNegatedPropertySet'(State, Iris, InvIris) -->
  "(", !, skip_ws,
  'PathNegatedPropertySet_'(State, Iris, InvIris),
  must_see_code(0')).

'PathNegatedPropertySet_'(State, Iris, InvIris) -->
  'PathOneInPropertySet'(State, Iris, IrisT, InvIris, InvIrisT), !,
  (   "|"
  ->  skip_ws,
      'PathNegatedPropertySet_'(State, IrisT, InvIrisT)
  ;   {IrisT = [], InvIrisT = []}
  ).
'PathNegatedPropertySet_'(_, [], []) --> "".



%! 'PathOneInPropertySet'(+State, -Iris, ?IrisT, -InvIris, ?InvIrisT)// is det.
%
% ```ebnf
% [96] PathOneInPropertySet ::= iri | 'a' | '^' ( iri | 'a' )
% ```

'PathOneInPropertySet'(State, Iris, IrisT, InvIris, InvIrisT) -->
  (   "^"
  ->  skip_ws,
      % Add an InversePath.
      {Iris = IrisT, InvIris = [Iri|InvIrisT]}
  ;   % Add a PredicatePath.
      {Iris = [Iri|IrisT], InvIris = InvIrisT}
  ),
  (iri(State, Iri) -> "" ; "a" -> {rdf_equal(rdf:type, Iri)}).



%! 'PathPrimary'(+State, -Path:compound)// is det.
%
% ```ebnf
% [94] PathPrimary ::= iri
%                    | 'a'
%                    | '!' PathNegatedPropertySet
%                    | '(' Path ')'
% ```

% AST: i
%
% Algebra: PredicatePath
'PathPrimary'(State, link(Iri)) -->
  iri(State, Iri), !.
% AST: "a"
%
% Algebra: PredicatePath
'PathPrimary'(_, link(Iri)) -->
  "a", !, skip_ws,
  {rdf_equal(rdf:type, Iri)}.
% AST: !(i_1|...|i_n),
%      !(^i_1|...|^i_n),
%      !(i_1|...|i_n|^i_{i+1}|...|i_n)
%
% Algebra: NPS(I_1),
%          inv(NPS(I_1)),
%          alt(NPS(I_1),inv(NPS(I_2)))
'PathPrimary'(State, Path) -->
  "!", !, skip_ws,
  'PathNegatedPropertySet'(State, Iris, Invs),
  {(  Invs == []
  ->  Path = 'NPS'(Iris)
  ;   Iris == []
  ->  Path = inv('NPS'(Invs))
  ;   Path = alt('NPS'(Iris),inv('NPS'(Invs)))
  )}.
% Bracketed path expression, can influence precedence.
'PathPrimary'(State, Path) -->
  "(", !, skip_ws,
  'Path'(State, Path),
  must_see_code(0')).



%! 'PathSequence'(+State, -Path)// is det.
%
% ```ebnf
% [90] PathSequence ::= PathEltOrInverse ( '/' PathEltOrInverse )*
% ```
%
% @see /SPARQL 1.1 Query Language/, §18.2.2.3

'PathSequence'(State, Path3) -->
  'PathEltOrInverse'(State, Path1),
  (   "/"
  ->  skip_ws,
      % AST: a/b
      % algebra: seq(Path1,Path2)
      {Path3 = seq(Path1,Path2)},
      must_see('PathSequence'(State, Path2))
  ;   {Path3 = Path1}
  ).



%! 'PrefixDecl'(+State)// is det.
%
% ```bnf
% [6] PrefixDecl ::= 'PREFIX' PNAME_NS IRIREF
% ```

'PrefixDecl'(State) -->
  keyword(`prefix`),
  must_see('PNAME_NS'(Prefix)),
  skip_ws,
  must_see('IRIREF'(State, Iri)),
  skip_ws,
  {
    _{prefixes: Prefixes1} :< State,
    (selectchk(Prefix-_, Prefixes1, Prefixes2) -> true ; Prefixes2 = Prefixes1),
    nb_set_dict(prefixes, State, [Prefix-Iri|Prefixes2])
  }.



%! 'PrefixedName'(+State, -Iri)// is det.
%
% ```ebnf
% [137] PrefixedName ::= PNAME_LN | PNAME_NS
% ```

'PrefixedName'(State, Iri) -->
  'PNAME_LN'(State, Iri), !,
  skip_ws.
'PrefixedName'(State, Iri) -->
  'PNAME_NS'(Prefix),
  skip_ws,
  {prefix_iri(State, Prefix, Iri)}.



%! 'PrimaryExpression'(+State, -E)// is det.
%
% ```ebnf
% [119] PrimaryExpression ::= BrackettedExpression
%                           | BuiltInCall
%                           | iriOrFunction
%                           | RDFLiteral
%                           | NumericLiteral
%                           | BooleanLiteral
%                           | Var
% ```

'PrimaryExpression'(State, E) -->
  'BrackettedExpression'(State, E), !.
'PrimaryExpression'(State, E) -->
  'BuiltInCall'(State, E), !.
'PrimaryExpression'(State, E) -->
  iriOrFunction(State, E), !.
'PrimaryExpression'(State, E) -->
  'RDFLiteral'(State, E), !.
'PrimaryExpression'(_, E) -->
  'NumericLiteral'(E), !.
'PrimaryExpression'(_, E) -->
  'BooleanLiteral'(E), !.
'PrimaryExpression'(State, Var) -->
  'Var'(State, Var).



%! 'Prologue'(+State)// is det.
%
% The Prologue consists of zero or more BASE and PREFIX declarations.
% The result is the last BASE declaration and each PREFIX is resolved
% against the last preceeding BASE declaration.
%
% ```bnf
% [4] Prologue ::= ( BaseDecl | PrefixDecl )*
% ```

'Prologue'(State) -->
  'BaseDecl'(State), !,
  'Prologue'(State).
'Prologue'(State) -->
  'PrefixDecl'(State), !,
  'Prologue'(State).
'Prologue'(_) --> "".



%! 'PropertyList'(+State, +S, -Triples:dlist(compound))// is det.
%
% Parses a sequence of pairs $(P,Os)$, implementing predicate-object
% list notation (‘;’).
%
% ```ebnf
% [76] PropertyList ::= PropertyListNotEmpty?
% ```

'PropertyList'(State, S, Triples) -->
  'PropertyListNotEmpty'(State, S, Triples), !.
'PropertyList'(_, _, T-T) --> "".



%! 'PropertyListNotEmpty'(+State, +S, -Triples:dlist(compound))// is det.
%
% Parses a non-empty sequence of pairs $(P,Os)$, implementing
% predicate-object list notation (‘;’).
%
% ```ebnf
% [77] PropertyListNotEmpty ::= Verb ObjectList
%                               ( ';' ( Verb ObjectList )? )*
% ```

'PropertyListNotEmpty'(State, S, Triples) -->
  'Verb'(State, P1),
  must_see('ObjectList'(State, S, P1, Triples1)),
  (   ";"
  ->  skip_ws,
      must_see('Verb'(State, P2)),
      must_see('ObjectList'(State, S, P2, Triples2)),
      {dappend(Triples1, Triples2, Triples)}
  ;   {Triples = Triples1}
  ).



%! 'PropertyListPath'(+State, +S, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [82] PropertyListPath ::= PropertyListPathNotEmpty?
% ```

'PropertyListPath'(State, S, Triples) -->
  'PropertyListPathNotEmpty'(State, S, Triples), !.
'PropertyListPath'(_, _, T-T) --> "".



%! 'PropertyListPathNotEmpty'(+State, +S, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [83] PropertyListPathNotEmpty ::= ( VerbPath | VerbSimple )
%                                   ObjectListPath
%                                   (   ';'
%                                       (   ( VerbPath | VerbSimple )
%                                           ObjectList
%                                       )?
%                                   )*
% ```

'PropertyListPathNotEmpty'(State, S, Triples) -->
  ('VerbPath'(State, Path) -> "" ; 'VerbSimple'(State, Path)),
  must_see('ObjectListPath'(State, S, Path, Triples1)),
  (   ";"
  ->  skip_ws,
      'VerbObjectLists'(State, S, Triples2),
      {dappend(Triples1, Triples2, Triples)}
  ;   {Triples = Triples1}
  ).



%! 'Query'(+State, -Form, -Algebra)// is det.
%
% ```bnf
% [2] Query ::= Prologue
%               (   SelectQuery
%               |   ConstructQuery
%               |   DescribeQuery
%               |   AskQuery
%               )
%               ValuesClause
% ```

'Query'(State, Form, Algebra) -->
  skip_ws,
  'Prologue'(State),
  (   'SelectQuery'(State, P1, P2, Algebra)
  ->  {Form = select}
  ;   'ConstructQuery'(State, P1, P2, Algebra)
  ->  {Form = construct}
  ;   'DescribeQuery'(State, P1, P2, Algebra)
  ->  {Form = describe}
  ;   'AskQuery'(State, P1, P2, Algebra)
  ->  {Form = ask}
  ),
  'ValuesClause'(State, P1, P2).



%! 'QueryUnit'(+State, -Form, -Algebra)// is det.
%
% ```bnf
% [1] QueryUnit ::= Query
% ```

'QueryUnit'(State, Form, Algebra) -->
  'Query'(State, Form, Algebra).



%! 'RDFLiteral'(+State, -Lit)// is det.
%
% ```ebnf
% [129] RDFLiteral ::= String ( LANGTAG | ( '^^' iri ) )?
% ```
%
% Examples:
%
%   * "chat"
%
%   * 'chat'@fr with language tag "fr"
%
%   * "xyz"^^<http://example.org/ns/userDatatype>
%
%   * "abc"^^appNS:appDataType
%
%   * '''The librarian said, "Perhaps you would enjoy 'War and
%        Peace'."'''
%
%   * 1, which is the same as "1"^^xsd:integer
%
%   * 1.3, which is the same as "1.3"^^xsd:decimal
%
%   * 1.300, which is the same as "1.300"^^xsd:decimal
%
%   * 1.0e6, which is the same as "1.0e6"^^xsd:double
%
%   * true, which is the same as "true"^^xsd:boolean
%
%   * false, which is the same as "false"^^xsd:boolean

'RDFLiteral'(State, Lit) -->
  'String'(Lex),
  (   'LANGTAG'(LTag)
  ->  skip_ws,
      {rdf_literal(Lit, rdf:langString, Lex, LTag)}
  ;   "^^"
  ->  skip_ws,
      iri(State, D),
      {rdf_literal(Lit, D, Lex, _)}
  ;   {Lit = Lex}
  ).



%! 'RegexExpression'(+State, -Regex:compound)// is det.
%
% ```ebnf
% [122] RegexExpression ::= 'REGEX' '('
%                             Expression ','
%                             Expression
%                             ( ',' Expression )?
%                           ')'
% ```

'RegexExpression'(State, Function) -->
  keyword(`regex`),
  must_see_code(0'(),
  must_see('Expression'(State, Target)),
  must_see_code(0',),
  must_see('Expression'(State, Pattern)),
  (   ","
  ->  skip_ws,
      must_see('Expression'(State, Flags))
  ;   {rdf_literal(Flags, xsd:string, "", _)}
  ),
  must_see_code(0')),
  {
    rdf_equal(fn:matches, Functor),
    Function =.. [Functor,Target,Pattern,Flags]
  }.



%! 'RelationalExpression'(+State, -E)// is det.
%
% ```ebnf
% [114] RelationalExpression ::= NumericExpression
%                                ( '='  NumericExpression
%                                | '!=' NumericExpression
%                                | '<'  NumericExpression
%                                | '>'  NumericExpression
%                                | '<=' NumericExpression
%                                | '>=' NumericExpression
%                                | 'IN'       ExpressionList
%                                | 'NOT' 'IN' ExpressionList
%                                )?
% ```

'RelationalExpression'(State, E3) -->
  'NumericExpression'(State, E1),
  (   relational_op(Op)
  ->  skip_ws,
      'NumericExpression'(State, E2),
      {E3 =.. [Op,E1,E2]}
  ;   keyword(`in`)
  ->  'ExpressionList'(State, Es),
      {E3 = in(E1,Es)}
  ;   keyword(`not`),
      keyword(`in`)
  ->  'ExpressionList'(State, Es),
      {E3 = not_in(E1,Es)}
  ;   {E3 = E1}
  ).

relational_op(=) --> "=".
relational_op('!=') --> "!=".
relational_op(<) --> "<".
relational_op(>) --> ">".
relational_op(=<) --> "<=".
relational_op(>=) --> ">=".



%! 'SelectClause'(+State, -Proj, -Mod)// is det.
%
% Parse the projection of a SPARQL SELECT Query.
%
% ```ebnf
% [9] SelectClause ::= 'SELECT'
%                      (  'DISTINCT' | 'REDUCED' )?
%                      (  (   Var
%                         |   '(' Expression 'AS' Var ')'
%                         )+
%                      |  '*'
%                      )
% ```
%
% ‘Proj’ is one of the following:
%
%   * ‘*’
%
%   * A list of variables and bindings ‘binding(Var,E)’.

'SelectClause'(State, Proj, Mod) -->
  keyword(`select`), !,
  (   keyword(`distinct`)
  ->  {Mod = distinct}
  ;   keyword(`reduced`)
  ->  {Mod = reduced}
  ;   {Mod = none}
  ),
  (   "*"
  ->  skip_ws,
      {Proj = *}
  ;   must_see(projection(State, H)),
      *(projection(State), T)
  ->  {Proj = [H|T]}
  ).

projection(State, H) -->
  'Var'(State, H), !,
  skip_ws.
projection(State, binding(Var,E)) -->
  "(", !,
  skip_ws,
  must_see('Expression'(State, E)),
  keyword(`as`),
  must_see('Var'(State, Var)),
  must_see_code(0')).



%! 'SelectQuery'(+State, -P1, -P2, -P3)// is det.
%
% Parse into a query term ‘Q=select(Proj,Datasets,Query,Sols)’.
%
% ```bnf
% [7] SelectQuery ::= SelectClause
%                     DatasetClause*
%                     WhereClause
%                     SolutionModifier
% ```

'SelectQuery'(State, P2, P3, P4) -->
  'SelectClause'(State, Proj, Mod),
  'DatasetClause*'(State),
  'WhereClause'(State, P1),
  'SolutionModifier'(State, P1, select(Proj,Mod), P2, P3, P4).



%! 'ServiceGraphPattern'(+State, -Service:compound)// is det.
%
% Process a federated query.  We need to find three[?] things:
%
%   * If there is a =SELECT=, the variables exposed through the
%     projection, _otherwise_, the default * projection variables.
%
%   * What prefixes are required to execute the query?
%
% ```ebnf
% [59] ServiceGraphPattern ::= 'SERVICE'
%                              'SILENT'?
%                              VarOrIri
%                              GroupGraphPattern
% ```

'ServiceGraphPattern'(State, service(Silent,Term,G)) -->
  keyword(`service`), !,
  'SILENT?'(Silent),
  'VarOrIri'(State, Term),
  'GroupGraphPattern'(State, G).



%! 'SolutionModifier'(+State, +P1, +TypeSpecific, -P3, -P4, -P11)// is det.
%
% Processes solution modifiers into a term
% ‘solutions(Group,Having,Order,Limit,Offset)’.
%
% ```ebnf
% [18] SolutionModifier ::= GroupClause?
%                           HavingClause?
%                           OrderClause?
%                           LimitOffsetClauses?
% ```

'SolutionModifier'(State, P1, TypeSpecific, P3, P4, P10) -->
  % Extract the inner SELECT expression.
  {(  TypeSpecific = describe(Proj)
  ->  true
  ;   TypeSpecific = select(Proj,_)
  ->  true
  ;   true
  )},

  % §18.2.4.1 Grouping and Aggregation: explicit grouping (AST: GROUP
  % BY)
  %
  (   'GroupClause'(State, AggrEs)
  ->  {(  Proj == *
      ->  syntax_error("SELECT * and GROUP BY cannot appear together.")
      ;   P2 = 'Group'(AggrEs, P1)
      )}
  ;   {P2 = P1}
  ),

  % @tbd §18.2.4.1 Grouping and Aggregation: implicit grouping by an
  % aggregate in HAVING.
  %
  % §18.2.4.2 HAVING
  ('HavingClause'(State, P2, P3) -> "" ; {P3 = P2}),

  % §18.2.4.3 VALUES
  %
  % These are brought in later through ‘P3’.

  % @tbd §18.2.4.1 Grouping and Aggregation: implicit grouping by an
  % aggregate in SELECT.
  %
  % §18.2.4.4
  {(  TypeSpecific = describe(PV)
  ->  P4 = P3,
      P5 = P3
  ;   TypeSpecific = select(_,_)
  ->  select_expression(Proj, P3, P4, P5, PV)
  ;   P5 = P3
  )},

  % §18.2.5 Convert Solution Modifiers
  %
  % ```algebra
  % M := ToList(Pattern)
  % ```
  {P6 = 'ToList'(P5)},

  % @tbd §18.2.4.1 Grouping and Aggregation: implicit grouping by an
  % aggregate in ORDER BY.
  %
  % §18.2.5.1
  ('OrderClause'(State, P6, P7) -> "" ; {P7 = P6}),

  % @tbd Here we know whether there is an explicit grouping or
  % implicit groupings.  If there are, we can continue with handling
  % aggregates.

  % §18.2.5.2 Projection
  %
  % The set of projection variables, PV, was calculated in the
  % processing of SELECT expressions.
  %
  % ```algebra
  % M := Project(M, PV)
  % ```
  {(var(PV) -> P8 = P7 ; P8 = 'Project'(P7,PV))},

  % §18.2.5.3 DISTINCT
  %
  % ```algebra
  % M := Distinct(M)
  % ```
  %
  % §18.2.5.4 REDUCED
  %
  % ```algebra
  % M := Reduced(M)
  % ```
  {(  TypeSpecific = select(_,Mode)
  ->  (   Mode == distinct
      ->  P9 = 'Distinct'(P8)
      ;   Mode == reduced
      ->  P9 = 'Reduced'(P8)
      ;   Mode == none
      ->  P9 = P8
      )
  ;   P9 = P8
  )},

  % §18.2.5.5 OFFSET and LIMIT
  'LimitOffsetClauses'(P9, P10).


%! implicit_grouping_comparators(+Comparators, ?AggrEs) is det.
%
% Else If Q contains an aggregate in SELECT, HAVING, ORDER BY
%   Let G := Group((1), P)
%
% @tbd

implicit_grouping_comparators(Comparators, '(1)') :-
  member(Comparator, Comparators),
  (   Comparator = ascending(binding(_,_))
  ;   Comparator = descending(binding(_,_))
  ), !.
implicit_grouping_comparators(_, _).


%! implicit_grouping_expressions(+Es, ?AggrEs) is det.
%
% Else If Q contains an aggregate in SELECT, HAVING, ORDER BY
%   Let G := Group((1), P)
%
% @tbd

implicit_grouping_expressions(Es, '(1)') :-
  memberchk(binding(_,_), Es), !.
implicit_grouping_expressions(_, _).


%! select_expression(+Proj, +P3, +P4, -P5, -PV2) is det.
%
% ```algebra
% SELECT selItem ... { pattern }
% SELECT * { pattern }
% Let X := algebra from earlier steps
% Let VS := list of all variables visible in the pattern,
%           so restricted by sub-SELECT projected variables and GROUP BY
%           variables.
%           Not visible: only in filter, exists/not exists, masked by a
%                        subselect, non-projected GROUP variables, only in the
%                        right hand side of MINUS
%
% Let PV := {}, a set of variable names
% Note, E is a list of pairs of the form (variable, expression), defined in
%       section 18.2.4
%
% If "SELECT *"
%     PV := VS
%
% If  "SELECT selItem ...:"
%     For each selItem:
%         If selItem is a variable
%             PV := PV ∪ { variable }
%         End
%         If selItem is (expr AS variable)
%             variable must not appear in VS nor in PV; if it does then
%               generate a syntax error and stop
%             PV := PV ∪ { variable }
%             E := E append (variable, expr)
%         End
%     End
%
% For each pair (var, expr) in E
%     X := Extend(X, var, expr)
%     End
%
% Result is X
% The set PV is used later for projection.
% ```

select_expression(Proj, P3, P4, P5, PV) :-
  algebra_to_vars(P3, VS),
  select_expression(Proj, VS, P4, P5, [], PV).

select_expression(*, VS, P, P, _, PV) :- !,
  PV = VS.
select_expression([], _, P, P, PV1, PV2) :- !,
  % @tbd optimize
  reverse(PV1, PV2).
select_expression([var(VarName)|T], VS, P1, P2, PV1, PV3) :- !,
  (memberchk(var(VarName), PV1) -> PV2 = PV1 ; PV2 = [var(VarName)|PV1]),
  select_expression(T, VS, P1, P2, PV2, PV3).
select_expression([binding(Var,E)|T], VS, P1, P2, PV1, PV2) :- !,
  (   (memberchk(Var, VS) ; memberchk(Var, PV1))
  ->  syntax_error(cannot_use_var(Var))
  ;   true
  ),
  select_expression(T, VS, 'Extend'(P1,Var,E), P2, [Var|PV1], PV2).



%! 'SourceSelector'(+State, -G)// is det.
%
% ```bnf
% [16] SourceSelector ::= iri
% ```

'SourceSelector'(State, G) -->
  iri(State, G).



%! 'String'(-Lex)// is det.
%
% ```ebnf
% [135] String ::= STRING_LITERAL1
%                | STRING_LITERAL2
%                | STRING_LITERAL_LONG1
%                | STRING_LITERAL_LONG2
% ```
%
% # String-specific Unicode escape sequences
%
% | *Escape* | *Unicode code point*                         |
% |----------|----------------------------------------------|
% | '\t'     | U+0009 (tab)                                 |
% | '\n'     | U+000A (line feed)                           |
% | '\r'     | U+000D (carriage return)                     |
% | '\b'     | U+0008 (backspace)                           |
% | '\f'     | U+000C (form feed)                           |
% | '\"'     | U+0022 (quotation mark, double quote mark)   |
% | "\'"     | U+0027 (apostrophe-quote, single quote mark) |
% | '\\'     | U+005C (backslash)                           |
%
% # Examples
%
% ```txt
% "abc\n"
% "xy\rz"
% 'xy\tz'
% ```

'String'(Lex) --> 'STRING_LITERAL_LONG1'(Lex), !, skip_ws.
'String'(Lex) --> 'STRING_LITERAL_LONG2'(Lex), !, skip_ws.
'String'(Lex) --> 'STRING_LITERAL1'(Lex),      !, skip_ws.
'String'(Lex) --> 'STRING_LITERAL2'(Lex),         skip_ws.



%! 'StrReplaceExpression'(+State, -E)// is det.
%
% ```ebnf
% [124] StrReplaceExpression ::= 'REPLACE' '('
%                                  Expression ','
%                                  Expression ','
%                                  Expression
%                                ( ',' Expression )? ')'
% ```

'StrReplaceExpression'(State, Function) -->
  keyword(`replace`),
  must_see_code(0'(),
  must_see('Expression'(State, Arg)),
  must_see_code(0',),
  must_see('Expression'(State, Pattern)),
  must_see_code(0',),
  must_see('Expression'(State, Repl)),
  (   ","
  ->  skip_ws,
      must_see('Expression'(State, Flags))
  ;   {Flags = literal('')}
  ),
  must_see_code(0')),
  {
    rdf_equal(fn:replace, Functor),
    Function =.. [Functor,Arg,Pattern,Repl,Flags]
  }.



%! 'SubSelect'(+State, -P)// is det.
%
% ```ebnf
% [8] SubSelect ::= SelectClause
%                   WhereClause
%                   SolutionModifier
%                   ValuesClause
% ```

'SubSelect'(State, P4) -->
  'SelectClause'(State, Proj, Mod),
  'WhereClause'(State, P1),
  'SolutionModifier'(State, P1, select(Proj,Mod), P2, P3, P4),
  'ValuesClause'(State, P2, P3).



%! 'SubstringExpression'(+State, -Function:compound)// is det.
%
% ```ebnf
% [123] SubstringExpression ::= 'SUBSTR' '('
%                                 Expression ','
%                                 Expression
%                                 ( ',' Expression )?
%                               ')'
% ```

'SubstringExpression'(State, Function) -->
  keyword(`substr`),
  must_see_code(0'(),
  must_see('Expression'(State, Source)),
  must_see_code(0',),
  must_see('Expression'(State, Start)),
  (   ","
  ->  skip_ws,
      must_see('Expression'(State, Len)),
      {Args = [Source,Start,Len]}
  ;   {Args = [Source,Start]}
  ),
  must_see_code(0')),
  {
    rdf_equal(fn:substring, Functor),
    Function =.. [Functor|Args]
  }.



%! 'TriplesBlock'(+State, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [55] TriplesBlock ::= TriplesSameSubjectPath ( '.' TriplesBlock? )?
% ```

'TriplesBlock'(State, Triples) -->
  'TriplesSameSubjectPath'(State, Triples1),
  (   "."
  ->  skip_ws,
      (   'TriplesBlock'(State, Triples2)
      ->  {dappend(Triples1, Triples2, Triples)}
      ;   {Triples = Triples1})
  ;   {Triples = Triples1}
  ).



%! 'TriplesNode'(+State, -Node, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [98] TriplesNode ::= Collection | BlankNodePropertyList
% ```

'TriplesNode'(State, Node, Triples) -->
  'Collection'(State, Node, Triples), !.
'TriplesNode'(State, Node, Triples) -->
  'BlankNodePropertyList'(State, Node, Triples).



%! 'TriplesNodePath'(+State, -Node, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [100] TriplesNodePath ::= CollectionPath | BlankNodePropertyListPath
% ```

'TriplesNodePath'(State, Node, Triples) -->
  'CollectionPath'(State, Node, Triples), !.
'TriplesNodePath'(State, Node, Triples) -->
  'BlankNodePropertyListPath'(State, Node, Triples).



%! 'TriplesSameSubject'(+State, -Triples:dlist(compound))// is det.
%
% Return a difference list of triples.
%
% ```ebnf
% [75] TriplesSameSubject ::= VarOrTerm PropertyListNotEmpty
%                           | TriplesNode PropertyList
% ```

'TriplesSameSubject'(State, Triples) -->
  'VarOrTerm'(State, S), !,
  'PropertyListNotEmpty'(State, S, Triples).
'TriplesSameSubject'(State, Triples) -->
  'TriplesNode'(State, S, Triples1),
  'PropertyList'(State, S, Triples2),
  {dappend(Triples1, Triples2, Triples)}.



%! 'TriplesSameSubjectPath'(+State, -Triples:dlist(compound))// is det.
%
% ```ebnf
% [81] TriplesSameSubjectPath ::= VarOrTerm PropertyListPathNotEmpty
%                               | TriplesNodePath PropertyListPath
% ```

'TriplesSameSubjectPath'(State, Triples) -->
  'VarOrTerm'(State, S), !,
  'PropertyListPathNotEmpty'(State, S, Triples).
'TriplesSameSubjectPath'(State, Triples) -->
  'TriplesNodePath'(State, S, Triples1),
  'PropertyListPath'(State, S, Triples2),
  {dappend(Triples1, Triples2, Triples)}.



%! 'TriplesTemplate'(+State, -Triples:dlist(compound))// is det.
%
% Parses an arbitrary number of, possibly abbreviated, triple
% templates.  A *triple template* allows a positive number of triples
% with the same subject term to be expressed through the use of
% predicate-object list (‘;’) and object list (‘,’) notation.
% Non-final triple templates must end in ‘.’,
%
% ```ebnf
% [52] TriplesTemplate ::= TriplesSameSubject
%                          ( '.' TriplesTemplate? )?
% ```

'TriplesTemplate'(State, Triples) -->
  'TriplesSameSubject'(State, Triples1),
  (   "."
  ->  skip_ws,
      (   'TriplesTemplate'(State, Triples2)
      ->  {dappend(Triples1, Triples2, Triples)}
      ;   {Triples = Triples1}
      )
  ;   {Triples = Triples1}
  ).



%! 'UnaryExpression'(+State, -Function)// is det.
%
% ```ebnf
% [118] UnaryExpression ::= '!' PrimaryExpression
%                         | '+' PrimaryExpression
%                         | '-' PrimaryExpression
%                         |     PrimaryExpression
% ```

'UnaryExpression'(State, Function) -->
  (   "!"
  ->  {rdf_equal(fn:not, Functor)}
  ;   "+"
  ->  {Functor = 'op:numeric-unary-plus'}
  ;   "-"
  ->  {Functor = 'op:numeric-unary-minus'}
  ), !,
  skip_ws,
  'PrimaryExpression'(State, E),
  {Function =.. [Functor,E]}.
'UnaryExpression'(State, Function) -->
  'PrimaryExpression'(State, Function).



%! 'ValuesClause'(+State, +P1, -P2)// is det.
%
% ‘QVal’ is either of the following:
%   * ‘var_in(Var,Vals)’
%   * ‘vars_in(Vars,Vals)’
%   * ‘true’
%
% ```ebnf
% [28] ValuesClause ::= ( 'VALUES' DataBlock )?
% ```
%
% §18.2.4.3 VALUES
%
% Let P := the algebra translation of the query level so far
% P := Join(P, ToMultiSet(data))
%   where data is a solution sequence formed from the VALUES clause

'ValuesClause'(State, P, 'Join'(P,'ToMultiset'(Data))) -->
  keyword(`values`), !,
  'DataBlock'(State, Data).
'ValuesClause'(_, P, P) --> "".



%! 'ValueLogical'(+State, -E)// is det.
%
% ```ebnf
% [113] ValueLogical ::= RelationalExpression
% ```

'ValueLogical'(State, E) -->
  'RelationalExpression'(State, E).



%! 'Var'(+State, -Var)// is det.
%
% ```ebnf
% [108] Var ::= VAR1 | VAR2
% ```

'Var'(State, Var) -->
  'VAR1'(State, Var), !,
  skip_ws.
'Var'(State, Var) -->
  'VAR2'(State, Var),
  skip_ws.


'Var*'(State, [Var|Vars]) -->
  'Var'(State, Var), !,
  'Var*'(State, Vars).
'Var*'(_, []) --> "".



%! 'VarOrIri'(+State, -Term)// is det.
%
% ```ebnf
% [107] VarOrIri ::= Var | iri
% ```

'VarOrIri'(State, Var) -->
  'Var'(State, Var), !.
'VarOrIri'(State, Iri) -->
  iri(State, Iri).



%! 'VarOrTerm'(+State, -Term)// is det.
%
% ```ebnf
% [106] VarOrTerm ::= Var | GraphTerm
% ```

'VarOrTerm'(State, Var) -->
  'Var'(State, Var), !.
'VarOrTerm'(State, Term) -->
  'GraphTerm'(State, Term).



%! 'Verb'(+State, -Term)// is det.
%
% ```ebnf
% [78] Verb ::= VarOrIri | 'a'
% ```

'Verb'(State, Term) -->
  'VarOrIri'(State, Term), !.
'Verb'(_, P) -->
  "a",
  skip_ws,
  {rdf_equal(rdf:type, P)}.



%! 'VerbObjectLists'(+State, +S, -Triples)// is det.
%
% ```ebnf
% Parses ( ';' ( ( VerbPath | VerbSimple ) ObjectList )? )*
% ```

'VerbObjectLists'(State, S, Triples) -->
  ('VerbPath'(State, Path) -> "" ; 'VerbSimple'(State, Path)),
  must_see('ObjectList'(State, S, Path, Triples1)),
  (   ";"
  ->  skip_ws,
      'VerbObjectLists'(State, S, Triples2),
      {dappend(Triples1, Triples2, Triples)}
  ;   {Triples = Triples1}
  ).
'VerbObjectLists'(_, _, T-T) --> "".



%! 'VerbPath'(+State, -Path)// is det.
%
% ```ebnf
% [84] VerbPath ::= Path
% ```

'VerbPath'(State, Path) -->
  'Path'(State, Path).



%! 'VerbSimple'(+State, -Var)// is det.
%
% ```ebnf
% [85] VerbSimple ::= Var
% ```

'VerbSimple'(State, Var) -->
  'Var'(State, Var).



%! 'WhereClause'(+State, -G:compound)// is det.
%
% ```ebnf
% [17] WhereClause ::= 'WHERE'? GroupGraphPattern
% ```

'WhereClause'(State, G) -->
  (keyword(`where`) -> "" ; ""),
  'GroupGraphPattern'(State, G).





% GRAMMAR :TERMINALS %

%! 'ANON'(-BNode)// .
%
% Anonymous resource.
%
% ```ebnf
% [163] ANON ::= '[' WS* ']'
% ```

'ANON'(bnode(_)) -->
  "[",
  *('WS'),
  "]".



%! 'BLANK_NODE_LABEL'(-BNode)// is semidet.
%
%  Processes "_:..." into a bnode(Name) term.
%
% ```ebnf
% [142] BLANK_NODE_LABEL ::= '_:'
%                            ( PN_CHARS_U | [0-9] )
%                            ((PN_CHARS|'.')* PN_CHARS)?
% ```
%
% Blank nodes are represented as ‘bnode(Lbl)’.

'BLANK_NODE_LABEL'(bnode(Name)) -->
  "_:",
  'BLANK_NODE_LABEL_1'(Codes, T),
  'BLANK_NODE_LABEL_2s3'(T),
  {atom_codes(Name, Codes)}.

'BLANK_NODE_LABEL_1'([H|T], T) -->
  'PN_CHARS_U'(H).
'BLANK_NODE_LABEL_1'([H|T], T) -->
  digit(H).

'BLANK_NODE_LABEL_2s3'(Codes) -->
  'BLANK_NODE_LABEL_2s'(Codes, [H]),
  'PN_CHARS'(H).
'BLANK_NODE_LABEL_2s3'([]) --> "".

'BLANK_NODE_LABEL_2s'(Codes, T) -->
  'BLANK_NODE_LABEL_2'(Codes, T0),
  'BLANK_NODE_LABEL_2s'(T0, T).
'BLANK_NODE_LABEL_2s'(T, T) --> "".

'BLANK_NODE_LABEL_2'([0'.|T], T) -->
  ".".
'BLANK_NODE_LABEL_2'([H|T], T) -->
  'PN_CHARS'(H).



%! 'DECIMAL'(-Val)// .
%
% ```ebnf
% [147] DECIMAL ::= [0-9]* '.' [0-9]+
% ```

'DECIMAL'(Val) -->
  'digit*'(Codes, T1),
  '.'(T1, T2),
  +(digit, T2),
  {
    string_codes(Str, Codes),
    xsd_number_string(Val, Str)
  }.



%! 'DECIMAL_NEGATIVE'(-Val)// .
%
% ```ebnf
% [153] DECIMAL_NEGATIVE ::= '-' DECIMAL
% ```
%
% @note No white space is allowed between the sign and the number.

'DECIMAL_NEGATIVE'(Val) -->
  "-",
  'DECIMAL'(Val0),
  {Val is -Val0}.



%! 'DECIMAL_POSITIVE'(-Val)// .
%
% ```ebnf
% [150] DECIMAL_POSITIVE ::= '+' DECIMAL
% ```

'DECIMAL_POSITIVE'(Val) -->
  "+",
  'DECIMAL'(Val).



%! 'DOUBLE'(-Val)// .
%
% ```ebnf
% [148] DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT
%                | '.' [0-9]+ EXPONENT
%                | [0-9]+ EXPONENT
% ```

'DOUBLE'(Val) -->
  (   'digit+'(Codes, T1)
  ->  (   '.'(T1, T2)
      ->  'digit*'(T2, T3),
          'EXPONENT'(T3)
      ;   'EXPONENT'(T1)
      )
  ;   '.'(Codes, T1),
      'digit+'(T1, T2), !,
      'EXPONENT'(T2)
  ),
  {
    string_codes(Str, Codes),
    xsd_number_string(Val, Str)
  }.



%! 'DOUBLE_NEGATIVE'(-Val)// .
%
% ```ebnf
% [154] DOUBLE_NEGATIVE ::= '-' DOUBLE
% ```
%
% @note No white space is allowed between the sign and the number.

'DOUBLE_NEGATIVE'(Val) -->
  "-",
  'DOUBLE'(Val0),
  {Val is -Val0}.



%! 'DOUBLE_POSITIVE'(-Val)// .
%
% ```ebnf
% [151] DOUBLE_POSITIVE ::= '+' DOUBLE
% ```

'DOUBLE_POSITIVE'(Val) -->
  "+",
  'DOUBLE'(Val).



%! 'ECHAR'(-C)// .
%
%  Escaped sequences in strings.
%
% ```ebnf
% [160] ECHAR ::= '\' [tbnrf\"']
% ```

'ECHAR'(C) -->
  "\\",
  'ECHAR_'(C).

'ECHAR_'(0'\t) --> "t".
'ECHAR_'(0'\b) --> "b".
'ECHAR_'(0'\n) --> "n".
'ECHAR_'(0'\r) --> "r".
'ECHAR_'(0'\f) --> "f".
'ECHAR_'(0'\\) --> "\\".
'ECHAR_'(0'")  --> "\"". %"
'ECHAR_'(0'')  --> "'".



%! 'EXPONENT'(-Codes)// .
%
% Floating point exponent.
%
% ```ebnf
% [155] EXPONENT ::= [eE] [+-]? [0-9]+
% ```

'EXPONENT'(Codes) -->
  'EXPONENT_1'(Codes, T1),
  'EXPONENT_2'(T1, T2),
  +(digit, T2).

'EXPONENT_1'([0'e|T], T) --> ("e" ; "E"), !.
'EXPONENT_1'(T, T) --> "".

'EXPONENT_2'([0'+|T], T) --> "+".
'EXPONENT_2'([0'-|T], T) --> "-".
'EXPONENT_2'(T, T) --> "".



%! 'INTEGER'(-Val)// .
%
% ```ebnf
% [146] INTEGER ::= [0-9]+
% ```

'INTEGER'(Val) -->
  +(digit, Codes),
  {
    string_codes(Str, Codes),
    xsd_number_string(Val, Str)
  }.



%! 'INTEGER_NEGATIVE'(-Val)// .
%
% ```ebnf
% [152] INTEGER_NEGATIVE ::= '-' INTEGER
% ```
%
% @note No white space is allowed between the sign and the number.

'INTEGER_NEGATIVE'(Val) -->
  "-",
  'INTEGER'(Val0),
  {Val is -Val0}.



%! 'INTEGER_POSITIVE'(-Val)// .
%
% ```ebnf
% [149] INTEGER_POSITIVE ::= '+' INTEGER
% ```

'INTEGER_POSITIVE'(Val) -->
  "+",
  'INTEGER'(Val).



%! 'IRIREF'(+State, -Iri)// is det.
%
% ```ebnf
% [139] IRIREF ::= '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ```

'IRIREF'(State, Iri) -->
  "<",
  iri_codes(Codes),
  {
    atom_codes(RelIri, Codes),
    % Relative IRIs are combined with base IRIs as per Uniform
    % Resource Identifier (URI): Generic Syntax [RFC3986] using only
    % the basic algorithm in section 5.2.  Neither Syntax-Based
    % Normalization nor Scheme-Based Normalization (described in
    % sections 6.2.2 and 6.2.3 of RFC3986) are performed.  Characters
    % additionally allowed in IRI references are treated in the same
    % way that unreserved characters are treated in URI references,
    % per section 6.5 of Internationalized Resource Identifiers (IRIs)
    % [RFC3987].

    % @bug Dot notation does not work here (State.base_iri).
    get_dict(base_iri, State, BaseIri),
    uri_normalized(RelIri, BaseIri, Iri)
  }.

iri_codes([]) -->
  ">", !.
iri_codes([H|T]) -->
  iri_code(H),
  iri_codes(T).

iri_code(C) -->
  [C],
  {is_legal_iri_code(C)}.

is_legal_iri_code(C) :-
  illegal_iri_code(C), !,
  char_code(Char, C),
  syntax_error(illegal_iri_code(Char)).
is_legal_iri_code(_).

illegal_iri_code(0'<).
illegal_iri_code(0'").%"
illegal_iri_code(0'{).
illegal_iri_code(0'}).
illegal_iri_code(0'|).
illegal_iri_code(0'^).
illegal_iri_code(0'`).
illegal_iri_code(0'\\).
illegal_iri_code(C) :-
  between(0x00, 0x20, C).



%! 'LANGTAG'(-LTag)// .
%
%  Return language tag (without leading @)
%
% ```ebnf
% [145] LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
% ```

'LANGTAG'(LTag) -->
  "@",
  'alpha+'(Codes, T0),
  'LANGTAG_'(T0, []),
  {atom_codes(LTag, Codes)}.

'LANGTAG_'([0'-|Codes], T) -->
  "-", !,
  'alphanum+'(Codes, T0),
  'LANGTAG_'(T0, T).
'LANGTAG_'(T, T) --> "".



%! 'NIL'(-Iri)// .
%
% End-of-collection (rdf:nil)
%
% ```ebnf
% [161] NIL ::= '(' WS* ')'
% ```

'NIL'(Iri) -->
  "(",
  *('WS'),
  ")",
  {rdf_equal(Iri, rdf:nil)}.



%! 'PERCENT'(-Codes, -T)// .
%
% ```ebnf
% [171] PERCENT ::= '%' HEX HEX
% ```

'PERCENT'(Codes, T) -->
  "%", xdigit(H2), xdigit(H3),
  {Codes = [0'%,H2,H3|T]}.



%! 'PLX'(-Codes, -T)// .
%
% ```ebnf
% [170] PLX ::= PERCENT | PN_LOCAL_ESC
% ```

'PLX'(Codes, T) --> 'PERCENT'(Codes, T).
'PLX'(Codes, T) --> 'PN_LOCAL_ESC'(Codes, T).



%! 'PN_CHARS'(-C)// .
%
% ```ebnf
% [167] PN_CHARS ::= PN_CHARS_U
%                  | '-'
%                  | [0-9]
%                  | #x00B7
%                  | [#x0300-#x036F]
%                  | [#x203F-#x2040]
% ```

'PN_CHARS'(C) --> 'VARNAME_2'(C).
'PN_CHARS'(0'-) --> "-".



%! 'PN_CHARS_BASE'(-C)// .
%
%  Basic identifier characters
%
% ```ebnf
% [164] PN_CHARS_BASE ::= [A-Z]
%                       | [a-z]
%                       | [#x00C0-#x00D6]
%                       | [#x00D8-#x00F6]
%                       | [#x00F8-#x02FF]
%                       | [#x0370-#x037D]
%                       | [#x037F-#x1FFF]
%                       | [#x200C-#x200D]
%                       | [#x2070-#x218F]
%                       | [#x2C00-#x2FEF]
%                       | [#x3001-#xD7FF]
%                       | [#xF900-#xFDCF]
%                       | [#xFDF0-#xFFFD]
%                       | [#x10000-#xEFFFF]
% ```

'PN_CHARS_BASE'(C) -->
  [C],
  {'PN_CHARS_BASE_'(C)}, !.

'PN_CHARS_BASE_'(C) :- between(0'A, 0'Z, C).
'PN_CHARS_BASE_'(C) :- between(0'a, 0'z, C).
'PN_CHARS_BASE_'(C) :- between(0x00C0, 0x00D6, C).
'PN_CHARS_BASE_'(C) :- between(0x00D8, 0x00F6, C).
'PN_CHARS_BASE_'(C) :- between(0x00F8, 0x02FF, C).
'PN_CHARS_BASE_'(C) :- between(0x0370, 0x037D, C).
'PN_CHARS_BASE_'(C) :- between(0x037F, 0x1FFF, C).
'PN_CHARS_BASE_'(C) :- between(0x200C, 0x200D, C).
'PN_CHARS_BASE_'(C) :- between(0x2070, 0x218F, C).
'PN_CHARS_BASE_'(C) :- between(0x2C00, 0x2FEF, C).
'PN_CHARS_BASE_'(C) :- between(0x3001, 0xD7FF, C).
'PN_CHARS_BASE_'(C) :- between(0xF900, 0xFDCF, C).
'PN_CHARS_BASE_'(C) :- between(0xFDF0, 0xFFFD, C).
'PN_CHARS_BASE_'(C) :- between(0x10000, 0xEFFFF, C).



%! 'PN_CHARS_U'(-C)// .
%
% ```ebnf
% [165] PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ```

'PN_CHARS_U'(C) --> 'PN_CHARS_BASE'(C).
'PN_CHARS_U'(0'_) --> "_".



%! 'PN_LOCAL'(-Atom)// .
%
% ```ebnf
% [169] PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX )
%                    (  (PN_CHARS | '.' | ':' | PLX)*
%                       (PN_CHARS | ':' | PLX)
%                    )?
% ```
%
% @tbd “SPARQL local names also allow the non-alphanumeric characters
%      allowed in IRIs via backslash character escapes
%      (e.g. ns:id\=123).”

'PN_LOCAL'(Atom) -->
  'PN_LOCAL_1'(Codes, T),
  'PN_LOCAL_2s3'(T),
  {atom_codes(Atom, Codes)}.

'PN_LOCAL_1'([H|T], T) -->
  'PN_CHARS_U'(H), !.
'PN_LOCAL_1'([0':|T], T) -->
  ":", !.
'PN_LOCAL_1'([H|T], T) -->
  digit(H), !.
'PN_LOCAL_1'(Codes, T) -->
  'PLX'(Codes, T).

'PN_LOCAL_2s3'(Codes) -->
  'PN_LOCAL_2s'(Codes, T),
  'PN_LOCAL_3'(T, []).
'PN_LOCAL_2s3'([]) --> "".

% @note No cuts for middle characters, because we may need to retract
%       for the last character.
'PN_LOCAL_2s'(Codes, T) -->
  'PN_LOCAL_2'(Codes, T0),
  'PN_LOCAL_2s'(T0, T).
'PN_LOCAL_2s'(T, T) --> "".

'PN_LOCAL_2'([0'.|T], T) -->
  ".", !.
'PN_LOCAL_2'(Codes, T) -->
  'PN_LOCAL_3'(Codes, T).

'PN_LOCAL_3'([H|T], T) -->
  'PN_CHARS'(H), !.
'PN_LOCAL_3'([0':|T], T) -->
  ":", !.
'PN_LOCAL_3'(Codes, T) -->
  'PLX'(Codes, T).



%! 'PN_LOCAL_ESC'(-Codes, -T)// .
%
% ```ebnf
% [173] PN_LOCAL_ESC ::= '\'
%                        ( '_' | '~' | '.' | '-' | '!' | '$' | '&' | "'"
%                        | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/'
%                        | '?' | '#' | '@' | '%'
%                        )
% ```

'PN_LOCAL_ESC'(Codes, T) -->
  "\\",
  'PN_LOCAL_ESC_'(H),
  {Codes = [H|T]}.

'PN_LOCAL_ESC_'(0'_) --> "_".
'PN_LOCAL_ESC_'(0'~) --> "~".
'PN_LOCAL_ESC_'(0'.) --> ".".
'PN_LOCAL_ESC_'(0'-) --> "-".
'PN_LOCAL_ESC_'(0'!) --> "!".
'PN_LOCAL_ESC_'(0'$) --> "$".
'PN_LOCAL_ESC_'(0'&) --> "&".
'PN_LOCAL_ESC_'(0'') --> "'".
'PN_LOCAL_ESC_'(0'() --> "(".
'PN_LOCAL_ESC_'(0')) --> ")".
'PN_LOCAL_ESC_'(0'*) --> "*".
'PN_LOCAL_ESC_'(0'+) --> "+".
'PN_LOCAL_ESC_'(0',) --> ",".
'PN_LOCAL_ESC_'(0';) --> ";".
'PN_LOCAL_ESC_'(0'=) --> "=".
'PN_LOCAL_ESC_'(0'/) --> "/".
'PN_LOCAL_ESC_'(0'?) --> "?".
'PN_LOCAL_ESC_'(0'#) --> "#".
'PN_LOCAL_ESC_'(0'@) --> "@".
'PN_LOCAL_ESC_'(0'%) --> "%".



%! 'PN_PREFIX'(-Prefix:atom)// is det.
%
% ```ebnf
% [168] PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
% ```

'PN_PREFIX'(Prefix) -->
  'PN_CHARS_BASE'(H),
  'PN_PREFIX_'(T),
  {atom_codes(Prefix, [H|T])}.

'PN_PREFIX_'(Codes) -->
  % @note We cannot place a cut here, because we may need to iterate
  %       over the last character.
  'PN_PREFIX__'(Codes, [Last]),
  'PN_CHARS'(Last).
'PN_PREFIX_'([]) --> "".

'PN_PREFIX__'([H|T0], T) -->
  % @note We cannot place a cut here, because we may need to iterate
  %       over the last character.
  ('PN_CHARS'(H) ; '.'(H)),
  'PN_PREFIX__'(T0, T).
'PN_PREFIX__'(T, T) --> "".



%! 'PNAME_LN'(+State, -Iri:atom)// is det.
%
% ```ebnf
% [141] PNAME_LN ::= PNAME_NS PN_LOCAL
% ```
%
% @error existence_error when no registration can be found for Prefix.

'PNAME_LN'(State, Iri) -->
  'PNAME_NS'(Prefix),
  'PN_LOCAL'(Local),
  {prefix_local_iri(State, Prefix, Local, Iri)}.



%! 'PNAME_NS'(-Prefix:atom)// is det.
%
% ```ebnf
% [140] PNAME_NS ::= PN_PREFIX? ':'
% ```

'PNAME_NS'(Prefix) -->
  ('PN_PREFIX'(Prefix) -> "" ; {Prefix = ''}),
  ":".



%! 'STRING_LITERAL1'(-Str:string)// is det.
%
% ```ebnf
% [156] STRING_LITERAL1 ::= "'" (([^#x27#x5C#xA#xD]) | ECHAR)* "'"
% ```

'STRING_LITERAL1'(Str) -->
  "'", !,
  'STRING_LITERAL_'([0''], Codes),
  {string_codes(Str, Codes)}.



%! 'STRING_LITERAL2'(-Str:string)// is det.
%
% ```ebnf
% [157] STRING_LITERAL2 ::= '"' (([^#x22#x5C#xA#xD]) | ECHAR)* '"'
% ```

'STRING_LITERAL2'(Str) -->
  "\"", !,
  'STRING_LITERAL_'([0'"], Codes), %"
  {string_codes(Str, Codes)}.



%! 'STRING_LITERAL_LONG1'(-Str:string)// is det.
%
% ```ebnf
% [158] STRING_LITERAL_LONG1 ::= "'''"
%                                (  ( "'" | "''" )?
%                                   ( [^'\] | ECHAR )
%                                )*
%                                "'''"
% ```

'STRING_LITERAL_LONG1'(Str) -->
  "'''", !,
  'STRING_LITERAL_'([0'',0'',0''], Codes),
  {string_codes(Str, Codes)}.



%! 'STRING_LITERAL_LONG2'(-Str:string)// is det.
%
% ```ebnf
% [159] STRING_LITERAL_LONG2 ::= '"""'
%                                (  ( '"' | '""' )?
%                                   ( [^"\] | ECHAR )
%                                )*
%                                '"""'
% ```

'STRING_LITERAL_LONG2'(Str) -->
  "\"\"\"", !,
  'STRING_LITERAL_'([0'",0'",0'"], Codes), %"
  {string_codes(Str, Codes)}.

'STRING_LITERAL_'(End, [H|T]) -->
  'ECHAR'(H), !,
  'STRING_LITERAL_'(End, T).
'STRING_LITERAL_'(End, []) -->
  End, !.
'STRING_LITERAL_'(End, [H|T]) -->
  [H],
  'STRING_LITERAL_'(End, T).



%! 'VAR1'(+State, -Var)// is semidet.
%
% ```ebnf
% [143] VAR1 ::= '?' VARNAME
% ```

'VAR1'(State, Var) -->
  "?",
  'VARNAME'(State, Var).



%! 'VAR2'(+State, -Var)// is semidet.
%
% ```ebnf
% [144] VAR2 ::= '$' VARNAME
% ```

'VAR2'(State, Var) -->
  "$",
  'VARNAME'(State, Var).



%! 'VARNAME'(+State, -Var)// is det.
%
% Name of a variable, i.e., the part after the ‘?’ or ‘$’ character.
%
% ```ebnf
% [166] VARNAME ::= ( PN_CHARS_U | [0-9] )
%                   ( PN_CHARS_U
%                   | [0-9]
%                   | #x00B7
%                   | [#x0300-#x036F]
%                   | [#x203F-#x2040]
%                   )*
% ```

'VARNAME'(State, var(VarName)) -->
  'VARNAME_1'(H),
  'VARNAME_2s'(T),
  {
    atom_codes(VarName, [H|T]),
    _{variable_map: VariableMap1} :< State,
    (   get_assoc(VarName, VariableMap1, _Var)
    ->  VariableMap2 = VariableMap1
    ;   put_assoc(VarName, VariableMap1, _Var, VariableMap2)
    ),
    nb_set_dict(variable_map, State, VariableMap2)
  }.

'VARNAME_1'(C) -->
  'PN_CHARS_U'(C), !.
'VARNAME_1'(C) -->
  digit(C).

'VARNAME_2s'([H|T]) -->
  'VARNAME_2'(H), !,
  'VARNAME_2s'(T).
'VARNAME_2s'([]) --> "".

'VARNAME_2'(C) -->
  'VARNAME_1'(C), !.
'VARNAME_2'(0x00B7) -->
  [0x00B7], !.
'VARNAME_2'(C) -->
  [C],
  {(between(0x0300, 0x036F, C) ; between(0x203F, 0x2040, C))}.





% ERROR REPORTING %

%! add_error_location(+E, +In) is det.

add_error_location(error(syntax_error(What), Location), Input) :-
  subsumes_term(end_of_file-CharCount, Location),
  end_of_file-CharCount = Location,
  length(After, CharCount),
  append(Before, After, Input),
  length(Before, BL),
  CLen = 80,
  atom_codes('...', Elipsis),
  atom_codes('\n**here**\n', Here),
  (   BL =< CLen
  ->  BC = Before
  ;   length(BC0, CLen),
      append(_, BC0, Before),
      append(Elipsis, BC0, BC)
  ),
  length(After, AL),
  (   AL =< CLen
  ->  AC = After
  ;   length(AC0, CLen),
      append(AC0, _, After),
      append(AC0, Elipsis, AC)
  ),
  append(Here, AC, HAC),
  append([0'\n|BC], HAC, ContextCodes),
  atom_codes(Context, ContextCodes), !,
  throw(error(syntax_error(sparql(What)),context(_,Context))).
add_error_location(Error, _Input) :-
  throw(Error).

http:bad_request_error(syntax_error(sparql(_)),_).

prolog:message(error(syntax_error(sparql(unknown)),_)) -->
  ["SPARQL: Unclassified syntax error in query"].
prolog:message(error(syntax_error(sparql(What)),context(_,Context))) -->
  ["SPARQL: syntax error: "],
  error_detail(What),
  (   {var(Context)}
  ->  []
  ;   {atomic_list_concat(Lines, "\n", Context)},
      [" at",nl],
      lines(Lines)
  ).

error_detail(expected(What)) -->
  ['"~w" expected'-[What]].
error_detail(What) -->
  ['~p'-[What]].

lines([]) --> [].
lines([H|T]) -->
  ['~w'-[H], nl],
  lines(T).





% HELPERS %

%! '.'(-C)// .
%! '.'(-Codes, -Tail)// .

'.'(0'.) --> ".".


'.'([0'.|T], T) --> ".".



%! 'digit*'(-Codes, -Tail)// .

'digit*'([H|T0], T) -->
  digit(H),
  'digit*'(T0, T).
'digit*'(T, T) --> "".



%! 'digit+'(-Codes, -Tail)// .

'digit+'([H|T0], T) -->
  digit(H),
  'digit*'(T0, T).



'alpha*'([H|T0], T) -->
  alpha(H), !,
  'alpha*'(T0, T).
'alpha*'(T, T) --> "".



'alpha+'([H|T0], T) -->
  alpha(H),
  'alpha*'(T0, T).



'alphanum*'([H|T0], T) -->
  alphanum(H), !,
  'alphanum*'(T0, T).
'alphanum*'(T, T) --> "".



'alphanum+'([H|T0], T) -->
  alphanum(H),
  'alphanum*'(T0, T).



%! prefix_iri(+State:dict, +Prefix:atom, -Iri:atom) is det.
%
% @throws existence_error If Prefix is not part of State.

prefix_iri(State, Prefix, Iri) :-
  prefix_local_iri(State, Prefix, '', Iri).



%! prefix_local_iri(+State:dict, +Prefix:atom, +Local:atom, -Iri:atom) is det.
%
% @throws existence_error If Prefix is not part of State.

prefix_local_iri(State, Prefix, Local, Iri2) :-
  % @bug Dot notation does not work here.
  _{prefixes: Prefixes} :< State,
  memberchk(Prefix-Iri1, Prefixes), !,
  atomic_concat(Iri1, Local, Iri2).
prefix_local_iri(_, Prefix, _, _) :-
  existence_error(rdf_prefix, Prefix).



%! algebra_to_vars(+Algebra, -Vars) is det.

algebra_to_vars(Term, Vars) :-
  algebra_to_vars([Term], [], Vars).

algebra_to_vars([], Vars, Vars) :- !.
algebra_to_vars([var(VarName)|T], Vars1, Vars3) :- !,
  ord_add_element(Vars1, var(VarName), Vars2),
  algebra_to_vars(T, Vars2, Vars3).
algebra_to_vars([H|T], Vars1, Vars2) :-
  compound(H), !,
  H =.. [_|Args],
  append(T, Args, L),
  algebra_to_vars(L, Vars1, Vars2).
algebra_to_vars([_|T], Vars1, Vars2) :-
  algebra_to_vars(T, Vars1, Vars2).



%! 'DISTINCT?'(+E, -Distinct:compound)// .
%
% Optionally wraps the argument Arg into a distinct(Arg) compound
% term.

'DISTINCT?'(E, distinct(E)) -->
  keyword(`distinct`), !.
'DISTINCT?'(E, E) --> "".



%! keyword(?Str)// .
%
% If Keyword is a ground string, then check wheter it occurs modulo
% case.
%
% If Keyword is uninstantated, then instantiate it to the next
% keyword, converted to lowercase.

keyword(Str) -->
  ({ground(Str)} -> keyword_ground(Str) ; keyword_var(Str)),
  skip_ws.

keyword_ground([H|T]) -->
  keyword_char(C),
  {code_type(H, to_lower(C))},
  keyword_ground(T).
keyword_ground([]) -->
  (keyword_char(_) -> !, {fail} ; "").

keyword_var(A) -->
  +(keyword_char, Codes),
  {
    string_codes(Str0, Codes),
    string_lower(Str0, Str),
    atom_string(A, Str)
  }.

keyword_char(C) -->
  alpha(C), !.
keyword_char(C) -->
  digit(C), !.
keyword_char(0'_) -->
  "_".



%! merge(A, B, C) is semidet.
%
% Merge two solution mappings.
%
% Fails if the solution mappins are inconsistent.

merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([Var-Term1|T1], [Var-Term2|T2], [Var-Term1|T3]) :- !,
  Term1 == Term2,
  merge(T1, T2, T3).
merge([Var1-Term1|T1], [Var2-Term2|T2], [Var1-Term1|T3]) :-
  Var1 @< Var2, !,
  merge(T1, [Var2-Term2|T2], T3).
merge([Var1-Term1|T1], [Var2-Term2|T2], [Var2-Term2|T3]) :-
  Var2 @< Var1, !,
  merge([Var1-Term1|T1], T2, T3).



%! must_see_code(+C)// is det.

must_see_code(C) -->
  must_see_code(C, skip_ws).



%! 'SILENT?'(-Silent:oneof([error,silent]))// is det.

'SILENT?'(silent) -->
  keyword(`silent`), !.
'SILENT?'(error) --> "".



%! skip_ws// .

skip_ws -->
  'WS', !,
  skip_ws.
skip_ws -->
  "#", !,
  skip_comment,
  skip_ws.
skip_ws --> "".

skip_comment --> "\n", !.
skip_comment --> "\r", !.
skip_comment --> eos, !.
skip_comment --> [_], skip_comment.



%! path_triples(+State, +S, +P, +O, -Triples:dlist) is det.
%
%  * `X' and `Y' are RDF terms or variables.
%
%  * `?V' is a fresh variable.
%
%  * `P' and `Q' are path expressions.
%
%  * These are only applied to property path patterns, not within
%    property path expressions.
%
%  * Translations earlier in the table are applied in preference to
%    the last translation.
%
%  * The final translation simply wraps any remaining property path
%    expression to use a common form `Path(…)'.
%
% | *Algebra (path)* | *Translation*   |
% |------------------|-----------------|
% | X link(iri) Y    | X iri Y         |
% | X inv(iri) Y     | Y iri X         |
% | X seq(P, Q) Y    | X P ?V . ?V Q P |
% | X P Y            | Path(X, P, Y)   |

path_triples(State, X, var(P), Y, ['BGP'([Triple])|T]-T) :- !,
  path_triple(State, X, var(P), Y, Triple).
% InversePath
%
% AST: ^iri
%
% Algebra: inv(iri)
path_triples(State, X, inv(Iri), Y, ['BGP'([Triple])|T]-T) :-
  rdf_is_iri(Iri), !,
  path_triple(State, Y, Iri, X, Triple).
% PredicatePath
%
% AST: iri
%
% Algebra: link(iri)
path_triples(State, X, link(Iri), Y, ['BGP'([Triple])|T]-T) :- !,
  path_triple(State, X, Iri, Y, Triple).
% SequencePath
%
% AST: p/q
%
% Algebra: seq(p,q))
path_triples(State, X, seq(P,Q), Y, Triples3) :- !,
  path_triples(State, X, P, V, Triples1),
  path_triples(State, V, Q, Y, Triples2),
  dappend(Triples1, Triples2, Triples3).
% OneOrMorePath
% ZeroOrMorePath
% ZeroOrOnePath
path_triples(State, X, P, Y, ['Path'(X, P, Y, Gs)|T1]-T1) :-
  active_graph(State, Gs).

path_triple(State, S, P, O, rdf(S,P,O,Gs)) :-
  active_graph(State, Gs).

active_graph(State, Gs) :-
  get_dict(active_graph, State, G),
  (G == default -> get_dict(default_graphs, State, Gs) ; Gs = [G]).