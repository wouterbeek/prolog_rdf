:- module(
  sparql_build,
  [
    sparql_build//5 % +Mode:oneof([ask,delete,select])
                    % +Aliases
                    % +Variables:list(atom)
                    % +Bgps:or([compound,list(compound)])
                    % +Opts
  ]
).

/** <module> SPARQL Build

Generic rules for building SPARQL requests (both Query and Update).

The following constructs are supported:

| **SPARQL**        | **Prolog**                                         |
| triple pattern    | rdf(+sparql_term,+sparql_term,+sparql_term)        |
| COUNT AS          | count(+Variables:list(atom))                       |
| FILTER            | filter(+Filter:compound)                           |
| FILTER NOT EXISTS | not(+Bgp:compound)                                 |
| FROM NAMED        | from(+NamedGraphs:list(iri)                        |
| GRAPH             | graph(+NamedGraph:atom,+Bgp:compound)              |
| LIMIT             | limit(+Indent:nonneg,+Limit:nonneg)                |
| OFFSET            | offset(+Indent:nonneg,+Offset:nonneg)              |
| OPTIONAL          | optional(+Bgp:compound)                            |
| ORDER BY          | order(+Indent:nonneg,+Order:pair(order_mode,atom)) |
| PREFIX            | aliases(+Indent:nonneg,+Aliases:list(atom))        |
| UNION             | union(+Bgp1:compound,+Bgp2:compound)               |

Order modes:

| **SPARQL** | **Prolog** |
| ASC        | ascending  |
| DESC       | descending |

Query modes:

| **SPARQL** | **Prolog** |
| ASK        | ask        |
| DELETE     | delete     |
| SELECT     | select     |

Regular expression flags:

| **SPARQL** | **Prolog**        |
| i          | case_insensitive  |
| ^          | at_start(+String) |

Supported filters:

| **SPARQL**  | **Prolog**                                             |
| =           | =                                                      |
| !=          | !=                                                     |
| <           | <                                                      |
| >           | >                                                      |
| <=          | =<                                                     |
| >=          | >=                                                     |
| &&          | and(+compound,+compound)                               |
| ||          | or(+compound,+compound)                                |
| REGEX/3     | regex(+Term:sparql_term,+Pattern:compound)             |
|             | regex(+Term:sparql_term,+Pattern:compound,+Flags:list) |
| STRENDS/2   | strends(+sparql_term,+sparql_term)                     |
| STRSTARTS/2 | strstarts(+sparql_term,+sparql_term)                   |

Supported expressions:

| **SPARQL** | **Prolog**        |
| variable   | var(+Name:atom)   |
| integer    | integer           |
| float      | float             |
| STR/1      | str(+sparql_term) |
| +          | +                 |
| -          | -                 |
| /          | /                 |
| *          | *                 |
| str/1      | str/1             |

Term closures:

| **SPARQL** | **Prolog**           |
| *          | reflexive,transitive |
| +          | transitive           |

Virtuoso-specific:

| **Virtuoso**       | **Prolog**                                    |
| definine inference | inference_regime(+Indend:nonneg,+Regime:atom) |

---

@author Wouter Beek
@version 2015/08, 2015/10-2016/01, 2016/05-2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_option)).
:- use_module(library(dcg/sparql11)).
:- use_module(library(error)).
:- use_module(library(rdf/rdf_term)).





%! bgp(+Indent:nonneg, +Bgp:list(compound))// .

bgp(I, [filter(Filter)|T]) --> !,
  tab(I), "FILTER ", filter(Filter), nl,
  bgp(I, T).
bgp(I, [graph(NG,Bgp)|T]) --> !,
  tab(I), "GRAPH ", term(iri(NG)), " {", nl,
    {J is I + 1}, bgp(J, Bgp),
  tab(I), "}",
  bgp(I, T).
bgp(I, [not(Bgp)|T]) --> !,
  tab(I), "FILTER NOT EXISTS {", nl,
    {J is I + 1}, bgp(J, Bgp),
  tab(I), "}", nl,
  bgp(I, T).
bgp(I, [optional(Optional)|T]) --> !,
  tab(I), "OPTIONAL {", nl,
    {J is I + 1}, bgp(J, Optional),
  tab(I), "}", nl,
  bgp(I, T).
bgp(I, [rdf(S,P,O)|T]) --> !,
  tab(I), term(S), " ", term(P), " ", term(O), " .", nl,
  bgp(I, T).
bgp(I, [union(Bgp1,Bgp2)|T]) --> !,
  tab(I), "{", nl,
    {J is I + 1}, bgp(J, Bgp1),
  tab(I), "} UNION {", nl,
    bgp(J, Bgp2),
  tab(I), "}", nl,
  bgp(I, T).
bgp(_, []) --> "".



%! default_graph(+Indent:nonneg, +DefaultGraph:atom)// .

default_graph(I, DefaultGraph) -->
  tab(I), "# Default graph (located at ",
  atom(DefaultGraph),
  ")", nl.



%! define(+Indent:nonneg, +Define:compound)// is det.
% Currently the following terms are supported for Define:
%   - inference(+Regime:atom)

define(I, inference(Regime)) -->
  tab(I), "define input:inference ",
  define_inference_regime(Regime), nl.



%! define_inference_regime(+Alias)// is det.

define_inference_regime(Alias) -->
  {rdf_alias_prefix(Alias, Prefix)},
  "\"", atom(Prefix), "\"".



%! distinct(+Distinct:boolean)// .

distinct(true)  --> !, " DISTINCT".
distinct(false) --> "".



%! filter(+Filter:compound)// .
% The following filters are supported:
%   * regex(+Term, +ReMatch:compound, +ReFlags:list(oneof([case_sensitive])))
%     Succeeds when `Term` matches `ReMatch` under the given `ReFlags`.
%     The following RE matches are supported:
%       * at_start(+String:atom)
%     The following RE flags are supported:
%       * `case_insensitive`
%   * strends(+Term, +Match:compound)
%     Succeeds if `Term` is instantiated by a string that ends in `Match`.
%     The following matching terms are supported:
%       * string(+String:atom)

filter(RelationalExpression) -->
  {
    RelationalExpression =.. [Comparator,M,N],
    memberchk(Comparator, [=,'!=',<,>,<=,>=])
  },
  "(",
  numeric_expression(M),
  " ",
  atom(Comparator),
  " ",
  numeric_expression(N),
  ")".
filter(and(Expression1,Expression2)) -->
  "(",
  filter(Expression1),
  " && ",
  filter(Expression2),
  ")".
filter(or(Expression1,Expression2)) -->
  "(",
  filter(Expression1),
  " || ",
  filter(Expression2),
  ")".
filter(regex(Term,Pattern)) -->
  filter(regex(Term,Pattern,[])).
filter(regex(Term,Pattern,Flags)) -->
  "REGEX(",
  term(Term),
  ",",
  term(Pattern),
  regex_flags(Flags),
  ")".
filter(strends(Arg1,Arg2)) -->
  "STRENDS(",
  term(Arg1),
  ",",
  term(Arg2),
  ")".
filter(strstarts(Arg1,Arg2)) -->
  "STRSTARTS(",
  term(Arg1),
  ",",
  term(Arg2),
  ")".



%! from(+Indent:nonneg, +NamedGraphs:list(iri))// is det.

from(I, [H|T]) --> tab(I), "FROM NAMED ", term(iri(H)), !, nl, from(I, T).
from(_, []) --> "".



%! inference_regime(+Indent:nonneg, +Regime:oneof([none,owl]))// .

inference_regime(_, none) --> !, "".
inference_regime(I, Regime) --> define(I, inference(Regime)).



%! sparql_iri(+Iri:iri)// .
% An IRI term.

sparql_iri(Iri) --> "<", atom(Iri), ">".



%! limit(+Indent:nonneg, +Limit:positive_integer)// is det.

limit(I, Limit) -->
  tab(I),
  "LIMIT ",
  integer(Limit),
  nl.



%! mode(+Mode:oneof([ask,delete,select]))// is det.

mode(ask)    --> !, "ASK".
mode(delete) --> !, "DELETE".
mode(select) --> "SELECT".



%! numeric_expression(+ArithmeticExpression:compound)// .

numeric_expression(str(Expr)) --> !,
  "STR(", numeric_expression(Expr), ")".
numeric_expression(var(Var)) --> !,
  variable(Var).
numeric_expression(ArithmeticExpression) -->
  {
    ArithmeticExpression =.. [ArithmeticOperator,M,N],
    memberchk(ArithmeticOperator, [+,-,/,*])
  }, !,
  'NumericLiteral'(M),
  " ", atom(ArithmeticOperator), " ",
  'NumericLiteral'(N).
numeric_expression(N) --> {integer(N)}, !, integer(N).
numeric_expression(N) --> {float(N)}, !, float(N).



%! offset(+Indent:nonneg, ?Offset:nonneg)// .

offset(I, Offset) -->
  tab(I),
  "OFFSET ",
  integer(Offset),
  nl.



%! order(+Indent:nonneg, ?Order:pair(oneof([ascending,descending]),atom))// .

order(I, Criterion-Variable) -->
  tab(I), "ORDER BY ",
  order_criterion(Criterion),
  "(", variable(Variable), ")",
  nl.



%! order_criterion(+Order:oneof([ascending,descending]))// is det.

order_criterion(ascending)  --> !, "ASC".
order_criterion(descending) --> "DESC".



%! prefix(+Indent, +Alias)// is det.

prefix(I, Alias) -->
  {rdf_alias_prefix(Alias, Prefix)}, !,
  tab(I), "PREFIX ",
  atom(Alias),
  ": ",
  sparql_iri(Prefix),
  nl.
prefix(_, Alias) -->
  {existence_error(rdf_alias, Alias)}.



%! prefixes(+Indent, +Aliases)// is det.

prefixes(I, [H|T]) --> !, prefix(I, H), prefixes(I, T).
prefixes(_, [])    --> "".



%! query_form(
%!   +Indent:nonneg,
%!   +Mode:oneof([ask,delete,select]),
%!   +Variables:list(atom),
%!   +Opts
%! )// .

% SPARQL Query: ASK
query_form(I, Mode, _, _) --> {Mode == ask}, !, tab(I, mode(Mode)), nl.
% SPARQL Update: DELETE
query_form(I, Mode, _, _) --> {Mode == delete}, !, tab(I, mode(Mode)), nl.
% SPARQL Query: SELECT
query_form(I, Mode, Variables, Opts) -->
  {Mode == select}, !,
  tab(I, mode(Mode)),

  % Distinct.
  dcg_if_option(distinct(Distinct), Opts, (
    distinct(Distinct)
  )),
  " ",

  % Variables.
  variables(Variables),
  nl.



regex_flags([])    --> !, "".
regex_flags(Flags) --> ",\"", regex_flags1(Flags), "\"".
regex_flags1([case_insensitive|T]) --> !, "i", regex_flags1(T).
regex_flags1([]) --> "".



%! sparql_build(
%!   +Mode:oneof([ask,delete,select]),
%!   +Aliases,
%!   +Variables:list(atom),
%!   +Bgps:or([compound,list(compound)]),
%!   +Opts
%! )// .
%
% # Example
%
% ```prolog
% sparql_build(
%   select,
%   [rdf,rdfs],
%   [class],
%   [
%     rdf(dbpedia:'Monkey',rdf:type,var(x)),
%     rdf(var(x),rdfs:subClassOf,var(class))
%   ],
%   [distinct(true),order(ascending-class)]
% )
% ```
%
% The above builds the following SPARQL query:
%
% ```sparql
% PREFIX rdf: <...>
% PREFIX rdfs: <...>
% SELECT DISTINCT ?class
% WHERE {
%   dbpedia:Monkey rdf:type ?x .
%   ?x rdfs:subClassOf ?class .
% }
% ORDER BY ASC(?class)
% ```
%
% @arg Mode The mode of the SPARQL query.
% @arg Aliases A list of registered atomic XML prefixes.
% @arg Variables A list of atomic variable names.
% @arg Bgps A list denoting a basic graph pattern or a compound term
%      of the form =|union(Bgps)|= where `Bgps` is a list
%      of basic graph patterns.
%
% The following options are defined:
%   * default_graph(+DefaultGraph:iri)
%     The default graph to query from.
%   * distinct(+Distinct:boolean)
%     Whether the returned results should be distinct (`true`)
%     or not (`false`, default).
%   * inference_regime(Regime:oneof([none,owl,rdf,rdfs]))
%     The inference regime under which the results are being closed.
%     Currently only OWL is supported (`owl`); default `none`.
%   * limit(+Limit:or([nonneg,oneof([inf])]))
%     Either a positive integer indicating the maximum number of
%     results that will be retrieved, or `inf` (default).
%   * named_graphs(+NamedGraphs:list(iri))
%     The named graphs form which to query.
%   * offset(+Offset:nonneg)
%     Default: 0.
%   * order(+Order:pair(oneof([ascending,descending]),atom))
%     A pair consisting of an ordering criterion
%     and the variables relative to which the ordering is performed.
%     Currently the only supported ordering criterion is
%     ascending (`ascending`) or descending (`descending`)
%     lexicographically .
%
% @tbd Update examples in predicate documentation.

sparql_build(Mode, Aliases, Variables, Bgps, Opts) -->
  {I = 0},

  % Inference regime.
  dcg_if_option(inference_regime(Regime), Opts, (
    inference_regime(I, Regime)
  )),

  % Default graph.
  dcg_if_option(default_graph(DefaultGraph), Opts, (
    default_graph(I, DefaultGraph)
  )),

  % RDF aliases.
  prefixes(I, Aliases),

  % Head?
  query_form(I, Mode, Variables, Opts),

  % Named graph
  dcg_if_option(named_graphs(NamedGraphs), Opts, (
    from(I, NamedGraphs)
  )),

  % Body? Where clause.
  where(I, Bgps),

  % Order.
  dcg_if_option(order(Order), Opts, (
    order(I, Order)
  )),

  % Limit.
  dcg_if_option(limit(Limit), Opts, (
    limit(I, Limit)
  )),

  % Offset.
  dcg_if_option(offset(Offset), Opts, (
    offset(I, Offset)
  )).



%! term(+Term:compound)// is det.
% The following terms are supported:
%   - `a`
%     Abbreviation for `rdf:type`.
%   - at_start(String)
%     String pattern matching the start of a string.
%   - iri(IRI)
%     Unprefixed IRI.
%   - var(Variable)
%     SPARQL variable.
%   - `Prefix:Postfix`
%     Prefixed IRI.

term(a) --> !, "a".
term(at_start(String)) --> !, "\"", "^", atom(String), "\"".
term(closure(Term,Closure)) --> !, term(Term), term_closure(Closure).
term(iri(Iri)) --> !, sparql_iri(Iri).
term(literal(lang(Lang,Lex))) --> !, "\"", Lex, "\"@", atom(Lang).
term(literal(type(D0,Lex))) --> !,
  "\"", atom(Lex), "\"^^",
  {rdf_global_id(D0, D)},
  term(iri(D)).
term(str(Term)) --> !, "str(", term(Term), ")".
term(string(String)) --> !, "\"", atom(String), "\"".
term(var(Variable)) --> !, variable(Variable).
term(Prefix:Postfix) --> !, atom(Prefix), ":", atom(Postfix).
% Assume IRI.
term(Iri) --> term(iri(Iri)).



term_closure([reflexive,transitive]) --> !, "*".
term_closure([transitive])           --> "+".



%! variable(+Variable:atom)// .

variable(Variable) --> "?", atom(Variable).

variables([]) --> !, "*".
variables([Count|Variables]) -->
  {
    nonvar(Count),
    Count = count(CountVariables)
  }, !,
  "(COUNT(",
  variables(CountVariables),
  ") AS ",
  variable(count),
  ")",
  ({Variables == []} -> "" ; " ", variables(Variables)).
variables([H]) --> !, variable(H).
variables([H|T]) --> variable(H), " ", variables(T).



%! where(+Indent:nonneg, +Content:compound)// is det.

where(I, Content) -->
  tab(I), "WHERE {", nl,
    {J is I + 1}, bgp(J, Content),
  tab(I), "}", nl.
