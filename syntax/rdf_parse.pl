:- module(
  rdf_parse,
  [
    rdf_parse_term//1 % -RDF_Term:ground
  ]
).

/** <module> RDF parse

Parses RDF terms.

@author Wouter Beek
@version 2013/07-2013/09, 2014/01
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_generics)).



% Blank node.
rdf_parse_term(BNode) -->
  rdf_parse_bnode(BNode).
% Literal.
rdf_parse_term(Literal) -->
  rdf_parse_literal(Literal).
% IRI.
rdf_parse_term(IRI) -->
  rdf_parse_iri(IRI).



% BNODE %

rdf_parse_bnode(BNode) -->
  '#'(2, underscore, [H1,H2], []),
  dcg_all([output_format(codes)], T),
  {atom_codes(BNode, [H1,H2|T])}.



% LITERAL %

rdf_parse_literal(Literal) -->
  rdf_parse_plain_literal(Literal).
rdf_parse_literal(Literal) -->
  rdf_parse_typed_literal(Literal).

rdf_parse_language_tag(Language) -->
  dcg_all([output_format(atom)], Language).

rdf_parse_plain_literal(literal(lang(Language,Value))) -->
  rdf_parse_simple_literal(Value),
  "@",
  rdf_parse_language_tag(Language).
rdf_parse_plain_literal(literal(Value)) -->
  rdf_parse_simple_literal(Value).

rdf_parse_simple_literal(Value) -->
  dcg_between(
    double_quote,
    dcg_until([end_mode(exclusive),output_format(atom)], double_quote, Value)
  ).

rdf_parse_typed_literal(literal(type(Datatype,Value))) -->
  dcg_between(
    double_quote,
    dcg_until([end_mode(exclusive),output_format(atom)], double_quote, Value)
  ),
  `^^`,
  rdf_parse_iri(Datatype).



% IRI %

rdf_parse_iri(IRI) -->
  {var(IRI)},
  dcg_until([end_mode(exclusive),output_format(atom)], colon, Prefix),
  colon,
  dcg_all([output_format(atom)], Postfix),
  {(
    rdf_current_prefix(Prefix, _)
  ->
    IRI = Prefix:Postfix
  ;
    atomic_list_concat([Prefix,Postfix], ':', IRI)
  )}.

