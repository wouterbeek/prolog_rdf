:- module(
  turtle11,
  [
    prefixID//2, % ?PrefixLabel:atom
                 % ?Iri:atom
    sparqlPrefix//2 % ?PrefixLabel:atom
                    % ?Iri:atom
  ]
).
:- reexport(library(dcg/turtle11), [
     nodeID//1, % ?BlankNode:bnode
     'PNAME_LN'//1, % ?Iri:atom
     'PNAME_NS'//1, % ?Prefix:atom
     'PrefixedName'//1 % ?Iri:atom
   ]).

/** <module> Turtle 1.1

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(semweb/rdf_db)).





%! prefixID(?PrefixLabel:atom, ?Iri:atom)// .
% ```ebnf
% prefixID ::= '@prefix' PNAME_NS IRIREF '.'
% ```

prefixID(PrefixLabel, Iri) -->
  "@prefix",
  'PNAME_NS'(PrefixLabel),
  'IRIREF'(Iri),
  ".",
  {rdf_register_prefix(PrefixLabel, Iri)}.



%! sparqlPrefix(?PrefixLabel:atom, ?Iri:atom)// .
% ```ebnf
% sparqlPrefix ::= "PREFIX" PNAME_NS IRIREF
% ```

sparqlPrefix(PrefixLabel, Iri) -->
  "PREFIX",
  'PNAME_NS'(PrefixLabel),
  'IRIREF'(Iri),
  {rdf_register_prefix(PrefixLabel, Iri)}.
