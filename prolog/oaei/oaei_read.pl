:- module(
  oaei_read,
  [
    oaei_alignment/3, % ?From:iri
                      % ?To:iri
                      % ?Graph:atom
    oaei_alignment/5 % ?From:iri
                     % ?To:iri
                     % ?Relation:atom
                     % ?Measure:between(0.0,1.0)
                     % ?Graph:atom
  ]    
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI): Read

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).

:- rdf_register_prefix(
     align,
     'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#',
     [keep(true)]
   ).

:- rdf_meta(oaei_alignment(o,o,?)).
:- rdf_meta(oaei_alignment(o,o,?,?,?)).





%! oaei_alignment(?From:iri, ?To:iri, ?Graph:atom) is nondet.

oaei_alignment(From, To, G):-
  oaei_alignment(From, To, =, 1.0, G).


%! oaei_alignment(
%!   ?From:iri,
%!   ?To:iri,
%!   ?Relation:atom,
%!   ?Measure:between(0.0,1.0),
%!   ?Graph:atom
%! ) is nondet.

oaei_alignment(From, To, Rel, Measure, G):-
  user:rdf(X, align:entity1, From, G),
  user:rdf(X, align:entity2, To, G),
  rdf_literal(X, align:relation, xsd:string, Rel, G),
  rdf_literal(X, align:measure, xsd:float, Measure, G).
