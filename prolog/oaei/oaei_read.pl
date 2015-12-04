:- module(
  oaei_read,
  [
    oaei_alignment/3, % ?From:rdf_term
                      % ?To:rdf_term
                      % ?Graph:atom
    oaei_alignment/5 % ?From:rdf_term
                     % ?To:rdf_term
                     % ?Relation:atom
                     % ?Measure:between(0.0,1.0)
                     % ?Graph:rdf_graph
  ]    
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI): Read

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).

:- rdf_register_prefix(
     align,
     'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#',
     [keep(true)]
   ).

:- rdf_meta(oaei_alignment(o,o,?)).
:- rdf_meta(oaei_alignment(o,o,?,?,?)).





%! oaei_alignment(?From:rdf_term, ?To:rdf_term, ?Graph:rdf_graph) is nondet.

oaei_alignment(From, To, G):-
  oaei_alignment(From, To, =, 1.0, G).


%! oaei_alignment(
%!   ?From:rdf_term,
%!   ?To:rdf_term,
%!   ?Relation:atom,
%!   ?Measure:between(0.0,1.0),
%!   ?Graph:rdf_graph
%! ) is nondet.

oaei_alignment(From, To, Rel, Measure, G):-
  rdf(X, align:entity1, From, G),
  rdf(X, align:entity2, To, G),
  rdf_literal(X, align:relation, xsd:string, Rel, G),
  rdf_literal(X, align:measure, xsd:float, Measure, G).
