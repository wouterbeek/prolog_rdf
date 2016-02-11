:- module(
  oaei_read,
  [
    oaei_alignment/2, % ?From, ?To
    oaei_alignment/4 % ?From, ?To, ?Relation:atom, ?Measure:between(0.0,1.0)
  ]    
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI): Read

@author Wouter Beek
@version 2015/10, 2015/12-2016/01
*/

:- use_module(library(semweb/rdf11)).

:- rdf_meta
   oaei_alignment(o, o),
   oaei_alignment(o, o, ?, ?).





%! oaei_alignment(?From:rdf_term, ?To:rdf_term) is nondet.

oaei_alignment(From, To) :-
  oaei_alignment(From, To, =, 1.0).


%! oaei_alignment(
%!   ?From:rdf_term,
%!   ?To:rdf_term,
%!   ?Relation:atom,
%!   ?Measure:between(0.0,1.0)
%! ) is nondet.

oaei_alignment(From, To, Rel, Measure) :-
  rdf_has(X, align:entity1, From),
  rdf_has(X, align:entity2, To),
  rdf_has(X, align:relation, Rel^^xsd:string),
  rdf_has(X, align:measure, Measure^^xsd:float).
