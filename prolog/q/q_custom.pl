:- module(
  q_custom,
  [
    q_image/4 % +M, ?S, -Img, ?G
  ]
).

/** <module> Quine: Custom rules

@author Wouter Beek
@version 2016/09
*/

:- use_module(library(q/q_rdf)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_image(?, r, -, r).





%! q_image(+M, ?S, -Img, ?G) is nondet.

q_image(M, S, Img, G) :-
  q(M, S, dbo:thumbnail, Img^^xsd:anyURI, G).
q_image(M, S, Img, G) :-
  q(M, S, foaf:depiction, Img^^xsd:anyURI, G).
q_image(M, S, Img, G) :-
  q(M, S, _, Img, G),
  q_instance(M, Img, dctype:'Image', G).
