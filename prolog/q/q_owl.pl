:- module(
  q_owl,
  [
    q_identity/4 % +M, ?I, ?J, ?G
  ]
).

/** <module> Quine: OWL

@author Wouter Beek
@version 2016/09
*/

:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_identity(?, r, r, r).





%! q_identity(?M, ?I, ?J, ?G) is nondet.

q_identity(M, I, J, G) :-
  q(M, I, owl:sameAs, J, G).
