:- module(
  rdf_default,
  [
    rdf_defval/2 % +Default:iri
                 % -Value:iri
  ]
).

/** <module> RDF default

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_defval(r,-)).





rdf_defval(_, X):-
  nonvar(X), !.
rdf_defval(X, X).
