:- module(
  rdf_default,
  [
    rdf_defval/2 % +Default:iri
                 % -Value:iri
  ]
).

/** <module> RDF default

@author Wouter Beek
@version 2015/07, 2015/12
*/

:- use_module(library(rdf/rdf_prefix)).

:- rdf_meta(rdf_defval(r,-)).





rdf_defval(_, X) :-
  nonvar(X), !.
rdf_defval(X, X).
