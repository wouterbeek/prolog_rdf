:- module(
  rdf_prefix,
  [
    rdf_prefix_iri/2 % +Iri:atom
                     % -PrefixIri:atom
  ]
).

/** <module> RDF prefix

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(semweb/rdf_db)).





%! rdf_prefix_iri(+Iri:atom, -PrefixIri:atom) is det.
% Returns the prefix of the given IRI that is abbreviated with a registered
%  RDF prefix, if any.
%
% If no registered RDF prefix occurs in Iri, then the full IRI is returned.

rdf_prefix_iri(Iri, PrefixIri):-
  rdf_global_id(Prefix:_, Iri), !,
  rdf_current_prefix(Prefix, PrefixIri).
rdf_prefix_iri(Iri, Iri).
