:- module(
  rdf_prefix,
  [
    rdf_member/2, % ?Term:rdf_term
                  % +Terms:list(rdf_term)
    rdf_memberchk/2, % ?Term:rdf_term
                     % +Terms:list(rdf_term)
    rdf_prefix_iri/2 % +Iri:atom
                     % -PrefixIri:atom
  ]
).

/** <module> RDF prefix

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_member(r,t)).
:- rdf_meta(rdf_memberchk(r,t)).





%! rdf_member(+Term:rdf_term, +PrefixedTerms:list(rdf_term)) is semidet.
%! rdf_member(-Term:rdf_term, +PrefixedTerms:list(rdf_term)) is det.

rdf_member(X, L):-
  member(X, L).



%! rdf_memberchk(+Term:rdf_term, +PrefixedTerms:list(rdf_term)) is semidet.
%! rdf_memberchk(-Term:rdf_term, +PrefixedTerms:list(rdf_term)) is det.

rdf_memberchk(X, L):-
  memberchk(X, L).



%! rdf_prefix_iri(+Iri:atom, -PrefixIri:atom) is det.
% Returns the prefix of the given IRI that is abbreviated with a registered
%  RDF prefix, if any.
%
% If no registered RDF prefix occurs in Iri, then the full IRI is returned.

rdf_prefix_iri(Iri, PrefixIri):-
  rdf_global_id(Prefix:_, Iri), !,
  rdf_current_prefix(Prefix, PrefixIri).
rdf_prefix_iri(Iri, Iri).
