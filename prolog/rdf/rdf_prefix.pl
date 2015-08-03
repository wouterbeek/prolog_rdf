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
@version 2015/07-2015/08
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_member(r,t)).
:- rdf_meta(rdf_memberchk(r,t)).





assert_cc_prefixes:-
  Uri = 'http://prefix.cc/popular/all.file.csv',
  download_to_file(Uri, File, []),
  csv_read_file(File, Rows0),
  % Since the more popular prefixes are stored towards the top of the file,
  % we assert them in reverse order. This way the Semweb library will
  % (1) be able to interpret all CC-registered prefixes,
  % while at the same time
  % (2) using only the most popular prefix in writing.
  reverse(Rows0, Rows),
  maplist(assert_cc_prefix, Rows).

assert_cc_prefix(row(Prefix, Uri)):-
  rdf_reset_prefix(Prefix, Uri).



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
