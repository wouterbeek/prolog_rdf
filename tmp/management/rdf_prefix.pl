:- module(
  rdf_prefix,
  [
    rdf_longest_prefix/3, % +Iri:iri
                          % -LongestPrefix:atom
                          % -ShortestLocalName:atom
    rdf_prefixes/5 % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?Object:rdf_term
                   % ?Graph:rdf_graph
                   % -Prefixes:ordset(pair(atom,positive_integer))
  ]
).

/** <module> RDF: Prefix

Namespace support for RDF(S), building on namespace prefix support for XML.

@author Wouter Beek
@version 2013/03-2013/05, 2014/01, 2014/07, 2014/09, 2014/11-2014/12, 2015/02,
         2015/12, 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_term)).

:- rdf_meta
   rdf_prefixe_iri(r, -),
   rdf_prefixes(r, r, o, ?, -).





%! rdf_longest_prefix(
%!   +Iri:iri,
%!   -LongestPrefix:atom,
%!   -ShortestLocalName:atom
%! ) is det.

rdf_longest_prefix(Iri, LongestPrefix, ShortestLocalName) :-
  findall(
    LocalNameLength-Prefix,
    (
      global(Prefix, LocalName, Iri),
      atom_length(LocalName, LocalNameLength)
    ),
    Pairs
  ),
  keysort(Pairs, [_-LongestPrefix|_]),
  rdf_global_id(LongestPrefix:ShortestLocalName, Iri).



%! rdf_prefixes(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:rdf_graph,
%!   -Prefixes:ordset(pair(atom,positive_integer))
%! ) is det.

rdf_prefixes(S, P, O, G, Pairs5) :-
  aggregate_all(
    set(Prefix-Term),
    (
      rdf(S, P, O, G),
      member(Term, [S,P,O]),
      rdf_global_id(Prefix:_, Term)
    ),
    Pairs1
  ),
  group_pairs_by_key(Pairs1, Pairs2),
  pairs_keys_values(Pairs2, Prefixes, Terms),
  maplist(length, Terms, Sizes),
  pairs_keys_values(Pairs3, Sizes, Prefixes),
  keysort(Pairs3, Pairs4),
  reverse(Pairs4, Pairs5).
