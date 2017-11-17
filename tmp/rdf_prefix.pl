:- module(
  rdf_prefix,
  [
    rdf_longest_prefix/3, % +Iri:iri
                          % -LongestPrefix:atom
                          % -ShortestLocalName:atom
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
:- use_module(library(rdf/rdf_term)).

:- rdf_meta
   rdf_prefixe_iri(r, -).





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
  rdf_prefix_iri(LongestPrefix:ShortestLocalName, Iri).
