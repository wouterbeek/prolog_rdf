:- module(
  bibtex2rdf,
  [
    bibtex2rdf/3, % +M, +Source, +G
    bibtex2rdf/4  % +M, +Source, +G, +Opts
  ]
).

/** <module> BibTeX to RDF converter

@author Wouter Beek
@version 2015/05, 2015/12, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(dcg/bibtex)).
:- use_module(library(hash_ext)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(semweb/rdf11)).

:- qb_alias(lobo, 'http://linkedopenbibtex.org/ontology/').
:- qb_alias(lobr, 'http://linkedopenbibtex.org/resource/').

:- rdf_meta
   bibtex2rdf(+, +, r),
   bibtex2rdf(+, +, r, +).





%! bibtex2rdf(+Source, +M, +G) is det.
%! bibtex2rdf(+Source, +M, +G, +Opts) is det.

bibtex2rdf(Source, M, G) :-
  bibtex2rdf(Source, M, G, []).


bibtex2rdf(Source, M, G, Opts) :-
  bibtex_load(Source, Entries, Opts),
  maplist(assert_bibtex_entry(M, G), Entries).


assert_bibtex_entry(M, G, Entry0) :-
  md5(Entry0, Hash),
  Entry0 = entry(CName,Name,Pairs),
  rdf_global_id(lobr:Hash, Entry),
  rdf_global_id(lobo:CName, C),
  qb_instance(M, Entry, C, G),
  qb(M, Entry, lobo:name, Name, G),
  maplist(assert_bibtex_property(M, Entry, G), Pairs).


assert_bibtex_property(M, Entry, G, Key-Val) :-
  rdf_global_id(lobo:Key, P),
  qb(M, Entry, P, Val, G).
