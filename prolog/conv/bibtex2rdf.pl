:- module(
  bibtex2rdf,
  [
    bibtex2rdf/2, % +Source, +G
    bibtex2rdf/3  % +Source, +G, +Opts
  ]
).

/** <module> BibTeX to RDF converter

@author Wouter Beek
@version 2015/05, 2015/12, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(dcg/bibtex)).
:- use_module(library(hash_ext)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(lobo, 'http://linkedopenbibtex.org/ontology/').
:- rdf_register_prefix(lobr, 'http://linkedopenbibtex.org/resource/').

:- rdf_meta
   bibtex2rdf(+, r),
   bibtex2rdf(+, r, +).





%! bibtex2rdf(+Source, +G) is det.
%! bibtex2rdf(+Source, +G, +Opts) is det.

bibtex2rdf(Source, G) :-
  bibtex2rdf(Source, G, []).


bibtex2rdf(Source, G, Opts) :-
  bibtex_load(Source, Entries, Opts),
  maplist(assert_bibtex_entry(G), Entries).


assert_bibtex_entry(G, Entry0) :-
  md5(Entry0, Hash),
  Entry0 = entry(CName,Name,Pairs),
  rdf_global_id(lobr:Hash, Entry),
  
  rdf_global_id(lobo:CName, C),
  rdf_assert_instance(Entry, C, G),
  
  rdf_assert(Entry, lobo:name, Name, G),
  maplist(assert_bibtex_property(Entry, G), Pairs).


assert_bibtex_property(Entry, G, Key-Val) :-
  rdf_global_id(lobo:Key, P),
  rdf_assert(Entry, P, Val, G).
