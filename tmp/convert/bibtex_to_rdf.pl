:- module(
  bibtex_to_rdf,
  [
    bibtex_to_rdf/2 % +Input:compound
                    % +Graph:atom
  ]
).

/** <module> BibTeX to RDF converter

@author Wouter Beek
@version 2015/05, 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(rdf/rdf_api)).

:- rdf_register_prefix(lobo, 'http://linkedopenbibtex.org/ontology/').
:- rdf_register_prefix(lobr, 'http://linkedopenbibtex.org/resource/').





bibtex_to_rdf(Input, G):-
  bibtex(Input, Entries),
  maplist(assert_bibtex_entry(G), Entries).


assert_bibtex_entry(G, Entry0):-
  % Resource.
  md5(Entry0, Hash),
  Entry0 = entry(ClassName,Name,Pairs),
  rdf_expand_ct(lobr:Hash, Entry),
  
  % Class.
  rdf_expand_ct(lobo:ClassName, Class),
  rdf_assert_instance(Entry, Class, G),
  
  % Properties.
  rdf_assert_string(Entry, lobo:name, Name, G),
  maplist(assert_bibtex_property(Entry, G), Pairs).


assert_bibtex_property(Entry, G, Key-Value):-
  rdf_expand_ct(lobo:Key, P),
  rdf_assert_string(Entry, P, Value, G).
