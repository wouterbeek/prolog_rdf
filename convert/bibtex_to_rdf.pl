:- module(
  bibtex_to_rdf,
  [
    bibtex_to_rdf/2 % +Input:compound
                    % +Graph:atom
  ]
).

/** <module> BibTeX to RDF converter

@author Wouter Beek
@version 2015/05
*/

:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/language/bibtex)).
:- use_module(plc(generics/hash)).

:- use_module(plRdf(api/rdf_build)).

:- rdf_register_prefix(lobo, 'http://linkedopenbibtex.org/ontology/').
:- rdf_register_prefix(lobr, 'http://linkedopenbibtex.org/resource/').





bibtex_to_rdf(Input, Graph):-
  bibtex(Input, Entries),
  maplist(\Entry^assert_bibtex_entry(Entry, Graph), Entries).



assert_bibtex_entry(Entry0, Graph):-
  % Resource.
  md5(Entry0, Hash),
  Entry0 = entry(ClassName,Name,Pairs),
  rdf_global_id(lobr:Hash, Entry),
  
  % Class.
  rdf_global_id(lobo:ClassName, Class),
  rdf_assert_instance(Entry, Class, Graph),
  
  % Properties.
  rdf_assert_string(Entry, lobo:name, Name, Graph),
  maplist(\Pair^assert_bibtex_property(Entry, Pair, Graph), Pairs).



assert_bibtex_property(Entry, Key-Value, Graph):-
  rdf_global_id(lobo:Key, P),
  rdf_assert_string(Entry, P, Value, Graph).
