:- module(
  bibtex_to_rdf,
  [
    bibtex_to_rdf/2, % +Source, +Graph
    bibtex_to_rdf/3 % +Source
                    % +Graph:rdf_graph
		    % +Options:list(compound)
  ]
).

/** <module> BibTeX to RDF converter

@author Wouter Beek
@version 2015/05, 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/bibtex)).
:- use_module(library(hash_ext)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_prefix)).

:- rdf_register_prefix(lobo, 'http://linkedopenbibtex.org/ontology/').
:- rdf_register_prefix(lobr, 'http://linkedopenbibtex.org/resource/').

:- rdf_meta(bibtex_to_rdf(+,r)).




%! bibtex_to_rdf(+Source, +Graph:rdf_graph) is det.
% Wrapper around bibtex_to_rdf/3 with default options.

bibtex_to_rdf(Source, G):-
  bibtex_to_rdf(Source, G, []).


%! bibtex_to_rdf(+Source, +Graph:rdf_graph, +Options:list(compound)) is det.

bibtex_to_rdf(Source, G, Opts):-
  bibtex_load(Source, Entries, Opts),
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
  rdf_assert_literal(Entry, lobo:name, xsd:string, Name, G),
  maplist(assert_bibtex_property(Entry, G), Pairs).


assert_bibtex_property(Entry, G, Key-Val):-
  rdf_expand_ct(lobo:Key, P),
  rdf_assert_literal(Entry, P, xsd:string, Val, G).
