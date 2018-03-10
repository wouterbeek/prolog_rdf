:- module(
  digging,
  [
    hierarchy/4,      % +User, +Dataset, +P, ?G
    hierarchy/5,      % +User, +Dataset, +P, ?G, +Options
    namespace/2,      % -G, -Namespace
    vocabulary_size/0
   ]).

/** <module> Digging Into the Knowledge Graph

@author Wouter Beek
@version 2017/11-2017/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(graph/graph_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(os_ext)).
:- use_module(library(real)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/schema_viz)).
:- use_module(library(solution_sequences)).
:- use_module(library(tapir)).
:- use_module(library(uri/uri_ext)).

:- maplist(rdf_create_prefix, [
     cidoc-'http://www.cidoc-crm.org/cidoc-crm/',
     cinelab-'http://advene.org/ns/cinelab/ld#',
     content-'https://www.w3.org/2011/content#',
     udc-'http://udcdata.info/076308'
   ]).

:- rdf_meta
   hierarchy(+, +, r, r),
   hierarchy(+, +, r, r, +).





hierarchy(User, Dataset, P, G) :-
  hierarchy(User, Dataset, P, G, _{}).


hierarchy(User, Dataset, P, G, Options1) :-
  merge_dicts(Options1, _{format: svg, method: sfdp}, Options2),
  (ground(G) -> true ; graph(User, Dataset, G)),
  show_hierarchy(tapir(User,Dataset,G), P, Options2).



namespace(G, Namespace) :-
  distinct(namespace(wbeek, 'LOV', G, Namespace)).


namespace(User, Dataset, G, Namespace) :-
  graph(User, Dataset, _, GDict),
  _{graphName: G} :< GDict,
  term(User, Dataset, G, Term),
  iri_namespace(Term, Namespace).

iri_namespace(Iri, Namespace) :-
  rdf_is_iri(Iri),
  uri_comps(Iri, uri(Scheme,Authority,Segments1,_,Fragment1)),
  (   var(Fragment1)
  ->  namespace_path(Segments1, Segments2)
  ;   Segments2 = Segments1,
      Fragment2 = ''
  ),
  uri_comps(Namespace, uri(Scheme,Authority,Segments2,_,Fragment2)).

namespace_path(Segments1, Segments) :-
  append(Segments2, [Last], Segments1),
  (Last == '' -> Segments = Segments1 ; append(Segments2, [''], Segments)).



%! vocabulary_size is det.

vocabulary_size :-
  File = 'vocabulary-size.pdf',
  findall(
    Size,
    (
      graph(digging, lov, _, Graph),
      _{graphName: _Name, numberOfStatements: Size} :< Graph
    ),
    Sizes
  ),
  sort(0, @>=, Sizes, Sorted),
  <- cairo_pdf(+File),
  <- plot(Sorted,
          main="Linked Open Vocabularies (LOV)",
          xlab="Vocabularies",
          ylab="Number of statements"),
  <- dev.off(),
  open_file(File).
