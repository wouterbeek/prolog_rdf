:- module(
  oaei,
  [
    oaei_check_alignment/2, % +ReferenceAlignments:list(pair(iri))
                            % +RawAlignments:list(pair(iri))
    oaei_file_to_alignments/2, % +File:atom
                               % -AlignmentPairs:ordset(pair(iri))
    tsv_convert_directory/4 % +FromDirectory:atom
                            % +ToDirectory:atom
                            % ?ToMIME:atom
                            % -ToFiles:list(atom)
  ]
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI)

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(debug)).
:- use_module(library(default)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_save)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).

:- rdf_register_prefix(
     align,
     'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#'
   ).

:- rdf_meta(alignment(r,r,?)).
:- rdf_meta(alignment(r,r,?,?,?)).
:- rdf_meta(assert_alignment(r,r,+)).
:- rdf_meta(assert_alignment(r,r,+,+,+)).

user:file_search_path(alignment2, data(alignment2)).
user:file_search_path(mapping2, alignment2(alignment)).
user:file_search_path(ontology2, alignment2(ontology)).
user:file_search_path(reference2, alignment2(reference)).





%! alignment(?From:iri, ?To:iri, ?Graph:atom) is nondet.

alignment(From, To, G):-
  alignment(From, To, =, 1.0, G).


%! alignment(
%!   ?From:iri,
%!   ?To:iri,
%!   ?Relation:atom,
%!   ?Measure:between(0.0,1.0),
%!   ?Graph:atom
%! ) is nondet.

alignment(From, To, Rel, Measure, G):-
  rdf(X, align:entity1, From, G),
  rdf(X, align:entity2, To, G),
  rdf_literal(X, align:relation, xsd:string, Rel, G),
  rdf_literal(X, align:measure, xsd:float, Measure, G).



%! oaei_save_alignments(+In, +Alignments:list(pair)) is det.

oaei_save_alignments(In, As):-
  rdf_write_to_graph(In, assert_alignments(As)).



%! assert_alignment(+Alignment:pair(iri), +Graph:atom) is det.

assert_alignment(From-To, G):-
  assert_alignment(From, To, G).


%! assert_alignment(+From:iri, +To:iri, +Graph:atom) is det.

assert_alignment(From, To, G):-
  assert_alignment(From, To, =, 1.0, G).


%! assert_alignment(
%!   +From:iri,
%!   +To:iri,
%!   +Relation:atom,
%!   +Measure:between(0.0,1.0),
%!   +Graph:atom
%! ) is det.

assert_alignment(From, To, Rel, Measure, G):-
  rdf_bnode(BNode),
  rdf_assert(BNode, align:entity1, From, G),
  rdf_assert(BNode, align:entity2, To, G),
  rdf_assert_literal(BNode, align:relation, xsd:string, Rel, G),
  rdf_assert_literal(BNode, align:measure, xsd:float, Measure, G).



%! assert_alignments(+Alignments:list(pair), +Graph:atom) is det.

assert_alignments(As, G):-
  maplist(\A^assert_alignment(A, G), As).



%! oaei_check_alignment(
%!   +ReferenceAlignments:list(pair),
%!   +RawAlignments:list(pair)
%! ) is det.
% Tests the quality of a raw alignment relative to the given reference.

oaei_check_alignment(ReferenceAlignments, RawAlignments):-
  t1_error(ReferenceAlignments, RawAlignments, FalsePositives),
  t2_error(ReferenceAlignments, RawAlignments, FalseNegatives),
  ord_intersection(ReferenceAlignments, RawAlignments, X),
  length(X, Overlap),
  % Write the results to user output.
  format(
    user_output,
    '\t~w\t~w\t~w\n',
    [Overlap, FalsePositives, FalseNegatives]
  ),
  flush_output(user_output).


oaei_file_to_alignments(File, Alignments):-
  setup_call_cleanup(
    (
      file_components(File, _, Graph1, _Ext),
      % Make sure the graph name is unique.
      rdf_new_graph(Graph1, Graph2),
      rdf_load_any(File, [graph(Graph2)])
    ),
    oaei_graph_to_alignments(Graph2, Alignments),
    rdf_unload_graph_deb(Graph2)
  ).


%! oaei_graph_to_alignments(+Graph:atom, -Alignments:ordset(pair(iri))) is det.

oaei_graph_to_alignments(Graph, Alignments):-
  % Retrieve all alignment pairs.
  aggregate_all(
    set(From-To),
    alignment(From, To, Graph),
    Alignments
  ).


%! tsv_convert_directory(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   ?ToFormat:atom,
%!   -ToFiles:list(atom)
%! ) is det.

tsv_convert_directory(FromDir, ToDir, ToFormat, ToFiles):-
  defval(turtle, ToFormat),
  directory_files(FromDir, FromFiles, [file_types([tsv])]),
  findall(
    ToFile,
    (
      member(FromFile, FromFiles),
      rdf_file_extension(ToExt, ToFormat),
      file_alternative(FromFile, ToDir, _, ToExt, ToFile),
      tsv_file_to_oaei_file(FromFile, ToFile)
    ),
    ToFiles
  ).


%! tsv_file_to_alignments(+File:atom, -Alignments:ordset(pair(iri))) is det.
% Opens a tab separated values file and extracts the alignments it contains
% as pairs.

tsv_file_to_alignments(File, Alignments):-
  csv_read_file(File, Rows, [arity(2),separator(9)]),
  aggregate_all(
    set(From-To),
    member(row(From,To), Rows),
    Alignments
  ).


%! tsv_file_to_oaei_file(+FromFile:atom, +ToFile:atom) is det.

tsv_file_to_oaei_file(FromFile, ToFile):-
  tsv_file_to_alignments(FromFile, Alignments),
  alignments_to_oaei_file(Alignments, ToFile).



% TMP %

oaei_graph(Graph):-
  enforce_mode('_oaei_graph'(Graph), [Graph], [['+']-semidet,['-']-nondet]).
'_oaei_graph'(Graph):-
  rdf_graph(Graph),
  once((
    rdfs_individual_of(Alignment, align:'Alignment'),
    rdf(Alignment, _, _, Graph)
  )).


%! oaei_ontologies(+Graph:atom, -File1:atom, -File2:atom) is det.
% Returns the files in which the linked ontologies are stored.

oaei_ontologies(Graph, File1, File2):-
  oaei_ontology(Graph, File1),
  oaei_ontology(Graph, File2),
  File1 \== File2, !.


%! oaei_ontology(+Graph:atom, -File:atom) is nondet.
% Returns an ontology file used in the given alignment graph.

oaei_ontology(G, File):-
  rdf_literal(Ontology, align:location, xsd:anyURI, Uri, G),
  rdfs_individual_of(Ontology, align:'Ontology'),
  uri_components(Uri, uri_components(_,_,Path,_,_)),
  file_base_name(Path, Base),
  absolute_file_name(ontology2(Base), File, [access(read)]).

