:- module(
  rdf_annotation,
  [
    html_annotations//3, % +Location:uri
                         % +Text:atom
                         % +Resource:or([bnode,iri])
    rdf_annotate/3, % +Resource:or([bnode,iri])
                    % +Text:atom
                    % +Graph:atom
    rdf_annotation/2, % +Resource:or([bnode,iri])
                      % -Concept:iri
    rdf_annotations/2 % +Resource:or([bnode,iri])
                      % -Concepts:ordset(iri)
  ]
).

/** <module> RDF: Annotation

Generates HTML based on an annotated text that uses
the RDF annotation vocabulary.

@author Wouter Beek
@version 2014/11-2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(thread_ext)).
:- use_module(pl(pl_log)).

:- use_module(plUri(uri_query)).

:- use_module(plNlp(dbpedia_spotlight)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(convert/json_to_rdf)).
:- use_module(plRdf(term/rdf_list)).

:- use_module(lodCache(lod_cache_egograph)).

:- rdf_meta(html_annotations(+,+,r,?,?)).
:- rdf_meta(rdf_annotate(r,+,?)).
:- rdf_meta(rdf_annotation(r,r)).
:- rdf_meta(rdf_annotations(r,t)).





%! html_annotations(
%!   +LocationPrefix:uri,
%!   +Text:atom,
%!   +Resource:or([bnode,iri])
%! )// is det.

html_annotations(LocationPrefix, Text, Resource) -->
  {
    rdf(Resource, bo:has_annotation_job, AnnotationJob),
    (   rdf_has(AnnotationJob, bo:'Resources', Annotations0)
    ->  rdf_list(Annotations0, Annotations)
    ;   Annotations = []
    ),
    atom_codes(Text, Codes)
  },
  html_annotations(LocationPrefix, Codes, 0, Annotations).

html_annotations(_, Codes, _, []) --> !,
  % Emit the remaining text.
  {atom_codes(Text, Codes)},
  html(Text).
html_annotations(LocationPrefix, Codes1, OffsetAdjustment1, [H|T]) -->
  % Firstly, emit the text that appears before the next annotation.
  {
    rdf_typed_literal(H, bo:'@offset', Offset0, xsd:nonNegativeInteger),
    Offset is Offset0 - OffsetAdjustment1
  },
  (   {Offset < 0}
  ->  % Skip annotations that overlap.
      {
        Codes3 = Codes1,
        OffsetAdjustment2 = OffsetAdjustment1
      }
  ;   {
        length(Prefix0, Offset),
        append(Prefix0, Codes2, Codes1),
        atom_codes(Prefix, Prefix0)
      },
      html(Prefix),
    
      % Secondly, emit the annotation.
      % This requires adjusting all pending annotations.
      {
        rdf_simple_literal(H, bo:'@surfaceForm', SurfaceForm),
        atom_length(SurfaceForm, Skip),
        length(SurfaceFormCodes, Skip),
        append(SurfaceFormCodes, Codes3, Codes2),
        OffsetAdjustment2 is OffsetAdjustment1 + Offset + Skip,
    
        % The hyperlink is based on the given LocationId, if any.
        rdf_typed_literal(H, bo:'@URI', Resource, xsd:anyURI),
        (   var(LocationPrefix)
        ->  Location = Resource
        ;   uri_query_add_nvpair(LocationPrefix, concept, Resource, Location)
        ),
    
        % Background caching of annotation concepts.
        (   rdf_graph(Resource)
        ->  % Already cached.
            true
        ;   thread_create(
              run_collect_messages(
                lod_cache_egographs_under_closure(Resource, [silent(true)])
              )
            )
        )
      },
      html(
        a([class=annotated,href=Location,resource=Resource], [SurfaceForm])
      )
  ),
  html_annotations(LocationPrefix, Codes3, OffsetAdjustment2, T).



%! rdf_annotate(+Resource:or([bnode,iri]), +Text:atom, +Graph:atom) is det.

rdf_annotate(Resource, Text, Graph):-
  % Annotate the paragraph text with DBpedia links.
  annotate(
    Text,
    Annotations,
    [
      concepts(Concepts),
      % NOTICE THAT THE DBPEDIA SPOTLIGHT BACKEND CANNOT PROCESS :0.0"!
      confidence(0),
      language(Language)
    ]
  ),
  % Store the annotations.
  json_to_rdf(Graph, dbpedia_spotlight, bo, b, Annotations, AnnotationJob),
  rdf_assert_simple_literal(
    AnnotationJob,
    bo:natural_language,
    Language,
    Graph
  ),
  rdf_assert(Resource, bo:has_annotation_job, AnnotationJob, Graph),

  % Relate the annotation concepts to the resource in whose text they occur.
  forall(
    member(Concept, Concepts),
    rdf_assert(Resource, bo:has_concept, Concept, Graph)
  ).



%! rdf_annotation(+Resource:or([bnode,iri]), -Concept:iri) is nondet.

rdf_annotation(Resource, Concept):-
  rdf(Resource, bo:has_annotation_job, AnnotationJob),
  rdf_has(AnnotationJob, bo:'Resources', Annotations),
  % NONDET.
  rdf_list_member(Annotation, Annotations),
  rdf_typed_literal(Annotation, bo:'@URI', Concept, xsd:anyURI).



%! rdf_annotations(
%!   +Resource:or([bnode,iri]),
%!   -Concepts:ordset(iri)
%! ) is nondet.

rdf_annotations(Resource, Concepts):-
  aggregate_all(
    set(Concept),
    rdf_annotation(Resource, Concept),
    Concepts
  ).
