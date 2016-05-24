:- module(
  rdf_annotate,
  [
    html_annotations//3, % +Location, +Text, +Res
    rdf_annotate/4,      % +Res, +Text, +G, +Opts
    rdf_annotation/2,    % +Res, -Concept
    rdf_annotations/2    % +Res, -Concepts
  ]
).

/** <module> RDF-based text annotation

@author Wouter Beek
@version 2014/11-2014/12, 2015/02-2015/03, 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(os/thread_ext)).
:- use_module(library(iri/iri_ext)).

:- rdf_meta
   html_annotations(+, +, r, ?, ?),
   rdf_annotate(r, +, ?, +),
   rdf_annotation(r, r),
   rdf_annotations(r, t).

:- rdf_register_prefix(annotate, 'http://lodlaundromat.org/annotate/').





%! html_annotations(+LocationPrefix, +Text, +Res)// is det.

html_annotations(LocationPrefix, Text, Resource) -->
  {
    rdf(Resource, annotate:has_annotation_job, AnnotationJob),
    (   rdf_has(AnnotationJob, annotate:'Resources', Annotations0)
    ->  rdf_list(Annotations0, Annotations)
    ;   Annotations = []
    ),
    atom_codes(Text, Cs)
  },
  html_annotations(LocationPrefix, Cs, 0, Annotations).


html_annotations(_, Cs, _, []) --> !,
  % Emit the remaining text.
  {atom_codes(Text, Cs)},
  html(Text).
html_annotations(LocationPrefix, Cs1, OffsetAdjustment1, [H|T]) -->
  % Firstly, emit the text that appears before the next annotation.
  {
    rdf_has(H, annotate:'@offset', Offset0^^xsd:nonNegativeInteger),
    Offset is Offset0 - OffsetAdjustment1
  },
  (   {Offset < 0}
  ->  % Skip annotations that overlap.
      {
        Cs3 = Cs1,
        OffsetAdjustment2 = OffsetAdjustment1
      }
  ;   {
        length(Prefix0, Offset),
        append(Prefix0, Cs2, Cs1),
        atom_codes(Prefix, Prefix0)
      },
      html(Prefix),

      % Secondly, emit the annotation.
      % This requires adjusting all pending annotations.
      {
        rdf(H, annotate:'@surfaceForm', SurfaceForm^^xsd:string),
        atom_length(SurfaceForm, Skip),
        length(SurfaceFormCodes, Skip),
        append(SurfaceFormCodes, Cs3, Cs2),
        OffsetAdjustment2 is OffsetAdjustment1 + Offset + Skip,

        % The hyperlink is based on the given LocationId, if any.
        rdf_has(H, annotate:'@URI', Res^^xsd:anyURI),
        (   var(LocationPrefix)
        ->  Location = Res
        ;   iri_add_query_comp(LocationPrefix, concept=Res, Location)
        ),

        % Background caching of annotation concepts.
        (   rdf_graph(Res)
        ->  % Already cached.
            true
        ;   create_thread(run_collect_messages(lod_cache(Res, [silent(true)])))
        )
      },
      html(
        a([class=annotated,href=Location,resource=Res], [SurfaceForm])
      )
  ),
  html_annotations(LocationPrefix, Cs3, OffsetAdjustment2, T).



%! rdf_annotate(+Res, +Text, +G, +Opts) is det.

rdf_annotate(Res, Text, G, Opts0):-
  merge_options(Opts0, [concepts(Concepts)], Opts),
  
  % Annotate the paragraph text with DBpedia links.
  annotate(Text, Annotations, Opts),
  
  % Store the annotations.
  json_to_rdf(G, dbpedia_spotlight, annotate, annotate, Annotations, AnnotationJob),
  option(language(Lang), Opts),
  rdf_assert(AnnotationJob, annotate:natural_language, Lang, G),
  rdf_assert(Res, annotate:has_annotation_job, AnnotationJob, G),

  % Relate the annotation concepts to the resource in whose text they occur.
  maplist(
    {Res,G}/[Concept]>>rdf_assert(Res, annotate:has_concept, Concept, G),
    Concepts
  ).



%! rdf_annotation(+Res, -Concept) is nondet.

rdf_annotation(Res, Concept):-
  rdf(Res, annotate:has_annotation_job, AnnotationJob),
  rdf_has(AnnotationJob, annotate:'Resources', Annotations),
  % NONDET.
  rdf_member(Annotation, Annotations),
  rdf_has(Annotation, annotate:'@URI', Concept^^xsd:anyURI).



%! rdf_annotations(+Res, -Concepts) is nondet.

rdf_annotations(Res, Concepts):-
  aggregate_all(set(Concept), rdf_annotation(Res, Concept), Concepts).
