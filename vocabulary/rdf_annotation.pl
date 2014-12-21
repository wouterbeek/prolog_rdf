:- module(
  rdf_annotation,
  [
    html_annotation_job//3 % +Location:uri
                           % +Text:atom
                           % +AnnotationJob:iri
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

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(term/rdf_list)).

:- use_module(lodCache(lod_cache_egograph)).

:- rdf_meta(html_annotation_job(+,+,r,?,?)).





%! html_annotation_job(
%!   +LocationPrefix:uri,
%!   +Text:atom,
%!   +AnnotationJob:dict
%! )// is det.

html_annotation_job(LocationPrefix, Text, AnnotationJob) -->
  {
    rdf_has(AnnotationJob, bo:'Resources', Annotations0),
    rdf_list(Annotations0, Annotations),
    atom_codes(Text, Codes)
  },
  html_annotations(LocationPrefix, Codes, Annotations).

%! html_annotations(
%!   +LocationPrefix:uri,
%!   +Codes:list(code),
%!   +Annotations:iri
%! )// is det.

html_annotations(LocationPrefix, Codes, Annotations) -->
  html_annotations(LocationPrefix, Codes, 0, Annotations).

html_annotations(_, Codes, _, []) --> !,
  % Emit the remaining text.
  {atom_codes(Text, Codes)},
  html(Text).
html_annotations(LocationPrefix, Codes1, OffsetAdjustment1, [H|T]) -->
  % Firstly, emit the text that appears before the next annotation.
  {
    rdf_typed_literal(H, bo:'@offset', Offset0, xsd:nonNegativeInteger),
    Offset is Offset0 - OffsetAdjustment1,
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
    ;   uri_query_add_nvpair(LocationPrefix, term, Resource, Location)
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
  ),
  html_annotations(LocationPrefix, Codes3, OffsetAdjustment2, T).
