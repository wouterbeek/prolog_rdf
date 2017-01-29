:- module(
  rdf_annotate,
  [
    rdf_annotate/5,         % +M, +S, +Txt, +G, +Opts
    rdf_annotation/4,       % +M, +S, +G, -Concept
    rdf_annotations/4,      % +M, +S, +G, -Concepts
    rdf_html_annotations//5 % +M, ?Uri, +Txt, +S, +G
  ]
).

/** <module> RDF-based text annotation

@author Wouter Beek
@version 2015/08, 2016/05-2016/06, 2017/01
*/

:- use_module(library(aggregate)).
:- use_module(library(debug_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(nlp/dbpedia_spotlight)).
:- use_module(library(os/thread_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(solution_sequences)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(yall)).

:- rdf_meta
   rdf_html_annotations(+, +, +, r, +, ?, ?),
   rdf_annotate(+, r, +, r, +),
   rdf_annotation(+, r, r, -),
   rdf_annotations(+, r, r, -).

:- rdf_create_alias(annotate, 'http://lodlaundromat.org/annotate/').





%! rdf_html_annotations(+M, ?Uri, +Txt, +S, +G)// is det.

rdf_html_annotations(M, Uri, Txt, S, G) -->
  {
    t(M, S, annotate:hasAnnotationJob, Job, G),
    findall(Ann, rdf_list_member(M, Job, annotate:'Resource', Ann, G), Anns),
    atom_codes(Txt, Cs)
  },
  rdf_html_annotations(M, Uri, Cs, 0, Anns, G).


rdf_html_annotations(_, _, Cs, _, [], _) --> !,
  % Emit the remaining text.
  {atom_codes(Txt, Cs)},
  html(Txt).
rdf_html_annotations(M, Uri1, Cs1, OffsetAdjustment1, [H|T], G) -->
  % Firstly, emit the text that appears before the next annotation.
  {
    t(M, H, annotate:'@offset', Offset0^^xsd:nonNegativeInteger, G),
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
        t(M, H, annotate:'@surfaceForm', SurfaceForm^^xsd:string, G),
        atom_length(SurfaceForm, Skip),
        length(SurfaceFormCs, Skip),
        append(SurfaceFormCs, Cs3, Cs2),
        OffsetAdjustment2 is OffsetAdjustment1 + Offset + Skip,

        % The hyperlink is based on the given UriId, if any.
        t(M, H, annotate:'@URI', Uri2^^xsd:anyURI, G),
        (   var(Uri1)
        ->  Uri = Uri2
        ;   iri_add_query_comp(Uri1, concept=Uri2, Uri)
        )
      },
      html(
        a([class(annotated),href(Uri),resource(Uri2)], [SurfaceForm])
      )
  ),
  rdf_html_annotations(M, Uri1, Cs3, OffsetAdjustment2, T, G).



%! rdf_annotate(+M, +S, +Txt, +G, +Opts) is det.

rdf_annotate(M, S, Txt, G, Opts0):-
  merge_options(Opts0, [concepts(Concepts)], Opts),
  annotate(Txt, Anns1, Opts),

  % Store the annotations.
  Context = context{
    annotate: 'http://lodlaundromat.org/annotate/',
    xsd: 'http://www.w3.org/2001/XMLSchema#',
    '@confidence': object{'@id': 'annotate:confidence', '@type': 'xsd:decimal'},
    '@offset': object{'@id': 'annotate:offset', '@type': 'xsd:nonNegativeInteger'},
    '@percentageOfSecondRank': object{'@id': 'annotate:percentageOfSecondRank', '@type': 'xsd:float'},
    '@policy': object{'@id': 'annotate:policy', '@type': 'xsd:string'},
    'Resources': object{'@id': 'annotate:Resource', '@container': '@list'},
    '@similarityScore': object{'@id': 'annotate:similarityScore', '@type': 'xsd:decimal'},
    '@sparql': object{'@id': 'annotate:sparql', '@type': 'xsd:string'},
    '@support': object{'@id': 'annotate:support', '@type': 'xsd:nonNegativeInteger'},
    '@surfaceForm': object{'@id': 'annotate:surfaceForm', '@type': 'xsd:string'},
    '@text': object{'@id': 'annotate:text', '@type': 'xsd:string'},
    '@types': object{'@id': 'annotate:types', '@type': 'xsd:string'},
    '@URI': object{'@id': 'annotate:URI', '@type': '@id'}
  },
  put_dict('@context', Anns1, Context, Anns2),
  uri_segments_uuid(Job, [annotate]),
  put_dict('@id', Anns2, Job, Anns3),
  rdf_global_id(annotate:'AnnotationJob', C),
  put_dict('@type', Anns3, C, Anns4),
  forall(jsonld_tuple(Anns4, rdf(S,P,O)), rdf_assert(M, S, P, O, G)),
  option(language(Lang), Opts),
  rdf_assert(M, Job, annotate:naturalLanguage, Lang^^xsd:string, G),
  rdf_assert(M, S, annotate:hasAnnotationJob, Job, G),
  
  % Relate the annotation concepts to the resource in whose text they occur.
  maplist(
    {M,S,G}/[Concept]>>rdf_assert(M, S, annotate:hasConcept, Concept, G),
    Concepts
  ).



%! rdf_annotation(+M, +S, -Concept, +G) is nondet.

rdf_annotation(M, S, Concept, G) :-
  t(M, S, annotate:hasAnnotationJob, Job, G),
  rdf_list_member(M, Job, annotate:'Resources', Ann, G),
  t(M, Ann, annotate:'@URI', Concept^^xsd:anyURI, G).



%! rdf_annotations(+M, +S, +G, -Concepts) is nondet.

rdf_annotations(M, S, G, Concepts) :-
  aggregate_all(set(Concept), rdf_annotation(M, S, G, Concept), Concepts).
