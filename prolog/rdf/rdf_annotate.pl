:- module(
  rdf_annotate,
  [
    html_annotations//3, % +Location, +Txt, +S
    rdf_annotate/4,      % +S, +Txt,     +Opts
    rdf_annotate/4,      % +S, +Txt, ?G, +Opts
    rdf_annotation/2,    % +S, -Concept
    rdf_annotations/2    % +S, -Concepts
  ]
).

/** <module> RDF-based text annotation

@author Wouter Beek
@version 2015/08, 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(debug_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(nlp/dbpedia_spotlight)).
:- use_module(library(os/thread_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

:- rdf_meta
   html_annotations(+, +, r, ?, ?),
   rdf_annotate(r, +, +),
   rdf_annotate(r, +, r, +),
   rdf_annotation(r, r),
   rdf_annotations(r, -).

:- rdf_register_prefix(ann, 'http://lodlaundromat.org/annotate/').





%! html_annotations(+Location, +Txt, +S)// is det.

html_annotations(Location, Txt, S) -->
  {
    rdf(S, ann:hasAnnotationJob, Job),
    (rdf_has(Job, ann:'Resource', Anns0) -> rdf_list(Anns0, Anns) ; Anns = []),
    atom_codes(Txt, Cs)
  },
  html_annotations(Location, Cs, 0, Anns).


html_annotations(_, Cs, _, []) --> !,
  % Emit the remaining text.
  {atom_codes(Txt, Cs)},
  html(Txt).
html_annotations(Location1, Cs1, OffsetAdjustment1, [H|T]) -->
  % Firstly, emit the text that appears before the next annotation.
  {
    rdf_has(H, ann:'@offset', Offset0^^xsd:nonNegativeInteger),
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
        rdf_has(H, ann:'@surfaceForm', SurfaceForm^^xsd:string),
        atom_length(SurfaceForm, Skip),
        length(SurfaceFormCs, Skip),
        append(SurfaceFormCs, Cs3, Cs2),
        OffsetAdjustment2 is OffsetAdjustment1 + Offset + Skip,

        % The hyperlink is based on the given LocationId, if any.
        rdf_has(H, ann:'@URI', Location2^^xsd:anyURI),
        (   var(Location1)
        ->  Location = Location2
        ;   iri_add_query_comp(Location1, concept=Location2, Location)
        )
      },
      html(a([class=annotated,href=Location,resource=Location2], [SurfaceForm]))
  ),
  html_annotations(Location1, Cs3, OffsetAdjustment2, T).



%! rdf_annotate(+S, +Txt, +Opts) is det.
%! rdf_annotate(+S, +Txt, ?G, +Opts) is det.

rdf_annotate(S, Txt, Opts):-
  rdf_annotate(S, Txt, _, Opts).


rdf_annotate(S, Txt, G, Opts0):-
  merge_options(Opts0, [concepts(Concepts)], Opts),
  annotate(Txt, Anns1, Opts),

  % Store the annotations.
  Context = context{
    ann: 'http://www.wouterbeek.com/annotate/',
    xsd: 'http://www.w3.org/2001/XMLSchema#',
    '@confidence': object{'@id': 'ann:confidence', '@type': 'xsd:decimal'},
    '@offset': object{'@id': 'ann:offset', '@type': 'xsd:nonNegativeInteger'},
    '@percentageOfSecondRank': object{'@id': 'ann:percentageOfSecondRank', '@type': 'xsd:float'},
    '@policy': object{'@id': 'ann:policy', '@type': 'xsd:string'},
    'Resources': object{'@id': 'ann:Resource', '@container': '@list'},
    '@similarityScore': object{'@id': 'ann:similarityScore', '@type': 'xsd:decimal'},
    '@sparql': object{'@id': 'ann:sparql', '@type': 'xsd:string'},
    '@support': object{'@id': 'ann:support', '@type': 'xsd:nonNegativeInteger'},
    '@surfaceForm': object{'@id': 'ann:surfaceForm', '@type': 'xsd:string'},
    '@text': object{'@id': 'ann:text', '@type': 'xsd:string'},
    '@types': object{'@id': 'ann:types', '@type': 'xsd:string'},
    '@URI': object{'@id': 'ann:URI', '@type': '@id'}
  },
  put_dict('@context', Anns1, Context, Anns2),
  rdf_create_iri(ann, [job], Job),
  put_dict('@id', Anns2, Job, Anns3),
  rdf_global_id(ann:'AnnotationJob', C),
  put_dict('@type', Anns3, C, Anns4),
  forall(jsonld_tuple(Anns4, rdf(S,P,O)), rdf_assert(S, P, O, G)),
  option(language(Lang), Opts),
  rdf_assert(Job, ann:naturalLanguage^^xsd:string, Lang, G),
  rdf_assert(S, ann:hasAnnotationJob, Job, G),
  
  % Relate the annotation concepts to the resource in whose text they occur.
  maplist({S,G}/[Concept]>>rdf_assert(S, ann:hasConcept, Concept, G), Concepts).



%! rdf_annotation(+S, -Concept) is nondet.

rdf_annotation(S, Concept) :-
  rdf_has(S, ann:hasAnnotationJob, Job),
  rdf_has(Job, ann:'Resources', Anns),
  rdf_member(Ann, Anns),
  rdf_has(Ann, ann:'@URI', Concept^^xsd:anyURI).



%! rdf_annotations(+S, -Concepts) is nondet.

rdf_annotations(S, Concepts) :-
  aggregate_all(set(Concept), rdf_annotation(S, Concept), Concepts).
