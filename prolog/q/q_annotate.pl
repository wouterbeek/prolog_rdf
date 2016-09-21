:- module(
  q_annotate,
  [
    qh_annotations//5, % +M, +Location, +Txt, +S, +G
    q_annotate/5,      % +M, +S, +Txt, +G, +Opts
    q_annotation/4,    % +M, +S, +G, -Concept
    q_annotations/4    % +M, +S, +G, -Concepts
  ]
).

/** <module> RDF-based text annotation

@author Wouter Beek
@version 2015/08, 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(debug_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(nlp/dbpedia_spotlight)).
:- use_module(library(os/thread_ext)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

:- rdf_meta
   qh_annotations(+, +, +, r, +, ?, ?),
   q_annotate(+, r, +, r, +),
   q_annotation(+, r, r, -),
   q_annotations(+, r, r, -).

:- qb_alias(annotate, 'http://lodlaundromat.org/annotate/').





%! qh_annotations(+M, +Location, +Txt, +S, +G)// is det.

qh_annotations(M, Location, Txt, S, G) -->
  {
    q(M, S, annotate:hasAnnotationJob, Job, G),
    findall(Ann, q_list_member(M, Job, annotate:'Resource', Ann, G), Anns),
    atom_codes(Txt, Cs)
  },
  qh_annotations(M, Location, Cs, 0, Anns, G).


qh_annotations(_, _, Cs, _, [], _) --> !,
  % Emit the remaining text.
  {atom_codes(Txt, Cs)},
  html(Txt).
qh_annotations(M, Location1, Cs1, OffsetAdjustment1, [H|T], G) -->
  % Firstly, emit the text that appears before the next annotation.
  {
    q(M, H, annotate:'@offset', Offset0^^xsd:nonNegativeInteger, G),
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
        q(M, H, annotate:'@surfaceForm', SurfaceForm^^xsd:string, G),
        atom_length(SurfaceForm, Skip),
        length(SurfaceFormCs, Skip),
        append(SurfaceFormCs, Cs3, Cs2),
        OffsetAdjustment2 is OffsetAdjustment1 + Offset + Skip,

        % The hyperlink is based on the given LocationId, if any.
        q(M, H, annotate:'@URI', Location2^^xsd:anyURI, G),
        (   var(Location1)
        ->  Location = Location2
        ;   iri_add_query_comp(Location1, concept=Location2, Location)
        )
      },
      html(a([class=annotated,href=Location,resource=Location2], [SurfaceForm]))
  ),
  qh_annotations(M, Location1, Cs3, OffsetAdjustment2, T, G).



%! q_annotate(+M, +S, +Txt, +G, +Opts) is det.

q_annotate(M, S, Txt, G, Opts0):-
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
  qb_iri(annotate, Job),
  put_dict('@id', Anns2, Job, Anns3),
  rdf_global_id(annotate:'AnnotationJob', C),
  put_dict('@type', Anns3, C, Anns4),
  forall(jsonld_tuple(Anns4, rdf(S,P,O)), qb(M, S, P, O, G)),
  option(language(Lang), Opts),
  qb(M, Job, annotate:naturalLanguage, Lang^^xsd:string, G),
  qb(M, S, annotate:hasAnnotationJob, Job, G),
  
  % Relate the annotation concepts to the resource in whose text they occur.
  maplist({M,S,G}/[Concept]>>qb(M, S, annotate:hasConcept, Concept, G), Concepts).



%! q_annotation(+M, +S, -Concept, +G) is nondet.

q_annotation(M, S, Concept, G) :-
  q(M, S, annotate:hasAnnotationJob, Job, G),
  q_list_member(M, Job, annotate:'Resources', Ann, G),
  q(M, Ann, annotate:'@URI', Concept^^xsd:anyURI, G).



%! q_annotations(+M, +S, +G, -Concepts) is nondet.

q_annotations(M, S, G, Concepts) :-
  aggregate_all(set(Concept), q_annotation(M, S, G, Concept), Concepts).
