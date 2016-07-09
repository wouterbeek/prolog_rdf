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
:- use_module(library(q/q_stmt)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

:- rdf_meta
   qh_annotations(+, +, +, r, +, ?, ?),
   q_annotate(+, r, +, r, +),
   q_annotation(+, r, r, -),
   q_annotations(+, r, r, -).

:- qb_alias(ann, 'http://lodlaundromat.org/annotate/').





%! qh_annotations(+M, +Location, +Txt, +S, +G)// is det.

qh_annotations(M, Location, Txt, S, G) -->
  {
    q(M, S, ann:hasAnnotationJob, Job, G),
    (q_list_pl(M, Job, ann:'Resource', Anns, G) -> true ; Anns = []),
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
    q(M, H, ann:'@offset', Offset0^^xsd:nonNegativeInteger, G),
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
        q(M, H, ann:'@surfaceForm', SurfaceForm^^xsd:string, G),
        atom_length(SurfaceForm, Skip),
        length(SurfaceFormCs, Skip),
        append(SurfaceFormCs, Cs3, Cs2),
        OffsetAdjustment2 is OffsetAdjustment1 + Offset + Skip,

        % The hyperlink is based on the given LocationId, if any.
        q(M, H, ann:'@URI', Location2^^xsd:anyURI, G),
        (   var(Location1)
        ->  Location = Location2
        ;   iri_add_query_comp(Location1, concept=Location2, Location)
        )
      },
      html(a([class=annotated,href=Location,resource=Location2], [SurfaceForm]))
  ),
  qh_annotations(M, Location1, Cs3, OffsetAdjustment2, T, G).



%! rdf_annotate(+M, +S, +Txt, +G, +Opts) is det.

rdf_annotate(M, S, Txt, G, Opts0):-
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
  qb_iri(ann, [job], Job),
  put_dict('@id', Anns2, Job, Anns3),
  rdf_global_id(ann:'AnnotationJob', C),
  put_dict('@type', Anns3, C, Anns4),
  forall(jsonld_tuple(Anns4, rdf(S,P,O)), qb(M, S, P, O, G)),
  option(language(Lang), Opts),
  qb(Job, ann:naturalLanguage, Lang^^xsd:string, G),
  qb(M, S, ann:hasAnnotationJob, Job, G),
  
  % Relate the annotation concepts to the resource in whose text they occur.
  maplist({M,S,G}/[Concept]>>qb(M, S, ann:hasConcept, Concept, G), Concepts).



%! rdf_annotation(+M, +S, -Concept, +G) is nondet.

rdf_annotation(M, S, Concept, G) :-
  q(M, S, ann:hasAnnotationJob, Job, G),
  q_list_pl(M, Job, ann:'Resources', Anns, G),
  member(Ann, Anns),
  q(M, Ann, ann:'@URI', Concept^^xsd:anyURI, G).



%! rdf_annotations(+M, +S, +G, -Concepts) is nondet.

rdf_annotations(M, S, G, Concepts) :-
  aggregate_all(set(Concept), rdf_annotation(M, S, G, Concept), Concepts).
