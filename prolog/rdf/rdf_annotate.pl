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
:- use_module(library(conv/json2rdf)).
:- use_module(library(debug_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(lod/lod_cache)).
:- use_module(library(os/thread_ext)).
:- use_module(library(owl/id_store)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(service/dbpedia_spotlight)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

:- rdf_meta
   html_annotations(+, +, r, ?, ?),
   rdf_annotate(r, +, +),
   rdf_annotate(r, +, r, +),
   rdf_annotation(r, r),
   rdf_annotations(r, -).

:- rdf_register_prefix(ann, 'http://lodlaundromat.org/annotate/').





%! html_annotations(+Link, +Txt, +S)// is det.

html_annotations(Link, Txt, S) -->
  {
    rdf(S, ann:hasAnnotationJob, Job),
    (rdf_has(Job, ann:'Resource', Anns0) -> rdf_list(Anns0, Anns) ; Anns = []),
    atom_codes(Txt, Cs)
  },
  html_annotations(Link, Cs, 0, Anns).


html_annotations(_, Cs, _, []) --> !,
  % Emit the remaining text.
  {atom_codes(Txt, Cs)},
  html(Txt).
html_annotations(Location, Cs1, OffsetAdjustment1, [H|T]) -->
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
        rdf_has(H, annotate:'@surfaceForm', SurfaceForm^^xsd:string),
        atom_length(SurfaceForm, Skip),
        length(SurfaceFormCs, Skip),
        append(SurfaceFormCs, Cs3, Cs2),
        OffsetAdjustment2 is OffsetAdjustment1 + Offset + Skip,

        % The hyperlink is based on the given LocationId, if any.
        rdf_has(H, annotate:'@URI', Res^^xsd:anyURI),
        (   var(Location0)
        ->  Location = S
        ;   iri_add_query_comp(Location0, concept=S, Location)
        ),

        % Background caching of annotation concepts.
        (   rdf_graph(S)
        ->  % Already cached.
            true
        ;   create_thread(call_collect_messages(lod_cache(S, [silent(true)])))
        )
      },
      html(a([class=annotated,href=Location,resource=S], [SurfaceForm]))
  ),
  html_annotations(Location0, Cs3, OffsetAdjustment2, T).



%! rdf_annotate(+S, +Txt, +Opts) is det.
%! rdf_annotate(+S, +Txt, ?G, +Opts) is det.

rdf_annotate(S, Txt, Opts):-
  rdf_annotate(S, Txt, _, Opts).


rdf_annotate(S, Txt, G, Opts0):-
  merge_options(Opts0, [concepts(Concepts)], Opts),
  annotate(Txt, Anns1, Opts),

  % Store the annotations.
  Context = context{
    annot: 'http://www.wouterbeek.com/annotate/',
    xsd: 'http://www.w3.org/2001/XMLSchema#',
    '@confidence': object{'@id': 'annot:confidence', '@type': 'xsd:decimal'},
    '@offset': object{'@id': 'annot:offset', '@type': 'xsd:nonNegativeInteger'},
    '@percentageOfSecondRank': object{'@id': 'annot:percentageOfSecondRank', '@type': 'xsd:float'},
    '@policy': object{'@id': 'annot:policy', '@type': 'xsd:string'},
    'Resources': object{'@id': 'annot:Resource', '@container': '@list'},
    '@similarityScore': object{'@id': 'annot:similarityScore', '@type': 'xsd:decimal'},
    '@sparql': object{'@id': 'annot:sparql', '@type': 'xsd:string'},
    '@support': object{'@id': 'annot:support', '@type': 'xsd:nonNegativeInteger'},
    '@surfaceForm': object{'@id': 'annot:surfaceForm', '@type': 'xsd:string'},
    '@text': object{'@id': 'annot:text', '@type': 'xsd:string'},
    '@types': object{'@id': 'annot:types', '@type': 'xsd:string'},
    '@URI': object{'@id': 'annot:URI', '@type': '@id'}
  },
  put_dict('@context', Anns1, Context, Anns2),
  rdf_create_iri(annot, [job], Job),
  put_dict('@id', Anns2, Job, Anns3),
  rdf_global_id(annot:'AnnotationJob', C),
  put_dict('@type', Anns3, C, Anns4),
  forall(jsonld_read(Anns4, rdf(S,P,O)), rdf_assert(S, P, O, G)),
  option(language(Lang), Opts),
  rdf_assert(Job, annot:naturalLanguage^^xsd:string, Lang, G),
  rdf_assert(S, annot:hasAnnotationJob, Job, G),
  
  % Relate the annotation concepts to the resource in whose text they occur.
  maplist({S,G}/[Con]>>rdf_assert(S, annot:hasConcept, Con, G), Cons).



%! rdf_annotation(+S, -Concept) is nondet.

rdf_annotation(S, Concept) :-
  rdf_has(S, annotate:hasAnnotationJob, Job),
  rdf_has(Job, annotate:'Resources', Anns),
  rdf_member(Ann, Anns),
  rdf_has(Ann, annotate:'@URI', Concept^^xsd:anyURI).



%! rdf_annotations(+S, -Concepts) is nondet.

rdf_annotations(S, Concepts) :-
  aggregate_all(set(Concept), rdf_annotation(S, Concept), Concepts).
