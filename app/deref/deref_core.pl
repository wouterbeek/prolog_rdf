:- module(
  deref_core,
  [
    deref_iri/2 % +Out, +Iri
  ]
).

/** <module> Dereferencing core

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_error)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(service/lov)).
:- use_module(library(yall)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- debug(deref(meta)).
%:- debug(deref(print)).
:- debug(deref(status)).





deref_iri(Out, Iri) :-
  Opts = [base_iri(Iri),triples(NumTriples),quads(NumQuads),timeout(5)],
  asserta((
    user:thread_message_hook(Term,Kind,_) :-
      error_kind(Kind),
      rdf_store_warning(Out, Iri, Term)
  )),
  catch(
    rdf_call_on_graph(
      Iri,
      {Out,Iri}/[G,M,M]>>deref_graph(Out, Iri, G, M),
      Opts
    ),
    E,
    true
  ), !,
  (   var(E)
  ->  % Number of triples
      rdf_store(Out, Iri, deref:number_of_triples, NumTriples^^xsd:nonNegativeInteger),
      debug(deref, "Number of triples: ~D", [NumTriples]),
      
      % Number of quadruples
      rdf_store(Out, Iri, deref:number_of_quads, NumQuads^^xsd:nonNegativeInteger),
      debug(deref, "Number of quads: ~D", [NumQuads])
  ;   % HTTP error status code
      E = error(existence_error(open_any2,M),_)
  ->  store_metadata(Out, Iri, M)
  ;   % Exception
      rdf_store_warning(Out, Iri, E)
  ),
  rdf_store_now(Out, Iri, dc:created).


deref_graph(Out, Iri, G, M) :-
  (debugging(deref(print)) -> rdf_print_graph(G) ; true),

  % Concise Bounded Description are _hinted at_ by ‘lone blank nodes’.
  aggregate_all(set(B), lone_bnode(B), Bs),
  length(Bs, NumBs),
  rdf_store(Out, Iri, deref:lone_bnodes, NumBs^^xsd:nonNegativeInteger),
  
  % Hash IRI?
  (sub_atom(Iri, _, 1, _, #) -> HashIri = true ; HashIri = false),
  rdf_store(Out, Iri, deref:is_hash_iri, HashIri^^xsd:boolean),

  % Vocabulary IRI?
  (   iri_vocab(Iri, Voc)
  ->  rdf_store(Out, Iri, deref:vocabulary, Voc)
  ;   true
  ),
  
  % Number of occurrences in the subject position
  rdf_aggregate_all(count, rdf(Iri, _, _, G), NumSubjects),
  rdf_store(Out, Iri, deref:number_of_subjects, NumSubjects^^xsd:nonNegativeInteger),
  debug(deref, "Appears in subject position: ~D", [NumSubjects]),

  % Number of occurrences in the predicate position
  rdf_aggregate_all(count, rdf(_, Iri, _, G), NumPredicates),
  rdf_store(Out, Iri, deref:number_of_predicates, NumPredicates^^xsd:nonNegativeInteger),
  debug(deref, "Appears in predicate position: ~D", [NumPredicates]),

  % Number of occurrences in the object position
  rdf_aggregate_all(count, rdf(_, _, Iri, G), NumObjects),
  rdf_store(Out, Iri, deref:number_of_objects, NumObjects^^xsd:nonNegativeInteger),
  debug(deref, "Appears in object position: ~D", [NumObjects]),

  % Serialization format
  get_dict('llo:rdf_format', M, RdfFormat0),
  Context = _{formats: 'http://www.w3.org/ns/formats/'},
  jsonld_expand_term(Context, RdfFormat0, RdfFormat),
  rdf_store(Out, Iri, deref:serialization_format, RdfFormat),

  % Metadata
  store_metadata(Out, Iri, M).


store_metadata(Out, Iri, M) :-
  get_dict('llo:http_communication', M, L1),
  maplist(store_http_metadata(Out, Iri), L1, L2),
  rdf_store_list(Out, L2, RdfList),
  rdf_store(Out, Iri, deref:responses, RdfList).


store_http_metadata(Out, Iri, M, B) :-
  rdf_create_bnode(B),
  maplist(store_http_headers(Out, B), M.'llo:headers'),
  rdf_store(Out, B, deref:iri, M.'llo:iri'^^xsd:anyURI),
  rdf_store(Out, B, deref:status, M.'llo:status'^^xsd:nonNegativeInteger),
  debug(deref(status), "HTTP status: ~d ~a", [M.'llo:status',Iri]),
  rdf_store(Out, B, deref:time, M.'llo:time'^^xsd:float),
  Major-Minor = M.'llo:version',
  format(string(Version), "~d.~d", [Major,Minor]),
  rdf_store(Out, B, deref:version, Version^^xsd:string).


store_http_headers(Out, B, Key-Vals) :-
  maplist(store_http_header(Out, B, Key), Vals).


store_http_header(Out, B, Key1, Val) :-
  atomic_list_concat(KeyComps, :, Key1),
  last(KeyComps, Key2),
  rdf_global_id(deref:Key2, P),
  rdf_store(Out, B, P, Val^^xsd:string).


%! lone_bnode(-BNode) is nondet.

lone_bnode(B) :-
  rdf(_, _, B),
  rdf_is_bnode(B),
  \+ rdf(B, _, _).


%! error_kind(+Kind) is semidet.

error_kind(warning).
error_kind(error).
