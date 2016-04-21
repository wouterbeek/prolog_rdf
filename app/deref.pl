:- module(
  deref,
  [
    deref_all/0,
    deref_all/1, % +N
    deref_hdt/1, % +S
    deref_hdt/3, % +S, +P, +O
    deref_iri/1  % +Iri
  ]
).

/** <module> Dereference application

@author Wouter Beek
@author Niels Ockeloen
@version 2016/04
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(hdt)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(os/thread_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_error)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(thread)).
:- use_module(library(zlib)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- rdf_meta
   deref_hdt(r, r, o).

:- dcg_ext:set_setting(tab_size, 2).

:- debug(deref(flag)).





%! deref_all is det.
%! deref_all(+N) is det.

deref_all :-
  default_number_of_threads(N),
  deref_all(N).


deref_all(N) :-
  flag(deref, _, 1),
  setup_call_cleanup(
    (
      gzopen('/scratch/lodlab/crawls/iri.gz', read, In),
      open('/scratch/lodlab/crawls/deref.nt', write, Out)
    ),
    deref_all(In, Out, N),
    (
      close(Out),
      close(In)
    )
  ).


deref_all(In, Out, N) :-
  % Remove the first line which contains RocksDB rubbish.
  read_line_to_codes(In, _),
  deref_all_nonfirst(In, Out, N).


deref_all_nonfirst(In, Out, N) :-
  read_lines(In, N, Lines),
  (   Lines == []
  ->  true
  ;   concurrent_maplist(deref_line(Out), Lines),
      deref_all_nonfirst(In, Out, N)
  ).



deref_hdt(S) :-
  var(S), !,
  hdt_file0(HdtFile),
  hdt_open(Hdt, HdtFile),
  distinct(S, deref_subject0(Hdt, S)),
  deref_hdt(S).
deref_hdt(S) :-
  findall(Triple, deref_triple0(S, Triple), Triples),
  rdf_print_triples(Triples).

deref_subject0(Hdt, S) :-
  hdt_subject(Hdt, S),
  deref_hdt(S, deref:responses, _).

deref_triple0(S, Triple) :-
  deref_hdt(S, P, O),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(O),
      deref_triple0(O, Triple)
  ).

hdt_file0('/scratch/lodlab/crawls/deref.hdt').


deref_hdt(S, P, O) :-
  hdt_file0(HdtFile),
  (   exists_file(HdtFile)
  ->  true
  ;   NTriplesFile = '/scratch/lodlab/crawls/deref.nt',
      hdt_create_from_file(HdtFile, NTriplesFile, [])
  ),
  setup_call_cleanup(
    hdt_open(Hdt, HdtFile),
    hdt_search(Hdt, S, P, O),
    hdt_close(Hdt)
  ).



deref_graph(Out, Iri, G, M, _) :-
  rdf_print_graph(G), nl,

  % Number of occurrences in the subject position
  rdf_aggregate_all(count, rdf(Iri, _, _, G), NumSubjects),
  rdf_store(Out, Iri, deref:number_of_subjects, NumSubjects^^xsd:nonNegativeInteger),
  debug(deref, "Appears in subject position: ~D", [NumSubjects]),

  % Number of occurrences in the predicate position
  rdf_aggregate_all(count, rdf(_, Iri, _, G), NumPredicates),
  rdf_store(Out, Iri, deref:number_of_subjects, NumPredicates^^xsd:nonNegativeInteger),
  debug(deref, "Appears in predicate position: ~D", [NumPredicates]),

  % Number of occurrences in the object position
  rdf_aggregate_all(count, rdf(_, _, Iri, G), NumObjects),
  rdf_store(Out, Iri, deref:number_of_subjects, NumObjects^^xsd:nonNegativeInteger),
  debug(deref, "Appears in object position: ~D", [NumObjects]),

  % Serialization format
  get_dict('llo:rdf_format', M, RdfFormat0),
  Context = _{formats: 'http://www.w3.org/ns/formats/'},
  jsonld_expand_term(Context, RdfFormat0, RdfFormat),
  rdf_store(Out, Iri, deref:serialization_format, RdfFormat),

  % Metadata
  store_metadata(Out, Iri, M).


store_http_metadata0(Out, M, B) :-
  rdf_create_bnode(B),
  maplist(store_http_headers0(Out, B), M.'llo:headers'),
  rdf_store(Out, B, deref:iri, M.'llo:iri'^^xsd:anyURI),
  rdf_store(Out, B, deref:status, M.'llo:status'^^xsd:nonNegativeInteger),
  rdf_store(Out, B, deref:time, M.'llo:time'^^xsd:float),
  Major-Minor = M.'llo:version',
  format(string(Version), "~d.~d", [Major,Minor]),
  rdf_store(Out, B, deref:version, Version^^xsd:string).


store_http_headers0(Out, B, Key-Vals) :-
  maplist(store_http_header0(Out, B, Key), Vals).


store_http_header0(Out, B, Key, Val) :-
  rdf_global_id(deref:Key, P),
  rdf_store(Out, B, P, Val^^xsd:string).


store_metadata(Out, Iri, M) :-
  get_dict('llo:http_communication', M, L1),
  maplist(store_http_metadata0(Out), L1, L2),
  rdf_store_list(Out, L2, RdfList),
  rdf_store(Out, Iri, deref:responses, RdfList).



deref_iri(Iri) :-
  rdf_snap((
    deref_iri(Iri, Iri),
    rdf_print_graph(Iri),
    rdf_unload_graph(Iri)
  )).


deref_iri(Out, Iri) :-
  % Debug index
  flag(deref, X, X + 1),
  debug(deref(flag), "~D  ~t  ~a", [X,Iri]),
  (X = -1 -> gtrace ; true),
  Opts = [base_iri(Iri),triples(NumTriples),quads(NumQuads)],
  catch(rdf_call_on_graph(Iri, deref_graph(Out, Iri), Opts), E, true), !,
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
  ).
% O NO!
deref_iri(Out, Iri) :-
  gtrace,
  deref_iri(Out, Iri).


deref_iri(Out, Iri, NumDocs) :-
  deref_iri(Out, Iri),
  rdf_store(Out, Iri, deref:number_of_documents, NumDocs^^xsd:nonNegativeInteger),
  debug(deref, "Number of documents: ~D", [NumDocs]).


deref_iri(NumDocs, Iri) -->
  integer(NumDocs), " ", rest(Cs), {atom_codes(Iri, Cs)}.



deref_line(Out, Cs) :-
  phrase(deref_iri(NumDocs, Iri), Cs),
  deref_iri(Out, Iri, NumDocs).





% DEBUG %

iri('http://%20ossiane.blog@studio-amarante.com/').
iri('http://dbpedia.org/resource/Tim_Berners-Lee').
iri('http://%5Cdementialcore.blogspot.com').





% HELPERS %

read_lines(_, 0, []) :- !.
read_lines(In, N1, L) :-
  read_line_to_codes(In, H),
  (   H == end_of_file
  ->  L = []
  ;   N2 is N1 - 1,
      L = [H|T],
      read_lines(In, N2, T)
  ).
