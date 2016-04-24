:- module(
  deref_hdt,
  [
    deref_hdt/1, % +S
    deref_hdt/3  % +S, +P, +O
  ]
).

/** <module> HDT dereference

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(hdt)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- rdf_meta
   deref_hdt(r, r, o).





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
