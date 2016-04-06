:- module(
  gen_ntuples,
  [
    gen_nquad/4,   % +S, +P, +O, +G
    gen_ntriple/3, % +S, +P, +O
    gen_ntriple/4, % +S, +P, +O, +G
    gen_ntuples/4, % ?S, ?P, ?O, ?G
    gen_ntuples/5  % ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Generate N-Quads

@author Wouter Beek
@version 2016/03-2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(option)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)). % Private
:- use_module(library(uri)).

:- rdf_meta
   gen_nquad(r, r, o, r),
   gen_ntriple(r, r, o),
   gen_ntriple(r, r, o, r),
   gen_ntuple(+, r, r, o, r),
   gen_ntuple(+, +, r, r, o, r),
   gen_ntuples(r, r, o, r, +).

:- thread_local
   bnode_map/2.




gen_nquad(S, P, O, G) :-
  gen_empty_state(State),
  gen_ntuple(State, S, P, O, G).



gen_ntriple(S, P, O) :-
  gen_nquad(S, P, O, default).


gen_ntriple(S, P, O, _) :-
  gen_ntriple(S, P, O).



gen_ntuple(State, S, P, O, G) :-
  gen_subject(State, S),
  put_char(' '),
  gen_predicate(P),
  put_char(' '),
  gen_object(State, O),
  put_char(' '),
  (   rdf_default_graph(G)
  ->  dict_inc(triples, State)
  ;   gen_graph(G),
      put_char(' '),
      dict_inc(quads, State)
  ),
  put_char(.),
  put_code(10).


gen_ntuple(Sink, State, S, P, O, G) :-
  with_output_to(Sink, gen_ntuple(State, S, P, O, G)).



%! gen_ntuples(?S, ?P, ?O, ?G) is det.
%! gen_ntuples(?S, ?P, ?O, ?G, +Opts) is det.
% The following options are supported:
%   - quads(-nonneg)
%   - triples(-nonneg)
%   - tuples(-nonneg)
%   - warn(+stream)

gen_ntuples(S, P, O, G) :-
  gen_ntuples(S, P, O, G, []).

gen_ntuples(S, P, O, G, Opts) :-
  setup_call_cleanup(
    gen_ntuples_begin(State, Opts),
    (
      aggregate_all(set(S), rdf(S, P, O, G), Ss),
      maplist(gen_ntuples_for_subject(State, P, O, G), Ss)
    ),
    gen_ntuples_end(State, Opts)
  ).




% STAGE SETTING %

gen_empty_state(_{bnode: 0, bprefix: '_:', quads: 0, triples: 0}).


%! gen_ntuples_begin(-State, +Opts) is det.

gen_ntuples_begin(State2, Opts) :-
  gen_empty_state(State1),
  (option(warn(Warn), Opts) -> put_dict(warn, State1, Warn, State2) ; State2 = State1),
  (   option(base_iri(BaseIri), Opts)
  ->  uri_components(BaseIri, uri_components(Scheme,Auth,Path0,_,_)),
      atom_ending_in(Path0, '#', Suffix),
      atomic_list_concat(['','.well-known',genid,Suffix], /, Path),
      uri_components(BPrefix, uri_components(Scheme,Auth,Path,_,_)),
      nb_set_dict(bprefix, State2, BPrefix)
  ;   true
  ).


%! gen_ntuples_end(+State, +Opts) is det.
% The following options are supported:
%   - quads(-nonneg)
%   - triples(-nonneg)
%   - tuples(-nonneg)

gen_ntuples_end(State, Opts) :-
  option(quads(State.quads), Opts, _),
  option(triples(State.triples), Opts, _),
  NoTuples is State.triples + State.quads,
  option(tuples(NoTuples), Opts, _).




% AGGRREGATION %

gen_ntuples_for_subject(State, P, O, G, S) :-
  aggregate_all(set(P), rdf(S, P, O, G), Ps),
  maplist(gen_ntuples_for_predicate(State, O, G, S), Ps).


gen_ntuples_for_predicate(State, O, G, S, P) :-
  aggregate_all(set(O), rdf(S, P, O, G), Os),
  maplist(gen_ntuples_for_object(State, G, S, P), Os).


gen_ntuples_for_object(State, G, S, P, O) :-
  aggregate_all(set(G), rdf(S, P, O, G), Gs),
  maplist(gen_ntuple(State, S, P, O), Gs).




% TERMS BY POSITION %

gen_subject(State, B) :-
  rdf_is_bnode(B), !,
  gen_bnode(State, B).
gen_subject(_, Iri) :-
  gen_iri(Iri).


gen_predicate(P) :-
  gen_iri(P).


gen_object(State, B) :-
  rdf_is_bnode(B), !,
  gen_bnode(State, B).
gen_object(_, Iri) :-
  rdf_is_iri(Iri), !,
  gen_iri(Iri).
% Literal term comes last to support modern and legacy formats.
gen_object(State, Lit) :-
  gen_literal(State, Lit), !.


gen_graph(G) :-
  gen_iri(G).




% TERMS BY KIND %

gen_bnode(State, B) :-
  % Retrieve (existing) or create (new) a numeric blank node identifier.
  (bnode_map(B, Id) -> true ; dict_inc(bnode, State, Id)),
  atomic_concat(State.bprefix, Id, Name),
  write(Name).


gen_iri(Iri) :-
  turtle:turtle_write_uri(current_output, Iri).


gen_literal(_, V^^D) :- !,
  rdf_literal_lexical_form(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  gen_iri(D).
gen_literal(_, V@LTag) :- !,
  rdf_literal_lexical_form(V@LTag, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  format(current_output, '@~w', [LTag]).
