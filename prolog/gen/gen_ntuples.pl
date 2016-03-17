:- module(
  gen_ntuples,
  [
    gen_nquad/4,   % +S, +P, +O, +G
    gen_ntriple/3, % +S, +P, +O
    gen_ntuple/7,  % +BPrefix, +CT, +CQ, +S, +P, +O, +G
    gen_ntuple/8,  % +Sink, +BPrefix, +CT, +CQ, +S, +P, +O, +G
    gen_ntuples/5  % ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Generate N-Quads

@author Wouter Beek
@version 2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(os/thread_counter)).
:- use_module(library(pl/pl_term)).
:- use_module(library(rdf/rdf_debug)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)). % Private
:- use_module(library(uri)).

:- thread_local
   bnode_map/2.




gen_nquad(S, P, O, G) :-
  gen_ntuple('_:', _, _, _, S, P, O, G).



gen_ntriple(S, P, O) :-
  gen_nquad(S, P, O, default).



gen_ntuple(BPrefix, TC, QC, S, P, O, G) :-
  gen_subject(BPrefix, S),
  put_char(' '),
  gen_predicate(P),
  put_char(' '),
  gen_object(BPrefix, O),
  put_char(' '),
  (   rdf_default_graph(G)
  ->  increment_thread_counter(TC)
  ;   gen_graph(G),
      put_char(' '),
      increment_thread_counter(QC)
  ),
  put_char(.),
  put_code(10).



gen_ntuple(Sink, BPrefix, TC, QC, S, P, O, G) :-
  with_output_to(Sink, gen_ntuple(BPrefix, TC, QC, S, P, O, G)).



%! gen_ntuples(?S, ?P, ?O, ?G, +Opts) is det.
% The following options are supported:
%   - quads(-nonneg)
%   - triples(-nonneg)
%   - tuples(-nonneg)

gen_ntuples(S, P, O, G, Opts) :-
  gen_ntuples_begin(BPrefix, TC, QC, Opts),
  aggregate_all(set(S), rdf(S, P, O, G), Ss),
  maplist(gen_ntuples_for_subject(BPrefix, TC, QC, P, O, G), Ss),
  gen_ntuples_end(TC, QC, Opts).




% STAGE SETTING %

%! gen_ntuples_begin(-BPrefix, -TripleleCounter, -QuadCounter, +Opts) is det.

gen_ntuples_begin(BPrefix, triples, quads, Opts) :-
  create_thread_counter(triples),
  create_thread_counter(quads),
  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (   option(base_iri(BaseIri), Opts)
  ->  uri_components(BaseIri, uri_components(Scheme,Auth,Path0,_,_)),
      atom_ending_in(Path0, '#', Suffix),
      atomic_list_concat(['','.well-known',genid,Suffix], /, Path),
      uri_components(BPrefix, uri_components(Scheme,Auth,Path,_,_))
  ;   BPrefix = '_:'
  ),
  create_thread_counter(bnode_map).


%! gen_ntuples_end(+TripleCounter, +QuadCounter, +Opts) is det.
% The following options are supported:
%   - quads(-nonneg)
%   - triples(-nonneg)
%   - tuples(-nonneg)

gen_ntuples_end(TC, QC, Opts) :-
  % Triples.
  delete_thread_counter(TC, NoTriples),
  option(triples(NoTriples), Opts, _),
  % Quads.
  delete_thread_counter(QC, NoQuads),
  option(quads(NoQuads), Opts, _),
  % Tuples.
  NoTuples is NoTriples + NoQuads,
  option(tuples(NoTuples), Opts, _),
  % Blank node map.
  retractall(bnode_map(_,_)),
  delete_thread_counter(bnode_map).




% AGGRREGATION %

gen_ntuples_for_subject(BPrefix, TC, QC, P, O, G, S) :-
  aggregate_all(set(P), rdf(S, P, O, G), Ps),
  maplist(gen_ntuples_for_predicate(BPrefix, TC, QC, O, G, S), Ps).


gen_ntuples_for_predicate(BPrefix, TC, QC, O, G, S, P) :-
  aggregate_all(set(O), rdf(S, P, O, G), Os),
  maplist(gen_ntuples_for_object(BPrefix, TC, QC, G, S, P), Os).


gen_ntuples_for_object(BPrefix, TC, QC, G, S, P, O) :-
  aggregate_all(set(G), rdf(S, P, O, G), Gs),
  maplist(gen_ntuple(BPrefix, TC, QC, S, P, O), Gs).




% TERMS BY POSITION %

gen_subject(BPrefix, B) :-
  rdf_is_bnode(B), !,
  gen_bnode(BPrefix, B).
gen_subject(_, Iri) :-
  gen_iri(Iri).


gen_predicate(P) :-
  gen_iri(P).


gen_object(BPrefix, B) :-
  rdf_is_bnode(B), !,
  gen_bnode(BPrefix, B).
gen_object(_, Iri) :-
  rdf_is_iri(Iri), !,
  gen_iri(Iri).
% Literal term comes last to support modern and legacy formats.
gen_object(_, Lit) :-
  gen_literal(Lit), !.


gen_graph(G) :-
  gen_iri(G).




% TERMS BY KIND %

gen_bnode(Prefix, B) :-
  with_mutex(bnode_map, (
    % Retrieve (existing) or create (new) a numeric blank node identifier.
    (   bnode_map(B, Id)
    ->  true
    ;   increment_thread_counter(bnode_map, Id),
        assert(bnode_map(B, Id))
    )
  )),
  atomic_concat(Prefix, Id, Name),
  write(Name).


gen_iri(Iri) :-
  turtle:turtle_write_uri(current_output, Iri).


gen_literal(V^^D) :- !,
  rdf_literal_lexical_form(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  turtle:turtle_write_uri(current_output, D).
gen_literal(V@LTag) :- !,
  rdf_literal_lexical_form(V@LTag, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  format(current_output, '@~w', [LTag]).
% Literal legacy representations.
gen_literal(Lit0) :-
  rdf_legacy_literal_components(Lit0, D, Lex0, LTag0),
  rdf11:post_object(Lit, Lit0),
  rdf_literal_components(Lit, D, Lex, LTag),
  (   Lex \== Lex0
  ->  increment_thread_counter(rdf_warning),
      threadsafe_format(warn, "~w~n", [error(non_canonical_lex(D,Lex),gen_ntuples)])
  ;   true
  ),
  (   ground(LTag0),
      LTag \== LTag0
  ->  increment_thread_counter(rdf_warning),
      threadsafe_format(warn, "~w~n", [error(non_canonical_ltag(LTag),gen_ntuples)])
  ;   true
  ),
  gen_literal(Lit).
