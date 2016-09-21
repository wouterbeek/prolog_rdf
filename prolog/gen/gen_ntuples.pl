:- module(
  gen_ntuples,
  [
    call_to_nquads/2,     % +Sink, :Goal_2
    call_to_nquads/3,     % +Sink, :Goal_2, +Opts
    call_to_ntriples/2,   % +Sink, :Goal_2
    call_to_ntriples/3,   % +Sink, :Goal_2, +Opts
    call_to_ntuples/2,    % +Sink, :Goal_2
    call_to_ntuples/3,    % +Sink, :Goal_2, +Opts
    gen_ntuple/3,         % +Tuple, +State, +Out
    gen_ntuple/5,         % +S, +P, +O, +State, +Out
    gen_ntuple/6,         % +S, +P, +O, +G, +State, +Out
    gen_ntuples/3,        % +Tuples, +State, +Out
    gen_ntuples/4,        % ?M, ?G, +State, +Out
    gen_ntuples/7,        % ?M, ?S, ?P, ?O, ?G, +State, +Out
    ntuples_media_type/2, % ?MT, ?Format
    write_nquad/2,        % +Sink, +Quad
    write_nquad/5,        % +Sink, +S, +P, +O, +G
    write_ntriple/2,      % +Sink, +Triple
    write_ntriple/4       % +Sink, +S, +P, +O
  ]
).

/** <module> Generate N-Tuples, i.e., N-Triples and N-Quads

The following debug flags are used:

  * gen_ntuples

@author Wouter Beek
@version 2016/03-2016/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).

:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/io)).
:- use_module(library(q/q_bnode_map)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle), []).
:- use_module(library(yall)).

:- meta_predicate
    call_to_nquads(+, 2),
    call_to_nquads(+, 2, +),
    call_to_ntriples(+, 2),
    call_to_ntriples(+, 2, +),
    call_to_ntuples(+, 2),
    call_to_ntuples(+, 2, +).

:- rdf_meta
   call_to_nquads(+, t, +),
   call_to_ntriples(+, t, +),
   gen_ntuple(t, +, +),
   gen_ntuple(r, r, o, +, +),
   gen_ntuple(r, r, o, r, +, +),
   gen_ntuples(t, +, +),
   gen_ntuples(?, r, +, +),
   gen_ntuples(?, r, r, o, r, +, +),
   write_nquad(+, t),
   write_nquad(+, r, r, o, r),
   write_ntriple(+, t),
   write_ntriple(+, r, r, o).





%! call_to_nquads(+Sink, :Goal_2) is det.
%! call_to_nquads(+Sink, :Goal_2, +Opts) is det.
%
% Wrapper around call_to_ntuples/[2,3] where the RDF serialization
% format is set to N-Quads.

call_to_nquads(Sink, Goal_2) :-
  call_to_nquads(Sink, Goal_2, []).


call_to_nquads(Sink, Goal_2, Opts1) :-
  merge_options([rdf_format(nquads)], Opts1, Opts2),
  call_to_ntuples(Sink, Goal_2, Opts2).



%! call_to_ntriples(+Sink, :Goal_2) is det.
%! call_to_ntriples(+Sink, :Goal_2, +Opts) is det.
%
% Wrapper around call_to_ntuples/[2,3] where the RDF serialization
% format is set to N-Triples.

call_to_ntriples(Sink, Goal_2) :-
  call_to_ntriples(Sink, Goal_2, []).


call_to_ntriples(Sink, Goal_2, Opts1) :-
  merge_options([rdf_format(ntriples)], Opts1, Opts2),
  call_to_ntuples(Sink, Goal_2, Opts2).



%! call_to_ntuples(+Sink, :Goal_2) is det.
%! call_to_ntuples(+Sink, :Goal_2, +Opts) is det.
%
% Stage-setting for writing N-Tuples (N-Triples or N-Quads).  Tuples
% are written to Sink which is one of the sinks supported by
% open_any/5.
%
% Goal_2 is called in the following way: `call(Goal_2, State, Out)`,
% where State is a dictionary that records the state of writing tuples
% (format, number of tuples, etc.) and where Out is a writable stream.
%
% The following options are supported:
%
%   * base_iri(+iri) The base IRI against which relative IRIs are
%   resolved.
%
%   * quads(-nonneg) The number of written quads.
%
%   * rdf_format(+oneof([nquads,ntriples])) The RDF serialization
%   format that is used.  Possible values are `nquads` (default) for
%   N-Quads 1.1 and `ntriples` for N-Triples 1.1.
%
%   * triples(-nonneg) The number of written triples.
%
%   * tuples(-nonneg) The number of written tuples.
%
%   * warn(+stream) The output stream, if any, where warnings are
%   written to.
%
%   * Other options are written to call_to_stream/3.

call_to_ntuples(Sink, Goal_2) :-
  call_to_ntuples(Sink, Goal_2, []).


call_to_ntuples(Sink, Mod:Goal_2, Opts) :-
  setup_call_cleanup(
    gen_ntuples_begin(State, Opts),
    (
      Goal_2 =.. Comps1,
      append(Comps1, [State], Comps2),
      Goal_1 =.. Comps2,
      call_to_stream(Sink, Mod:Goal_1, Opts)
    ),
    gen_ntuples_end(State, Opts)
  ).



%! gen_ntuple(+Tuple, +State, +Out) is det.
%! gen_ntuple(+S, +P, +O, +State, +Out) is det.
%! gen_ntuple(+S, +P, +O, +G, +State, +Out) is det.

gen_ntuple(rdf(S,P,O), State, Out) :- !,
  gen_ntuple(S, P, O, State, Out).
gen_ntuple(rdf(S,P,O,G), State, Out) :- !,
  gen_ntuple(S, P, O, G, State, Out).


gen_ntuple(S, P, O, State, Out) :-
  gen_ntuple(S, P, O, _, State, Out).


gen_ntuple(S, P, O, G, State, Out) :-
  with_output_to(Out, (
    gen_subject(State, S),
    put_char(' '),
    gen_predicate(P),
    put_char(' '),
    gen_object(State, O),
    put_char(' '),
    (   State.rdf_format == ntriples
    ->  dict_inc(triples, State)
    ;   q_default_graph(G)
    ->  dict_inc(triples, State)
    ;   gen_graph(G),
        put_char(' '),
        dict_inc(quads, State)
    ),
    put_char(.),
    put_code(10)
  )).



%! gen_ntuples(+Tuples, +State, +Out) is det.
%! gen_ntuples(?M, ?G, +State, +Out) is det.
%! gen_ntuples(?M, ?S, ?P, ?O, ?G, +State, +Out) is det.

gen_ntuples(Tuples, State, Out) :-
  maplist({State,Out}/[Tuple]>>gen_ntuple(Tuple, State, Out), Tuples).


gen_ntuples(M, G, State, Out) :-
  gen_ntuples(M, _, _, _, G, State, Out).


gen_ntuples(M, S, P, O, G, State, Out) :-
  aggregate_all(set(S), q(M, S, P, O, G), Ss),
  maplist(gen_ntuples_for_subject0(State, Out, M, P, O, G), Ss).



%! ntuples_media_type(+MT, -Format) is semidet.
%
% Succeeds for media types supported by this module.

ntuples_media_type(application/'n-quads', nquads) :- !.
ntuples_media_type(application/'n-triples', ntriples).



%! write_nquad(+Sink, +Quad) is det.
%! write_nquad(+Sink, +S, +P, +O, +G) is det.

write_nquad(Sink, rdf(S,P,O,G)) :-
  write_nquad(Sink, S, P, O, G).


write_nquad(Sink, S, P, O, G) :-
  call_to_ntuples(Sink, gen_ntuple(S, P, O, G)).



%! write_ntriple(+Sink, +Triple) is det.
%! write_ntriple(+Sink, +S, +P, +O) is det.

write_ntriple(Sink, rdf(S,P,O)) :-
  write_ntriple(Sink, S, P, O).


write_ntriple(Sink, S, P, O) :-
  call_to_ntuples(Sink, gen_ntuple(S, P, O)).





% STAGE SETTING %

%! gen_ntuples_begin(-State, +Opts) is det.

gen_ntuples_begin(State2, Opts) :-
  option(rdf_format(Format), Opts, nquads),
  State1 = _{
    bprefix: '_:',
    quads: 0,
    rdf_format: Format,
    triples: 0
  },
  % Stream to write warnings to, if any.
  (   option(warn(Warn), Opts)
  ->  State2 = State1.put(_{warn: Warn})
  ;   State2 = State1
  ),
  % Well-known IRI prefix for blank nodes
  (   option(base_iri(BaseIri), Opts)
  ->  iri_comps(BaseIri, uri_components(Scheme,Auth,Path0,_,_)),
      atom_ending_in(Path0, '#', Suffix),
      atomic_list_concat(['','.well-known',genid,Suffix], /, Path),
      iri_comps(BPrefix, uri_components(Scheme,Auth,Path,_,_)),
      nb_set_dict(bprefix, State2, BPrefix)
  ;   true
  ),
  indent_debug(gen_ntuples, "> Writing N-Tuples").



%! gen_ntuples_end(+State, +Opts) is det.

gen_ntuples_end(State, Opts) :-
  option(quads(State.quads), Opts, _),
  option(triples(State.triples), Opts, _),
  NoTuples is State.triples + State.quads,
  option(tuples(NoTuples), Opts, _),
  indent_debug(gen_ntuples, "< Written N-Tuples").





% AGGRREGATION %

gen_ntuples_for_subject0(State, Out, M, P, O, G, S) :-
  aggregate_all(set(P), q(M, S, P, O, G), Ps),
  maplist(gen_ntuples_for_predicate0(State, Out, M, O, G, S), Ps).



gen_ntuples_for_predicate0(State, Out, M, O, G, S, P) :-
  aggregate_all(set(O), q(M, S, P, O, G), Os),
  maplist(gen_ntuples_for_object0(State, Out, M, G, S, P), Os).



gen_ntuples_for_object0(State, Out, M, G, S, P, O) :-
  aggregate_all(set(G), q(M, S, P, O, G), Gs),
  maplist({S,P,O,State,Out}/[G]>>gen_ntuple(S, P, O, G, State, Out), Gs).





% TERMS BY POSITION %

gen_subject(State, B) :-
  q_is_bnode(B), !,
  gen_bnode(State, B).
gen_subject(_, Iri) :-
  gen_iri(Iri).



gen_predicate(P) :-
  gen_iri(P).



gen_object(State, B) :-
  q_is_bnode(B), !,
  gen_bnode(State, B).
gen_object(_, Iri) :-
  q_is_iri(Iri), !,
  gen_iri(Iri).
% Literal term comes last to support both modern (`rdf11`) and legacy
% (`rdf_db`) formats.
gen_object(_, Lit) :-
  gen_literal(Lit).



gen_graph(G) :-
  gen_iri(G).





% TERMS BY KIND %

gen_bnode(State, B) :-
  q_bnode_map(B, Name),
  write(State.bprefix),
  write(Name).



gen_iri(Iri) :-
  turtle:turtle_write_uri(current_output, Iri).



gen_literal(V^^D) :- !,
  q_literal_lex(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  gen_iri(D).
gen_literal(V@LTag) :- !,
  q_literal_lex(V@LTag, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  format(current_output, '@~w', [LTag]).
gen_literal(V) :-
  rdf_equal(xsd:string, D),
  gen_literal(V^^D).
