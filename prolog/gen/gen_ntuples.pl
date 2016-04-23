:- module(
  gen_ntuples,
  [
    gen_nquad/4,    % +S, +P, +O, +G
    gen_nquads/1,   % +Tuples
    gen_nquads/2,   % +Tuples,        +Opts
    gen_nquads/4,   % ?S, ?P, ?O,     +Opts
    gen_nquads/5,   % ?S, ?P, ?O, ?G, +Opts
    gen_ntriple/3,  % +S, +P, +O
    gen_ntriple/4,  % +S, +P, +O, +G
    gen_ntriples/1, % +Triples
    gen_ntriples/2, % +Triples,       +Opts
    gen_ntriples/4, % ?S, ?P, ?O      +Opts
    gen_ntriples/5  % ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Generate N-Tuples, i.e., N-Triples and N-Quads

@author Wouter Beek
@version 2016/03-2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(option)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)). % Private

:- rdf_meta
   gen_nquad(r, r, o, r),
   gen_nquads(r, r, o, +),
   gen_nquads(r, r, o, r, +),
   gen_ntriple(r, r, o),
   gen_ntriple(r, r, o, r),
   gen_ntriples(r, r, o, +),
   gen_ntriples(r, r, o, r, +),
   gen_ntuple(+, r, r, o, r),
   gen_ntuple(+, +, r, r, o, r),
   gen_ntuples(r, r, o, r, +).





% SINGULAR STATEMENTS %

%! gen_nquad(+S, +P, +O, +G) is det.
% Write a fully instantiated quadruple to current output.

gen_nquad(S, P, O, G) :-
  gen_one_ntuple(nquads, S, P, O, G).



%! gen_ntriple(+S, +P, +O) is det.
%! gen_ntriple(+S, +P, +O, +G) is det.
% Write a fully instantiated triple to current output.

gen_ntriple(S, P, O) :-
  gen_one_ntuple(ntriples, S, P, O, default).


gen_ntriple(S, P, O, _) :-
  gen_one_ntuple(ntriples, S, P, O, default).


gen_one_ntuple(Format, S, P, O, G) :-
  gen_empty_state(Format, State),
  gen_ntuple(State, S, P, O, G).





% MULTIPLE STATEMENTS %

%! gen_nquads(+Tuples) is det.
%! gen_nquads(+Tuples, +Opts) is det.
%! gen_nquads(?S, ?P, ?O, +Opts) is det.
%! gen_nquads(?S, ?P, ?O, ?G, +Opts) is det.

gen_nquads(Tuples) :-
  gen_nquads(Tuples, []).


gen_nquads(Tuples, Opts) :-
  gen_ntuples(Tuples, nquads, Opts).


gen_nquads(S, P, O, Opts) :-
  gen_nquads(S, P, O, _, Opts).


gen_nquads(S, P, O, G, Opts0) :-
  merge_options([rdf_format(nquads)], Opts0, Opts),
  gen_ntuples(S, P, O, G, Opts).



%! gen_ntriples(+Triples) is det.
%! gen_ntriples(+Triples, +Opts) is det.
%! gen_ntriples(?S, ?P, ?O, +Opts) is det.
%! gen_ntriples(?S, ?P, ?O, ?G, +Opts) is det.

gen_ntriples(Triples) :-
  gen_ntriples(Triples, []).


gen_ntriples(Triples, Opts) :-
  gen_ntuples(Triples, ntriples, Opts).


gen_ntriples(S, P, O, Opts) :-
  gen_ntriples(S, P, O, _, Opts).


gen_ntriples(S, P, O, G, Opts0) :-
  merge_options([rdf_format(ntriples)], Opts0, Opts),
  gen_ntuples(S, P, O, G, Opts).



%! gen_ntuple(+State, +Tuple) is det.
%! gen_ntuple(+State, +S, +P, +O, +G) is det.
%! gen_ntuple(+Sink, +State, +S, +P, +O, +G) is det.
% Low-level tuple writer.

gen_ntuple(State, rdf(S,P,O)) :- !,
  gen_ntuple(State, S, P, O, _).
gen_ntuple(State, rdf(S,P,O,G)) :-
  gen_ntuple(State, S, P, O, G).


gen_ntuple(State, S, P, O, G) :-
  gen_subject(S),
  put_char(' '),
  gen_predicate(P),
  put_char(' '),
  gen_object(O),
  put_char(' '),
  (   State.rdf_format == ntriples
  ->  dict_inc(triples, State)
  ;   rdf_default_graph(G)
  ->  dict_inc(triples, State)
  ;   gen_graph(G),
      put_char(' '),
      dict_inc(quads, State)
  ),
  put_char(.),
  put_code(10).


gen_ntuple(Sink, State, S, P, O, G) :-
  with_output_to(Sink, gen_ntuple(State, S, P, O, G)).



%! gen_ntuples(+Tuples, +Format, +Opts) is det.
%! gen_ntuples(?S, ?P, ?O, ?G, +Opts) is det.
% Options are passed to gen_ntuples_begin/2 and gen_ntuples_end/2.

gen_ntuples(Tuples, Format, Opts1) :-
  merge_options([rdf_format(Format)], Opts1, Opts2),
  setup_call_cleanup(
    gen_ntuples_begin(State, Opts2),
    maplist(gen_ntuple(State), Tuples),
    gen_ntuples_end(State, Opts2)
  ).


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

%! gen_emtpy_state(-State) is det.
%! gen_emtpy_state(+Format, -State) is det.

gen_empty_state(_{bnode: 0, bprefix: '_:', quads: 0, triples: 0}).


gen_empty_state(Format, State) :-
  gen_empty_state(State0),
  State = State0.put(_{rdf_format: Format}).



%! gen_ntuples_begin(-State, +Opts) is det.
% The following options are supported:
%   * base_iri(+iri)
%   * rdf_format(+oneof([nquads,ntriples]))
%     Default is `nquads`.
%   * warn(+stream)

gen_ntuples_begin(State2, Opts) :-
  % RDF serialization format
  option(rdf_format(Format), Opts, nquads),
  must_be(oneof([nquads,ntriples]), Format),
  gen_empty_state(Format, State1),
  
  % Stream for warnings
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
  ).



%! gen_ntuples_end(+State, +Opts) is det.
% The following options are supported:
%   * quads(-nonneg)
%   * triples(-nonneg)
%   * tuples(-nonneg)

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

gen_subject(B) :-
  rdf_is_bnode(B), !,
  gen_bnode(B).
gen_subject(Iri) :-
  gen_iri(Iri).



gen_predicate(P) :-
  gen_iri(P).



gen_object(B) :-
  rdf_is_bnode(B), !,
  gen_bnode(B).
gen_object(Iri) :-
  rdf_is_iri(Iri), !,
  gen_iri(Iri).
% Literal term comes last to support modern and legacy formats.
gen_object(Lit) :-
  gen_literal(Lit), !.



gen_graph(G) :-
  gen_iri(G).





% TERMS BY KIND %

gen_bnode(B) :-
  write(B).



gen_iri(Iri) :-
  turtle:turtle_write_uri(current_output, Iri).



gen_literal(V^^D) :- !,
  rdf_literal_lexical_form(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  gen_iri(D).
gen_literal(V@LTag) :- !,
  rdf_literal_lexical_form(V@LTag, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  format(current_output, '@~w', [LTag]).
