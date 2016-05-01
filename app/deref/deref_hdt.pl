:- module(
  deref_hdt,
  [
    deref/1,               % ?Iri
    deref/3,               % ?S, ?P, ?O
    deref_header/3,        % ?Iri, ?P, ?O
    file_stmts/0,
    iri_deref_length/2,    % ?Iri, ?NumReqs
    iri_status/2,          % ?Iri, ?Code
    overview_archive/0,
    overview_format/0,
    overview_lone_bnodes/0,
    overview_position/0,
    overview_socket_error/0,
    overview_status/0,
    overview_stmts/0,
    overview_time/0
  ]
).

/** <module> HDT dereference

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(csv_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(debug)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

:- debug(deref(hdt)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- meta_predicate
    hdt_goal0(1).

:- rdf_meta
   deref(r),
   deref(r, r, o),
   deref_header(r, r, o).





% OVERVIEWS %

overview_archive :-
  hdt_goal0(overview_archive0).


overview_archive0(Hdt) :-
  rdf_aggregate_all(
    set(Error),
    hdt0(Hdt, _, deref:archive_error, Error),
    Errors
  ),
  findall(N-Error, (
    member(Error, Errors),
    rdf_aggregate_all(count, hdt0(Hdt, _, deref:archive_error, Error), N)
  ), Pairs),
  finish_pairs(["Number of IRIs","Archive error"], Pairs).



overview_format :-
  hdt_goal0(overview_format0).


overview_format0(Hdt) :-
  aggregate_all(set(Format), iri_format0(Hdt, _, Format), Formats),
  forall(member(Format, Formats), flag(Format, _, 0)),
  forall(iri_format0(Hdt, _, Format), flag(Format, X, X + 1)),
  findall(N-Format, (member(Format, Formats), flag(Format, N, N)), Pairs),
  finish_pairs(["Number of IRIs","Content type"], Pairs).


iri_format0(Hdt, Iri, Format) :-
  hdt0(Hdt, Iri, deref:serialization_format, Format).



overview_lone_bnodes :-
  hdt_goal0(overview_lone_bnodes0).


overview_lone_bnodes0(Hdt) :-
  aggregate_all(set(M), iri_lone_bnodes0(Hdt, _, M), Ms),
  findall(N-M, (
    member(M, Ms),
    aggregate_all(count, iri_lone_bnodes0(Hdt, _, M), N)
  ), Pairs),
  finish_pairs(["Number of IRIs","Number of lone blank nodes"], Pairs).


iri_lone_bnodes0(Hdt, Iri, N) :-
  hdt0(Hdt, Iri, deref:lone_bnodes, N^^xsd:nonNegativeInteger).
  


overview_position :-
  hdt_goal0(overview_position0).


overview_position0(Hdt) :-
  forall(hdt0(Hdt, Iri, deref:serialization_format, _), (
    (hdt0(Hdt, Iri, deref:number_of_subjects, 0^^xsd:nonNegativeInteger)->S='';S=s),
    (hdt0(Hdt, Iri, deref:number_of_predicates, 0^^xsd:nonNegativeInteger)->P='';P=p),
    (hdt0(Hdt, Iri, deref:number_of_objects, 0^^xsd:nonNegativeInteger)->O='';O=o),
    atomic_list_concat([S,P,O], Flag),
    flag(Flag, X, X + 1)
  )),
  findall(N-Flag, (
    member(S, ['',s]),
    member(P, ['',p]),
    member(O, ['',o]),
    atomic_list_concat([S,P,O], Flag),
    flag(Flag, N, 0)
  ), Pairs),
  finish_pairs(["Number of IRIs","Positions"], Pairs).

  



overview_socket_error :-
  hdt_goal0(overview_socket_error0).


overview_socket_error0(Hdt) :-
  findall(N-Error, socket_error0(Hdt, Error, N), Pairs),
  finish_pairs(["Number of IRIs","Socket errors"], Pairs).


socket_error0(Hdt, Error, N) :-
  distinct(Error, hdt0(Hdt, _, deref:socket_error, Error)),
  rdf_aggregate_all(count, hdt0(Hdt, _, deref:socket_error, Error), N).



overview_status :-
  hdt_goal0(overview_status0).


overview_status0(Hdt) :-
  findall(N-Code, deref_status0(Hdt, Code, N), Pairs),
  finish_pairs(["Number of replies","HTTP status code"], Pairs).


deref_status0(Hdt, Code, N) :-
  distinct(Code, iri_status0(Hdt, _, Code)),
  rdf_aggregate_all(count, iri_status0(Hdt, _, Code), N).



overview_stmts :-
  hdt_goal0(overview_stmts0).


overview_stmts0(Hdt) :-
  aggregate_all(set(M), iri_number_of_stmts0(Hdt, _, M), Ms),
  findall(N-M, (
    member(M, Ms),
    aggregate_all(count, iri_number_of_stmts0(Hdt, _, M), N)
  ), Pairs),
  finish_pairs(["Number of replies","Number of statements"], Pairs).



file_stmts :-
  hdt_goal0(file_stmts0).


file_stmts0(Hdt) :-
  aggregate_all(set(M), iri_number_of_stmts0(Hdt, _, M), Ms),
  forall(member(M, Ms), flag(M, _, 0)),
  forall(iri_number_of_stmts0(Hdt, _, N), flag(N, X, X + 1)),
  findall(Bucket-N, (
    member(M, Ms), Bucket is ceil(M / 10), flag(M, N, 0)
  ), Pairs),
  group_pairs_by_key(Pairs, GroupedPairs),
  maplist(sum_value, GroupedPairs, SummedPairs),
  maplist(pair_row, SummedPairs, Rows),
  csv_to_file('stmts.csv', Rows).



overview_time :-
  hdt_goal0(overview_time0).


overview_time0(Hdt) :-
  flag(total, _, 0),
  flag(deref, _, 0),
  iri_time0(Hdt, _, Total0, Deref0),
  Deref0 > 0,
  flag(total, Total, Total + Total0),
  flag(deref, Deref, Deref + Deref0),
  fail.
overview_time0(_) :-
  flag(total, Total, Total),
  flag(deref, Deref, Deref),
  format(user_output, "Total: ~w~nDeref: ~w~n", [Total,Deref]).


iri_time0(Hdt, Iri, TotalTime, DerefTime) :-
  findall(Time, deref_header0(Hdt, Iri, deref:time, Time^^xsd:float), Times),
  sum_list(Times, TotalTime),
  append(DerefTimes, [_], Times),
  sum_list(DerefTimes, DerefTime).






% API %

%! deref(+S) is det.
%! deref(-S) is multi.
% Print the full result of an IRI dereference.

deref(S) :-
  hdt_goal0({S}/[Hdt]>>deref0(Hdt, S)).


deref0(Hdt, S) :-
  var(S), !,
  distinct(S, (
    hdt_subject(Hdt, S),
    rdf_is_iri(S),
    writeln(S),
    hdt0(Hdt, S, deref:responses, _)
  )),
  deref0(Hdt, S).
deref0(Hdt, S) :-
  findall(Triple, deref_triple0(Hdt, S, Triple), Triples),
  rdf_print_triples(Triples).


deref_triple0(Hdt, S, Triple) :-
  hdt0(Hdt, S, P, O),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(O),
      deref_triple0(Hdt, O, Triple)
  ).



%! deref(?S, ?P, ?O) is nondet.

deref(S, P, O) :-
  hdt_goal0({S,P,O}/[Hdt]>>hdt0(Hdt, S, P, O)).



%! deref_header(?Iri, ?P, ?O) is nondet.

deref_header(Iri, P, O) :-
  hdt_goal0({Iri,P,O}/[Hdt]>>deref_header0(Hdt, Iri, P, O)).


deref_header0(Hdt, Iri, P, O) :-
  hdt0(Hdt, Iri, deref:responses, Reqs),
  hdt_member0(Hdt, Req, Reqs),
  hdt0(Hdt, Req, P, O).



iri_deref_length(Iri, N) :-
  hdt_goal0({Iri,N}/[Hdt]>>iri_deref_length0(Hdt, Iri, N)).


iri_deref_length0(Hdt, Iri, NumReqs) :-
  hdt0(Hdt, Iri, deref:responses, _),
  rdf_aggregate_all(
    count,
    deref_header0(Hdt, Iri, deref:time, _^^xsd:float),
    NumReqs
  ).



iri_number_of_stmts0(Hdt, Iri, N) :-
  hdt0(Hdt, Iri, deref:number_of_triples, N1^^xsd:nonNegativeInteger),
  hdt0(Hdt, Iri, deref:number_of_quads, N2^^xsd:nonNegativeInteger),
  N is N1 + N2.



iri_status(Iri, Code) :-
  hdt_goal0({Iri,Code}/[Hdt]>>iri_status0(Hdt, Iri, Code)).


iri_status0(Hdt, Iri, Code) :-
  deref_header0(Hdt, Iri, deref:status, Code^^xsd:nonNegativeInteger).





% SETTINGS %

hdt_file0('/ssd/lodlab/wouter/deref/deref.hdt').
nt_file0('/ssd/lodlab/wouter/deref/deref.nt').





% HELPERS %

finish_pairs(HeaderRow, Pairs) :-
  desc_pairs(Pairs, SortedPairs),
  maplist(pair_list, SortedPairs, Rows),
  dcg_with_output_to(user_output, dcg_table([head(HeaderRow)|Rows])).



%! hdt_goal0(:Goal_1) is det.

hdt_goal0(Goal_1) :-
  hdt_prepare0(File),
  hdt_goal(File, Goal_1).


hdt_prepare0(File) :-
  hdt_file0(File),
  (   exists_file(File)
  ->  true
  ;   nt_file0(File0),
      hdt_create_from_file(File, File0, [])
  ).
