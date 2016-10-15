:- module(
  q_dataset_db,
  [
    q_create_dataset/3,        % +DRef, +DefGName, +Pairs
    q_create_dataset/4,        % +DRef, +DefGName, +Pairs, -D
    q_dataset/1,               % ?D
    q_dataset/3,               % ?D, ?DefGName, ?Pairs
    q_dataset_add_graph/2,     % +D, +NG
    q_dataset_add_graph/3,     % +D, +GName, +NG
    q_dataset_default_graph/2, % ?D, ?DefG
    q_dataset_graph/2,         % ?D, ?G
    q_dataset_graph/3,         % ?D, ?GName, ?G
    q_dataset_named_graph/2,   % ?D, ?G
    q_dataset_named_graph/3,   % ?D, ?GName, ?G
    q_rm_dataset/1,            % +D
    q_rm_datasets/0,
    q_set_default_graph/2      % +D, +DefGName
  ]
).

/** <module> Quine: Dataset DB

@author Wouter Beek
@version 2016/08-2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(persistency)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- initialization(db_attach('q_dataset.db', [])).

%! q_dataset(?D:atom, ?GName:atom, -Pairs:list(pair(atom))) is nondet.

:- persistent
   q_dataset(
     dataset:atom,
     default_graph:atom,
     named_graphs:list(pair(atom))
   ).

:- rdf_meta
   q_dataset(r),
   q_dataset_add_graph(r, r),
   q_dataset_default_graph(r, r),
   q_dataset_graph(r, r),
   q_dataset_graph(r, +, r),
   q_dataset_named_graph(r, r),
   q_dataset_named_graph(r, ?, r),
   q_rm_dataset(r).





%! q_create_dataset(+RefD, +DefG, +NGs) is det.
%! q_create_dataset(+RefD, +DefG, +NGs, -D) is det.

q_create_dataset(RefD, DefG, NGs) :-
  q_create_dataset(RefD, DefG, NGs, _).


q_create_dataset(RefD, DefG, NGs, D) :-
  q_dataset_iri(RefD, D),
  with_mutex(q_dataset, (
    (   q_dataset(D, _, _)
    ->  print_message(informational, "Dataset already exists.")
    ;   memberchk(DefG, NGs)
    ->  assert_q_dataset(D, DefG, NGs)
    ;   print_message(
          informational,
          "The default graph is not part of the named graphs."
        )
    )
  )).



%! q_dataset(+D) is semidet.
%! q_dataset(-D) is nondet.

q_dataset(D) :-
  q_dataset(D, _, _).



%! q_dataset_add_graph(+D, +NG) is det.
%! q_dataset_add_graph(+D, +GName, +NG) is det.

q_dataset_add_graph(D, NG) :-
  q_dataset_add_graph(D, NG, NG).


q_dataset_add_graph(D, GName, NG) :-
  with_mutex(q_dataset, (
    (   q_dataset(D, GName0, Pairs1)
    ->  retractall_q_dataset(D, GName0, Pairs1),
        ord_add_element(Pairs1, GName-NG, Pairs2)
    ;   GName0 = GName,
        Pairs2 = [GName-NG]
    ),
    assert_q_dataset(D, GName0, Pairs2)
  )).



%! q_dataset_default_graph(?D, ?DefG) is nondet.

q_dataset_default_graph(D, DefG) :-
  q_dataset(D, GName, Pairs),
  memberchk(GName-DefG, Pairs).



%! q_dataset_graph(+D, +G) is semidet.
%! q_dataset_graph(+D, -G) is multi.
%! q_dataset_graph(-D, +G) is nondet.
%! q_dataset_graph(-D, -G) is nondet.
%! q_dataset_graph(+D, +GName, -G) is semidet.

q_dataset_graph(D, G) :-
  q_dataset(D, _, Pairs),
  member(_-G, Pairs).
  

q_dataset_graph(D, GName, G) :-
  q_dataset_graph(D, _, Pairs),
  memberchk(GName-G, Pairs).



%! q_dataset_named_graph(?D, ?NG) is nondet.
%! q_dataset_named_graph(?D, ?GName, ?NG) is nondet.

q_dataset_named_graph(D, NG) :-
  q_dataset_named_graph(D, _, NG).


q_dataset_named_graph(D, GName, NG) :-
  q_dataset(D, GName0, Pairs),
  member(GName-NG, Pairs),
  GName \== GName0.



%! q_rm_dataset(+D) is det.

q_rm_dataset(D) :-
  retractall_q_dataset(D, _, _).



%! q_rm_dataset is det.

q_rm_datasets :-
  forall(
    q_dataset(D),
    q_rm_dataset(D)
  ).



%! q_set_default_graph(+D, +DefGName) is det.

q_set_default_graph(D, DefGName) :-
  with_mutex(q_dataset, (
    q_dataset(D, DefGName0, Pairs),
    (   DefGName0 == DefGName
    ->  print_message(informational, "Default graph unchanged.")
    ;   memberchk(DefGName-_, Pairs)
    ->  retractall_q_dataset(D, DefGName0, Pairs),
        assert_q_dataset(D, DefGName, Pairs)
    ;   print_message(informational, "Cannot set default graph.")
    )
  )).
