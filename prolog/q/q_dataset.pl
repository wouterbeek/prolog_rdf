:- module(
  q_dataset,
  [
  % DB
    qb_dataset/3,                  % +DRef, +DefGName, +Pairs
    qb_dataset/4,                  % +DRef, +DefGName, +Pairs, -D
    q_dataset/1,                   % ?D
    q_dataset/3,                   % ?D, ?DefGName, ?Pairs
    q_dataset_add_graph/2,         % +D, +NG
    q_dataset_add_graph/3,         % +D, +GName, +NG
    q_dataset_db_exists/0,
    q_dataset_default_graph/2,     % ?D, ?DefG
    q_dataset_graph/2,             % ?D, ?G
    q_dataset_graph/3,             % ?D, ?GName, ?G
    q_dataset_named_graph/2,       % ?D, ?G
    q_dataset_named_graph/3,       % ?D, ?GName, ?G
    q_dataset_set_default_graph/2, % +D, +DefGName
    q_rm_dataset/1,                % +D
    q_rm_dataset_graph/1,          % +G
    q_rm_dataset_graph/2,          % +D, +G
    q_rm_datasets/0,
  % API
    q_dataset_label/2,             % +D, -Lbl
    q_dataset_tree/4,              % ?M, +Order, +D, -Tree
    q_dataset_trees/3              % ?M, +Order, -Trees
  ]
).

/** <module> Quine: Dataset

@author Wouter Beek
@version 2016/08-2016/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(persistency)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(tree/s_tree)).
:- use_module(library(uri)).

:- setting(
     dataset_file,
     atom,
     '~/Data/source/q_dataset.db',
     "The file that stores Quine dataset assertions."
   ).

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
   q_dataset_label(r, -),
   q_dataset_named_graph(r, r),
   q_dataset_named_graph(r, ?, r),
   q_dataset_tree(?, +, r, -),
   q_rm_dataset(r).

:- initialization(q_dataset_db_init).

q_dataset_db_init :-
  setting(dataset_file, File),
  db_attach(File, []).





%! qb_dataset(+RefD, +DefG, +NGs) is det.
%! qb_dataset(+RefD, +DefG, +NGs, -D) is det.

qb_dataset(RefD, DefG, NGs) :-
  qb_dataset(RefD, DefG, NGs, _).


qb_dataset(RefD, DefG, NGs, D) :-
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



%! q_dataset_db_exists is semidet.
%
% Succeeds iff the Quine dataset persistent DB file exists.

q_dataset_db_exists :-
  setting(dataset_file, File),
  exists_file(File).



%! q_dataset_default_graph(?D, ?DefG) is nondet.

q_dataset_default_graph(D, DefG) :-
  q_dataset(D, GName, Pairs),
  memberchk(GName-DefG, Pairs).



%! q_dataset_graph(+D, +G) is semidet.
%! q_dataset_graph(+D, -G) is multi.
%! q_dataset_graph(-D, +G) is nondet.
%! q_dataset_graph(-D, -G) is nondet.

q_dataset_graph(D, G) :-
  q_dataset(D, _, Pairs),
  member(_-G, Pairs).
  


%! q_dataset_graph(+D, +GName, -G) is semidet.
%! q_dataset_graph(+D, -GName, -G) is nondet.

q_dataset_graph(D, GName, G) :-
  ground(GName), !,
  q_dataset(D, _, Pairs),
  memberchk(GName-G, Pairs).
q_dataset_graph(D, GName, G) :-
  q_dataset(D, _, Pairs),
  member(GName-G, Pairs).



%! q_dataset_named_graph(?D, ?NG) is nondet.
%! q_dataset_named_graph(?D, ?GName, ?NG) is nondet.

q_dataset_named_graph(D, NG) :-
  q_dataset_named_graph(D, _, NG).


q_dataset_named_graph(D, GName, NG) :-
  q_dataset(D, _, Pairs),
  member(GName-NG, Pairs).



%! q_dataset_set_default_graph(+D, +DefGName) is det.

q_dataset_set_default_graph(D, DefGName) :-
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



%! q_rm_dataset(+D) is det.

q_rm_dataset(D) :-
  retractall_q_dataset(D, _, _).



%! q_rm_dataset_graph(+G) is det.
%! q_rm_dataset_graph(+D, +G) is det.

q_rm_dataset_graph(G) :-
  forall(
    q_dataset_graph(D, G),
    q_rm_dataset_graph(D, G)
  ).


q_rm_dataset_graph(D, G) :-
  q_dataset(D, DefGName1, Pairs1),
  selectchk(GName-G, Pairs1, Pairs2),
  (   % Remove the dataset in case there are no more graphs.
      Pairs2 == []
  ->  retractall_q_dataset(D, _, _)
  ;   retractall_q_dataset(D, DefGName1, Pairs1),
      (   % Change the default graph in case it is removed, but some
          % other graph remains.
          DefGName1 == GName
      ->  Pairs2 = [DefGName2-_|_]
      ;   DefGName2 = DefGName1
      ),
      assert_q_dataset(D, DefGName2, Pairs2)
  ).



%! q_rm_dataset is det.

q_rm_datasets :-
  forall(
    q_dataset(D),
    q_rm_dataset(D)
  ).





% API %

%! q_dataset_label(+D, -Lbl) is det.

q_dataset_label(D, Str) :-
  q_dataset_named_graph(D, meta, MetaG),
  q_pref_label(hdt, D, Lit, MetaG),
  q_literal_string(Lit, Str).



%! q_dataset_tree(?M, +Order, +D, -Tree) is det.
%
% Return a dataset tree in which the graphs are ordered in one of the
% following ways:
%
%   - `lexicographic`: by graph name
%
%   - `number_of_triples`: by the number of triples per graph

q_dataset_tree(M, Order, D, Tree) :-
  q_dataset_tree0(M, Order, D, _, Tree).
  

q_dataset_tree0(M, Order, D, SumNumTriples, t(D,OrderedTrees)) :-
  findall(G, q_dataset_graph(D, G), Gs),
  maplist(q_graph_tree0(M), Gs, NumTriples, Trees),
  KeyDict = _{lexicographic: Gs, number_of_triples: NumTriples},
  q_dataset_tree_order0(Order, KeyDict, Trees, OrderedTrees),
  sum_list(NumTriples, SumNumTriples).


q_graph_tree0(M, G, NumTriples, t(G,[])) :-
  (var(M) -> once(q_view_graph(M, G)) ; true),
  q_number_of_triples(M, G, NumTriples).



%! q_dataset_trees(?M, +Order, -Trees) is det.
%
% Return all dataset trees ordered in one of the following ways:
%
%   - `lexicographic`: by dataset and graph name
%
%   - `number_of_triples`: by the number of triples per dataset and
%     graph

q_dataset_trees(M, Order, OrderedTrees) :-
  aggregate_all(set(D), q_dataset(D), Ds),
  maplist(q_dataset_tree0(M, Order), Ds, NumTriples, Trees),
  KeyDict = _{lexicographic: Ds, number_of_triples: NumTriples},
  q_dataset_tree_order0(Order, KeyDict, Trees, OrderedTrees).





% HELPERS %

q_dataset_tree_order0(Order, KeyDict, Trees, OrderedTrees) :-
  pairs_keys_values(Pairs, KeyDict.Order, Trees),
  desc_pairs_values(Pairs, OrderedTrees).
