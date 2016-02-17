:- module(
  rdf_chr,
  [
    rdf_chr/2 % +Trips, -SortedPairs
  ]
).

/** <module> RDF CHR

http://lodlaundromat.org/ontology/

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(chr)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_debug)).
:- use_module(library(dcg/dcg_tree)).
:- use_module(library(debug_ext)).
:- use_module(library(list_ext)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(settings)).

:- chr_constraint
   bnode/1,
   edge/4,
   tree/2.

rdf_chr(Trips, SortedPairs) :-
  length(Trips, Len0),
  debug_maplist(rdf(chr), rdf_chr_assert, Trips),
  findall(N-tree(T), find_chr_constraint(tree(N,T)), Pairs1),
  length(Pairs1, Len1),
  findall(N-edge(S,P,O), find_chr_constraint(edge(N,S,P,O)), Pairs2),
  length(Pairs2, Len2),
  append(Pairs1, Pairs2, Pairs3),
  sort(1, @>=, Pairs3, SortedPairs),
  debug(rdf(chr), "~D triples → ~D trees & ~D edges", [Len0,Len1,Len2]).

rdf_chr_assert(rdf(S,P,O)) :- call(edge(1, S, P, O)).

% Remove redundancies.
edge(N1, S, P, O) \ edge(N2, S, P, O) <=> N1 >= N2 | true.


%% STEP 1: REMOVE REDUNDANCIES AT THE TRIPLE LEVEL %%

% Remove strings the user understands less.
edge(N1, S, P, Lbl1@LTag1),
edge(N2, S, P, ____@LTag2)
<=>
setting(user:language_priority_list, LRange),
basic_filtering(LRange, LTag1),
debug(rdf(chr1), "Prefering language-tag ~a over ~a.", [LTag1,LTag2]),
sum_list([N1,N2], N)
| edge(N, S, P, Lbl1@LTag1).


%% STEP 2: REPLACE TERMS WITH LABELS %%

% Labels for predicate terms.
edge(N1, S, P, O),
edge(N2, P, 'http://www.w3.org/2000/01/rdf-schema#label', Lbl)
<=>
debug(rdf(chr2), "Labeled predicate term ~w → ~w", [P,Lbl]),
sum_list([N1,N2], N)
| edge(N, S, Lbl, O).


%% STEP 3: REMOVE BLANK NODES %%

edge(N, B, P, O)
<=>
rdf_is_bnode(B),
\+ rdf_is_bnode(O),
debug(rdf(chr3a), "Initial tree ~w", [B-[P-[O-[]]]])
| tree(N, B-[P-[O-[]]]).

tree(N1, S-[P-Trees1]),
tree(N2, S-[P-Trees2])
<=>
ord_union(Trees1, Trees2, Trees3),
debug(rdf(chr3b), "Merged children ~w", [S-[P-Trees3]]),
sum_list([N1,N2], N)
| tree(N, S-[P-Trees3]).

tree(N1, S-[P1-Trees1]),
tree(N2, S-Trees2)
<=>
\+ memberchk(P1-_, Trees2),
list_to_ord_set([P1-Trees1|Trees2], Trees3),
debug(rdf(chr3c), "Added child to tree ~w", [S-Trees3]),
sum_list([N1,N2], N)
| tree(N, S-Trees3).


%% STEP 4: DOMAIN-SPECIFIC SKIPPING OF EDGES %%

edge(N1, S, P, B),
tree(N2, B-Trees)
<=>
rdf_is_bnode(B),
dcg_debug(rdf(chr4), (
  atom('Removed blank node '),
  rdf_print_bnode(B),
  nl,
  dcg_tree(rdf_print_term,S-Trees)
)),
sum_list([N1,N2], N)
| tree(N, S-[P-Trees]).
