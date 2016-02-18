:- module(
  rdf_chr,
  [
    rdf_chr/2 % +S, -Widgets
  ]
).

/** <module> RDF CHR

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(chr)).
:- use_module(library(closure)).
:- use_module(library(debug)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(solution_sequences)).

:- chr_constraint triple/4, widget/3.

:- dynamic rdf_chr:is_well_known_entity/1.

rdf_chr(S, Widgets3) :-
  findall(Trip, distinct(Trip, s_triple(S, Trip)), Trips),
  maplist(rdf_chr_assert_triple, Trips),

  % Widgets.
  findall(N-widget(N,S,W), find_chr_constraint(widget(N,S,W)), KVPairs),
  top_pairs(KVPairs, SortedKVPairs),
  pairs_values(SortedKVPairs, Widgets1),

  % The remaining triples are widgets as well.
  findall(widget(N,S,po_pair(P,O)), find_chr_constraint(triple(N, S, P, O)), Widgets2),
  append(Widgets1, Widgets2, Widgets3).

rdf_chr_is_seed(_, S) :- rdf_is_bnode(S), !.
rdf_chr_is_seed(1, S) :- rdf_chr:is_well_known_entity(S).

rdf_chr_assert_triple(rdf(S,P,O)) :- call(triple(1,S,P,O)).


%% STEP 1: LOAD SEEDS %%
% ```chr
% seed(S, D1, false)
% <=>
% forall(rdf(S,P,O), (
%   call(triple(1, S, P, O)),
%   (rdf_chr_is_seed(O) -> D2 is D1 + 1, call(seed(O, D2, false)) ; true)
% ))
% | seed(S, D1, true).
% ```

s_triple(S, Trip) :- s_triple(0, S, Trip).

s_triple(_, S, rdf(S,P,O)) :- rdf(S, P, O).
s_triple(D1, S, Trip) :- rdf(S, _, O), rdf_chr_is_seed(D1, O), D2 is D1 + 1, s_triple(D2, O, Trip).


widget(N1, S, http_header(P1-W1)),
widget(N2, S, http_header(P2-W2))
<=>
sum_list([N1,N2], N),
bot_pairs([P1-W1,P2-W2], L)
| widget(N, S, http_headers(L)).

widget(N1, S, http_headers(T)),
triple(N2, S, P, O),
widget(N3, O, http_header(H))
<=>
sum_list([N1,N2,N3], N),
bot_pairs([P-H|T], L)
| widget(N, S, http_headers(L)).

widget(N1, O, http_header(W)),
triple(N2, S, P, O)
<=>
sum_list([N1,N2], N)
| widget(N, S, http_header(P-W)).

triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/ValidHttpHeader'),
triple(N2, S, 'http://lodlaundromat.org/ontology/value', O),
widget(N3, O, W)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, http_header(W)).

triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/EntityTag'),
triple(N2, S, 'http://lodlaundromat.org/ontology/opaque_tag', Tag),
triple(N3, S, 'http://lodlaundromat.org/ontology/weak', Weak)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, etag(Tag,Weak)).

triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/MediaType'),
triple(N2, S, 'http://lodlaundromat.org/ontology/type', Type),
triple(N3, S, 'http://lodlaundromat.org/ontology/subtype', Subtype)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, media_type(Type,Subtype)).

triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/Product'),
triple(N2, S, 'http://lodlaundromat.org/ontology/name', Name),
triple(N3, S, 'http://lodlaundromat.org/ontology/version', Version)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, product(Name,Version)).



% HELPERS %

chr_debug(Flag, Format, Args) :-
  with_output_to(user_output, chr_show_store(rdf_chr)),
  debug(Flag, Format, Args).

bot_pairs(L1, L2) :- keysort(L1, L2).
top_pairs(L1, L2) :- sort(1, @>=, L1, L2).

pairs_to_assoc(A, [], A) :- !.
pairs_to_assoc(A1, [kv(K,V)|T], A) :- put_assoc(K, A1, V, A2), pairs_to_assoc(A2, T, A).
