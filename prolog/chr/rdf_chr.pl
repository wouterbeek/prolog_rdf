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
:- use_module(library(debug_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_load)).
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

rdf_chr_is_seed(_, _, S) :- rdf_is_bnode(S), !.
rdf_chr_is_seed(_, P, _) :- rdf_equal(owl:sameAs, P), !.
rdf_chr_is_seed(1, _, S) :- rdf_chr:is_well_known_entity(S).

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

s_triple(_, S, rdf(S,P,O)) :-
  catch(rdf(S, P, O), _, fail).
s_triple(D1, S, Trip) :-
  catch(rdf(S, P, O), _, fail),
  rdf_chr_is_seed(D1, P, O),
  catch(rdf_load(O), _, fail),
  D2 is D1 + 1,
  s_triple(D2, O, Trip).


% Label in a preferred language.
triple(N1, S, P, Lbl@LTag),
triple(N2, S, P, ___@____)
<=>
setting(user:language_priority_list, LRange),
basic_filtering(LRange, LTag),
sum_list([N1,N2], N)
| triple(N, S, P, Lbl@LTag).

% Archive entry.
triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/ArchiveEntry'),
triple(N2, S, 'http://lodlaundromat.org/ontology/filetype', Filetype),
triple(N3, S, 'http://lodlaundromat.org/ontology/format', Format),
triple(N4, S, 'http://lodlaundromat.org/ontology/mtime', ModifiedTime),
triple(N5, S, 'http://lodlaundromat.org/ontology/name', Name),
triple(N6, _, _, S)
<=>
sum_list([N1,N2,N3,N4,N5,N6], N)
| widget(N, S, archive_entry(Filetype,Format,ModifiedTime,Name)).

% Raw archive entry.
widget(_, _, archive_entry(_,"raw"^^_,_,_))
<=>
true.

% RDF processed statements counter.
triple(N1, S, 'http://lodlaundromat.org/ontology/processed_quadruples', NQuads),
triple(N2, S, 'http://lodlaundromat.org/ontology/processed_statements', NStmts),
triple(N3, S, 'http://lodlaundromat.org/ontology/processed_triples', NTrips)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, rdf_counter(NQuads,NStmts,NTrips)).

% HTTP version.
triple(N1, O, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/Version'),
triple(N2, O, 'http://lodlaundromat.org/ontology/major', Major),
triple(N3, O, 'http://lodlaundromat.org/ontology/minor', Minor),
triple(N4, S, 'http://lodlaundromat.org/ontology/version', O)
<=>
sum_list([N1,N2,N3,N4], N)
| widget(N, S, http_version(Major,Minor)).

% HTTP headers: recursive case.
widget(N1, S, http_headers(T)),
widget(N2, S, http_header(P,H))
<=>
sum_list([N1,N2], N),
bot_pairs([P-H|T], L)
| widget(N, S, http_headers(L)).

% HTTP headers: base case.
widget(N1, S, http_header(P1,W1)),
widget(N2, S, http_header(P2,W2))
<=>
sum_list([N1,N2], N),
bot_pairs([P1-W1,P2-W2], L)
| widget(N, S, http_headers(L)).

% HTTP info.
widget(N1, S, http_headers(L)),
widget(N2, S, http_version(Major,Minor)),
triple(N3, S, 'http://lodlaundromat.org/ontology/status_code', Code)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, http_info(L,Major-Minor,Code)).

% Valid HTTP header: literal value.
triple(N1, O, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/ValidHttpHeader'),
triple(N2, O, 'http://lodlaundromat.org/ontology/value', V),
triple(N3, S, P, O)
<=>
rdf_is_literal(V),
sum_list([N1,N2,N3], N)
| widget(N, S, http_header(P, V)).

% Valid HTTP header: complex value.
triple(N1, O, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/ValidHttpHeader'),
triple(N2, O, 'http://lodlaundromat.org/ontology/value', V),
widget(N3, V, W),
triple(N4, S, P, O)
<=>
sum_list([N1,N2,N3,N4], N)
| widget(N, S, http_header(P,W)).

% ETag
triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/EntityTag'),
triple(N2, S, 'http://lodlaundromat.org/ontology/opaque_tag', Tag),
triple(N3, S, 'http://lodlaundromat.org/ontology/weak', Weak)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, etag(Tag,Weak)).

% MediaType
triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/MediaType'),
triple(N2, S, 'http://lodlaundromat.org/ontology/type', Type),
triple(N3, S, 'http://lodlaundromat.org/ontology/subtype', Subtype)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, media_type(Type,Subtype)).

% Product
triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/Product'),
triple(N2, S, 'http://lodlaundromat.org/ontology/name', Name),
triple(N3, S, 'http://lodlaundromat.org/ontology/version', Version)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, product(Name,Version)).

% Skip blank nodes to get to widgets.
widget(N1, B, W),
triple(N2, S, P, B)
<=>
sum_list([N1,N2], N)
| widget(N, S, P-W).



% HELPERS %

chr_debug(Flag, Format, Args) :-
  with_output_to(user_output, chr_show_store(rdf_chr)),
  debug(Flag, Format, Args).

bot_pairs(L1, L2) :- keysort(L1, L2).
top_pairs(L1, L2) :- sort(1, @>=, L1, L2).

pairs_to_assoc(A, [], A) :- !.
pairs_to_assoc(A1, [kv(K,V)|T], A) :- put_assoc(K, A1, V, A2), pairs_to_assoc(A2, T, A).
