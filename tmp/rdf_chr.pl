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
:- use_module(library(html/rdfh)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(solution_sequences)).

:- chr_constraint triple/4, widget/3.

:- dynamic rdf_chr:is_well_known_entity/1.

rdf_chr(S, L) :-
  findall(Trip, distinct(Trip, s_triple(S, Trip)), Trips),
  maplist(rdf_chr_assert_triple, Trips),

  % Batch 1: Widgets.
  findall(N-widget(N,SS,W), find_chr_constraint(widget(N,SS,W)), KVPairs),
  desc_pairs_values(KVPairs, L1),gtrace,

  % Batch 2: Triples about S.
  findall(widget(N,S,po_pair(P,O)), find_chr_constraint(triple(N, S, P, O)), L2),
  % Batch 3: Other triples.
  findall(widget(N,NonS,triple(NonS,P,O)), (find_chr_constraint(triple(N, NonS, P, O)), NonS \== S), L3),

  append([L1,L2,L3], L).

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
  D2 is D1 + 1,
  s_triple(D2, O, Trip).


% Label in a preferred language.
triple(N1, S, P, Lbl@LTag),
triple(N2, S, P, ___@____)
<=>
current_lrange(LRange),
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

/*
% HTTP headers: recursive case.
widget(N1, S, http_headers(T)),
widget(N2, S, http_header(P,H))
<=>
sum_list([N1,N2], N),
asc_pairs([P-H|T], L)
| widget(N, S, http_headers(L)).

% HTTP headers: base case.
widget(N1, S, http_header(P1,W1)),
widget(N2, S, http_header(P2,W2))
<=>
sum_list([N1,N2], N),
asc_pairs([P1-W1,P2-W2], L)
| widget(N, S, http_headers(L)).

% HTTP info.
widget(N1, S, http_headers(L)),
widget(N2, S, http_version(Major,Minor)),
triple(N3, S, 'http://lodlaundromat.org/ontology/status_code', Code)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, http_info(L,Major-Minor,Code)).
*/

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
| widget(N, S, media_type(Type,Subtype,[])).

% MediaType, parameterized.
widget(N1, S, media_type(Type,Subtype,T)),
triple(N2, S, 'http://lodlaundromat.org/ontology/parameters', O),
widget(N3, O, H)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, media_type(Type,Subtype,[H|T])).

% Parameter
triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/Parameter'),
triple(N2, S, 'http://lodlaundromat.org/ontology/key', Key),
triple(N3, S, 'http://lodlaundromat.org/ontology/value', Value)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, parameter(Key,Value)).

% Product
triple(N1, S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/Product'),
triple(N2, S, 'http://lodlaundromat.org/ontology/name', Name),
triple(N3, S, 'http://lodlaundromat.org/ontology/version', Version)
<=>
sum_list([N1,N2,N3], N)
| widget(N, S, product(Name,Version)).

% Access-Control-Allow-Headers: recursive case.
widget(N1, S, access_control_allow_headers(T)),
triple(N2, S, 'http://lodlaundromat.org/ontology/value', H)
<=>
sum_list([N1,N2], N)
| widget(N, S, access_control_allow_headers([H|T])).

% Access-Control-Allow-Headers: base case.
triple(_, _, 'http://lodlaundromat.org/ontology/access-control-allow-headers', O) \
triple(N, O, 'http://lodlaundromat.org/ontology/value', H)
<=>
widget(N, O, access_control_allow_headers([H])).

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

% Skip blank nodes to get to widgets.
widget(N1, B, W),
triple(N2, S, P, B)
<=>
sum_list([N1,N2], N)
| widget(N, S, path([P],W)).



% HELPERS %

chr_debug(Flag, Format, Args) :-
  with_output_to(user_output, chr_show_store(rdf_chr)),
  debug(Flag, Format, Args).

pairs_to_assoc(A, [], A) :- !.
pairs_to_assoc(A1, [kv(K,V)|T], A) :- put_assoc(K, A1, V, A2), pairs_to_assoc(A2, T, A).



% WIDGETS %

term_or_tile(X) -->
  {rdf_is_term(X)}, !,
  html(p(\rdfh_term(X))).
term_or_tile(X) -->
  tile(X).

access_control_allow_headers(L) -->
  html([
    h1("Access-Control-Allow-Headers"),
    div(\html_maplist(rdfh_literal, L))
  ]).

archive_entry(Filetype, Format, ModifiedTime, Name) -->
  html([
    h1("Archive entry"),
    p([
      \rdfh_literal(Filetype),
      \rdfh_literal(Format),
      \rdfh_literal(ModifiedTime),
      \rdfh_literal(Name)
    ])
  ]).

etag(Tag, Weak) -->
  html([\rdfh_literal(Tag),\rdfh_literal(Weak)]).

http_header(P, W) -->
  html([h1(\rdfh_predicate(P)),\term_or_tile(W)]).

http_version(Major, Minor) -->
  html([
    h1("HTTP version"),
    p([\rdfh_literal(Major),".",\rdfh_literal(Minor)])
  ]).

media_type(Type, Subtype, Parameters) -->
  html([
    p([\rdfh_literal(Type),"/",\rdfh_literal(Subtype)]),
    p(\html_calls(Parameters))
  ]).

parameter(Key, Value) -->
  html(p([\rdfh_literal(Key),": ",\rdfh_literal(Value)])).

path(Ps, Widget_2) -->
  html([div(\path_items(Ps)),\html_call(Widget_2)]).

path_items([]) --> !, [].
path_items([H]) --> !,
  rdfh_predicate(H).
path_items([H1,H2|T]) -->
  html([\rdfh_predicate(H1),"/",\rdfh_predicate(H2),\path_items(T)]).

po_pair(P, O) -->
  html([h1(\rdfh_predicate(P)),p(\rdfh_object(O))]).

product(Name, Version) -->
  html(p([\rdfh_literal(Name),": ",\rdfh_literal(Version)])).

rdf_counter(NQuads, NStmts, NTrips) -->
  html([
    h1("Processed RDF statements"),
    main([
      div([h1("Statements"),p(\rdfh_literal(NStmts))]),
      div([h1("Triples"),p(\rdfh_literal(NTrips))]),
      div([h1("Quadruples"),p(\rdfh_literal(NQuads))])
    ])
  ]).

tree(Tree) -->
  rdfh_tree(Tree).

triple(S, P, O) -->
  rdfh_triple(S, P, O).

widget(N,S,W) -->
  html(
    div(about=S, [
      div(id=topright, ["(",N,")"]),
      \tile(W)
    ])
  ).



/* TMP

http_headers(L) --> !,
  html([h1("HTTP headers"),\http_headers0(L)]).

http_info(L, Major-Minor, S) -->
  html([
    h1("HTTP information"),
    main([
      div([h1("Version"),\http_version0(Major,Minor)]),
      div([h1("Status code"),p(\rdfh_literal(S))])
    ]),
    h1("Headers"),\http_headers0(L)
  ]).

http_headers0(L) -->
  bs_table(
    \bs_table_header(["Header","Value"]),
    \html_maplist(http_header_row0, L)
  ).

http_header_row0(P-W) -->
  html(tr([td(\rdfh_predicate(P)),td(\rdfh_or_tile(W))])).
*/
