:- module(
  rdf_chr,
  [
    rdf_chr/2 % +Trips, -Results
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
:- use_module(library(debug_ext)).
:- use_module(library(list_ext)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(settings)).

:- chr_constraint
   % edge(?Importance:nonneg, ?Dimensions:pair(nonneg), ?Edge).
   edge/3,
   % graph(?Importance:nonneg, ?Dimensions:pair(nonneg), ?C, ?Graph).
   graph/4.

rdf_chr(Trips1, L) :-
  list_truncate(Trips1, 100, Trips2),
  debug_maplist(rdf(chr), rdf_chr_assert, Trips2),
  findall(N-edge(N,X-Y,L), find_chr_constraint(edge(N,X-Y,L)), Pairs1),
  length(Pairs1, N1),
  debug(rdf(chr), "Found ~D CHR edges.", [N1]),
  findall(N-graph(N,X-Y,C,G), find_chr_constraint(graph(N,X-Y,C,G)), Pairs2),
  length(Pairs2, N2),
  debug(rdf(chr), "Found ~D CHR graphs.", [N2]),
  append(Pairs1, Pairs2, Pairs3),
  sort(1, @>=, Pairs3, Pairs4),
  pairs_values(Pairs4, L).

rdf_chr_assert(rdf(S,P,O)) :- call(edge(1,1-1,[S,P,O])).

% Remove redundancies.
edge(N, Dim, L)     \ edge(N, Dim, L)     <=> true.
graph(N, Dim, C, G) \ graph(N, Dim, C, G) <=> true.

/*
% Qualified blank node.
edge(N1, _, [S,P,B]),
graph(N2, _, _, [B-POs])
<=>
rdf_is_bnode(B),
sum_list([N1,N2], N) |
graph(N, 2-2, P, [S-POs]).
*/

% Etag
edge(N1, _, [S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/EntityTag']),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/opaque_tag',OpaqueTag]),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/weak',Weak])
<=>
sum_list([N1,N2,N3], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/EntityTag', [S-[
  'http://lodlaundromat.org/ontology/opaque_tag'-OpaqueTag,
  'http://lodlaundromat.org/ontology/weak'-Weak
]]).

% Media type
edge(N1, _, [S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/MediaType']),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/type',Type]),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/subtype',Subtype])
<=>
sum_list([N1,N2,N3], N) |
graph(N, 2-2, 'http://lodlaundromat.org/ontology/MediaType', [S-[
  'http://lodlaundromat.org/ontology/type'-Type,
  'http://lodlaundromat.org/ontology/subtype'-Subtype
]]).

% Product.
edge(N1, _, [S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/Product']),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/name',Name]),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/version',Version])
<=>
sum_list([N1,N2,N3], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/Product', [S-[
  'http://lodlaundromat.org/ontology/name'-Name,
  'http://lodlaundromat.org/ontology/version'-Version
]]).

% Archive entry.
edge(N1, _, [S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/ArchiveEntry']),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/filetype',Filetype]),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/format',Format]),
edge(N4, _, [S,'http://lodlaundromat.org/ontology/mtime',ModifiedTime]),
edge(N5, _, [S,'http://lodlaundromat.org/ontology/name',Name]),
edge(N6, _, [_,_,S])
<=>
sum_list([N1,N2,N3,N4,N5,N6], N) |
graph(N, 2-2, 'http://lodlaundromat.org/ontology/ArchiveEntry', [S-[
  'http://lodlaundromat.org/ontology/filetype'-Filetype,
  'http://lodlaundromat.org/ontology/format'-Format,
  'http://lodlaundromat.org/ontology/mtime'-ModifiedTime,
  'http://lodlaundromat.org/ontology/name'-Name
]]).

% HTTP version.
edge(N1, _, [S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/Version']),
edge(N2, _, [S, 'http://lodlaundromat.org/ontology/major', Major]),
edge(N3, _, [S, 'http://lodlaundromat.org/ontology/minor', Minor]),
edge(N4, _, [_, 'http://lodlaundromat.org/ontology/version', S])
<=>
sum_list([N1,N2,N3,N4], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/Version', [S-[
  'http://lodlaundromat.org/ontology/major'-Major,
  'http://lodlaundromat.org/ontology/minor'-Minor
]]).

% HTTP header: bad.
edge(N1, _, [S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/InvalidHttpHeader']),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/raw',O]),
edge(N3, _, [_,P,S])
<=>
sum_list([N1,N2,N3], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/InvalidHttpHeader', [S-[P-[O]]]).

% HTTP header: good.
edge(N1, _, [O,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/ValidHttpHeader']),
edge(N2, _, [O,'http://lodlaundromat.org/ontology/raw',_]),
edge(N4, _, [S,P,O]) \
edge(N3, _, [O,'http://lodlaundromat.org/ontology/value',V]),
graph(N5, Dim5, C, [V-VL])
<=>
debug(rdf(chr), "~w Good HTTP header ~w with value ~w ", [O,P,VL]),
sum_list([N1,N2,N3,N4,N5], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/ValidHttpHeader', [S-[P-[graph(N5,Dim5,C,[V-VL])]]]).

% HTTP header: good.
edge(N1, _, [O,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/ValidHttpHeader']),
edge(N2, _, [O,'http://lodlaundromat.org/ontology/raw',_]),
edge(N3, _, [O,'http://lodlaundromat.org/ontology/value',V]),
edge(N4, _, [S,P,O])
==>
rdf_is_literal(V),
%debug(rdf(chr), "Good HTTP header ~w with value ~w.", [P,V]),
sum_list([N1,N2,N3,N4], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/ValidHttpHeader', [S-[P-[V]]]).

% Merge identical HTTP headers.
graph(N1, _, Status, [S-[P-Os1]]),
graph(N2, _, Status, [S-[P-Os2]])
<=>
sum_list([N1,N2], N),
debug(rdf(chr), "Merge ~w and ~w for ~w.", [Os1,Os2,P]),
append(Os1, Os2, Os3) |
graph(N, 1-1, Status, [S-[P-Os3]]).

% Remove depleted HTTP headers.
edge(_, _, [O,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/ValidHttpHeader']),
edge(_, _, [O,'http://lodlaundromat.org/ontology/raw',_]),
edge(_, _, [O,'http://lodlaundromat.org/ontology/value',_]),
edge(_, _, [_,_,O])
<=>
debug(rdf(chr), "Removed depleted HTTP header ~w.", [O]) |
true.

% HTTP headers: base case.
graph(N1, _, 'http://lodlaundromat.org/ontology/ValidHttpHeader', [S-POs1]),
graph(N2, _, 'http://lodlaundromat.org/ontology/ValidHttpHeader', [S-POs2])
<=>
sum_list([N1,N2], N),
append(POs1, POs2, POs3),
sort(POs3, POs4) |
graph(N, 3-3, valid_http_headers, [S-POs4]).

% HTTP headers: recursive case.
graph(N1, _, 'http://lodlaundromat.org/ontology/ValidHttpHeader', [S-POs1]),
graph(N2, _, valid_http_headers, [S-POs2])
<=>
sum_list([N1,N2], N),
append(POs1, POs2, POs3),
sort(POs3, POs4) |
graph(N, 3-3, valid_http_headers, [S-POs4]).

% RDF processed statements counter.
edge(N1, _, [S,'http://lodlaundromat.org/ontology/processed_quadruples',NQuads]),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/processed_statements',NStmts]),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/processed_triples',NTrips])
<=>
sum_list([N1,N2,N3], N) |
graph(N, 2-2, rdf_processed_stmts, [S-[
  'http://lodlaundromat.org/ontology/processed_quadruples'-NQuads,
  'http://lodlaundromat.org/ontology/processed_statements'-NStmts,
  'http://lodlaundromat.org/ontology/processed_triples'-NTrips
]]).

% Label in a preferred language.
edge(N1, _, [S,P,Lbl@LTag]),
edge(N2, _, [S,P,___@____])
<=>
setting(user:language_priority_list, LRange),
basic_filtering(LRange, LTag),
sum_list([N1,N2], N) |
edge(N, 2-1, [S,P,Lbl@LTag]).
