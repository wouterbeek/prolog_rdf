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
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(settings)).

:- chr_constraint
   % edge(?Importance:nonneg, ?Dimensions:pair(nonneg), ?Edge).
   edge/3,
   % graph(?Importance:nonneg, ?Dimensions:pair(nonneg), ?C, ?Graph).
   graph/4.

rdf_chr(Trips, L) :-
  maplist(rdf_chr_assert, Trips),
  findall(N-edge(N,X-Y,L), find_chr_constraint(edge(N,X-Y,L)), Pairs1),
  findall(N-graph(N,X-Y,C,G), find_chr_constraint(graph(N,X-Y,C,G)), Pairs2),
  append(Pairs1, Pairs2, Pairs3),
  sort(1, @>=, Pairs3, Pairs4),
  pairs_values(Pairs4, L).

rdf_chr_assert(rdf(S,P,O)) :- call(edge(1,1-1,[S,P,O])).

% Remove redundancies.
edge(N, Dim, L)     \ edge(N, Dim, L)     <=> true.
graph(N, Dim, C, G) \ graph(N, Dim, C, G) <=> true.

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
edge(N1, _, [S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/product']),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/name',Name]),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/version',Version])
<=>
sum_list([N1,N2,N3], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/product', [S-[
  'http://lodlaundromat.org/ontology/name'-Name,
  'http://lodlaundromat.org/ontology/version'-Version
]]).

% Archive entry.
edge(N1, _, [S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/ArchiveEntry']),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/filetype',Filetype]),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/format',Format]),
edge(N4, _, [S,'http://lodlaundromat.org/ontology/mtime',ModifiedTime]),
edge(N5, _, [S,'http://lodlaundromat.org/ontology/name',Name])
<=>
sum_list([N1,N2,N3,N4,N5], N) |
graph(N, 2-2, 'http://lodlaundromat.org/ontology/ArchiveEntry', [S-[
  'http://lodlaundromat.org/ontology/filetype'-Filetype,
  'http://lodlaundromat.org/ontology/format'-Format,
  'http://lodlaundromat.org/ontology/mtime'-ModifiedTime,
  'http://lodlaundromat.org/ontology/name'-Name
]]).

% HTTP version.
edge(N1, _, [S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/HTTP-version']),
edge(N2, _, [S, 'http://lodlaundromat.org/ontology/major', Major]),
edge(N3, _, [S, 'http://lodlaundromat.org/ontology/minor', Minor]),
edge(N4, _, [_, 'http://lodlaundromat.org/ontology/HTTP-version', S])
<=>
sum_list([N1,N2,N3,N4], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/HTTP-version', [S-[
  'http://lodlaundromat.org/ontology/major'-Major,
  'http://lodlaundromat.org/ontology/minor'-Minor
]]).

% HTTP header: bad.
edge(N1, _, [S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/HTTP-header']),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/parser',"invalid"^^'http://www.w3.org/2001/XMLSchema#string']),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/raw',O]),
edge(N4, _, [_,P,S])
<=>
sum_list([N1,N2,N3,N4], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/Invalid-HTTP-header', [S-[P-O]]).

% HTTP header: good.
edge(N1, _, [O,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/HTTP-header']),
edge(N2, _, [O,'http://lodlaundromat.org/ontology/parser',"valid"^^'http://www.w3.org/2001/XMLSchema#string']),
edge(N3, _, [O,'http://lodlaundromat.org/ontology/raw',_]),
edge(N4, _, [O,'http://lodlaundromat.org/ontology/value',V]),
edge(N5, _, [S,P,O])
<=>
rdf_is_literal(V),
sum_list([N1,N2,N3,N4,N5], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/Valid-HTTP-header', [S-[P-V]]).
% HTTP header: good.
edge(N1, _, [O,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://lodlaundromat.org/ontology/HTTP-header']),
edge(N2, _, [O,'http://lodlaundromat.org/ontology/parser',"valid"^^'http://www.w3.org/2001/XMLSchema#string']),
edge(N3, _, [O,'http://lodlaundromat.org/ontology/raw',_]),
edge(N4, _, [O,'http://lodlaundromat.org/ontology/value',V]),
edge(N4, _, [S,P,O]),
graph(N5, Dim5, C, [V-VL])
<=>
sum_list([N1,N2,N3,N4,N5], N) |
graph(N, 1-1, 'http://lodlaundromat.org/ontology/Valid-HTTP-header', [S-[P-graph(N5,Dim5,C,[V-VL])]]).

% HTTP headers: base case.
graph(N1, _, 'http://lodlaundromat.org/ontology/Valid-HTTP-header', [S-POs1]),
graph(N2, _, 'http://lodlaundromat.org/ontology/Valid-HTTP-header', [S-POs2])
<=>
sum_list([N1,N2], N),
append(POs1, POs2, POs3) |
graph(N, 3-3, valid_http_headers, [S-POs3]).

% HTTP headers: recursive case.
graph(N1, _, 'http://lodlaundromat.org/ontology/Valid-HTTP-header', [S-POs1]),
graph(N2, _, valid_http_headers, [S-POs2])
<=>
sum_list([N1,N2], N),
append(POs1, POs2, POs3) |
graph(N, 3-3, valid_http_headers, [S-POs3]).

% RDF processed statements counter.
edge(N1, _, [S,'http://lodlaundromat.org/ontology/processed-quadruples',NQuads]),
edge(N2, _, [S,'http://lodlaundromat.org/ontology/processed-statements',NStmts]),
edge(N3, _, [S,'http://lodlaundromat.org/ontology/processed-triples',NTrips])
<=>
sum_list([N1,N2,N3], N) |
graph(N, 2-2, rdf_processed_stmts, [S-[
  'http://lodlaundromat.org/ontology/processed-quadruples'-NQuads,
  'http://lodlaundromat.org/ontology/processed-statements'-NStmts,
  'http://lodlaundromat.org/ontology/processed-triples'-NTrips
]]).

% Label in a preferred language.
edge(N1, _, [S,P,Lbl@LTag]),
edge(N2, _, [S,P,___@____])
<=>
setting(user:language_priority_list, LRange),
basic_filtering(LRange, LTag),
sum_list([N1,N2], N) |
edge(N, 2-1, [S,P,Lbl@LTag]).
