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
   x/3.

rdf_chr(Trips, L) :-
  maplist(rdf_chr_assert, Trips),
  findall(N-x(N,X-Y,T), find_chr_constraint(x(N,X-Y,T)), Pairs1),
  sort(1, @>=, Pairs1, Pairs2),
  pairs_values(Pairs2, L).

rdf_chr_assert(rdf(S,P,O)) :- call(x(1,1-1,rdf(S,P,O))).

% Remove redundancies.
x(N, Dim, X) \ x(N, Dim, X) <=> true.

% Archive entry.
x(N1, _, rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/archive-entry')),
x(N2, _, rdf(S, 'http://lodlaundromat.org/ontology/filetype', Filetype)),
x(N3, _, rdf(S, 'http://lodlaundromat.org/ontology/format', Format)),
x(N4, _, rdf(S, 'http://lodlaundromat.org/ontology/mtime', ModifiedTime)),
x(N5, _, rdf(S, 'http://lodlaundromat.org/ontology/name', Name))
<=>
sum_list([N1,N2,N3,N4,N5], N) |
x(N, 2-2, archive_entry([
  'http://lodlaundromat.org/ontology/filetype'-Filetype,
  'http://lodlaundromat.org/ontology/format'-Format,
  'http://lodlaundromat.org/ontology/mtime'-ModifiedTime,
  'http://lodlaundromat.org/ontology/name'-Name
])).

% HTTP version.
x(N1, _, rdf(_, 'http://lodlaundromat.org/ontology/HTTP-version', S)),
x(N2, _, rdf(S, 'http://lodlaundromat.org/ontology/major', Major)),
x(N3, _, rdf(S, 'http://lodlaundromat.org/ontology/minor', Minor)),
x(N4, _, rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/HTTP-version'))
<=>
sum_list([N1,N2,N3,N4], N) |
x(N, 1-1, http_version(Major,Minor)).

% HTTP header.
x(N1, _, rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/HTTP-header')),
x(N2, _, rdf(S, 'http://lodlaundromat.org/ontology/parser', Status)),
x(N3, _, rdf(S, 'http://lodlaundromat.org/ontology/raw', O)),
x(N4, _, rdf(_, P, S))
<=>
sum_list([N1,N2,N3,N4], N) |
x(N, 1-1, http_header(Status, P, O)).

% HTTP headers: base case.
x(N1, _, http_header(Status, P1, O1)),
x(N2, _, http_header(Status, P2, O2))
<=>
sum_list([N1,N2], N) |
x(N, 3-3, http_headers(Status, [P1-O1,P2-O2])).

% HTTP headers: recursive case.
x(N1, _, http_header(Status, P, O)),
x(N2, _, http_headers(Status, T))
<=>
sum_list([N1,N2], N) |
x(N, 3-3, http_headers(Status, [P-O|T])).

% RDF processed statements counter.
x(N1, _, rdf(S, 'http://lodlaundromat.org/ontology/processed-quadruples', NQuads)),
x(N2, _, rdf(S, 'http://lodlaundromat.org/ontology/processed-statements', NStmts)),
x(N3, _, rdf(S, 'http://lodlaundromat.org/ontology/processed-triples',    NTrips))
<=>
sum_list([N1,N2,N3], N) |
x(N, 1-1, rdf_processed_stmts([
  'http://lodlaundromat.org/ontology/processed-quadruples'-NQuads,
  'http://lodlaundromat.org/ontology/processed-statements'-NStmts,
  'http://lodlaundromat.org/ontology/processed-triples'-NTrips
])).

% Label in a preferred language.
x(N1, _, rdf(S, P, Lbl@LTag)),
x(N2, _, rdf(S, P, ___@____))
<=>
setting(user:language_priority_list, LRange),
basic_filtering(LRange, LTag),
sum_list([N1,N2], N) |
x(N, 2-1, rdf(S,P,Lbl@LTag)).
