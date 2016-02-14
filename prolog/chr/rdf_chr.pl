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
   x/2.

rdf_chr(Trips, L3) :-
  maplist(rdf_chr_assert, Trips),
  findall(Support-Term, find_chr_constraint(x(Support, Term)), Pairs1),
  sort(1, @>=, Pairs1, Pairs2),
  pairs_values(Pairs2, L1),
  findall(rdf(S,P,O), find_chr_constraint(x(_, rdf(S,P,O))), L2),
  append(L1, L2, L3).

rdf_chr_assert(rdf(S,P,O)) :- call(x(1, rdf(S,P,O))).

% Remove redundancies.
x(N, X) \ x(N, X) <=> true.

% HTTP version.
x(N1, rdf(_, 'http://lodlaundromat.org/ontology/HTTP-version', S)),
x(N2, rdf(S, 'http://lodlaundromat.org/ontology/major', Major^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger')),
x(N3, rdf(S, 'http://lodlaundromat.org/ontology/minor', Minor^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger')),
x(N4, rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/HTTP-version'))
<=>
sum_list([N1,N2,N3,N4], N) |
x(N, http_version(Major,Minor)).

% Valid HTTP header.
x(N1, rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/HTTP-header')),
x(N2, rdf(S, 'http://lodlaundromat.org/ontology/parser', Status)),
x(N3, rdf(S, 'http://lodlaundromat.org/ontology/raw', Raw)),
x(N4, rdf(S, 'http://lodlaundromat.org/ontology/value', _)),
x(N5, rdf(_, P, S))
<=>
sum_list([N1,N2,N3,N4,N5], N) |
x(N, http_header(Status, P, Raw)).

% RDF processed statements counter.
x(N1, rdf(S, 'http://lodlaundromat.org/ontology/processed-quadruples', NQuads)),
x(N2, rdf(S, 'http://lodlaundromat.org/ontology/processed-statements', NStmts)),
x(N3, rdf(S, 'http://lodlaundromat.org/ontology/processed-triples',    NTrips))
<=>
sum_list([N1,N2,N3], N) |
x(N, rdf_processed_stmts(NQuads,NStmts,NTrips)).

% Label in a preferred language.
x(N1, rdf(S, P, Lbl@LTag)),
x(N2, rdf(S, P, ___@____))
<=>
setting(user:language_priority_list, LRange),
basic_filtering(LRange, LTag),
sum_list([N1,N2], N) |
x(N, rdf(S,P,Lbl@LTag)).
