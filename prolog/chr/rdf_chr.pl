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
   rdf_chr/3,
   result/2.



rdf_chr(Trips, L3) :-
  length(Trips, N),
  debug(rdf(chr), "Adding ~D facts to CHR store.", [N]),
  maplist(rdf_chr_assert, Trips),
  findall(Support-Term, find_chr_constraint(result(Support, Term)), Pairs1),
  sort(1, @>=, Pairs1, Pairs2),
  pairs_values(Pairs2, L1),
  findall(rdf(S,P,O), find_chr_constraint(rdf_chr(S, P, O)), L2),
  append(L1, L2, L3).

rdf_chr_assert(rdf(S,P,O)) :- call(rdf_chr(S, P, O)).


% Remove redundancies.
result(X, Y) \ result(X, Y) <=> true.

% HTTP version.
rdf_chr(_, 'http://lodlaundromat.org/ontology/version', V),
rdf_chr(V, 'http://lodlaundromat.org/ontology/major', Major^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
rdf_chr(V, 'http://lodlaundromat.org/ontology/minor', Minor^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger')
<=>
result(3, http_version(Major,Minor)).

% Valid HTTP header.
rdf_chr(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/HTTP-header'),
rdf_chr(S, 'http://lodlaundromat.org/ontology/parser', Status),
rdf_chr(S, 'http://lodlaundromat.org/ontology/raw', Raw),
rdf_chr(S, 'http://lodlaundromat.org/ontology/value', _),
rdf_chr(_, P, S)
<=>
result(5, http_header(Status, P, Raw)).

% RDF processed statements counter.
rdf_chr(S, 'http://lodlaundromat.org/ontology/processed-quadruples', NQuads),
rdf_chr(S, 'http://lodlaundromat.org/ontology/processed-statements', NStmts),
rdf_chr(S, 'http://lodlaundromat.org/ontology/processed-triples',    NTrips)
<=>
result(3, rdf_processed_stmts(NQuads,NStmts,NTrips)).

% Label in a preferred language.
rdf_chr(S, P, Lbl@LTag),
rdf_chr(S, P, ___@____)
<=>
setting(user:language_priority_list, LRange), basic_filtering(LRange, LTag) |
result(2, rdf(S,P,Lbl@LTag)).
