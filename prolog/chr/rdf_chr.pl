:- module(
  rdf_chr,
  [
    rdf_chr/2 % +Trips, -Pairs
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
:- use_module(library(html/rdf_html_stmt)).
:- use_module(library(lists)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(settings)).

:- chr_constraint
   rdf_chr/3,
   result/3.



rdf_chr(Trips, PairsC) :-
  length(Trips, N),
  debug(rdf(chr), "Adding ~D facts to CHR store.", [N]),
  maplist(rdf_chr_assert, Trips),
  findall(Support-[Attrs,Term], find_chr_constraint(result(Support, Attrs, Term)), Pairs1),
  sort(1, @>=, Pairs1, Pairs2),
  pairs_values(Pairs2, Lists),
  maplist(pair_list, PairsA, Lists),
  findall(
    []-html([
      h3(\(rdf_html_term:rdf_html_predicate(P, []))),
      div(\(rdf_html_term:rdf_html_object(O, [])))
    ]),
    find_chr_constraint(rdf_chr(_, P, O)),
    PairsB
  ),
  append(PairsA, PairsB, PairsC).

rdf_chr_assert(rdf(S,P,O)) :- call(rdf_chr(S, P, O)).

% Remove redundancies.
%redundant @
result(X, Y, Z) \ result(X, Y, Z) <=> true.

% HTTP version.
rdf_chr(_, 'http://lodlaundromat.org/ontology/version', V),
rdf_chr(V, 'http://lodlaundromat.org/ontology/major', Major^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
rdf_chr(V, 'http://lodlaundromat.org/ontology/minor', Minor^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger')
<=>
result(3, [class=valid], html([h3('HTTP version'),p([Major,.,Minor])])).

% Valid HTTP header.
rdf_chr(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://lodlaundromat.org/ontology/HTTP-header'),
rdf_chr(S, 'http://lodlaundromat.org/ontology/parser', Status^^'http://www.w3.org/2001/XMLSchema#string'),
rdf_chr(S, 'http://lodlaundromat.org/ontology/raw', Raw^^'http://www.w3.org/2001/XMLSchema#string'),
rdf_chr(S, 'http://lodlaundromat.org/ontology/value', _),
rdf_chr(_, P, S)
<=>
atom_string(Class, Status), rdf_global_id(llo:PLocal, P) |
result(5, [class=Class], html([h3(PLocal),p(Raw)])).

% Label in preferred language.
rdf_chr(S, P, Lbl1@LTag1),
rdf_chr(S, P, ____@_____)
<=>
setting(user:language_priority_list, LRange), basic_filtering(LRange, LTag1) |
result(2, [class=default], html([h3(\(rdf_html_predicate(P))),div([Lbl1,'@',LTag1])])).
