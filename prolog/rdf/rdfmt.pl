:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).
:- use_module('/home/wbeek/Git/tabling_library/tabling').

:- rdf_meta
   axiom(r, r, o),
   rdfmt(r, r, o),
   rule(t, t).

:- table
   rdfmt/3.

:- debug(rdfmt).

rdfmt(S1, P1, O1) :-
  debug(rdfmt, dcg_print_triple(S1, P1, O1)),
  rule(rdf(S1,P1,O1), L),
  forall(member(rdf(S2,P2,O2), L), rdfmt(S2, P2, O2)).
rdfmt(S, P, O) :-
  rdf(S, P, O).
rdfmt(S, P, O) :-
  axiom(S, P, O).

rule(rdf(I,rdf:type,C), [rdf(P, rdfs:domain, C),rdf(I, P, _)]).

axiom(rdf:type,           rdf:type,           rdf:'Property').
axiom(rdf:subject,        rdf:type,           rdf:'Property').
axiom(rdf:predicate,      rdf:type,           rdf:'Property').
axiom(rdf:object,         rdf:type,           rdf:'Property').
axiom(rdf:first,          rdf:type,           rdf:'Property').
axiom(rdf:rest,           rdf:type,           rdf:'Property').
axiom(rdf:value,          rdf:type,           rdf:'Property').
axiom(rdf:nil,            rdf:type,           rdf:'List').
axiom(rdf:type,           rdfs:domain,        rdfs:'Resource').
axiom(rdfs:domain,        rdfs:domain,        rdf:'Property').
axiom(rdfs:range,         rdfs:domain,        rdf:'Property').
axiom(rdfs:subPropertyOf, rdfs:domain,        rdf:'Property').
axiom(rdfs:subClassOf,    rdfs:domain,        rdfs:'Class').
axiom(rdf:subject,        rdfs:domain,        rdf:'Statement').
axiom(rdf:predicate,      rdfs:domain,        rdf:'Statement').
axiom(rdf:object,         rdfs:domain,        rdf:'Statement').
axiom(rdfs:member,        rdfs:domain,        rdfs:'Resource').
axiom(rdf:first,          rdfs:domain,        rdf:'List').
axiom(rdf:rest,           rdfs:domain,        rdf:'List').
axiom(rdfs:seeAlso,       rdfs:domain,        rdfs:'Resource').
axiom(rdfs:isDefinedBy,   rdfs:domain,        rdfs:'Resource').
axiom(rdfs:comment,       rdfs:domain,        rdfs:'Resource').
axiom(rdfs:label,         rdfs:domain,        rdfs:'Resource').
axiom(rdf:value,          rdfs:domain,        rdfs:'Resource').
axiom(rdf:type,           rdfs:range,         rdfs:'Class').
axiom(rdfs:domain,        rdfs:range,         rdfs:'Class').
axiom(rdfs:range,         rdfs:range,         rdfs:'Class').
axiom(rdfs:subPropertyOf, rdfs:range,         rdf:'Property').
axiom(rdfs:subClassOf,    rdfs:range,         rdfs:'Class').
axiom(rdf:subject,        rdfs:range,         rdfs:'Resource').
axiom(rdf:predicate,      rdfs:range,         rdfs:'Resource').
axiom(rdf:object,         rdfs:range,         rdfs:'Resource').
axiom(rdfs:member,        rdfs:range,         rdfs:'Resource').
axiom(rdf:first,          rdfs:range,         rdfs:'Resource').
axiom(rdf:rest,           rdfs:range,         rdf:'List').
axiom(rdfs:seeAlso,       rdfs:range,         rdfs:'Resource').
axiom(rdfs:isDefinedBy,   rdfs:range,         rdfs:'Resource').
axiom(rdfs:comment,       rdfs:range,         rdfs:'Literal').
axiom(rdfs:label,         rdfs:range,         rdfs:'Literal').
axiom(rdf:value,          rdfs:range,         rdfs:'Resource').
axiom(rdf:'Alt',          rdfs:subClassOf,    rdfs:'Container').
axiom(rdf:'Bag',          rdfs:subClassOf,    rdfs:'Container').
axiom(rdf:'Seq',          rdfs:subClassOf,    rdfs:'Container').
axiom(rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property').
axiom(rdfs:isDefinedBy,   rdfs:subPropertyOf, rdfs:seeAlso).
axiom(rdfs:'Datatype',    rdfs:subClassOf,    rdfs:'Class').
