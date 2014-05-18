:- module(
  rdfs_ent,
  [
    axiom/4, % ?Regime:atom
             % ?Subject:or([bnode,iri])
             % ?Predicate:iri
             % ?Object:or([bnode,literal,iri])
    explanation/3, % ?Regime:atom
                   % ?Rule:atom
                   % ?Explanation:atom
    rule/7 % ?Regime:atom
           % ?Rule:atom
           % ?Premises:list(compound)
           % ?Subject:or([bnode,iri])
           % ?Predicate:iri
           % ?Object:or([bnode,iri,literal])
           % ?Graph:atom
  ]
).

/** <module> RDFS materialization

@author Wouter Beek
@see Hayes2004
@version 2013/08-2013/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_plain_literal)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_reasoning(rdf_bnode_map)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

%! axiom(
%!   ?Regime:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

:- discontiguous(axiom/4).
:- multifile(axiom/4).
:- rdf_meta(axiom(?,r,r,r)).

%! explanation(?Regime:atom, ?Rule:atom, ?Explanation:atom) is nondet.

:- discontiguous(explanation/3).
:- multifile(explanation/3).
:- rdf_meta(explanation(?,?,?)).

%! rule(
%!   ?Regime:atom,
%!   ?Rule:atom,
%!   ?Premises:list(compound),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:atom
%! ) is nondet.

:- discontiguous(rule/7).
:- multifile(rule/7).
:- rdf_meta(rule(?,?,t,r,r,r,?)).

:- discontiguous(user:regime/1).
:- multifile(user:regime/1).



user:regime(rdfs).

% [gl] Literal instantiation rule
%      This ensures that every triple that contains a literal and
%      its similar triple that contains the allocated blank node
%      (according to the literal generation rule [lg])
%      are derivable from each other.
rule(rdfs, gl, [rdf(S,P,B)], S, P, Lit, G):-
  rdf(S, P, B, G),
  % If the object term is not a blank node,
  % then we do not have to search the blank node-literal mapping.
  rdf_is_bnode(B),

  % Not every blank node that is an object term in some triple
  % is a generalization for a literal.
  % Therefore, it has to occur in the mapping established by rule [lg].
  b2r(G, B, Lit),
  rdf_is_literal(Lit).

% [rdfs1] Literals are instances of =|rdfs:'Literal'|=.
explanation(
  rdfs,
  rdfs1,
  'Literal terms belong to the extension of the RDFS literal class.'
).
rule(rdfs, rdfs1, [rdf(S,P,PlainLit)], B, rdf:type, rdfs:'Literal', G):-
  rdf(S, P, PlainLit, G),
  rdf_plain_literal(PlainLit),

  r2b(G, PlainLit, B).

% [rdfs2] Class membership through domain restriction.
rule(rdfs, rdfs2, [rdf(P,rdfs:domain,C),rdf(S,P,O)], S, rdf:type, C, G):-
  rdf(P, rdfs:domain, C, G),
  rdf_iri(P),
  rdf(S, P, O, G).

% [rdfs3] Class membership through range restriction.
rule(rdfs, rdfs3, [rdf(P,rdfs:range,C),rdf(S,P,O)], O, rdf:type, C, G):-
  rdf(P, rdfs:range, C, G),
  rdf_iri(P),
  rdf(S, P, O, G),
  \+ rdf_is_literal(O).

% [rdfs4a] Subject terms are resources.
explanation(
  rdfs,
  rdfs4a,
  'Terms that occur in the subject position are RDFS resources.'
).
rule(rdfs, rdfs4a, [rdf(S,P,O)], S, rdf:type, rdfs:'Resource', G):-
  rdf(S, P, O, G).
% [rdfs4b] Object terms are resources.
explanation(
  rdfs,
  rdfs4b,
  'Terms that occur in the object position are RDFS resources.'
).
rule(rdfs, rdfs4b, [rdf(S,P,O)], O, rdf:type, rdfs:'Resource', G):-
  rdf(S, P, O, G),
  \+ rdf_is_literal(O).

% [rdfs5] Transitive closure of the property hierarchy relation.
rule(rdfs, rdfs5, [rdf(P1,rdfs:subPropertyOf,P2),rdf(P2,rdfs:subPropertyOf,P3)], P1, rdfs:subPropertyOf, P3, G):-
  rdf(P1, rdfs:subPropertyOf, P2, G),
  % `P2` is automatically constrained to blank nodes and IRIs,
  % since is must appear as the subject term of some triple.
  rdf(P2, rdfs:subPropertyOf, P3, G).

% [rdfs6] Reflexivity of the property hierarchy relation.
rule(rdfs, rdfs6, [rdf(P,rdf:type,rdf:'Property')], P, rdfs:subPropertyOf, P, G):-
  rdf(P, rdf:type, rdf:'Property', G).

% [rdfs7] Using the property hierarchy.
rule(rdfs, rdfs7, [rdf(P1,rdfs:subPropertyOf,P2),rdf(S,P1,O)], S, P2, O, G):-
  rdf(P1, rdfs:subPropertyOf, P2, G),
  rdf_iri(P1),
  rdf_iri(P2),
  rdf(S, P1, O, G).

% [rdfs8] Classes are instances of =|rdfs:Resource|=.
rule(rdfs, rdfs8, [rdf(C,rdf:type,rdfs:'Class')], C, rdfs:subClassOf, rdfs:'Resource', G):-
  rdf(C, rdf:type, rdfs:'Class', G).

% [rdfs9] Using the class hierarchy.
rule(rdfs, rdfs9, [rdf(C1,rdfs:subClassOf,C2),rdf(I,rdf:type,C1)], I, rdf:type, C2, G):-
  rdf(C1, rdfs:subClassOf, C2, G),
  % `C1` is automatically constrained to blank nodes and IRIs,
  % since is must have appeared as the subject term of some triple.
  rdf(I, rdf:type, C1, G).

% [rdfs10] Reflexivity of the class hierarchy relation.
rule(rdfs, rdfs10, [rdf(C,rdf:type,rdfs:'Class')], C, rdfs:subClassOf, C, G):-
  rdf(C, rdf:type, rdfs:'Class', G).

% [rdfs11] Transitivity of the class hierarchy relation.
rule(rdfs, rdfs11, [rdf(C1,rdfs:subClassOf,C2),rdf(C2,rdfs:subClassOf,C3)], C1, rdfs:subClassOf, C3, G):-
  rdf(C1, rdfs:subClassOf, C2, G),
  \+ rdf_is_literal(C2),
  rdf(C2, rdfs:subClassOf, C3, G).

% [rdfs12]
rule(rdfs, rdfs12, [rdf(P,rdf:type,rdfs:'ContainerMembershipProperty')], P, rdfs:subPropertyOf, rdfs:member, G):-
  rdf(P, rdf:type, rdfs:'ContainerMembershipProperty', G).

% [rdfs13]
rule(rdfs, rdfs13, [rdf(D,rdf:type,rdfs:'Datatype')], D, rdfs:subClassOf, rdfs:'Literal', G):-
  rdf(D, rdf:type, rdfs:'Datatype', G).



% RDFS axiomatic triples: domain.
axiom(rdfs,  rdf:type,          rdfs:domain, rdfs:'Resource' ).
axiom(rdfs, rdfs:domain,        rdfs:domain,  rdf:'Property' ).
axiom(rdfs, rdfs:range,         rdfs:domain,  rdf:'Property' ).
axiom(rdfs, rdfs:subPropertyOf, rdfs:domain,  rdf:'Property' ).
axiom(rdfs, rdfs:subClassOf,    rdfs:domain, rdfs:'Class'    ).
axiom(rdfs,  rdf:subject,       rdfs:domain,  rdf:'Statement').
axiom(rdfs,  rdf:predicate,     rdfs:domain,  rdf:'Statement').
axiom(rdfs,  rdf:object,        rdfs:domain,  rdf:'Statement').
axiom(rdfs, rdfs:member,        rdfs:domain, rdfs:'Resource' ).
axiom(rdfs,  rdf:first,         rdfs:domain,  rdf:'List'     ).
axiom(rdfs,  rdf:rest,          rdfs:domain,  rdf:'List'     ).
axiom(rdfs, rdfs:seeAlso,       rdfs:domain, rdfs:'Resource' ).
axiom(rdfs, rdfs:isDefinedBy,   rdfs:domain, rdfs:'Resource' ).
axiom(rdfs, rdfs:comment,       rdfs:domain, rdfs:'Resource' ).
axiom(rdfs, rdfs:label,         rdfs:domain, rdfs:'Resource' ).
axiom(rdfs,  rdf:value,         rdfs:domain, rdfs:'Resource' ).

% RDFS axiomatic triples: range.
axiom(rdfs,  rdf:type,          rdfs:range, rdfs:'Class'   ).
axiom(rdfs, rdfs:domain,        rdfs:range, rdfs:'Class'   ).
axiom(rdfs, rdfs:range,         rdfs:range, rdfs:'Class'   ).
axiom(rdfs, rdfs:subPropertyOf, rdfs:range,  rdf:'Property').
axiom(rdfs, rdfs:subClassOf,    rdfs:range, rdfs:'Class'   ).
axiom(rdfs,  rdf:subject,       rdfs:range, rdfs:'Resource').
axiom(rdfs,  rdf:predicate,     rdfs:range, rdfs:'Resource').
axiom(rdfs,  rdf:object,        rdfs:range, rdfs:'Resource').
axiom(rdfs, rdfs:member,        rdfs:range, rdfs:'Resource').
axiom(rdfs,  rdf:first,         rdfs:range, rdfs:'Resource').
axiom(rdfs,  rdf:rest,          rdfs:range,  rdf:'List'    ).
axiom(rdfs, rdfs:seeAlso,       rdfs:range, rdfs:'Resource').
axiom(rdfs, rdfs:isDefinedBy,   rdfs:range, rdfs:'Resource').
axiom(rdfs, rdfs:comment,       rdfs:range, rdfs:'Literal' ).
axiom(rdfs, rdfs:label,         rdfs:range, rdfs:'Literal' ).
axiom(rdfs,  rdf:value,         rdfs:range, rdfs:'Resource').

% RDFS axiomatic triples: subclass hierarchy.
axiom(rdfs,  rdf:'Alt', rdfs:subClassOf, rdfs:'Container').
axiom(rdfs,  rdf:'Bag', rdfs:subClassOf, rdfs:'Container').
axiom(rdfs,  rdf:'Seq', rdfs:subClassOf, rdfs:'Container').
axiom(rdfs, rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property').

% RDFS axiomatic triples: subproperty hierarchy.
axiom(rdfs, rdfs:isDefinedBy, rdfs:subPropertyOf, rdfs:seeAlso).

% RDFS axiomatic triples: datatypes.
axiom(rdfs,  rdf:'XMLLiteral',  rdf:type,       rdfs:'Datatype').
axiom(rdfs,  rdf:'XMLLiteral', rdfs:subClassOf, rdfs:'Literal' ).
axiom(rdfs, rdfs:'Datatype',   rdfs:subClassOf, rdfs:'Class'   ).

% RDFS axiomatic triples: container membership properies.
/*
axiom(rdfs, rdf:'_1',  rdf:type,   rdfs:'ContainerMembershipProperty').
axiom(rdfs, rdf:'_1', rdfs:domain, rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_1', rdfs:range,  rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_2',  rdf:type,   rdfs:'ContainerMembershipProperty').
axiom(rdfs, rdf:'_2', rdfs:domain, rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_2', rdfs:range,  rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_3',  rdf:type,   rdfs:'ContainerMembershipProperty').
axiom(rdfs, rdf:'_3', rdfs:domain, rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_3', rdfs:range,  rdfs:'Resource'                   ).
*/
% There is an infinite number of RDFS axioms for integer enumeration...
axiom(rdfs, UriRef, rdf:type, rdfs:'ContainerMembershipProperty'):-
  between(1, 3, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
axiom(rdfs, UriRef, rdfs:domain, rdfs:'Resource'):-
  between(1, 3, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
axiom(rdfs, UriRef, rdfs:range, rdfs:'Resource'):-
  between(1, 3, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
