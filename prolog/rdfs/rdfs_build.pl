:- module(
  rdfs_build,
  [
    rdfs_assert_class/6,       % +M, +C, ?D, ?Lbl, ?Comm, +G
    rdfs_assert_comment/4,     % +M, +S, +Comm, +G
    rdfs_assert_domain/4,      % +M, +P, +C, +G
    rdfs_assert_isDefinedBy/3, % +M, +S, +G
    rdfs_assert_isDefinedBy/4, % +M, +S, ?Iri, +G
    rdfs_assert_label/4,       % +M, +S, +O, +G
    rdfs_assert_property/5,    % +M, +C, +P, +D, +G
    rdfs_assert_range/4,       % +M, +P, +C, +G
    rdfs_assert_rm/2,          % +M, +Quad
    rdfs_assert_rm/3,          % +M, +Triple, +G
    rdfs_assert_rm/5,          % +M, +S, +P, +O, +G
    rdfs_assert_seeAlso/4,     % +M, +S, +Iri, +G
    rdfs_assert_subclass/4,    % +M, +C, +D, +G
    rdfs_assert_subproperty/4  % +M, +P, +Q, +G
  ]
).
:- reexport(library(rdf/rdf_build)).

/** <module> RDFS build API

@author Wouter Beek
@version 2016/06-2017/01
*/

:- rdf_meta
   rdfs_assert_class(+, r, t, ?, ?, r),
   rdfs_assert_comment(+, r, +, r),
   rdfs_assert_domain(+, r, r, r),
   rdfs_assert_isDefinedBy(+, r, r),
   rdfs_assert_isDefinedBy(+, r, r, r),
   rdfs_assert_label(+, r, o, r),
   rdfs_assert_property(+, r, r, r, r),
   rdfs_assert_range(+, r, r, r),
   rdfs_assert_seeAlso(+, r, +, r),
   rdfs_assert_subclass(+, r, t, r),
   rdfs_assert_subproperty(+, r, t, r).





%! rdfs_assert_class(+M, +C, ?D, ?Lbl, ?Comment, +G) is det.

rdfs_assert_class(M, C, D, Lbl, Comm, G) :-
  (var(D) -> true ; rdfs_assert_subclass(M, C, D, G)),
  (var(Lbl) -> true ; rdfs_assert_label(M, C, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(M, C, Comm, G)).



%! rdfs_assert_comment(+M, +S, +Comment, +G) is det.

rdfs_assert_comment(M, S, Comment, G) :-
  rdfs_assert(M, S, rdfs:comment, Comment, G).



%! rdfs_assert_domain(+M, +P, +C, +G) is det.

rdfs_assert_domain(M, P, D, G) :-
  rdfs_assert(M, P, rdfs:domain, D, G).



%! rdfs_assert_isDefinedBy(+M, +S, +G) is det.
%! rdfs_assert_isDefinedBy(+M, +S, ?Iri, +G) is det.
%
% If Iri is uninstantiated, the IRI denoted by the registered RDF
% prefix of Term, if any, is used.

rdfs_assert_isDefinedBy(M, S, G) :-
  rdf_iri_prefix(S, Prefix),
  rdfs_assert_isDefinedBy(M, S, Prefix, G).


rdfs_assert_isDefinedBy(M, S, Iri, G) :-
  (var(Iri) -> rdf_iri_prefix(S, Iri) ; true),
  rdfs_assert(M, S, rdfs:isDefinedBy, Iri^^xsd:anyURI, G).



%! rdfs_assert_label(+M, +S, +O, +G) is det.

rdfs_assert_label(M, S, O, G) :-
  rdfs_assert(M, S, rdfs:label, O, G).



%! rdfs_assert_property(+M, +C, +P, +D, +G) is det.

rdfs_assert_property(M, C, P, D, G) :-
  rdfs_assert_domain(M, P, C, G),
  rdfs_assert_range(M, P, D, G).



%! rdfs_assert_range(+M, +P, +C, +G) is det.

rdfs_assert_range(M, P, C, G) :-
  rdfs_assert(M, P, rdfs:range, C, G).



%! rdfs_assert_seeAlso(+M, +S, +Iri, +G) is det.

rdfs_assert_seeAlso(M, S, Iri, G) :-
  rdfs_assert(M, S, rdfs:seeAlso, Iri, G).



%! rdfs_assert_subclass(+M, +C, +D, +G) is det.
%
% If D is uninstantiated it defaults to `rdfs:Resource`.

rdfs_assert_subclass(M, C, D, G) :-
  rdfs_assert(M, C, rdfs:subClassOf, D, G).



%! rdfs_assert_subproperty(+M, +P, +Q, +G) is det.
%
% Creates a new property that is a subproperty of the given parent
% property.

rdfs_assert_subproperty(M, P, Q, G) :-
  rdfs_assert(M, P, rdfs:subPropertyOf, Q, G).
