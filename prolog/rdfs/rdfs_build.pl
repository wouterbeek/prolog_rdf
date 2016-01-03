:- module(
  rdfs_build,
  [
    rdfs_assert_class/5,	% +C, ?D, ?Label, ?Comment, ?G
    rdfs_assert_comment/3,	% +S, +Comment, ?G
    rdfs_assert_domain/3,	% +P, +C, ?G
    rdfs_assert_isDefinedBy/2,	% +S, ?G
    rdfs_assert_isDefinedBy/3,	% +S, ?Uri, ?G
    rdfs_assert_label/2,	% +S, +Label
    rdfs_assert_label/3,	% +S, +Label, ?G
    rdfs_assert_property/4,	% +C, +P, +D, ?G
    rdfs_assert_range/3,	% +P, +C, ?G
    rdfs_assert_seeAlso/3,	% +S, +Uri, +G
    rdfs_assert_subclass/2,	% +C, ?G
    rdfs_assert_subclass/3,	% +C, ?D, ?G
    rdfs_assert_subproperty/3,	% +P, +Q, ?G
    rdfs_retractall_class/1	% +C
  ]
).

/** <module> RDFS build

Predicates for asseritng RDFS statements in an easy way.

@author Wouter Beek
@version 2015/07-2015/09, 2015/12-2016/01
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).

:- rdf_meta
	rdfs_assert_class(r, t, ?, ?, r),
	rdfs_assert_comment(o, +, r),
	rdfs_assert_domain(r, r, r),
	rdfs_assert_isDefinedBy(o, r),
	rdfs_assert_isDefinedBy(o, ?, r),
	rdfs_assert_label(o, +),
	rdfs_assert_label(o, +, r),
	rdfs_assert_property(r, r, r, r),
	rdfs_assert_range(r, r, r),
	rdfs_assert_seeAlso(o, +, r),
	rdfs_assert_subclass(r, r),
	rdfs_assert_subclass(r, t, r),
	rdfs_assert_subproperty(r, t, r),
	rdfs_retractall_class(o).





%! rdfs_assert_class(+C, ?D, ?Label, ?Comment, ?G) is det.

rdfs_assert_class(C, Parent, Lbl, Comm, G) :-
  rdf_assert_instance(C, rdfs:'Class', G),
  rdfs_assert_class0(C, Parent, Lbl, Comm, G).



%! rdfs_assert_comment(+S, +Comment, ?G) is det.

rdfs_assert_comment(S, Comment, G) :-
  rdf_assert(S, rdfs:comment, Comment, G).



%! rdfs_assert_domain(+P, +C, ?G) is det.
% Asserts the following propositions:
%
% ```nquads
% 〈P, rdfs:domain, C, G〉
% ```

rdfs_assert_domain(P, D, G) :-
  rdf_assert(P, rdfs:domain, D, G).



%! rdfs_assert_isDefinedBy(+S, ?G) is det.

rdfs_assert_isDefinedBy(S, G) :-
  rdfs_assert_isDefinedBy(S, _, G).


%! rdfs_assert_isDefinedBy(+S, ?Iri, ?G) is det.
% Asserts the following propositions:
%
% ```nquads
% 〈S, rdfs:isDefinedBy, Iri, G〉
% ```
%
% If the given RDF term is a literal,
% then rdfs:isDefinedBy is asserted of its assigned blank node.
%
% If Uri is uninstantiated, the IRI denoted by the registered RDF prefix
% of Term is used, if any.

rdfs_assert_isDefinedBy(S, Iri, G) :-
  var(Iri), !,
  rdf_prefix_iri(S, Iri),
  rdf_assert(S, rdfs:isDefinedBy, Iri, G).
rdfs_assert_isDefinedBy(S, Iri, G) :-
  rdf_assert(S, rdfs:isDefinedBy, Iri, G).



%! rdfs_assert_label(+S, +Label) is det.
% Wrapper around rdfs_assert_label/3 with uninstantiated graph.

rdfs_assert_label(S, Label) :-
  rdfs_assert_label(S, Label, _).


%! rdfs_assert_label(+S, +Label, ?G) is det.
% Assigns an RDFS label to the resource denoted by the given RDF term.
%
% This predicate stores the label as an RDF language-tagged string.
% The default language is `en-US`.

rdfs_assert_label(S, Label, G) :-
  rdf_assert(S, rdfs:label, Label, G).



%! rdfs_assert_property(+C, +P, +D, ?G) is det.

rdfs_assert_property(C, P, D, G) :-
  rdfs_assert_domain(P, C, G),
  rdfs_assert_range(P, D, G).



%! rdfs_assert_range(+P, +C, ?G) is det.

rdfs_assert_range(P, C, G) :-
  rdf_assert(P, rdfs:range, C, G).



%! rdfs_assert_seeAlso(+S, +Iri, ?G) is det.

rdfs_assert_seeAlso(S, Iri, G) :-
  rdf_assert(S, rdfs:seeAlso, Iri, G).



%! rdfs_assert_subclass(+C, ?G) is det.
%! rdfs_assert_subclass(+C, ?D, ?G) is det.
% If D is uninstantiated it defaults to `rdfs:Resource`.

rdfs_assert_subclass(C, G) :-
  rdfs_assert_subclass(C, _, G).
rdfs_assert_subclass(C, D, G) :-
  % Allow the parent class to be uninstantiated.
  (   var(D)
  ->  rdf_assert(C, rdfs:subClassOf, rdfs:'Resource', G)
  ;   is_list(D)
  ->  forall(member(D0, D), rdf_assert(C, rdfs:subClassOf, D0, G))
  ;   rdf_assert(C, rdfs:subClassOf, D, G)
  ).



%! rdfs_assert_subproperty(+P, ?Q, ?G) is det.
% Creates a new property that is a subproperty of the given parent property.
%
% If Q is uninstantiated it defaults to `rdf:Property`.

rdfs_assert_subproperty(P, Qs, G) :-
  is_list(Qs), !,
  forall(member(Q, Qs), rdfs_assert_subproperty(P, Q, G)).
rdfs_assert_subproperty(P, Q, G) :-
  rdf_defval(rdf:'Property', Q),
  rdf_assert(P, rdfs:subPropertyOf, Q, G).



%! rdfs_retractall_class(+C) is det.
% Removes the given class from the triple store.
%
% This is the same as removing class terms that are closed under identity.
%
% This connects all subclasses of Class to all superclasses of Class.

rdfs_retractall_class(C) :-
  % [1] Remove the links to subclasses.
  %     Connect all subclasses of Class to all superclasses of Class.
  forall(
    (
      rdf(SubC, rdfs:subClassOf, C),
      rdf(C, rdfs:subClassOf, SuperC)
    ),
    (
      % The transitive link is now a direct one.
      rdfs_assert_subclass(SubC, SuperC, _),
      % Remove the link to a subclass.
      rdf_retractall(SubC, rdfs:subClassOf, C)
    )
  ),

  % [2] Remove the links to superclasses.
  rdf_retractall(C, rdfs:subClassOf, _),

  % [3] Remove other triples in which the class occurs.
  rdf_retractall_term(C, _).





% HELPERS %

rdfs_assert_class0(C, Parent, Lbl, Comm, G) :-
  rdfs_assert_subclass(C, Parent, G),
  (var(Lbl) -> true ; rdfs_assert_label(C, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(C, Comm, G)),
  rdfs_assert_isDefinedBy(C, G).
