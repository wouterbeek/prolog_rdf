:- module(
  rdfs_build2,
  [
    rdfs_assert_class/5, % +Class:iri
                         % ?Superclass:iri
                         % ?Label:atom
                         % ?Comment:or([atom,list(atom)])
                         % ?Graph:atom
    rdfs_assert_instance/5, % +Instance:iri
                            % ?Class:iri
                            % ?Label:atom
                            % ?Comment:or([atom,list(atom)])
                            % ?Graph:atom
    rdfs_assert_property/6, % +Property:iri
                            % ?Domain:iri
                            % ?Range:iri
                            % ?Label:or([atom,pair(atom)])
                            % ?Comment:or([atom,pair(atom)])
                            % ?Graph:atom
    rdfs_assert_property/7 % +Property:iri
                           % +SuperProperty:iri
                           % ?Domain:iri
                           % ?Range:iri
                           % ?Label:or([atom,pair(atom)])
                           % ?Comment:or([atom,pair(atom)])
                           % ?Graph:atom
  ]
).

/** <module> RDF API: Build higher-level descriptions in RDFS

Predicates for building higher-level RDFS constructs.

@author Wouter Beek
@versdion 2014/06, 2014/08-2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- rdf_meta(rdfs_assert_class(r,r,?,?,?)).
:- rdf_meta(rdfs_assert_instance(r,r,?,?,?)).
:- rdf_meta(rdfs_assert_property(r,r,r,?,?,?)).
:- rdf_meta(rdfs_assert_property(r,r,r,r,?,?,?)).





%! rdfs_assert_class(
%!   +Class:iri,
%!   ?Superclass:iri,
%!   ?Label:or([atom,list(atom)]),
%!   ?Comment:or([atom,list(atom)]),
%!   ?Graph:atom
%! ) is det.

rdfs_assert_class(Class, Superclass, Label, Comment, Graph):-
  rdfs_assert_subclass(Class, Superclass, Graph),
  rdfs_assert_label_if_nonvar(Class, Label, Graph),
  rdfs_assert_comment_if_nonvar(Class, Comment, Graph).



%! rdfs_assert_instance(
%!   +Instance:iri,
%!   ?Class:iri,
%!   ?Label:or([atom,list(atom)]),
%!   ?Comment:or([atom,list(atom)]),
%!   ?Graph:atom
%! ) is det.

rdfs_assert_instance(Instance, Class, Label, Comment, Graph):-
  rdf_assert_instance(Instance, Class, Graph),
  rdfs_assert_label_if_nonvar(Instance, Label, Graph),
  rdfs_assert_comment_if_nonvar(Instance, Comment, Graph).



%! rdfs_assert_property(
%!   +Property:iri,
%!   ?Domain:iri,
%!   ?Range:iri,
%!   ?Label:or([atom,list(atom)]),
%!   ?Comment:or([atom,list(atom)]),
%!   ?Graph:atom
%! ) is det.

rdfs_assert_property(
  Property,
  Domain,
  Range,
  Label,
  Comment,
  Graph
):-
  rdf_assert_property(Property, Graph),
  rdfs_assert_domain(Property, Domain, Graph),
  rdfs_assert_range(Property, Range, Graph),
  rdfs_assert_label_if_nonvar(Property, Label, Graph),
  rdfs_assert_comment_if_nonvar(Property, Comment, Graph).


%! rdfs_assert_property(
%!   +Property:iri,
%!   +SuperProperty:iri,
%!   ?Domain:iri,
%!   ?Range:iri,
%!   ?Label:or([atom,list(atom)]),
%!   ?Comment:or([atom,list(atom)]),
%!   ?Graph:atom
%! ) is det.

rdfs_assert_property(
  Property,
  SuperProperty,
  Domain,
  Range,
  Label,
  Comment,
  Graph
):-
  rdfs_assert_property(Property, Domain, Range, Label, Comment, Graph),
  rdfs_assert_subproperty(Property, SuperProperty, Graph).





% HELPERS

rdfs_assert_label_if_nonvar(_, Label, _):-
  var(Label), !.
rdfs_assert_label_if_nonvar(Class, Label, Graph):-
  rdfs_assert_label(Class, Label, Graph).



rdfs_assert_comment_if_nonvar(_, Comment, _):-
  var(Comment), !.
rdfs_assert_comment_if_nonvar(Class, Comment, Graph):-
  rdfs_assert_comment(Class, Comment, Graph).

