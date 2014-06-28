:- module(
  rdfs_build2,
  [
    rdfs_assert_class/5, % +Class:iri
                         % +Parent:iri
                         % +Label:atom
                         % +Comment:atom
                         % +Graph:atom
    rdfs_assert_property/6 % +Property:iri
                           % +Domain:iri
                           % +Range:iri
                           % +Label:atom
                           % +Comment:atom
                           % +Graph:atom
  ]
).

/** <module> RDFS build2

Predicates for building higher-level RDFS constructs.

@author Wouter Beek
@versdion 2014/06
*/

:- use_module(library(semweb/rdf_db)). % Declaration.

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdfs_build)).
:- use_module(plRdf(rdfs_label_ext)).

:- rdf_meta(rdf_assert_class(r,r,+,+,+)).
:- rdf_meta(rdf_assert_property(r,r,r,+,+,+)).



%! rdfs_assert_class(
%!   +Class:iri,
%!   +Parent:iri,
%!   +Label:atom,
%!   +Comment:atom,
%!   +Graph:atom
%! ) is det.

rdfs_assert_class(Class, Parent, Label, Comment, Graph):-
  rdfs_assert_class(Class, Parent, Graph),
  rdfs_assert_label(Class, Label, en, Graph),
  rdfs_assert_comment(Class, Comment, en, Graph).


%! rdfs_assert_property(
%!   +Property:iri,
%!   +Domain:iri,
%!   +Range:iri,
%!   +Label:atom,
%!   +Comment:atom,
%!   +Graph:atom
%! ) is det.

rdfs_assert_property(Property, Domain, Range, Label, Comment, Graph):-
  rdf_assert_property(Property, Graph),
  rdfs_assert_domain(Property, Domain, Graph),
  rdfs_assert_range(Property, Range, Graph),
  rdfs_assert_label(Property, Label, en, Graph),
  rdfs_assert_comment(Property, Comment, en, Graph).

