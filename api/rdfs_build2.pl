:- module(
  rdfs_build2,
  [
    rdfs_assert_class/6, % +Class:iri
                         % ?Superclass:iri
                         % ?Label:atom
                         % ?Comment:atom
                         % ?LangTag:list(atom)
                         % ?Graph:atom
    rdfs_assert_instance/6, % +Instance:iri
                            % ?Class:iri
                            % ?Label:atom
                            % ?Comment:atom
                            % ?LangTag:list(atom)
                            % ?Graph:atom
    rdfs_assert_property/8 % +Property:iri
                           % ?Superproperty:iri
                           % ?Domain:iri
                           % ?Range:iri
                           % ?Label:or([atom,pair(atom)])
                           % ?Comment:or([atom,pair(atom)])
                           % ?LangTag:list(atom)
                           % ?Graph:atom
  ]
).

/** <module> RDF API: Build higher-level descriptions in RDFS

Predicates for building higher-level RDFS constructs.

@author Wouter Beek
@versdion 2014/06, 2014/08-2014/11
*/

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- rdf_meta(rdfs_assert_class(r,r,?,?,?,?)).
:- rdf_meta(rdfs_assert_instance(r,r,?,?,?,?)).
:- rdf_meta(rdfs_assert_property(r,r,r,r,?,?,?,?)).



%! rdfs_assert_class(
%!   +Class:iri,
%!   ?Superclass:iri,
%!   ?Label:atom,
%!   ?Comment:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is det.

rdfs_assert_class(Class, Superclass, Label, Comment, LangTag, Graph):-
  rdfs_assert_subclass(Class, Superclass, Graph),
  rdfs_assert_label_if_nonvar(Class, Label, LangTag, Graph),
  rdfs_assert_comment_if_nonvar(Class, Label, LangTag, Graph).



%! rdfs_assert_instance(
%!   +Instance:iri,
%!   ?Class:iri,
%!   ?Label:atom,
%!   ?Comment:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is det.

rdfs_assert_instance(Instance, Class, Label, Comment, LangTag, Graph):-
  rdf_assert_instance(Instance, Class, Graph),
  rdfs_assert_label_if_nonvar(Instance, Label, LangTag, Graph),
  rdfs_assert_comment_if_nonvar(Instance, Comment, LangTag, Graph).



%! rdfs_assert_property(
%!   +Property:iri,
%!   ?SuperProperty:iri,
%!   ?Domain:iri,
%!   ?Range:iri,
%!   ?Label:atom,
%!   ?Comment:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is det.

rdfs_assert_property(
  Property,
  Domain,
  Range,
  Label,
  Comment,
  LangTag,
  Graph
):-
  rdf_assert_property(Property, Graph),
  (   var(Superproperty)
  ->  true
  ;   rdfs_assert_subproperty(Property, Superproperty, Graph)
  ),
  rdfs_assert_domain(Property, Domain, Graph),
  rdfs_assert_range(Property, Range, Graph),
  rdfs_assert_label_if_nonvar(Property, Label, LangTag, Graph),
  rdfs_assert_comment_if_nonvar(Property, Comment, LangTag, Graph).





% HELPERS

rdfs_assert_label_if_nonvar(_, Label, _, _):-
  var(Label), !.
rdfs_assert_label_if_nonvar(Class, Label, LangTag, Graph):-
  rdfs_assert_label(Class, Label, LangTag, Graph).



rdfs_assert_comment_if_nonvar(_, Comment, _, _):-
  var(Comment), !.
rdfs_assert_comment_if_nonvar(Class, Comment, LangTag, Graph):-
  rdfs_assert_comment(Class, Comment, LangTag, Graph).

