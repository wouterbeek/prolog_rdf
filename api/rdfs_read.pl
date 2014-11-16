:- module(
  rdfs_read,
  [
    
  ]
).

/** <module> RDF API: Read RDFS constructs

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(entailment/rdf_bnode_map)).
:- use_module(plRdf(term/rdf_list)).

:- rdf_meta(rdfs_label(r,?,?,?)).



%! rdfs_label(
%!   ?Subject:or([bnode,iri]),
%!   ?Label:atom,
%!   ?LangTags:list(list(atom)),
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is nondet.
% Reads RDFS labels attributed to resources.
%
% Special support for:
%   - RDF lists include labels of all their elements.
%   - Optional language tags argument:
%     either present (`rdf:langString`) or not (`xsd:string`).
%   - Multiple language tags in descending order of preference;
%     non-deterministically returns labels of descending preference.
%   _ Optional graph argument.
%
% Also returns listified labels for RDF list resources.

% An RDF list, compose the lexical form based on its members.
rdfs_label(List, Label, LangTags, LangTag, Graph):-
  rdf_is_list(List), !,
  rdf_list(List, Subterms, [recursive(false)]),
  findall(
    Sublabel,
    (
      member(Subterm, Subterms),
      rdfs_label(Subterm, Sublabel, LangTags, LangTag, Graph)
    ),
    Sublabels
  ),
  dcg_with_output_to(atom(Label), list(pl_term, Sublabels)).
% A string with no language tag.
rdfs_label(Node, Label, LangTags, _, Graph):-
  var(LangTags), !,
  rdf_string(Node, rdfs:label, Label, Graph).
% A language-tagged string.
rdfs_label(Node, Label, LangTags, LangTag, Graph):-
  member(LangTag, LangTags),
  rdf_language_tagged_string(Node, rdfs:label, Label, LangTag, Graph).

