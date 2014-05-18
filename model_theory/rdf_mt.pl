:- module(
  rdf_mt,
  [
% MAP %
    i_l/4, % +Graph:atom
           % ?TypedLiteral:compound
           % +Model:atom
           % ?Resource
    i_s/4, % +Graph:atom
           % ?IRI:iri
           % +Model:atom
           % ?Resource
% MODEL %
    i_ext/4, % +Model:atom
             % ?Property
             % ?Resource1
             % ?Resource2
    lv/3, % +Graph:atom
          % +Model:atom
          % ?PlainLiteral:compound
    model/1, % ?Model:atom
    property/2, % +Model:atom
                % ?Property
    resource/2 % +Model:atom
               % ?Resource
  ]
).

/** <module> RDF_MT

The data structures for RDFS model theory.

@author Wouter Beek
@version 2013/08
*/

:- use_module(rdf_term(rdf_term)).



% MAP %

% From typed literals onto resources.
:- dynamic(i_l/4).

% From IRIs onto resources and properties.
:- dynamic(i_s/4).



% MODEL %

% Extension function: mapping properties into pairs of resources.
:- dynamic(i_ext/4).

% The name of the semantic model.
% Similar to rdf_graph/1 for the syntax.
:- dynamic(model/1).

% Properties.
:- dynamic(property/2).

% Resources.
:- dynamic(resource/2).

% Literal values are resources.
lv(G, M, Lit):-
  resource(M, Lit),
  rdf_plain_literal(G, Lit).

