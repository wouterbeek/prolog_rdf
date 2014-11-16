:- module(
  rdf_graph_name,
  [
    rdf_new_graph/1, % ?Graph:atom
    rdf_new_graph/2 % +Graph1:atom
                    % -Graph2:atom
  ]
).

/** <module> RDF graph name

Support for naming graphs.

@author Wouter Beek
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11-2014/01
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(generics(atom_ext)).
:- use_module(os(file_ext)).



%! rdf_new_graph(?Graph:atom) is det.

rdf_new_graph(G):-
  var(G), !,
  rdf_new_graph(user, G).
rdf_new_graph(_).


%! rdf_new_graph(+GraphSuggestion:atom, -NewGraph:atom) is det.
% Returns a graph name that is close to the given graph name,
% and which it is guaranteed to not already exist.
%
% @arg GraphSuggestion The atomic name of the graph the user wants to use,
%        or uninstantiated.
% @arg NewGraph The atomic name of an RDF graph that is ensured
%        to be new, staying quite close to the name the user suggested,
%        if any.

% No user preference.
rdf_new_graph(G1, G2):-
  var(G1), !,
  rdf_new_graph(temp, G2).
% No RDF graph with the given name exists, so it is safe to use.
rdf_new_graph(G, G):-
  \+ rdf_graph(G), !,
  % Make sure the RDF graph now exists,
  % since otherwise it will not exist until
  % a first triple is stored inside it.
  rdf_create_graph(G).
% An RDF graph with the same name already exists, so the name is altered.
rdf_new_graph(G1, G3):-
  var(G3),
  new_atom(G1, G2),
  rdf_new_graph(G2, G3).

