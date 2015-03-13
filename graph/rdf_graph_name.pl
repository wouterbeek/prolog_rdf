:- module(
  rdf_graph_name,
  [
    rdf_new_graph/1, % -Graph:atom
    rdf_new_graph/2 % +Name:atom
                    % -Graph:atom
  ]
).

/** <module> RDF graph name

Support for naming graphs.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uri)).

:- use_module(plc(generics/atom_ext)).





%! rdf_new_graph(-Graph:atom) is det.
%! rdf_new_graph(+Name:atom, -Graph:atom) is det.
% Returns a graph name that is close to the given graph name,
%  and which it is guaranteed to not already exist.

rdf_new_graph(Graph):-
  rdf_new_graph(user, Graph).

% No RDF graph with the given name exists, so it is safe to use.
rdf_new_graph(Name, Graph2):-
  atomic_concat('/', Name, Path),
  uri_components(Graph1, uri_components(http,'example.com',Path,_,_)),
  
  % Make sure the RDF graph now exists,
  % since otherwise it will not exist until
  % a first triple is stored inside it.
  with_mutex(rdf_graph_name, (
    rdf_new_graph0(Graph1, Graph2),
    rdf_create_graph(Graph2)
  )).

% The graph name is new.
rdf_new_graph0(Name, Name):-
  \+ rdf_graph(Name), !.
% An RDF graph with the same name already exists, so the name is altered.
rdf_new_graph0(Name1, Name):-
  new_atom(Name1, Name2),
  rdf_new_graph0(Name2, Name).

