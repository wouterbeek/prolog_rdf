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

:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(atom_ext)).



%! rdf_new_graph(-Graph:atom) is det.
%! rdf_new_graph(+Name:atom, -Graph:atom) is det.
% Returns a graph name that is close to the given graph name,
%  and which it is guaranteed to not already exist.

rdf_new_graph(Graph):-
  rdf_new_graph(user, Graph).

% No RDF graph with the given name exists, so it is safe to use.
rdf_new_graph(Name, Graph):-
  atomic_concat('/', Name, Path),
  uri_compound(Uri, uri_components(http,'www.example.com',Path,_,_)),
  \+ rdf_graph(Uri), !,
  % Make sure the RDF graph now exists,
  % since otherwise it will not exist until
  % a first triple is stored inside it.
  rdf_create_graph(Uri).
% An RDF graph with the same name already exists, so the name is altered.
rdf_new_graph(Name1, Graph):-
  new_atom(Name1, Name2),
  rdf_new_graph(Name2, Graph).

