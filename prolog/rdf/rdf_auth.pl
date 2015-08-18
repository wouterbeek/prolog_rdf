:- module(
    rdf_auth,
    [
        rdf_user_transaction/4 % +User:iri
                               % :Auth_1
                               % +Graphs:list(atom)
                               % :Goal_0
    ]
).

/** <module> RDF authenticated

Authenticated read/write access to the RDF DB.

@author Wouter Beek
@license MIT License
@version 2015/07
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- meta_predicate(authenticated_graph(2,+,+)).
:- meta_predicate(rdf_user_transaction(+,2,+,0)).

:- rdf_meta(rdf_user_transaction(r,:,+,:)).





%! rdf_user_transaction(
%!   +User:iri,
%!   :Auth_1,
%!   +Graphs:list(atom),
%!   :Goal_0
%! ) is semidet.

rdf_user_transaction(User, Auth_2, Gs, Goal_0):-
  maplist(authenticated_graph(Auth_2, User), Gs),
  rdf_transaction(Goal_0).

authenticated_graph(Auth_2, User, G):-
  call(Auth_2, User, G).
