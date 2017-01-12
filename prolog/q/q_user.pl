:- module(
  q_user,
  [
    current_user/2,    % ?User, +Group
    has_current_user/1 % +Group
  ]
).
:- reexport(library(http/http_user)).

/** <module> Quine user

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(q/q_iri)).
:- use_module(library(q/q_prefix), []).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).

:- setting(
     backend,
     oneof([hdt,trp]),
     trp,
     "The backend used for storing user information."
   ).

:- multifile
    google_client:create_user_hook/2,
    google_client:current_user_hook/2.

:- rdf_meta
   current_user(?, r),
   qb_user(+, +, r, +, +, +, r).

google_client:create_user_hook(Profile, UserName) :-
  setting(backend, M),
  q_graph_iri([user], G),
  q_abox_iri(user, Refs, User),
  Refs = [UserName],
  qb_user(
    M,
    User,
    q:'User',
    Profile.picture,
    Profile.given_name,
    Profile.family_name,
    G
  ),
  atomic_list_concat([mailto,Profile.email], :, EMail),
  qb(M, User, foaf:mbox, EMail^^xsd:anyURI, G),
  qb(M, User, q:googleName, Profile.sub^^xsd:string, G),
  qb(M, User, q:locale, Profile.locale^^xsd:string, G),
  qb_now(M, User, q:loggedIn, G).

google_client:current_user_hook(Profile, User) :-
  q_graph_iri([user], G),
  q_pref_string(trp, User, q:googleName, Profile.sub^^xsd:string, G).





%! current_user(+User, +Group) is semidet.
%! current_user(-User, +Group) is semidet.

current_user(User, Group) :-
  current_user(User),
  setting(backend, M),
  q_graph_iri([user], G),
  q(M, User, rdf:type, Group, G).



%! has_current_user(+Group) is semidet.

has_current_user(Group) :-
  current_user(_, Group).
