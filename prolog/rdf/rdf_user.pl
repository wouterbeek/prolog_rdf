:- module(
  rdf_user,
  [
    current_user/2,    % ?User, +Group
    has_current_user/1 % +Group
  ]
).
:- reexport(library(http/http_user)).

/** <module> RDF user login/logout backend

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_iri)).
:- use_module(library(rdf/rdfs_api)).
:- use_module(library(semweb/rdf11), [rdf_meta/1]).
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
  rdf_graph_iri([user], G),
  rdf_abox_iri(user, Refs, User),
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
  rdf_graph_iri([user], G),
  rdf_pref_string(trp, User, q:googleName, Profile.sub^^xsd:string, G).





%! current_user(+User, +Group) is semidet.
%! current_user(-User, +Group) is semidet.

current_user(User, Group) :-
  current_user(User),
  setting(backend, M),
  rdf_graph_iri([user], G),
  t(M, User, rdf:type, Group, G).



%! has_current_user(+Group) is semidet.

has_current_user(Group) :-
  current_user(_, Group).
