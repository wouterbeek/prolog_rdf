:- module(q_user, []).

/** <module> Quine user

@author Wouter Beek
@version 2016/06-2016/07, 2016/10
*/

:- use_module(library(q/q_iri)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).

:- qb_alias(resu, 'http://www.swi-prolog.org/cliopatria/user/').

:- setting(
     user:backend,
     oneof([hdt,trp]),
     trp,
     "The backend used for storing user information."
   ).
:- setting(
     user:alias,
     atom,
     resu,
     "The IRI prefix of user resources."
   ).

:- multifile
    google_client:create_user_hook/2,
    google_client:current_user_hook/2.

:- rdf_meta
   qb_user(+, +, r, +, +, +, r).





google_client:create_user_hook(Profile, User) :-
  setting(user:backend, M),
  q_graph_iri([blog], G),
  setting(user:alias, Alias),
  qb_iri(Alias, User),
  rdf_global_id(Alias:'User', C),
  qb_user(M, User, C, Profile.picture, Profile.given_name, Profile.family_name, G),
  atomic_list_concat([mailto,Profile.email], :, EMail),
  qb(M, User, foaf:mbox, EMail^^xsd:anyURI, G),
  qb(M, User, resu:googleName, Profile.sub^^xsd:string, G),
  qb(M, User, resu:locale, Profile.locale^^xsd:string, G),
  qb_now(M, User, resu:loggedIn, G).



google_client:current_user_hook(Profile, User) :-
  q_pref_string(trp, User, resu:googleName, Profile.sub^^xsd:string, resu:'').
