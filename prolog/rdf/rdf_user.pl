:- module(rdf_user, []).

/** <module> RDFa user

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(q/qb)).
:- use_module(library(q/q_stmt)).
:- use_module(library(semweb/rdf11)).

:- qb_alias(resu, 'http://www.swi-prolog.org/cliopatria/user/').

:- multifile
    google_client:create_user_hook/2,
    google_client:current_user_hook/2.





google_client:create_user_hook(Profile, User) :-
  M = rdf,
  rdf_equal(resu:'', G),
  atomic_list_concat([mailto,Profile.email], :, EMail),
  atom_string(Picture, Profile.picture),
  q_create_iri(resu, User),
  qb_instance(M, User, resu:'User', G),
  qb(M, User, foaf:depiction, Picture^^xsd:anyURI, G),
  qb(M, User, foaf:familyName, Profile.family_name@nl, G),
  qb(M, User, foaf:givenName, Profile.given_name@nl, G),
  qb(M, User, foaf:mbox, EMail^^xsd:anyURI, G),
  qb(M, User, resu:googleName, Profile.sub^^xsd:string, G),
  qb(M, User, resu:locale, Profile.locale^^xsd:string, G),
  qb_now(M, User, resu:loggedIn, G).



google_client:current_user_hook(Profile, User) :-
  q_pref_string(rdf, User, resu:googleName, Profile.sub^^xsd:string, resu:'').
