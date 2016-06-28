:- module(rdf_user, []).

/** <module> RDFa user

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(q/q_stmt)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(user, 'http://www.swi-prolog.org/cliopatria/user/').

:- multifile
    google_client:create_user_hook/2,
    google_client:current_user_hook/2.





google_client:create_user_hook(Profile, User) :-
  atomic_list_concat([mailto,Profile.email], :, EMail),
  atom_string(Picture, Profile.picture),
  rdf_create_iri(user, User),
  rdf_equal(user:'', G),
  rdf_assert_instance(User, user:'User', G),
  rdf_assert(User, foaf:depiction, Picture^^xsd:anyURI, G),
  rdf_assert(User, foaf:familyName, Profile.family_name@nl, G),
  rdf_assert(User, foaf:givenName, Profile.given_name@nl, G),
  rdf_assert(User, foaf:mbox, EMail^^xsd:anyURI, G),
  rdf_assert(User, user:googleName, Profile.sub^^xsd:string, G),
  rdf_assert(User, user:locale, Profile.locale^^xsd:string, G),
  rdf_assert_now(User, user:loggedIn, G).



google_client:current_user_hook(Profile, User) :-
  q_pref_string(rdf, User, user:googleName, Profile.sub^^xsd:string).
