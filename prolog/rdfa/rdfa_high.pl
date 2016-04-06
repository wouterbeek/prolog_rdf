:- module(
  rdfa_high,
  [
    agent_image/2,    % +Agent, -Img:iri
    agent_image//1,   % +Agent
    agent_name/2,     % +Agent, -Name:string
    agent_name//1     % +Agent
  ]
).
:- reexport(library(rdfa/rdfa_api)).
:- reexport(library(rdfa/rdfa_low)).

/** <module> RDFa high-level structures

@author Wouter Beek
@version 2016/02-2016/03
*/

:- use_module(library(hash_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(string_ext)).

:- rdf_meta
   agent_image(r, -),
   agent_image(r, ?, ?),
   agent_name(r, -),
   agent_name(r, ?, ?).





%! agent_gravatar(+Agent, -Uri) is det.

agent_gravatar(Agent, Uri) :-
  once('foaf:mbox'(Agent, EMail)),
  downcase_atom(EMail, CanonicalEMail),
  md5(CanonicalEMail, Hash),
  atomic_list_concat(['',avatar,Hash], /, Path),
  iri_comps(Uri, uri_components(http,'www.gravatar.com',Path,_,_)).



%! agent_image(+Agent, -Image) is det.

agent_image(Agent, Img) :-
  'foaf:depiction'(Agent, Img).
agent_image(Agent, Img) :-
  agent_gravatar(Agent, Img).


%! agent_image(+Agent)// is det.

agent_image(Agent) -->
  {
    agent_name(Agent, Name),
    agent_image(Agent, Img)
  },
  html(a(href=Agent, img([alt=Name,property='foaf:depiction',src=Img], []))).



%! agent_name(+Agent, -Name) is det.

agent_name(Agent, String) :-
  'foaf:givenName'(Agent, GivenName),
  'foaf:familyName'(Agent, FamilyName), !,
  rdf_string(GivenName, String1),
  rdf_string(FamilyName, String2),
  string_list_concat([String1,String2], " ", String).
agent_name(Agent, String) :-
  'foaf:name'(Agent, Name),
  rdf_string(Name, String).



%! agent_name(+Agent)// is det.

agent_name(Agent) -->
  html(
    a(href=Agent, [
      \'foaf:givenName'(Agent), %'
      " ",
      \'foaf:familyName'(Agent) %'
    ])
  ), !.
agent_name(Agent) -->
  'foaf:name'(Agent).
