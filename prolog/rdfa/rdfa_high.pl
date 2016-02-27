:- module(
  rdfa_high,
  [
    agent_image/2,    % +Agent, -Img
    agent_image//1,   % +Agent
    agent_name/2,     % +Agent, -Name
    agent_name//1     % +Agent
  ]
).
:- reexport(library(rdfa/rdfa_api)).
:- reexport(library(rdfa/rdfa_low)).

/** <module> RDFa high-level structures

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(hash_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(string_ext)).
:- use_module(library(uri)).

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
  uri_components(Uri, uri_components(http,'www.gravatar.com',Path,_,_)).



%! agent_image(+Agent, -Image) is det.

agent_image(Agent, Img) :-
  'foaf:depiction'(Agent, Img).
agent_image(Agent, Img) :-
  agent_gravatar(Agent, Img).


%! agent_image(+Agent)// is det.

agent_image(Agent) -->
  {
    rdfa_prefixed_iri(Agent, Agent0),
    agent_name(Agent, Name),
    agent_image(Agent, Img)
  },
  html(a(href=Agent0, img([alt=Name,property='foaf:depiction',src=Img], []))).



%! agent_name(+Agent, -Name) is det.

agent_name(Agent, Name) :-
  'foaf:givenName'(Agent, GivenName),
  'foaf:familyName'(Agent, FamilyName), !,
  string_list_concat([GivenName,FamilyName], " ", Name).
agent_name(Agent, Name) :-
  'foaf:name'(Agent, Name).



%! agent_name(+Agent)// is det.

agent_name(Agent) -->
  html(
    a([href=Agent], [
      \'foaf:givenName'(Agent), %'
      " ",
      \'foaf:familyName'(Agent) %'
    ])
  ), !.
agent_name(Agent) -->
  'foaf:name'(Agent).
