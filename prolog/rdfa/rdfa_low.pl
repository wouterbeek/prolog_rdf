:- module(
  rdfa_low,
  [
    'bf:subtitle'/2,      % +Article, -Subtitle
    'bf:subtitle'//1,     % +Article
    'dc:abstract'/2,      % +Resource, -Abstract
    'dc:abstract'//1,     % +Resource
    'dc:created'/2,       % +Resource, -DT
    'dc:created'//1,      % +Resource
    'dc:creator'/2,       % +Resource, -Agent
    'dc:creator'//1,      % +Resource
    'dc:subject'/2,       % +Resource, -Subject
    'dc:title'/2,         % +Resource, -Title
    'dc:title'//1,        % +Resource
    'foaf:depiction'/2,   % +Agent, -Uri
    'foaf:depiction'//1,  % +Agent
    'foaf:familyName'/2,  % +Agent, -FamilyName
    'foaf:familyName'//1, % +Agent
    'foaf:givenName'/2,   % +Agent, -GivenName
    'foaf:givenName'//1,  % +Agent
    'foaf:homepage'/2,    % +Agent, -Uri
    'foaf:homepage'//1,   % +Agent
    'foaf:mbox'/2,        % +Agent, -Uri
    'foaf:mbox'//1,       % +Agent
    'foaf:name'/2,        % +Agent, -Name
    'foaf:name'//1,       % +Agent
    'org:memberOf'//1     % +Agent
  ]
).
:- reexport(library(rdfa/rdfa_api)).

/** <module> RDFa low-level structures

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(html/html_bs)).
:- use_module(library(http/html_write)).
:- use_module(library(rdf/rdf_ext)).

:- rdf_meta
   'bf:subtitle'(r, -),
   'bf:subtitle'(r, ?, ?),
   'dc:abstract'(r, -),
   'dc:abstract'(r, ?, ?),
   'dc:created'(r, -),
   'dc:created'(r, ?, ?),
   'dc:creator'(r, -),
   'dc:creator'(r, ?, ?),
   'dc:subject'(r, -),
   'dc:title'(r, -),
   'dc:title'(r, ?, ?),
   'foaf:depiction'(r, -),
   'foaf:depiction'(r, ?, ?),
   'foaf:familyName'(r, -),
   'foaf:familyName'(r, ?, ?),
   'foaf:givenName'(r, -),
   'foaf:givenName'(r, ?, ?),
   'foaf:homepage'(r, -),
   'foaf:homepage'(r, ?, ?),
   'foaf:mbox'(r, -),
   'foaf:mbox'(r, ?, ?),
   'foaf:name'(r, -),
   'foaf:name'(r, ?, ?),
   'org:memberOf'(r, ?, ?).





%! 'bf:subtitle'(+Article, -Subtitle)// is det.

'bf:subtitle'(Article, Subtitle) :-
  rdf_pref_string_lex(Article, bf:subtitle, Subtitle).


%! 'bf:subtitle'(+Article)// is det.

'bf:subtitle'(Article) -->
  {'bf:subtitle'(Article, Subtitle)},
  html(h2(span(property='bf:subtitle', Subtitle))).



%! 'dc:abstract'(+Resource, -Abstract) is det.

'dc:abstract'(Res, Abstract) :-
  rdf_pref_string_lex(Res, dc:abstract, Abstract).


%! 'dc:abstract'(+Resource)// is det.

'dc:abstract'(Res) -->
  {once('dc:abstract'(Res, Abstract))},
  html(p(property='dc:abstract', Abstract)).



%! 'dc:created'(+Resource, -DateTime) is det.

'dc:created'(Res, DT) :-
  rdf_has(Res, dc:created, DT^^xsd:date).


%! 'dc:created'(+Resource)// is det.

'dc:created'(Res) -->
  {once('dc:created'(Res, DT))},
  rdfa_date_time(dc:created, DT, [offset]).



%! 'dc:creator'(+Resource, -Agent) is det.

'dc:creator'(Res, Agent) :-
  rdf_has(Res, dc:creator, Agent).


%! 'dc:creator'(+Resource)// is det.

'dc:creator'(Res) -->
  {
    once('dc:creator'(Res, Agent)),
    rdfa_prefixed_iri(Agent, Agent0)
  },
  html(a([href=Agent0,property='dc:creator'], \agent_name(Agent))).



%! 'dc:subject'(+Resource, -Subject) is det.

'dc:subject'(Res, Tag) :-
  rdf_has(Res, dc:subject, Tag).



%! 'dc:title'(+Resource, -Title) is det.

'dc:title'(Res, Title) :-
  rdf_pref_string_lex(Res, dc:title, Title).


%! 'dc:title'(+Resource)// is det.

'dc:title'(Res) -->
  {'dc:title'(Res, Title)},
  html(h1(property='dc:title', Title)).



%! 'foaf:depiction'(+Agent, -Uri)// is det.

'foaf:depiction'(Agent, Uri) :-
  rdf_has(Agent, foaf:depiction, Uri^^xsd:anyURI).


%! 'foaf:depiction'(+Agent)// is det.

'foaf:depiction'(Agent) -->
  {once('foaf:depiction'(Agent, Uri))},
  html(img([property='foaf:depiction',src=Uri], [])).



%! 'foaf:familyName'(+Agent, -FamilyName) is det.

'foaf:familyName'(Agent, FamilyName) :-
  rdf_pref_string_lex(Agent, foaf:familyName, FamilyName).


%! 'foaf:familyName'(+Agent)// is det.

'foaf:familyName'(Agent) -->
  {once('foaf:familyName'(Agent, FamilyName))},
  html(span(property='foaf:familyName', FamilyName)).



%! 'foaf:givenName'(+Agent, -GivenName) is det.

'foaf:givenName'(Agent, GivenName) :-
  rdf_pref_string_lex(Agent, foaf:givenName, GivenName).


%! 'foaf:givenName'(+Agent)// is det.

'foaf:givenName'(Agent) -->
  {'foaf:givenName'(Agent, GivenName)}, !,
  html(span(property='foaf:givenName', GivenName)).



%! 'foaf:homepage'(+Agent, -Uri) is det.

'foaf:homepage'(Agent, Uri) :-
  rdf_has(Agent, foaf:homepage, Uri^^xsd:anyURI).


%! 'foaf:homepage'(+Agent)// is det.

'foaf:homepage'(Agent) -->
  {once('foaf:homepage'(Agent, Uri))},
  html(a([href=Uri,rel='foaf:homepage'], [\bs_icon(web)," ",code(Uri)])).



%! 'foaf:mbox'(+Agent, -Uri) is det.

'foaf:mbox'(Agent, Uri) :-
  rdf_has(Agent, foaf:mbox, Uri^^xsd:anyURI).


%! 'foaf:mbox'(+Agent)// is det.

'foaf:mbox'(Agent) -->
  {
    once('foaf:mbox'(Agent, Uri)),
    atomic_list_concat([mailto,Local], ':', Uri)
  },
  html(a([href=Uri,property='foaf:mbox'], [\bs_icon(mail)," ",code(Local)])).



%! 'foaf:name'(+Agent, -Name) is det.

'foaf:name'(Agent, Name) :-
  rdf_pref_string_lex(Agent, foaf:name, Name).


%! 'foaf:name'(+Agent)// is det.

'foaf:name'(Agent) -->
  {'foaf:name'(Agent, Name)},
  html(span(property='foaf:name', Name)).



%! 'org:memberOf'(+Agent)// is det.

'org:memberOf'(Agent) -->
  {
    once(rdf_has(Agent, org:memberOf, Organization)),
    once(rdfs_label_lex(Organization, Label)),
    rdfa_prefixed_iri(Organization, Organization0)
  },
  html(span(property=Organization0, Label)).
