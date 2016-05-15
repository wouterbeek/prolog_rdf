:- module(
  rdfa_ext,
  [
    agent_image/2,        % +Agent,   -Img
    agent_image//1,       % +Agent
    agent_name/2,         % +Agent,   -Name
    agent_name//1,        % +Agent
    'bf:subtitle'/2,      % +Article, -Subtitle
    'bf:subtitle'//1,     % +Article
    creators//1,          % +Res
    'dc:abstract'/2,      % +Res,     -Abstract
    'dc:abstract'//1,     % +Res
    'dc:created'/2,       % +Res,     -DT
    'dc:created'//1,      % +Res
    'dc:creator'/2,       % +Res,     -Agent
    'dc:creator'//1,      % +Res
    'dc:subject'/2,       % +Res,     -Subject
    'dc:title'/2,         % +Res,     -Title
    'dc:title'//1,        % +Res
    'foaf:depiction'/2,   % +Agent,   -Uri
    'foaf:depiction'//1,  % +Agent
    'foaf:familyName'/2,  % +Agent,   -FamilyName
    'foaf:familyName'//1, % +Agent
    'foaf:givenName'/2,   % +Agent,   -GivenName
    'foaf:givenName'//1,  % +Agent
    'foaf:homepage'/2,    % +Agent,   -Uri
    'foaf:homepage'//1,   % +Agent
    'foaf:mbox'/2,        % +Agent,   -Uri
    'foaf:mbox'//1,       % +Agent
    'foaf:name'/2,        % +Agent,   -Name
    'foaf:name'//1,       % +Agent
    'org:memberOf'//1,    % +Agent
    rdfa_date_time//3,    % +P,       +Something,  +Masks
    rdfa_prefixed_iri/2,  % +Iri,     -PrefixedIri
    rdfa_prefixes/2,      % +Aliases, -Prefixes
    'sioc:content'//1,    % +Article
    'sioc:reply_of'//1    % +Comment
  ]
).

/** <module> RDFa

@author Wouter Beek
@version 2016/02-2016/05
*/

:- use_module(library(apply)).
:- use_module(library(date_time/date_time)).
:- use_module(library(hash_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time_human)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/rdfh)).
:- use_module(library(http/html_write)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdfa/rdfa_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(string_ext)).

:- rdf_register_prefix(bf, 'http://bibframe.org/vocab/').
:- rdf_register_prefix(org, 'http://www.w3.org/ns/org#').
:- rdf_register_prefix(sioc, 'http://rdfs.org/sioc/ns#').

:- rdf_meta
   agent_image(r, -),
   agent_image(r, ?, ?),
   agent_name(r, -),
   agent_name(r, ?, ?),
   'bf:subtitle'(r, -),
   'bf:subtitle'(r, ?, ?),
   creators(r, ?, ?),
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
   'org:memberOf'(r, ?, ?),
   rdfa_date_time(r, o, +, ?, ?),
   'sioc:content'(r, ?, ?),
   'sioc:reply_of'(r, ?, ?).





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
  internal_link(Agent, \agent_name0(Agent)).
agent_name(Agent) -->
  'foaf:name'(Agent).

agent_name0(Agent) -->
  html([
    \'foaf:givenName'(Agent), %'
    " ",
    \'foaf:familyName'(Agent) %'
  ]), !.



%! 'bf:subtitle'(+Article, -Subtitle)// is det.

'bf:subtitle'(Article, Subtitle) :-
  rdf_pref_string(Article, bf:subtitle, Subtitle).


%! 'bf:subtitle'(+Article)// is det.

'bf:subtitle'(Article) -->
  {'bf:subtitle'(Article, Subtitle)},
  html(h2(span(property='bf:subtitle', \rdfh_literal(Subtitle)))).



%! creators(+Res)// is det.

creators(Res) -->
  {rdf_list(Res, dc:creator, Agents)},
  html(
    ol([inlist='',rel='dc:creator'],
      \html_maplist(agent_item0, Agents)
    )
  ).
agent_item0(Agent) --> html(li(\agent_name(Agent))).



%! 'dc:abstract'(+Res, -Abstract) is det.

'dc:abstract'(Res, Abstract) :-
  rdf_pref_string(Res, dc:abstract, Abstract).


%! 'dc:abstract'(+Res)// is det.

'dc:abstract'(Res) -->
  {once('dc:abstract'(Res, Abstract))},
  html(p(property='dc:abstract', \rdfh_literal(Abstract))).



%! 'dc:created'(+Res, -DateTime) is nondet.

'dc:created'(Res, DT) :-
  rdf_has(Res, dc:created, DT^^xsd:date).


%! 'dc:created'(+Res)// is det.

'dc:created'(Res) -->
  {once('dc:created'(Res, DT))},
  rdfa_date_time(dc:created, DT, [offset]).



%! 'dc:creator'(+Res, -Agent) is nondet.

'dc:creator'(Res, Agent) :-
  rdf_has(Res, dc:creator, Agent).
  


%! 'dc:creator'(+Res)// is det.

'dc:creator'(Res) -->
  {once('dc:creator'(Res, Agent))},
  html(a([href=Agent,property='dc:creator'], \agent_name(Agent))).



%! 'dc:subject'(+Res, -Subject) is nondet.

'dc:subject'(Res, Tag) :-
  rdf_has(Res, dc:subject, Tag).



%! 'dc:title'(+Res, -Title) is det.

'dc:title'(Res, Title) :-
  rdf_pref_string(Res, dc:title, Title).


%! 'dc:title'(+Res)// is det.

'dc:title'(Res) -->
  {'dc:title'(Res, Title)},
  html(h1(property='dc:title', \rdfh_literal(Title))).



%! 'foaf:depiction'(+Agent, -Uri) is nondet.

'foaf:depiction'(Agent, Uri) :-
  rdf_has(Agent, foaf:depiction, Uri^^xsd:anyURI).


%! 'foaf:depiction'(+Agent)// is det.

'foaf:depiction'(Agent) -->
  {once('foaf:depiction'(Agent, Uri))},
  html(img([property='foaf:depiction',src=Uri], [])).



%! 'foaf:familyName'(+Agent, -FamilyName) is det.

'foaf:familyName'(Agent, FamilyName) :-
  rdf_pref_string(Agent, foaf:familyName, FamilyName).


%! 'foaf:familyName'(+Agent)// is det.

'foaf:familyName'(Agent) -->
  {once('foaf:familyName'(Agent, FamilyName))},
  html(span(property='foaf:familyName', \rdfh_literal(FamilyName))).



%! 'foaf:givenName'(+Agent, -GivenName) is det.

'foaf:givenName'(Agent, GivenName) :-
  rdf_pref_string(Agent, foaf:givenName, GivenName).


%! 'foaf:givenName'(+Agent)// is det.

'foaf:givenName'(Agent) -->
  {'foaf:givenName'(Agent, GivenName)}, !,
  html(span(property='foaf:givenName', \rdfh_literal(GivenName))).



%! 'foaf:homepage'(+Agent, -Uri) is nondet.

'foaf:homepage'(Agent, Uri) :-
  rdf_has(Agent, foaf:homepage, Uri^^xsd:anyURI).


%! 'foaf:homepage'(+Agent)// is det.

'foaf:homepage'(Agent) -->
  {once('foaf:homepage'(Agent, Uri))},
  html(a([href=Uri,rel='foaf:homepage'], [\bs_icon(web)," ",code(Uri)])).



%! 'foaf:mbox'(+Agent, -Uri) is nondet.

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
  rdf_pref_string(Agent, foaf:name, Name).


%! 'foaf:name'(+Agent)// is det.

'foaf:name'(Agent) -->
  {'foaf:name'(Agent, Name)},
  html(span(property='foaf:name', \rdfh_literal(Name))).



%! 'org:memberOf'(+Agent)// is det.

'org:memberOf'(Agent) -->
  {
    once(rdf_has(Agent, org:memberOf, Organization)),
    once(rdfs_pref_label(Organization, Label)),
    rdfa_prefixed_iri(Organization, Organization0)
  },
  html(span(property=Organization0, \rdfh_literal(Label))).



%! rdfa_date_time(+P, +Something, +Masks)// is det.

rdfa_date_time(P1, Something, Masks) -->
  {
    something_to_date_time(Something, DT),
    date_time_masks(Masks, DT, MaskedDT),
    current_ltag([en,nl], LTag),
    html_machine_date_time(MaskedDT, MachineString),
    xsd_date_time_datatype(DT, D1),
    maplist(rdfa_prefixed_iri, [P1,D1], [P2,D2])
  },
  html(
    time([datatype=D2,datetime=MachineString,property=P2],
      \html_human_date_time(MaskedDT, _{ltag: LTag, masks: Masks})
    )
  ).



%! rdfa_prefixed_iri(+Iri, -PrefixedIri) is det.

rdfa_prefixed_iri(Iri, PrefixedIri) :-
  rdf_global_id(Alias:Local, Iri),
  atomic_list_concat([Alias,Local], :, PrefixedIri).



%! rdfa_prefixes(+Aliases, -Prefixes) is det.

rdfa_prefixes(Aliases, Defs) :-
  maplist(rdf_current_prefix, Aliases, Prefixes),
  pairs_keys_values(Pairs, Aliases, Prefixes),
  maplist(pair_to_prefix0, Pairs, Defs0),
  atomic_list_concat(Defs0, ' ', Defs).

pair_to_prefix0(Alias-Prefix, Def) :-
  atomic_list_concat([Alias,Prefix], ': ', Def).



'sioc:content'(Article) -->
  {once(rdf_has(Article, sioc:content, Content))},
  html(div(property='sioc:content', \rdfh_literal(Content))).



'sioc:reply_of'(Comment) -->
  {once(rdf_has(Comment, sioc:reply_of, Article))},
  html(span(rel='sbo:commentOf', \'dc:title'(Article))). %'
