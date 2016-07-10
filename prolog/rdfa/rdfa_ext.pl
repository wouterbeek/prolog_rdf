:- module(
  rdfa_ext,
  [
    agent_image/4,        % +M, +Agent,   -Img,        ?G
    agent_image//3,       % +M, +Agent,                ?G
    agent_name/4,         % +M, +Agent,   -Name,       ?G
    agent_name//3,        % +M, +Agent,                ?G
    'bf:subtitle'/4,      % +M, +Article, -Subtitle,   ?G
    'bf:subtitle'//3,     % +M, +Article,              ?G
    creators//3,          % +M, +Res,                  ?G
    'dc:abstract'/4,      % +M, +Res,     -Abstract,   ?G
    'dc:abstract'//3,     % +M, +Res,                  ?G
    'dc:created'/4,       % +M, +Res,     -DT,         ?G
    'dc:created'//3,      % +M, +Res,                  ?G
    'dc:creator'/4,       % +M, +Res,     -Agent,      ?G
    'dc:creator'//3,      % +M, +Res,                  ?G
    'dc:subject'/4,       % +M, +Res,     -Subject,    ?G
    'dc:title'/4,         % +M, +Res,     -Title,      ?G
    'dc:title'//3,        % +M, +Res,                  ?G
    'foaf:depiction'/4,   % +M, +Agent,   -Iri,        ?G
    'foaf:depiction'//3,  % +M, +Agent,                ?G
    'foaf:familyName'/4,  % +M, +Agent,   -FamilyName, ?G
    'foaf:familyName'//3, % +M, +Agent,                ?G
    'foaf:givenName'/4,   % +M, +Agent,   -GivenName,  ?G
    'foaf:givenName'//3,  % +M, +Agent,                ?G
    'foaf:homepage'/4,    % +M, +Agent,   -Iri,        ?G
    'foaf:homepage'//3,   % +M, +Agent,                ?G
    'foaf:mbox'/4,        % +M, +Agent,   -Iri,        ?G
    'foaf:mbox'//3,       % +M, +Agent,                ?G
    'foaf:name'/4,        % +M, +Agent,   -Name,       ?G
    'foaf:name'//3,       % +M, +Agent,                ?G
    'org:memberOf'//3,    % +M, +Agent,                ?G
    rdfa_date_time//2,    %     +P, +Something
    rdfa_date_time//3,    %     +P, +Something, +Masks
    rdfa_prefixed_iri/2,  % +Iri, -PrefixedIri
    rdfa_prefixes/2,      % +Aliases, -Prefixes
    'sioc:content'//3,    % +M, +Article,              ?G
    'sioc:reply_of'//3    % +M, +Comment,              ?G
  ]
).

/** <module> RDFa

@author Wouter Beek
@version 2016/02-2016/07
*/

:- use_module(library(apply)).
:- use_module(library(date_time/date_time)).
:- use_module(library(hash_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time_human)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(http/html_write)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pairs)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdfa/rdfa_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(string_ext)).
:- use_module(library(xsd/xsd)).

:- qb_alias(bf, 'http://bibframe.org/vocab/').
:- qb_alias(org, 'http://www.w3.org/ns/org#').
:- qb_alias(sioc, 'http://rdfs.org/sioc/ns#').

:- rdf_meta
   agent_image(+, r, -, r),
   agent_image(+, r, r, ?, ?),
   agent_name(+, r, -, r),
   agent_name(+, r, r, ?, ?),
   'bf:subtitle'(+, r, -, r),
   'bf:subtitle'(+, r, r, ?, ?),
   creators(+, r, r, ?, ?),
   'dc:abstract'(+, r, -, r),
   'dc:abstract'(+, r, r, ?, ?),
   'dc:created'(+, r, -, r),
   'dc:created'(+, r, r, ?, ?),
   'dc:creator'(+, r, -, r),
   'dc:creator'(+, r, r, ?, ?),
   'dc:subject'(+, r, -, r),
   'dc:title'(+, r, -, r),
   'dc:title'(+, r, r, ?, ?),
   'foaf:depiction'(+, r, -, r),
   'foaf:depiction'(+, r, r, ?, ?),
   'foaf:familyName'(+, r, -, r),
   'foaf:familyName'(+, r, r, ?, ?),
   'foaf:givenName'(+, r, -, r),
   'foaf:givenName'(+, r, r, ?, ?),
   'foaf:homepage'(+, r, -, r),
   'foaf:homepage'(+, r, r, ?, ?),
   'foaf:mbox'(+, r, -, r),
   'foaf:mbox'(+, r, r, ?, ?),
   'foaf:name'(+, r, -, r),
   'foaf:name'(+, r, r, ?, ?),
   'org:memberOf'(+, r, r, ?, ?),
   rdfa_date_time(r, +, ?, ?),
   rdfa_date_time(r, +, +, ?, ?),
   'sioc:content'(+, r, r, ?, ?),
   'sioc:reply_of'(+, r, r, ?, ?).





%! agent_gravatar(+M, +Agent, -Iri, +G) is det.

agent_gravatar(M, Agent, Iri, G) :-
  once('foaf:mbox'(M, Agent, EMail, G)),
  downcase_atom(EMail, CanonicalEMail),
  md5(CanonicalEMail, Hash),
  atomic_list_concat(['',avatar,Hash], /, Path),
  iri_comps(Iri, uri_components(http,'www.gravatar.com',Path,_,_)).



%! agent_image(+M, +Agent, -Img, ?G) is det.
%! agent_image(+M, +Agent, ?G)// is det.

agent_image(M, Agent, Img, G) :-
  'foaf:depiction'(M, Agent, Img, G).
agent_image(M, Agent, Img, G) :-
  agent_gravatar(M, Agent, Img, G).


agent_image(M, Agent, G) -->
  {
    agent_name(M, Agent, Name, G),
    agent_image(M, Agent, Img, G)
  },
  internal_link(Agent, image(Img, [alt=Name,property='foaf:depiction'])).



%! agent_name(+M, +Agent, -Name, ?G) is det.
%! agent_name(+M, +Agent, ?G)// is det.

agent_name(M, Agent, Str, G) :-
  'foaf:givenName'(M, Agent, GivenName, G),
  'foaf:familyName'(M, Agent, FamilyName, G), !,
  q_literal_string(GivenName, Str1),
  q_literal_string(FamilyName, Str2),
  string_list_concat([Str1,Str2], " ", Str).
agent_name(M, Agent, Str, G) :-
  'foaf:name'(M, Agent, Name, G),
  q_literal_string(Name, Str).


agent_name(M, Agent, G) -->
  internal_link(Agent, \agent_name0(M, Agent, G)).
agent_name(M, Agent, G) -->
  'foaf:name'(M, Agent, G).


agent_name0(M, Agent, G) -->
  html([
    \'foaf:givenName'(M, Agent, G), %'
    " ",
    \'foaf:familyName'(M, Agent, G) %'
  ]), !.



%! 'bf:subtitle'(+M, +Article, -Subtitle, G)// is det.
%! 'bf:subtitle'(+M, +Article, ?G)// is det.

'bf:subtitle'(M, Article, Subtitle, G) :-
  q_pref_string(M, Article, bf:subtitle, Subtitle, G).


'bf:subtitle'(M, Article, G) -->
  {'bf:subtitle'(M, Article, Subtitle, G)},
  html(h2(span(property='bf:subtitle', \qh_literal(Subtitle)))).



%! creators(+M, +Res, +G)// is det.

creators(M, Res, G) -->
  {q_list_pl(M, Res, dc:creator, Agents, G)},
  html(
    ol([inlist='',rel='dc:creator'],
      \html_maplist(agent_item0(M, G), Agents)
    )
  ).

agent_item0(M, G, Agent) -->
  html(li(\agent_name(M, Agent, G))).



%! 'dc:abstract'(+M, +Res, -Abstract, ?G) is det.
%! 'dc:abstract'(+M, +Res, ?G)// is det.

'dc:abstract'(M, Res, Abstract, G) :-
  q_pref_string(M, Res, dc:abstract, Abstract, G).


'dc:abstract'(M, Res, G) -->
  {once('dc:abstract'(M, Res, Abstract, G))},
  html(p(property='dc:abstract', \qh_literal(Abstract, _{max_length: 150}))).



%! 'dc:created'(+M, +Res, -DateTime, ?G) is nondet.
%! 'dc:created'(+M, +Res, ?G)// is det.

'dc:created'(M, Res, DT, G) :-
  q(M, Res, dc:created, DT^^xsd:date, G).


'dc:created'(M, Res, G) -->
  {once('dc:created'(M, Res, DT, G))},
  rdfa_date_time(dc:created, DT, [offset]).



%! 'dc:creator'(+M, +Res, -Agent, ?G) is nondet.
%! 'dc:creator'(+M, +Res, ?G)// is det.

'dc:creator'(M, Res, Agent, G) :-
  q(M, Res, dc:creator, Agent, G).
  

'dc:creator'(M, Res, G) -->
  {
    once('dc:creator'(M, Res, Agent, G))
  },
  internal_link(Agent, [property='dc:creator'], \agent_name(M, Agent, G)).



%! 'dc:subject'(+M, +Res, -Subject, ?G) is nondet.

'dc:subject'(M, Res, Tag, G) :-
  q(M, Res, dc:subject, Tag, G).



%! 'dc:title'(+M, +Res, -Title, ?G) is det.
%! 'dc:title'(+M, +Res, ?G)// is det.

'dc:title'(M, Res, Title, G) :-
  q_pref_string(M, Res, dc:title, Title, G).


'dc:title'(M, Res, G) -->
  {'dc:title'(M, Res, Title, G)},
  html(h1(property='dc:title', \qh_literal(Title))).



%! 'foaf:depiction'(+M, +Agent, -Img, ?G) is nondet.
%! 'foaf:depiction'(+M, +Agent, ?G)// is det.

'foaf:depiction'(M, Agent, Img, G) :-
  q(M, Agent, foaf:depiction, Img^^xsd:anyURI, G).


'foaf:depiction'(M, Agent, G) -->
  {once('foaf:depiction'(M, Agent, Img, G))},
  image(Img, [property='foaf:depiction']).



%! 'foaf:familyName'(+M, +Agent, -FamilyName, ?G) is det.
%! 'foaf:familyName'(+M, +Agent, ?G)// is det.

'foaf:familyName'(M, Agent, FamilyName, G) :-
  q_pref_string(M, Agent, foaf:familyName, FamilyName, G).


'foaf:familyName'(M, Agent, G) -->
  {once('foaf:familyName'(M, Agent, FamilyName, G))},
  html(span(property='foaf:familyName', \qh_literal(FamilyName))).



%! 'foaf:givenName'(+M, +Agent, -GivenName, ?G) is det.
%! 'foaf:givenName'(+M, +Agent, ?G)// is det.

'foaf:givenName'(M, Agent, GivenName, G) :-
  q_pref_string(M, Agent, foaf:givenName, GivenName, G).


'foaf:givenName'(M, Agent, G) -->
  {'foaf:givenName'(M, Agent, GivenName, G)}, !,
  html(span(property='foaf:givenName', \qh_literal(GivenName))).



%! 'foaf:homepage'(+M, +Agent, -Iri, G) is nondet.
%! 'foaf:homepage'(+M, +Agent, ?G)// is det.

'foaf:homepage'(M, Agent, Iri, G) :-
  q(M, Agent, foaf:homepage, Iri^^xsd:anyURI, G).


'foaf:homepage'(M, Agent, G) -->
  {once('foaf:homepage'(M, Agent, Iri, G))},
  external_link(Iri, [rel='foaf:homepage'], [\bs_icon(web)," ",code(Iri)]).



%! 'foaf:mbox'(+M, +Agent, -Iri,?G) is nondet.
%! 'foaf:mbox'(+M, +Agent, ?G)// is det.

'foaf:mbox'(M, Agent, Iri, G) :-
  q(M, Agent, foaf:mbox, Iri^^xsd:anyURI, G).


'foaf:mbox'(M, Agent, G) -->
  {once('foaf:mbox'(M, Agent, Iri, G))},
  mail_link(Iri).



%! 'foaf:name'(+M, +Agent, -Name, ?G) is det.
%! 'foaf:name'(+M, +Agent, ?G)// is det.

'foaf:name'(M, Agent, Name, G) :-
  q_pref_string(M, Agent, foaf:name, Name, G).


'foaf:name'(M, Agent, G) -->
  {'foaf:name'(M, Agent, Name, G)},
  html(span(property='foaf:name', \qh_literal(Name))).



%! 'org:memberOf'(+M, +Agent, ?G)// is det.

'org:memberOf'(M, Agent, G) -->
  {
    once(q(M, Agent, org:memberOf, Organization, G)),
    once(q_pref_label(M, Organization, Label, G)),
    rdfa_prefixed_iri(Organization, Organization0)
  },
  html(span(property=Organization0, \qh_literal(Label))).



%! rdfa_date_time(+P, +Something)// is det.
%! rdfa_date_time(+P, +Something, +Masks)// is det.

rdfa_date_time(P1, Something) -->
  rdfa_date_time(P1, Something, []).


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
  maplist(q_alias_prefix, Aliases, Prefixes),
  pairs_keys_values(Pairs, Aliases, Prefixes),
  maplist(pair_to_prefix0, Pairs, Defs0),
  atomic_list_concat(Defs0, ' ', Defs).

pair_to_prefix0(Alias-Prefix, Def) :-
  atomic_list_concat([Alias,Prefix], ': ', Def).



'sioc:content'(M, Article, G) -->
  {once(q(M, Article, sioc:content, Content, G))},
  html(div(property='sioc:content', \qh_literal(Content))).



'sioc:reply_of'(M, Comment, G) -->
  {once(q(M, Comment, sioc:reply_of, Article, G))},
  html(span(rel='sioc:reply_of', \'dc:title'(M, Article, G))). %'
