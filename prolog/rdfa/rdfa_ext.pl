:- module(
  rdfa_ext,
  [
    agent_image/4,       % +M, +Agent,   -Img,        ?G
    agent_image//3,      % +M, +Agent,                ?G
    agent_name/4,        % +M, +Agent,   -Name,       ?G
    agent_name//3,       % +M, +Agent,                ?G
    bf_subtitle/4,       % +M, +Article, -Subtitle,   ?G
    bf_subtitle//3,      % +M, +Article,              ?G
    creator/4,           % +M, +Res,     -Agent,      ?G
    creators//3,         % +M, +Res,                  ?G
    dc_abstract/4,       % +M, +Res,     -Abstract,   ?G
    dc_abstract//3,      % +M, +Res,                  ?G
    dc_abstract//4,      % +M, +Res,                  ?G, +Opts
    dc_created/4,        % +M, +Res,     -DT,         ?G
    dc_created//3,       % +M, +Res,                  ?G
    dc_creator/4,        % +M, +Res,     -Agent,      ?G
    dc_creator//3,       % +M, +Res,                  ?G
    dc_subject/4,        % +M, +Res,     -Subject,    ?G
    dc_title/4,          % +M, +Res,     -Title,      ?G
    dc_title//3,         % +M, +Res,                  ?G
    foaf_depiction/4,    % +M, +Agent,   -Iri,        ?G
    foaf_depiction//3,   % +M, +Agent,                ?G
    foaf_familyName/4,   % +M, +Agent,   -FamilyName, ?G
    foaf_familyName//3,  % +M, +Agent,                ?G
    foaf_givenName/4,    % +M, +Agent,   -GivenName,  ?G
    foaf_givenName//3,   % +M, +Agent,                ?G
    foaf_homepage/4,     % +M, +Agent,   -Iri,        ?G
    foaf_homepage//3,    % +M, +Agent,                ?G
    foaf_mbox/4,         % +M, +Agent,   -Iri,        ?G
    foaf_mbox//3,        % +M, +Agent,                ?G
    foaf_name/4,         % +M, +Agent,   -Name,       ?G
    foaf_name//3,        % +M, +Agent,                ?G
    org_memberOf//3,     % +M, +Agent,                ?G
    rdfa_date_time//2,   % +P, +Something
    rdfa_date_time//3,   % +P, +Something, +Masks
    rdfa_prefixed_iri/2, % +Iri, -PrefixedIri
    rdfa_prefixes/2,     % +Aliases, -Prefixes
    sioc_content//3,     % +M, +Article,              ?G
    sioc_reply_of//3     % +M, +Comment,              ?G
  ]
).

/** <module> RDFa

@author Wouter Beek
@version 2016/02-2016/08
*/

:- use_module(library(apply)).
:- use_module(library(date_time/date_time)).
:- use_module(library(hash_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time_human)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(http/http_user)).
:- use_module(library(http/html_write)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pairs)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_list)).
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
   bf_subtitle(+, r, -, r),
   bf_subtitle(+, r, r, ?, ?),
   creators(+, r, -, r),
   creators(+, r, r, ?, ?),
   dc_abstract(+, r, -, r),
   dc_abstract(+, r, r, ?, ?),
   dc_created(+, r, -, r),
   dc_created(+, r, r, ?, ?),
   dc_creator(+, r, -, r),
   dc_creator(+, r, r, ?, ?),
   dc_subject(+, r, -, r),
   dc_title(+, r, -, r),
   dc_title(+, r, r, ?, ?),
   foaf_depiction(+, r, -, r),
   foaf_depiction(+, r, r, ?, ?),
   foaf_familyName(+, r, -, r),
   foaf_familyName(+, r, r, ?, ?),
   foaf_givenName(+, r, -, r),
   foaf_givenName(+, r, r, ?, ?),
   foaf_homepage(+, r, -, r),
   foaf_homepage(+, r, r, ?, ?),
   foaf_mbox(+, r, -, r),
   foaf_mbox(+, r, r, ?, ?),
   foaf_name(+, r, -, r),
   foaf_name(+, r, r, ?, ?),
   org_memberOf(+, r, r, ?, ?),
   rdfa_date_time(r, +, ?, ?),
   rdfa_date_time(r, +, +, ?, ?),
   sioc_content(+, r, r, ?, ?),
   sioc_reply_of(+, r, r, ?, ?).





%! agent_gravatar(+M, +Agent, -Iri, +G) is det.

agent_gravatar(M, Agent, Iri, G) :-
  once(foaf_mbox(M, Agent, EMail, G)),
  downcase_atom(EMail, CanonicalEMail),
  md5(CanonicalEMail, Hash),
  atomic_list_concat(['',avatar,Hash], /, Path),
  iri_comps(Iri, uri_components(http,'www.gravatar.com',Path,_,_)).



%! agent_image(+M, +Agent, -Img, ?G) is det.
%! agent_image(+M, +Agent, ?G)// is det.

agent_image(M, Agent, Img, G) :-
  foaf_depiction(M, Agent, Img, G).
agent_image(M, Agent, Img, G) :-
  agent_gravatar(M, Agent, Img, G).


agent_image(M, Agent, G) -->
  {
    agent_name(M, Agent, Name, G),
    agent_image(M, Agent, Img, G)
  },
  data_link(Agent, \image(Img, [alt=Name,property='foaf:depiction'])).



%! agent_name(+M, +Agent, -Name, ?G) is det.
%! agent_name(+M, +Agent, ?G)// is det.

agent_name(_, Agent, "you", _) :-
  current_user(Agent), !.
agent_name(M, Agent, Str, G) :-
  foaf_givenName(M, Agent, GivenName, G),
  foaf_familyName(M, Agent, FamilyName, G), !,
  q_literal_string(GivenName, Str1),
  q_literal_string(FamilyName, Str2),
  string_list_concat([Str1,Str2], " ", Str).
agent_name(M, Agent, Str, G) :-
  foaf_name(M, Agent, Name, G),
  q_literal_string(Name, Str).


agent_name(_, Agent, _) -->
  {current_user(Agent)}, !,
  data_link(Agent, "you").
agent_name(M, Agent, G) -->
  data_link(Agent, \agent_name0(M, Agent, G)).
agent_name(M, Agent, G) -->
  foaf_name(M, Agent, G).


agent_name0(M, Agent, G) -->
  html([
    \foaf_givenName(M, Agent, G), %'
    " ",
    \foaf_familyName(M, Agent, G) %'
  ]), !.



%! bf_subtitle(+M, +Article, -Subtitle, G)// is det.

bf_subtitle(M, Article, Subtitle, G) :-
  q_pref_string(M, Article, bf:subtitle, Subtitle, G).


%! bf_subtitle(+M, +Article, ?G)// is det.

bf_subtitle(M, Article, G) -->
  {bf_subtitle(M, Article, Subtitle, G)},
  html(h2(span(property='bf:subtitle', \qh_literal(Subtitle)))).



%! creator(+M, ?Res, ?Agent, +G) is det.

creator(M, Res, Agent, G) :-
  q_list_member(M, Res, dc:creator, Agent, G).
creator(M, Res, Agent, G) :-
  q(M, Res, dc:creator, Agent, G),
  \+ q_list(M, Agent, G).



%! creators(+M, +Res, +G)// is det.
%
% Generates RDFa HTML for the creators of resource Res.
%
% Creators recorded with property `dc:creator`.
%
% This predicate uses module `http_user` to determine whether one of
% the creators is the current user, if there is one.

creators(M, Res, G) -->
  {findall(Agent, creator(M, Res, Agent, G), Agents)},
  html(
    ol([inlist='',rel='dc:creator'],
      \html_maplist(agent_item0(M, G), Agents)
    )
  ).

agent_item0(M, G, Agent) -->
  html(li(\agent_name(M, Agent, G))).



%! dc_abstract(+M, +Res, -Abstract, ?G) is det.
%! dc_abstract(+M, +Res, ?G)// is det.
%! dc_abstract(+M, +Res, ?G, +Opts)// is det.
%
% Options are passed to qh_literal//2.

dc_abstract(M, Res, Abstract, G) :-
  q_pref_string(M, Res, dc:abstract, Abstract, G).


dc_abstract(M, Res, G) -->
  dc_abstract(M, Res, G, _{}).


dc_abstract(M, Res, G, Opts) -->
  {once(dc_abstract(M, Res, Abstract, G))},
  html(p(property='dc:abstract', \qh_literal(Abstract, Opts))).



%! dc_created(+M, +Res, -DateTime, ?G) is nondet.
%! dc_created(+M, +Res, ?G)// is det.

dc_created(M, Res, DT, G) :-
  q(M, Res, dc:created, DT^^xsd:date, G).


dc_created(M, Res, G) -->
  {once(dc_created(M, Res, DT, G))},
  rdfa_date_time(dc:created, DT, [offset]).



%! dc_creator(+M, +Res, -Agent, ?G) is nondet.
%! dc_creator(+M, +Res, ?G)// is det.

dc_creator(M, Res, Agent, G) :-
  q(M, Res, dc:creator, Agent, G).
  

dc_creator(M, Res, G) -->
  {once(dc_creator(M, Res, Agent, G))},
  data_link(Agent, [property='dc:creator'], \agent_name(M, Agent, G)).



%! dc_subject(+M, +Res, -Subject, ?G) is nondet.

dc_subject(M, Res, Tag, G) :-
  q(M, Res, dc:subject, Tag, G).



%! dc_title(+M, +Res, -Title, ?G) is det.
%! dc_title(+M, +Res, ?G)// is det.

dc_title(M, Res, Title, G) :-
  q_pref_string(M, Res, dc:title, Title, G).


dc_title(M, Res, G) -->
  {dc_title(M, Res, Title, G)},
  html(h1(property='dc:title', \qh_literal(Title))).



%! foaf_depiction(+M, +Agent, -Img, ?G) is nondet.
%! foaf_depiction(+M, +Agent, ?G)// is det.

foaf_depiction(M, Agent, Img, G) :-
  q(M, Agent, foaf:depiction, Img^^xsd:anyURI, G).


foaf_depiction(M, Agent, G) -->
  {once(foaf_depiction(M, Agent, Img, G))},
  image(Img, [property='foaf:depiction']).



%! foaf_familyName(+M, +Agent, -FamilyName, ?G) is det.
%! foaf_familyName(+M, +Agent, ?G)// is det.

foaf_familyName(M, Agent, FamilyName, G) :-
  q_pref_string(M, Agent, foaf:familyName, FamilyName, G).


foaf_familyName(M, Agent, G) -->
  {once(foaf_familyName(M, Agent, FamilyName, G))},
  html(span(property='foaf:familyName', \qh_literal(FamilyName))).



%! foaf_givenName(+M, +Agent, -GivenName, ?G) is det.
%! foaf_givenName(+M, +Agent, ?G)// is det.

foaf_givenName(M, Agent, GivenName, G) :-
  q_pref_string(M, Agent, foaf:givenName, GivenName, G).


foaf_givenName(M, Agent, G) -->
  {foaf_givenName(M, Agent, GivenName, G)}, !,
  html(span(property='foaf:givenName', \qh_literal(GivenName))).



%! foaf_homepage(+M, +Agent, -Iri, G) is nondet.
%! foaf_homepage(+M, +Agent, ?G)// is det.

foaf_homepage(M, Agent, Iri, G) :-
  q(M, Agent, foaf:homepage, Iri^^xsd:anyURI, G).


foaf_homepage(M, Agent, G) -->
  {once(foaf_homepage(M, Agent, Iri, G))},
  external_link(Iri, [rel='foaf:homepage'], [\bs_icon(web)," ",code(Iri)]).



%! foaf_mbox(+M, +Agent, -Iri,?G) is nondet.
%! foaf_mbox(+M, +Agent, ?G)// is det.

foaf_mbox(M, Agent, Iri, G) :-
  q(M, Agent, foaf:mbox, Iri^^xsd:anyURI, G).


foaf_mbox(M, Agent, G) -->
  {once(foaf_mbox(M, Agent, Iri, G))},
  mail_link(Iri).



%! foaf_name(+M, +Agent, -Name, ?G) is det.
%! foaf_name(+M, +Agent, ?G)// is det.

foaf_name(M, Agent, Name, G) :-
  q_pref_string(M, Agent, foaf:name, Name, G).


foaf_name(M, Agent, G) -->
  {foaf_name(M, Agent, Name, G)},
  html(span(property='foaf:name', \qh_literal(Name))).



%! org_memberOf(+M, +Agent, ?G)// is det.

org_memberOf(M, Agent, G) -->
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



sioc_content(M, Article, G) -->
  {once(q(M, Article, sioc:content, Content, G))},
  html(div(property='sioc:content', \qh_literal(Content))).



sioc_reply_of(M, Comment, G) -->
  {once(q(M, Comment, sioc:reply_of, Article, G))},
  html(span(rel='sioc:reply_of', \dc_title(M, Article, G))).
