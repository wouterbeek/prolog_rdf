:- module(
  rdfa_ext,
  [
    rdfa_agent_image//3, % +M, +Agent, ?G
    rdfa_agent_name//3,  % +M, +Agent, ?G
    rdfa_creators//3,    % +M, +Resource, ?G
    rdfa_homepage//3,    % +M, +Agent, ?G
    rdfa_mbox//3,        % +M, +Agent, ?G
    rdfa_date_time//3,   % +P, +Datetime, +Options
    rdfa_prefixed_iri/2, % +Iri, -PrefixedIri
    rdfa_prefixes//0,
    rdfa_prefixes//1     % +Pairs:list(pair(atom))
  ]
).

/** <module> RDFa extensions

@author Wouter Beek
@version 2017/06-2017/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(date_time)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time_human)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/rdf_html)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(yall)).

:- multifile
    rdfa:prefix/1,
    rdfa:predefined_prefix/1.

rdfa:predefined_prefix(csvw).
rdfa:predefined_prefix(dcat).
rdfa:predefined_prefix(grddl).
rdfa:predefined_prefix(ma).
rdfa:predefined_prefix(org).
rdfa:predefined_prefix(owl).
rdfa:predefined_prefix(prov).
rdfa:predefined_prefix(qb).
rdfa:predefined_prefix(rdf).
rdfa:predefined_prefix(rdfa).
rdfa:predefined_prefix(rdfs).
rdfa:predefined_prefix(rif).
rdfa:predefined_prefix(rr).
rdfa:predefined_prefix(sd).
rdfa:predefined_prefix(skos).
rdfa:predefined_prefix(skosxl).
rdfa:predefined_prefix(void).
rdfa:predefined_prefix(wdr).
rdfa:predefined_prefix(wdrs).
rdfa:predefined_prefix(xhv).
rdfa:predefined_prefix(xml).
rdfa:predefined_prefix(xsd).

:- rdf_meta
   rdfa_agent_image(+, r, r, ?, ?),
   rdfa_agent_name(+, r, r, ?, ?),
   rdfa_creators(+, r, r, ?, ?),
   rdfa_familyName(+, r, r, ?, ?),
   rdfa_homepage(+, r, r, ?, ?),
   rdfa_mbox(+, r, r, ?, ?),
   rdfa_date_time(r, +, +, ?, ?).
   




%! rdfa_agent_image(+M, +Agent, ?G)// is det.

rdfa_agent_image(M, Agent, G) -->
  {
    rdf_agent_name(M, Agent, Name, G),
    rdf_agent_image(M, Agent, Image^^xsd:anyURI, G)
  },
  html(a(href=Agent, img([alt=Name,property='foaf:depiction',src=Image], []))).



%! rdfa_agent_name(+M, +Agent, ?G)// is det.

rdfa_agent_name(M, Agent, G) -->
  html(a(href=Agent, \rdfa_agent_name0(M, Agent, G))).
rdfa_agent_name(M, Agent, G) -->
  {rdf_pref_string_lexical_form(M, Agent, foaf:name, Name, G)},
  html(span(property='foaf:name', Name)).

rdfa_agent_name0(M, Agent, G) -->
  html([
    \rdfa_givenName(M, Agent, G), %'
    " ",
    \rdfa_familyName(M, Agent, G) %'
  ]), !.



%! rdfa_creators(+M, +Resource, +G)// is det.
%
% Generates RDFa HTML for the creators of resource Resource.
%
% Creators recorded with property `dc:creator`.

rdfa_creators(M, Resource, G) -->
  {findall(Agent, rdf_creator(M, Resource, Agent, G), Agents)},
  html(
    ol([inlist='',rel='dc:creator'],
      \html_maplist(rdfa_creators_item0(M, G), Agents)
    )
  ).

rdfa_creators_item0(M, G, Agent) -->
  html(li(\rdfa_agent_name(M, Agent, G))).



%! rdfa_date_time(+P, +Datetime:dt, +Options:list(compound))// is det.

rdfa_date_time(P1, Datetime1, Options) -->
  {
    html_date_time_machine(Datetime1, MachineString),
    dict_get(masks, Options, [], Masks),
    date_time_masks(Masks, Datetime1, Datetime2),
    xsd_date_time_datatype(Datetime2, D1),
    maplist(rdfa_prefixed_iri, [D1,P1], [D2,P2])
  },
  html(
    time([datatype=D2,datetime=MachineString,property=P2],
      \html_date_time_human(Datetime2, Options)
    )
  ).



%! rdfa_familyName(+M, +Agent, ?G)// is det.

rdfa_familyName(M, Agent, G) -->
  {once(rdf_familyName(M, Agent, FamilyName, G))},
  html(span(property='foaf:familyName', \rdf_html_literal(FamilyName))).



%! rdfa_givenName(+M, +Agent, ?G)// is det.

rdfa_givenName(M, Agent, G) -->
  {rdf_givenName(M, Agent, GivenName, G)}, !,
  html(span(property='foaf:givenName', \rdf_html_literal(GivenName))).



%! rdfa_homepage(+M, +Agent, ?G)// is det.

rdfa_homepage(M, Agent, G) -->
  {rdf_chk(M, Agent, foaf:homepage, Uri^^xsd:anyURI, G)},
  html(
    a([href=Uri,rel='foaf:homepage',target='_blank'], [
      \icon(web),
      " ",
      code(Uri)
    ])
  ).



%! rdfa_mbox(+M, +Agent, ?G)// is det.

rdfa_mbox(M, Agent, G) -->
  {rdf_chk(M, Agent, foaf:mbox, Uri^^xsd:anyURI, G)},
  mail_link_and_icon(Uri).



%! rdfa_prefixed_iri(+Iri, -PrefixedIri) is det.

rdfa_prefixed_iri(Iri, PrefixedIri) :-
  rdf_global_id(Alias:Local, Iri),
  atomic_list_concat([Alias,Local], :, PrefixedIri).



%! rdfa_prefixes// is det.
%! rdfa_prefixes(+Pairs:list(pair(atom)))// is det.
%
% Does not include RDF prefixes that are predefined in RDFa.
%
% Does not emit anything if there are no currently registered
% non-preferined RDF prefixes.

rdfa_prefixes -->
  {
    aggregate_all(
      set(Prefix-Iri),
      (
        rdfa:prefix(Prefix),
        \+ rdfa:predefined_prefix(Prefix),
        rdf_current_prefix(Prefix, Iri)
      ),
      Pairs
    )
  },
  rdfa_prefixes(Pairs).


rdfa_prefixes([]) --> !,
  html([]).
rdfa_prefixes(Pairs) -->
  {
    maplist(
      [Prefix-Iri,Atom]>>atomic_list_concat([Prefix,Iri], ': ', Atom),
      Pairs,
      Atoms
    ),
    atomic_list_concat(Atoms, ' ', Atom)
  },
  html_root_attribute(prefix, Atom).
