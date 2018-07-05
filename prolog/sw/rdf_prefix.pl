:- module(
  rdf_prefix,
  [
    rdf_assert_prefix/1,    % +PairOrAlias
    rdf_assert_prefix/2,    % +Alias, +Iri
    rdf_assert_prefixes/0,
   %rdf_global_id/2,        % ?PrefixedIri, ?Iri
    rdf_prefix/1,           % ?Alias
   %rdf_prefix/2,           % ?Alias, ?Iri
   %rdf_prefix_any/2,       % ?PrefixedPlTerm, ?PlTerm
   %rdf_prefix_term/2,      % ?PrefixedRdfTerm, ?RdfTerm
    rdf_prefix_iri/3,       % ?Alias, ?Local, ?Iri
    rdf_prefix_maplist/2,   % :Goal_1, +Args
    rdf_prefix_member/2,    % ?Elem, +L
    rdf_prefix_memberchk/2, % ?Elem, +L
    rdf_prefix_selectchk/3  % +Elem, +L, -Rest
   %rdf_prefix_term/2       % ?PrefixedRdfTerm, ?RdfTerm
  ]
).

/** <module> RDF prefix support

This module extends module `rdf_prefixes' that is part of the
standards SWI-Prolog distribution.

@tbd There is currently no API for retracting prefix declarations.

@author Wouter Beek
@version 2018
*/

:- reexport(library(semweb/rdf_prefixes), [
     rdf_current_prefix/2 as rdf_prefix,
     rdf_global_id/2,
     rdf_global_object/2 as rdf_prefix_term,
     rdf_global_term/2 as rdf_prefix_any
   ]).

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(semweb/rdf_prefixes), []).
:- use_module(library(uri)).

:- use_module(library(sw/rdf_term)).

:- initialization
   init_rdf_prefix.

:- meta_predicate
    rdf_prefix_maplist(1, +).

:- rdf_meta
   rdf_prefix_maplist(:, t),
   rdf_prefix_member(t, t),
   rdf_prefix_memberchk(t, t),
   rdf_prefix_selectchk(t, t, t).





%! rdf_assert_prefix(+PairOrAlias:or([atom,pair(atom)])) is det.
%
% Syntactic variant of rdf_assert_prefix/2 that allows for pair
% notation (thus keeping the alias and IRI prefix together) when used
% with maplist/2.

rdf_assert_prefix(Alias-Prefix) :- !,
  rdf_assert_prefix(Alias, Prefix).
rdf_assert_prefix(Alias) :-
  prefix_(Alias, Prefix),
  rdf_assert_prefix(Alias-Prefix).


%! rdf_assert_prefix(+Alias:atom, +Iri:atom) is det.
%
% Asserts that IRIs that have the given IRI (`Iri') as a prefix can
% from now on be written down in alternative way, using `Alias:'
% i.o. `Iri' as its prefix.
%
% Since abbreviating long and difficult to read, write, and memorize
% IRIs is the main use case, the prefix `Alias:' will in practice
% almost always be shorter than the prefix `Iri'.
%
% @throws rdf(prefix_exists(Iri,Current,Alias)) when trying to assert
% a new alias (`Alias') for the IRI (`Iri`) that is currently
% abbreviated by another alias (`Current').
%
% @throws rdf(alias_exists(Alias,Old,Iri)) when trying to assert an
% existing alias (`Alias'), that is currently bound to IRI `Current',
% in combination with a new IRI (`Iri`).
%
% @tbd Currently it is not allowed to define the same alias/IRI-pair
% multiple times.  This is not as bad as redefining an existing alias,
% nor is it as bad as introducing multiple definitions for the same
% IRI, so this may in future be changed to emit a warning.
%
% @note Alias can be any atom, which means that not all declarated
% aliases can be exported in N3-family formats, nor can they be used
% in SPARQL queries.

rdf_assert_prefix(Alias, Iri) :-
  rdf_prefixes:rdf_current_prefix(Alias, Iri), !.
rdf_assert_prefix(Alias, Iri) :-
  rdf_prefix(Alias, Old), !,
  throw(rdf(alias_exists(Alias,Old,Iri))).
rdf_assert_prefix(Alias, Iri) :-
  rdf_prefix(Current, Iri), !,
  throw(rdf(prefix_exists(Iri,Current,Alias))).
rdf_assert_prefix(Alias, Iri) :-
  rdf_prefixes:rdf_register_prefix(Alias, Iri).



%! rdf_assert_prefixes is det.

rdf_assert_prefixes :-
  rdf_assert_dbpedia_prefixes,
  forall(prefix_(Alias,Iri), rdf_assert_prefix(Alias, Iri)).

rdf_assert_dbpedia_prefixes :-
  forall(ltag_(LTag), rdf_assert_dbpedia_prefixes(LTag)).

rdf_assert_dbpedia_prefixes(LTag) :-
  atomic_list_concat([LTag,dbpedia,org], ., DBpediaAuthority),
  % category prefix
  atomic_list_concat([LTag,dbc], ., Alias1),
  uri_components(Uri1, uri_components(http,DBpediaAuthority,'/resource/Category:',_,_)),
  rdf_assert_prefix(Alias1, Uri1),
  % property prefix
  atomic_list_concat([LTag,dbp], ., Alias2),
  uri_components(Uri2, uri_components(http,DBpediaAuthority,'/property/',_,_)),
  rdf_assert_prefix(Alias2, Uri2),
  % resource prefix
  atomic_list_concat([LTag,dbr], ., Alias3),
  uri_components(Uri3, uri_components(http,DBpediaAuthority,'/resource/',_,_)),
  rdf_assert_prefix(Alias3, Uri3),
  % Wikidata
  atomic_list_concat([LTag,wikidata], ., Alias4),
  atomic_list_concat([LTag,wikipedia,org], ., WikidataAuthority),
  uri_components(Uri4, uri_components(http,WikidataAuthority,'/wiki/',_,_)),
  rdf_assert_prefix(Alias4, Uri4).



%! rdf_prefix(+Alias:atom) is semidet.
%! rdf_prefix(-Alias:atom) is nondet.

rdf_prefix(Alias) :-
  rdf_prefix(Alias, _).



%! rdf_prefix_iri(-Alias:atom, -Local:atom, +Iri:atom) is det.
%! rdf_prefix_iri(+Alias:atom, +Local:atom, -Iri:atom) is det.
%
% Syntactic variant of rdf_global_id/2 that works with maplist/3.

rdf_prefix_iri(Alias, Local, Iri) :-
  rdf_global_id(Alias:Local, Iri).



%! rdf_prefix_maplist(:Goal_1, +Args1:list) is det.

rdf_prefix_maplist(Goal_1, L) :-
  maplist(Goal_1, L).



%! rdf_prefix_member(?Elem, +L:list) is nondet.
%
% Calls member/2 under RDF prefix expansion.

rdf_prefix_member(Elem, L) :-
  member(Elem, L).



%! rdf_prefix_memberchk(?Elem, +L:list) is nondet.
%
% Calls memberchk/2 under RDF prefix expansion.

rdf_prefix_memberchk(Elem, L) :-
  memberchk(Elem, L).



%! rdf_prefix_selectchk(+Elem, +L:list, -Rest:list) is det.
%
% Calls selectchk/3 under RDF prefix expansion.

rdf_prefix_selectchk(Elem, L, Rest) :-
  selectchk(Elem, L, Rest).





% LANGUAGE TAGS THAT ARE USED IN DBPEDIA %

ltag_(ab).
ltag_(ace).
ltag_(af).
ltag_(als).
ltag_(am).
ltag_(an).
ltag_(ang).
ltag_(ar).
ltag_(arc).
ltag_(arz).
ltag_(as).
ltag_(ast).
ltag_(av).
ltag_(ay).
ltag_(az).
ltag_(ba).
ltag_(bar).
ltag_('bat-smg').
ltag_(bat_smg).
ltag_(bcl).
ltag_(bcl_smg).
ltag_(be).
ltag_('be-x-old').
ltag_(be_x_old).
ltag_(bg).
ltag_(bi).
ltag_(bjn).
ltag_(bm).
ltag_(bn).
ltag_(bo).
ltag_(bpy).
ltag_(br).
ltag_(bs).
ltag_(bxr).
ltag_(ca).
ltag_(cdo).
ltag_(ce).
ltag_(ceb).
ltag_(chr).
ltag_(chy).
ltag_(ckb).
ltag_(co).
ltag_(commons).
ltag_(cr).
ltag_(crh).
ltag_(cs).
ltag_(csb).
ltag_(cv).
ltag_(cy).
ltag_(da).
ltag_(de).
ltag_(diq).
ltag_(dv).
ltag_(ee).
ltag_(el).
ltag_(en).
ltag_(eo).
ltag_(es).
ltag_(et).
ltag_(ext).
ltag_(eu).
ltag_(fa).
ltag_(fi).
ltag_(fiu).
ltag_(fiu_vro).
ltag_(fj).
ltag_(fo).
ltag_(fr).
ltag_(frp).
ltag_(frr).
ltag_(fy).
ltag_(ga).
ltag_(gan).
ltag_(gd).
ltag_(gl).
ltag_(gn).
ltag_(got).
ltag_(gu).
ltag_(gv).
ltag_(ha).
ltag_(hak).
ltag_(he).
ltag_(hi).
ltag_(hif).
ltag_(hr).
ltag_(hsb).
ltag_(ht).
ltag_(hu).
ltag_(hy).
ltag_(ia).
ltag_(id).
ltag_(ig).
ltag_(ilo).
ltag_(io).
ltag_(is).
ltag_(it).
ltag_(ja).
ltag_(jbo).
ltag_(jv).
ltag_(ka).
ltag_(kaa).
ltag_(kab).
ltag_(kbd).
ltag_(ki).
ltag_(kk).
ltag_(kl).
ltag_(km).
ltag_(kn).
ltag_(ko).
ltag_(koi).
ltag_(ks).
ltag_(ku).
ltag_(kv).
ltag_(ky).
ltag_(la).
ltag_(lb).
ltag_(lbe).
ltag_(lez).
ltag_(li).
ltag_(lmo).
ltag_(ln).
ltag_(lt).
ltag_(lv).
ltag_(map_bms).
ltag_(mg).
ltag_(mhr).
ltag_(mk).
ltag_(ml).
ltag_(mn).
ltag_(mr).
ltag_(mrj).
ltag_(ms).
ltag_(my).
ltag_(na).
ltag_(nah).
ltag_(nap).
ltag_(nds).
ltag_(nds_nl).
ltag_(ne).
ltag_(new).
ltag_(nl).
ltag_(nn).
ltag_(no).
ltag_(nrm).
ltag_(nv).
ltag_(oc).
ltag_(or).
ltag_(pa).
ltag_(pam).
ltag_(pcd).
ltag_(pms).
ltag_(pnb).
ltag_(pl).
ltag_(pt).
ltag_(qu).
ltag_(ro).
ltag_('roa-rup').
ltag_(ru).
ltag_(rw).
ltag_(sa).
ltag_(sah).
ltag_(scn).
ltag_(sco).
ltag_(se).
ltag_(sh).
ltag_(si).
ltag_(simple).
ltag_(sk).
ltag_(sl).
ltag_(sm).
ltag_(sn).
ltag_(so).
ltag_(sq).
ltag_(sr).
ltag_(srn).
ltag_(su).
ltag_(sv).
ltag_(sw).
ltag_(szl).
ltag_(ta).
ltag_(te).
ltag_(tg).
ltag_(th).
ltag_(tl).
ltag_(to).
ltag_(tpi).
ltag_(tr).
ltag_(tt).
ltag_(tum).
ltag_(udm).
ltag_(ug).
ltag_(uk).
ltag_(ur).
ltag_(uz).
ltag_(vec).
ltag_(vi).
ltag_(vo).
ltag_(vls).
ltag_(wa).
ltag_(war).
ltag_(wo).
ltag_(wuu).
ltag_(xal).
ltag_(yi).
ltag_(yo).
ltag_(yoh).
ltag_(zh).
ltag_('zh-min-nan').
ltag_(zh_min_nan).
ltag_('zh-yue').
ltag_(zh_yue).





% COMMON AND/OR STANDARDIZED RDF PREFIXES %

prefix_(Alias, Prefix) :-
  rdf_db:ns(Alias, Prefix).
prefix_(b, 'http://lodlaundromat.org/.well-known/genid/').
prefix_(bag, 'http://bag.basisregistraties.overheid.nl/def/bag#').
prefix_(bibframe, 'http://id.loc.gov/ontologies/bibframe/').
prefix_(bibo, 'http://purl.org/ontology/bibo/').
prefix_(cms, 'http://SemanticCMS.cc/vocab/').
prefix_(crs, 'http://www.opengis.net/def/crs/OGC/1.3/').
prefix_(csvw, 'http://www.w3.org/ns/csvw#').
prefix_(cyc, 'http://sw.opencyc.org/concept/').
prefix_(dawgt, 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#').
prefix_(dbc, 'http://dbpedia.org/resource/Category:').
prefix_(dbd, 'http://dbpedia.org/datatype/').
prefix_(dbo, 'http://dbpedia.org/ontology/').
prefix_(dbp, 'http://dbpedia.org/property/').
prefix_(dbr, 'http://dbpedia.org/resource/').
prefix_(dby, 'http://dbpedia.org/class/yago/').
prefix_(dcat, 'http://www.w3.org/ns/dcat#').
prefix_(dce, 'http://purl.org/dc/elements/1.1/').
prefix_(dcterm, 'http://purl.org/dc/terms/').
prefix_(dctype, 'http://purl.org/dc/dcmitype/').
prefix_(dolce, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#').
prefix_(dqv, 'http://www.w3.org/ns/dqv#').
prefix_(earl, 'http://www.w3.org/ns/earl#').
prefix_(ex, 'https://example.org/').
prefix_(fabio, 'http://purl.org/spar/fabio/').
prefix_(fb, 'http://ogp.me/ns/fb#').
prefix_(freebase, 'http://rdf.freebase.com/ns/').
prefix_(fn, 'http://www.w3.org/2005/xpath-functions#').
prefix_(formats, 'http://www.w3.org/ns/formats/').
prefix_(geo, 'http://www.opengis.net/ont/geosparql#').
prefix_(geof, 'http://www.opengis.net/def/function/geosparql/').
prefix_(geonames, 'http://sws.geonames.org/').
prefix_(geor, 'http://www.opengis.net/def/rule/geosparql/').
prefix_(gg, 'http://www.gemeentegeschiedenis.nl/gg-schema#').
prefix_(gml, 'http://www.opengis.net/ont/gml#').
prefix_(gr, 'http://purl.org/goodrelations/v1#').
prefix_(grddl, 'http://www.w3.org/2003/g/data-view#').
prefix_(http, 'http://www.w3.org/2011/http#').
prefix_(hydra, 'http://www.w3.org/ns/hydra/core#').
prefix_(ical, 'http://www.w3.org/2002/12/cal/icaltzd#').
prefix_(iolite, 'http://www.ontologydesignpatterns.org/ont/dul/IOLite.owl#').
prefix_(lexvo, 'http://lexvo.org/ontology#').
prefix_(lmm1, 'http://www.ontologydesignpatterns.org/ont/lmm/LMM_L1.owl#').
prefix_(ma, 'http://www.w3.org/ns/ma-ont#').
prefix_(mf, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#').
prefix_(nyt, 'http://data.nytimes.com/').
prefix_(ontopic, 'http://www.ontologydesignpatterns.org/ont/dul/ontopic.owl#').
prefix_(openlinks, 'http://www.openlinksw.com/schemas/virtrdf#').
prefix_(orcid, 'http://orcid.org/').
prefix_(org, 'http://www.w3.org/ns/org#').
prefix_(prov, 'http://www.w3.org/ns/prov#').
prefix_(qb, 'http://purl.org/linked-data/cube#').
prefix_(qt, 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#').
prefix_(rdfa, 'http://www.w3.org/ns/rdfa#').
prefix_(rdft, 'http://www.w3.org/ns/rdftest#').
prefix_(rel, 'http://purl.org/vocab/relationship/').
prefix_(rif, 'http://www.w3.org/2007/rif#').
prefix_(role, 'http://www.w3.org/1999/xhtml/vocab#role').
prefix_(rr, 'http://www.w3.org/ns/r2rml#').
prefix_(schema, 'http://schema.org/').
prefix_(sd, 'http://www.w3.org/ns/sparql-service-description#').
prefix_(sf, 'http://www.opengis.net/ont/sf#').
prefix_(sfn, ' http://www.w3.org/ns/sparql#').
prefix_(sh, 'http://www.w3.org/ns/shacl#').
prefix_(sioc, 'http://rdfs.org/sioc/ns#').
prefix_(spitfire, 'http://spitfire-project.eu/ontology/ns/').
prefix_(skosxl, 'http://www.w3.org/2008/05/skos-xl#').
prefix_('sparql-results', 'http://www.w3.org/2005/sparql-results#').
prefix_(umbel, 'http://umbel.org/umbel#').
prefix_(uom, 'http://www.opengis.net/def/uom/OGC/1.0/').
prefix_(vcard, 'http://www.w3.org/2006/vcard/ns#').
prefix_(wdr, 'http://www.w3.org/2007/05/powder#').
prefix_(wdrs, 'http://www.w3.org/2007/05/powder-s#').
prefix_(wdt, 'http://www.wikidata.org/prop/direct/').
prefix_(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').
prefix_(wv, 'http://vocab.org/waiver/terms/norms').
prefix_(xhv, 'http://www.w3.org/1999/xhtml/vocab#').
prefix_(xml, 'http://www.w3.org/XML/1998/namespace').
prefix_(yago, 'http://yago-knowledge.org/resource/').





% INITIALIZATION %

init_rdf_prefix :-
  maplist([Alias]>>ignore(retract(rdf_db:ns(Alias,_))), [dc,dcterms,serql]).
