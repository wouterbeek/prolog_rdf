:- module(
  rdf_prefix,
  [
    rdf_assert_prefix/1,    % +Pair
    rdf_assert_prefix/2,    % +Alias, +Iri
    rdf_assert_prefixes/0,
    rdf_prefix/1,           % ?Alias
   %rdf_prefix/2,           % ?Alias, ?Iri
   %rdf_prefix_any/2,       % ?PrefixedPlTerm, ?PlTerm
   %rdf_prefix_iri/2,       % ?PrefixedIri, ?Iri
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
     rdf_global_id/2 as rdf_prefix_iri,
     rdf_global_object/2 as rdf_prefix_term,
     rdf_global_term/2 as rdf_prefix_any
   ]).

:- use_module(library(semweb/rdf_prefixes), []).
:- use_module(library(uri)).

:- use_module(library(sw/rdf_term)).

:- initialization
   retract(rdf_db:ns(dcterms,_)).

:- meta_predicate
    rdf_prefix_maplist(1, +).

:- rdf_meta
   rdf_prefix_maplist(:, t),
   rdf_prefix_member(t, t),
   rdf_prefix_memberchk(t, t),
   rdf_prefix_selectchk(t, t, t).





%! rdf_assert_prefix(+Pair:pair(atom)) is det.
%
% Syntactic variant of rdf_assert_prefix/2 that allows for pair
% notation (thus keeping the alias and IRI prefix together) when used
% with maplist/2.

rdf_assert_prefix(Alias-Iri) :-
  rdf_assert_prefix(Alias, Iri).


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
  forall(prefix(Alias,Iri), rdf_assert_prefix(Alias, Iri)).

rdf_assert_dbpedia_prefixes :-
  forall(ltag(LTag), rdf_assert_dbpedia_prefixes(LTag)).

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
% Syntactic variant of rdf_prefix_iri/2 that works with maplist/3.

rdf_prefix_iri(Alias, Local, Iri) :-
  rdf_prefix_iri(Alias:Local, Iri).



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



%! rdf_prefix_selectchk(+Elem:rdf_term, +L:list, -Rest:list) is det.
%
% Calls selectchk/3 under RDF prefix expansion.

rdf_prefix_selectchk(Elem, L, Rest) :-
  selectchk(Elem, L, Rest).





% LANGUAGE TAGS THAT ARE USED IN DBPEDIA %

ltag(ab).
ltag(ace).
ltag(af).
ltag(als).
ltag(am).
ltag(an).
ltag(ang).
ltag(ar).
ltag(arc).
ltag(arz).
ltag(as).
ltag(ast).
ltag(av).
ltag(ay).
ltag(az).
ltag(ba).
ltag(bar).
ltag('bat-smg').
ltag(bat_smg).
ltag(bcl).
ltag(bcl_smg).
ltag(be).
ltag('be-x-old').
ltag(be_x_old).
ltag(bg).
ltag(bi).
ltag(bjn).
ltag(bm).
ltag(bn).
ltag(bo).
ltag(bpy).
ltag(br).
ltag(bs).
ltag(bxr).
ltag(ca).
ltag(cdo).
ltag(ce).
ltag(ceb).
ltag(chr).
ltag(chy).
ltag(ckb).
ltag(co).
ltag(commons).
ltag(cr).
ltag(crh).
ltag(cs).
ltag(csb).
ltag(cv).
ltag(cy).
ltag(da).
ltag(de).
ltag(diq).
ltag(dv).
ltag(ee).
ltag(el).
ltag(en).
ltag(eo).
ltag(es).
ltag(et).
ltag(ext).
ltag(eu).
ltag(fa).
ltag(fi).
ltag(fiu).
ltag(fiu_vro).
ltag(fj).
ltag(fo).
ltag(fr).
ltag(frp).
ltag(frr).
ltag(fy).
ltag(ga).
ltag(gan).
ltag(gd).
ltag(gl).
ltag(gn).
ltag(got).
ltag(gu).
ltag(gv).
ltag(ha).
ltag(hak).
ltag(he).
ltag(hi).
ltag(hif).
ltag(hr).
ltag(hsb).
ltag(ht).
ltag(hu).
ltag(hy).
ltag(ia).
ltag(id).
ltag(ig).
ltag(ilo).
ltag(io).
ltag(is).
ltag(it).
ltag(ja).
ltag(jbo).
ltag(jv).
ltag(ka).
ltag(kaa).
ltag(kab).
ltag(kbd).
ltag(ki).
ltag(kk).
ltag(kl).
ltag(km).
ltag(kn).
ltag(ko).
ltag(koi).
ltag(ks).
ltag(ku).
ltag(kv).
ltag(ky).
ltag(la).
ltag(lb).
ltag(lbe).
ltag(lez).
ltag(li).
ltag(lmo).
ltag(ln).
ltag(lt).
ltag(lv).
ltag(map_bms).
ltag(mg).
ltag(mhr).
ltag(mk).
ltag(ml).
ltag(mn).
ltag(mr).
ltag(mrj).
ltag(ms).
ltag(my).
ltag(na).
ltag(nah).
ltag(nap).
ltag(nds).
ltag(nds_nl).
ltag(ne).
ltag(new).
ltag(nl).
ltag(nn).
ltag(no).
ltag(nrm).
ltag(nv).
ltag(oc).
ltag(or).
ltag(pa).
ltag(pam).
ltag(pcd).
ltag(pms).
ltag(pnb).
ltag(pl).
ltag(pt).
ltag(qu).
ltag(ro).
ltag('roa-rup').
ltag(ru).
ltag(rw).
ltag(sa).
ltag(sah).
ltag(scn).
ltag(sco).
ltag(se).
ltag(sh).
ltag(si).
ltag(simple).
ltag(sk).
ltag(sl).
ltag(sm).
ltag(sn).
ltag(so).
ltag(sq).
ltag(sr).
ltag(srn).
ltag(su).
ltag(sv).
ltag(sw).
ltag(szl).
ltag(ta).
ltag(te).
ltag(tg).
ltag(th).
ltag(tl).
ltag(to).
ltag(tpi).
ltag(tr).
ltag(tt).
ltag(tum).
ltag(udm).
ltag(ug).
ltag(uk).
ltag(ur).
ltag(uz).
ltag(vec).
ltag(vi).
ltag(vo).
ltag(vls).
ltag(wa).
ltag(war).
ltag(wo).
ltag(wuu).
ltag(xal).
ltag(yi).
ltag(yo).
ltag(yoh).
ltag(zh).
ltag('zh-min-nan').
ltag(zh_min_nan).
ltag('zh-yue').
ltag(zh_yue).





% COMMON AND/OR STANDARDIZED RDF PREFIXES %

prefix(bibframe, 'http://id.loc.gov/ontologies/bibframe/').
prefix(cms, 'http://SemanticCMS.cc/vocab/').
prefix(crs, 'http://www.opengis.net/def/crs/OGC/1.3/').
prefix(csvw, 'http://www.w3.org/ns/csvw#').
prefix(cyc, 'http://sw.opencyc.org/concept/').
prefix(dawgt, 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#').
prefix(dbc, 'http://dbpedia.org/resource/Category:').
prefix(dbo, 'http://dbpedia.org/ontology/').
prefix(dbp, 'http://dbpedia.org/property/').
prefix(dbr, 'http://dbpedia.org/resource/').
prefix(dbt, 'http://dbpedia.org/datatype/').
prefix(dby, 'http://dbpedia.org/class/yago/').
prefix(dcat, 'http://www.w3.org/ns/dcat#').
prefix(dct, 'http://purl.org/dc/terms/').
prefix(dctype, 'http://purl.org/dc/dcmitype/').
prefix(dqv, 'http://www.w3.org/ns/dqv#').
prefix(earl, 'http://www.w3.org/ns/earl#').
prefix(ex, 'https://example.org/').
prefix(fabio, 'http://purl.org/spar/fabio/').
prefix(fb, 'http://ogp.me/ns/fb#').
prefix(freebase, 'http://rdf.freebase.com/ns/').
prefix(fn, 'http://www.w3.org/2005/xpath-functions#').
prefix(formats, 'http://www.w3.org/ns/formats/').
prefix(geo, 'http://www.opengis.net/ont/geosparql#').
prefix(geof, 'http://www.opengis.net/def/function/geosparql/').
prefix(geonames, 'http://sws.geonames.org/').
prefix(geor, 'http://www.opengis.net/def/rule/geosparql/').
prefix(gg, 'http://www.gemeentegeschiedenis.nl/gg-schema#').
prefix(gml, 'http://www.opengis.net/ont/gml#').
prefix(gr, 'http://purl.org/goodrelations/v1#').
prefix(grddl, 'http://www.w3.org/2003/g/data-view#').
prefix(http, 'http://www.w3.org/2011/http#').
prefix(hydra, 'http://www.w3.org/ns/hydra/core#').
prefix(ical, 'http://www.w3.org/2002/12/cal/icaltzd#').
prefix(lexvo, 'http://lexvo.org/ontology#').
prefix(ma, 'http://www.w3.org/ns/ma-ont#').
prefix(mf, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#').
prefix(nyt, 'http://data.nytimes.com/').
prefix(odp, 'http://www.ontologydesignpatterns.org/').
prefix(openlinks, 'http://www.openlinksw.com/schemas/virtrdf#').
prefix(orcid, 'http://orcid.org/').
prefix(org, 'http://www.w3.org/ns/org#').
prefix(prov, 'http://www.w3.org/ns/prov#').
prefix(qb, 'http://purl.org/linked-data/cube#').
prefix(qt, 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#').
prefix(rdfa, 'http://www.w3.org/ns/rdfa#').
prefix(rdft, 'http://www.w3.org/ns/rdftest#').
prefix(rel, 'http://purl.org/vocab/relationship/').
prefix(rif, 'http://www.w3.org/2007/rif#').
prefix(role, 'http://www.w3.org/1999/xhtml/vocab#role').
prefix(rr, 'http://www.w3.org/ns/r2rml#').
prefix(schema, 'http://schema.org/').
prefix(sd, 'http://www.w3.org/ns/sparql-service-description#').
prefix(sf, 'http://www.opengis.net/ont/sf#').
prefix(sfn, ' http://www.w3.org/ns/sparql#').
prefix(sh, 'http://www.w3.org/ns/shacl#').
prefix(sioc, 'http://rdfs.org/sioc/ns#').
prefix(skosxl, 'http://www.w3.org/2008/05/skos-xl#').
prefix('sparql-results', 'http://www.w3.org/2005/sparql-results#').
prefix(umbel, 'http://umbel.org/umbel#').
prefix(uom, 'http://www.opengis.net/def/uom/OGC/1.0/').
prefix(vcard, 'http://www.w3.org/2006/vcard/ns#').
prefix(wdr, 'http://www.w3.org/2007/05/powder#').
prefix(wdrs, 'http://www.w3.org/2007/05/powder-s#').
prefix(wdt, 'http://www.wikidata.org/prop/direct/').
prefix(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').
prefix(wv, 'http://vocab.org/waiver/terms/norms').
prefix(xhv, 'http://www.w3.org/1999/xhtml/vocab#').
prefix(xml, 'http://www.w3.org/XML/1998/namespace').
prefix(yago, 'http://yago-knowledge.org/resource/').
