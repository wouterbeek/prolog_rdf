:- module(rdf_prefix, []).

/** <module> RDF prefix

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(uri/uri_ext)).

:- initialization
   forall(language(LTag), register_language_prefixes(LTag)).

language(ab).
language(ace).
language(af).
language(als).
language(am).
language(an).
language(ang).
language(ar).
language(arc).
language(arz).
language(as).
language(ast).
language(av).
language(ay).
language(az).
language(ba).
language(bar).
language('bat-smg').
language(bat_smg).
language(bcl).
language(bcl_smg).
language(be).
language('be-x-old').
language(be_x_old).
language(bg).
language(bi).
language(bjn).
language(bm).
language(bn).
language(bo).
language(bpy).
language(br).
language(bs).
language(bxr).
language(ca).
language(cdo).
language(ce).
language(ceb).
language(chr).
language(chy).
language(ckb).
language(co).
language(commons).
language(cr).
language(crh).
language(cs).
language(csb).
language(cv).
language(cy).
language(da).
language(de).
language(diq).
language(dv).
language(ee).
language(el).
language(en).
language(eo).
language(es).
language(et).
language(ext).
language(eu).
language(fa).
language(fi).
language(fiu).
language(fiu_vro).
language(fj).
language(fo).
language(fr).
language(frp).
language(frr).
language(fy).
language(ga).
language(gan).
language(gd).
language(gl).
language(gn).
language(got).
language(gu).
language(gv).
language(ha).
language(hak).
language(he).
language(hi).
language(hif).
language(hr).
language(hsb).
language(ht).
language(hu).
language(hy).
language(ia).
language(id).
language(ig).
language(ilo).
language(io).
language(is).
language(it).
language(ja).
language(jbo).
language(jv).
language(ka).
language(kaa).
language(kab).
language(kbd).
language(ki).
language(kk).
language(kl).
language(km).
language(kn).
language(ko).
language(koi).
language(ks).
language(ku).
language(kv).
language(ky).
language(la).
language(lb).
language(lbe).
language(lez).
language(li).
language(lmo).
language(ln).
language(lt).
language(lv).
language(map_bms).
language(mg).
language(mhr).
language(mk).
language(ml).
language(mn).
language(mr).
language(mrj).
language(ms).
language(my).
language(na).
language(nah).
language(nap).
language(nds).
language(nds_nl).
language(ne).
language(new).
language(nl).
language(nn).
language(no).
language(nrm).
language(nv).
language(oc).
language(or).
language(pa).
language(pam).
language(pcd).
language(pms).
language(pnb).
language(pl).
language(pt).
language(qu).
language(ro).
language('roa-rup').
language(ru).
language(rw).
language(sa).
language(sah).
language(scn).
language(sco).
language(se).
language(sh).
language(si).
language(simple).
language(sk).
language(sl).
language(sm).
language(sn).
language(so).
language(sq).
language(sr).
language(srn).
language(su).
language(sv).
language(sw).
language(szl).
language(ta).
language(te).
language(tg).
language(th).
language(tl).
language(to).
language(tpi).
language(tr).
language(tt).
language(tum).
language(udm).
language(ug).
language(uk).
language(ur).
language(uz).
language(vec).
language(vi).
language(vo).
language(vls).
language(wa).
language(war).
language(wo).
language(wuu).
language(xal).
language(yi).
language(yo).
language(yoh).
language(zh).
language('zh-min-nan').
language(zh_min_nan).
language('zh-yue').
language(zh_yue).

register_language_prefixes(Language) :-
  atomic_list_concat([Language,dbpedia,org], ., DBpediaAuth),
  % category prefix
  atomic_list_concat([Language,dbc], ., Prefix1),
  uri_comps(Uri1, uri(http,DBpediaAuth,[resource,'Category:'],_,_)),
  rdf_register_prefix(Prefix1, Uri1),
  % property prefix
  atomic_list_concat([Language,dbp], ., Prefix2),
  uri_comps(Uri2, uri(http,DBpediaAuth,[property,''],_,_)),
  rdf_register_prefix(Prefix2, Uri2),
  % resource prefix
  atomic_list_concat([Language,dbr], ., Prefix3),
  uri_comps(Uri3, uri(http,DBpediaAuth,[resource,''],_,_)),
  rdf_register_prefix(Prefix3, Uri3),
  % Wikidata
  atomic_list_concat([Language,wikidata], ., Prefix4),
  atomic_list_concat([Language,wikipedia,org], ., WikidataAuth),
  uri_comps(Uri4, uri(http,WikidataAuth,[wiki,''],_,_)),
  rdf_register_prefix(Prefix4, Uri4).

:- rdf_register_prefix(bibframe, 'http://bibframe.org/vocab/').
:- rdf_register_prefix('_', 'http://lodlaundromat.org/.well-known/genid/').
:- rdf_register_prefix(cms, 'http://SemanticCMS.cc/vocab/').
:- rdf_register_prefix(crs, 'http://www.opengis.net/def/crs/OGC/1.3/').
:- rdf_register_prefix(csvw, 'http://www.w3.org/ns/csvw#').
:- rdf_register_prefix(csvw, 'http://www.w3.org/ns/csvw#').
:- rdf_register_prefix(cyc, 'http://sw.opencyc.org/concept/').
:- rdf_register_prefix(dawgt, 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#').
:- rdf_register_prefix(dbc, 'http://dbpedia.org/resource/Category:').
:- rdf_register_prefix(dbo, 'http://dbpedia.org/ontology/').
:- rdf_register_prefix(dbp, 'http://dbpedia.org/property/').
:- rdf_register_prefix(dbr, 'http://dbpedia.org/resource/').
:- rdf_register_prefix(dbt, 'http://dbpedia.org/datatype/').
:- rdf_register_prefix(dby, 'http://dbpedia.org/class/yago/').
:- rdf_register_prefix(dcat, 'http://www.w3.org/ns/dcat#').
:- rdf_register_prefix(dcterms, 'http://purl.org/dc/terms/').
:- rdf_register_prefix(dctype, 'http://purl.org/dc/dcmitype/').
:- rdf_register_prefix(dqv, 'http://www.w3.org/ns/dqv#').
:- rdf_register_prefix(earl, 'http://www.w3.org/ns/earl#').
:- rdf_register_prefix(ex, 'https://example.org/').
:- rdf_register_prefix(fb, 'http://ogp.me/ns/fb#').
:- rdf_register_prefix(freebase, 'http://rdf.freebase.com/ns/').
:- rdf_register_prefix(fn, 'http://www.w3.org/2005/xpath-functions#').
:- rdf_register_prefix(formats, 'http://www.w3.org/ns/formats/').
:- rdf_register_prefix(geo, 'http://www.opengis.net/ont/geosparql#').
:- rdf_register_prefix(geof, 'http://www.opengis.net/def/function/geosparql/').
:- rdf_register_prefix(geonames, 'http://sws.geonames.org/').
:- rdf_register_prefix(geor, 'http://www.opengis.net/def/rule/geosparql/').
:- rdf_register_prefix(gg, 'http://www.gemeentegeschiedenis.nl/gg-schema#').
:- rdf_register_prefix(gml, 'http://www.opengis.net/ont/gml#').
:- rdf_register_prefix(gr, 'http://purl.org/goodrelations/v1#').
:- rdf_register_prefix(graph, 'https://example.org/graph/').
:- rdf_register_prefix(grddl, 'http://www.w3.org/2003/g/data-view#').
:- rdf_register_prefix(http, 'http://www.w3.org/2011/http#').
:- rdf_register_prefix(hydra, 'http://www.w3.org/ns/hydra/core#').
:- rdf_register_prefix(ical, 'http://www.w3.org/2002/12/cal/icaltzd#').
:- rdf_register_prefix(lexvo, 'http://lexvo.org/ontology#').
:- rdf_register_prefix(ma, 'http://www.w3.org/ns/ma-ont#').
:- rdf_register_prefix(mf, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#').
:- rdf_register_prefix(nyt, 'http://data.nytimes.com/').
:- rdf_register_prefix(odp, 'http://www.ontologydesignpatterns.org/').
:- rdf_register_prefix(openlinks, 'http://www.openlinksw.com/schemas/virtrdf#').
:- rdf_register_prefix(org, 'http://www.w3.org/ns/org#').
:- rdf_register_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_register_prefix(qb, 'http://purl.org/linked-data/cube#').
:- rdf_register_prefix(qt, 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#').
:- rdf_register_prefix(rdfa, 'http://www.w3.org/ns/rdfa#').
:- rdf_register_prefix(rdft, 'http://www.w3.org/ns/rdftest#').
:- rdf_register_prefix(rif, 'http://www.w3.org/2007/rif#').
:- rdf_register_prefix(rif, 'http://www.w3.org/2007/rif#').
:- rdf_register_prefix(role, 'http://www.w3.org/1999/xhtml/vocab#role').
:- rdf_register_prefix(rr, 'http://www.w3.org/ns/r2rml#').
:- rdf_register_prefix(schema, 'http://schema.org/').
:- rdf_register_prefix(sd, 'http://www.w3.org/ns/sparql-service-description#').
:- rdf_register_prefix(sf, 'http://www.opengis.net/ont/sf#').
:- rdf_register_prefix(sfn, ' http://www.w3.org/ns/sparql#').
:- rdf_register_prefix(sh, 'http://www.w3.org/ns/shacl#').
:- rdf_register_prefix(sioc, 'http://rdfs.org/sioc/ns#').
:- rdf_register_prefix(skosxl, 'http://www.w3.org/2008/05/skos-xl#').
:- rdf_register_prefix('sparql-results', 'http://www.w3.org/2005/sparql-results#').
:- rdf_register_prefix(t, 'https://triply.cc/ontology/').
:- rdf_register_prefix(uom, 'http://www.opengis.net/def/uom/OGC/1.0/').
:- rdf_register_prefix(vcard, 'http://www.w3.org/2006/vcard/ns#').
:- rdf_register_prefix(wdr, 'http://www.w3.org/2007/05/powder#').
:- rdf_register_prefix(wdrs, 'http://www.w3.org/2007/05/powder-s#').
:- rdf_register_prefix(wdt, 'http://www.wikidata.org/prop/direct/').
:- rdf_register_prefix(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').
:- rdf_register_prefix(wv, 'http://vocab.org/waiver/terms/norms').
:- rdf_register_prefix(xhv, 'http://www.w3.org/1999/xhtml/vocab#').
:- rdf_register_prefix(xml, 'http://www.w3.org/XML/1998/namespace').
:- rdf_register_prefix(yago, 'http://yago-knowledge.org/resource/').
