:- module(
  rdf_namespaces,
  [
    dbpedia_language_tag/1, % ?LanguageTag:atom
    rdf_reduced_location/2 % +FullUrl:url
                           % -ReducedUrl:url
  ]
).

/** <module> RDF database

XML namespace registrations.

@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- dynamic(rdf_reduced_location/1).



:- initialization(dbpedia_localizations).
dbpedia_localizations:-
  forall(
    dbpedia_language_tag(LangTag),
    dbpedia_register(LangTag)
  ).

dbpedia_register(LangTag):-
  atomic_list_concat([LangTag,dbpedia,org], '.', Authority),

  % XML namespace for resources.
  atomic_list_concat([LangTag,dbp], '.', ResourceNamespace),
  uri_components(
    ResourcePrefix,
    uri_components(http,Authority,'/resource/',_,_)
  ),
  rdf_register_prefix(ResourceNamespace, ResourcePrefix),

  % XML namespace for properties.
  atomic_list_concat([LangTag,dbpprop], '.', PropertyNamespace),
  uri_components(
    PropertyPrefix,
    uri_components(http,Authority,'/property/',_,_)
  ),
  rdf_register_prefix(PropertyNamespace, PropertyPrefix).


%! rdf_reduced_location(+FullUrl:url, -ReducedUrl:url) is semidet.

rdf_reduced_location(Url1, Url2):-
  rdf_global_id(Prefix:_, Url1),
  rdf_reduced_location(Prefix), !,
  rdf_current_prefix(Prefix, Url2).


%! rdf_register_reduced_location(+Prefix:atom) is det.

rdf_register_reduced_location(Prefix):-
  assert(rdf_reduced_location(Prefix)).



% Creative Commons
:- rdf_register_prefix(cc, 'http://creativecommons.org/ns#').

% DBpedia category
:- rdf_register_prefix(category, 'http://dbpedia.org/resource/Category:').

% DBpedia datatype
:- rdf_register_prefix(dt, 'http://dbpedia.org/datatype/').

% DBpedia describe
:- rdf_register_prefix('db:describe', 'http://dbpedia.org/describe').

%! dbpedia_language_tag(+LanguageTag:atom) is semidet.
%! dbpedia_language_tag(-LanguageTag:atom) is multi.

dbpedia_language_tag(ab).
dbpedia_language_tag(ace).
dbpedia_language_tag(af).
dbpedia_language_tag(als).
dbpedia_language_tag(am).
dbpedia_language_tag(an).
dbpedia_language_tag(ang).
dbpedia_language_tag(ar).
dbpedia_language_tag(arc).
dbpedia_language_tag(arz).
dbpedia_language_tag(as).
dbpedia_language_tag(ast).
dbpedia_language_tag(av).
dbpedia_language_tag(ay).
dbpedia_language_tag(az).
dbpedia_language_tag(ba).
dbpedia_language_tag(bar).
dbpedia_language_tag('bat-smg').
dbpedia_language_tag(bat_smg).
dbpedia_language_tag(bcl).
dbpedia_language_tag(bcl_smg).
dbpedia_language_tag(be).
dbpedia_language_tag('be-x-old').
dbpedia_language_tag(be_x_old).
dbpedia_language_tag(bg).
dbpedia_language_tag(bi).
dbpedia_language_tag(bjn).
dbpedia_language_tag(bm).
dbpedia_language_tag(bn).
dbpedia_language_tag(bo).
dbpedia_language_tag(bpy).
dbpedia_language_tag(br).
dbpedia_language_tag(bs).
dbpedia_language_tag(bxr).
dbpedia_language_tag(ca).
dbpedia_language_tag(cdo).
dbpedia_language_tag(ce).
dbpedia_language_tag(ceb).
dbpedia_language_tag(chr).
dbpedia_language_tag(chy).
dbpedia_language_tag(ckb).
dbpedia_language_tag(co).
dbpedia_language_tag(commons).
dbpedia_language_tag(cr).
dbpedia_language_tag(crh).
dbpedia_language_tag(cs).
dbpedia_language_tag(csb).
dbpedia_language_tag(cy).
dbpedia_language_tag(da).
dbpedia_language_tag(de).
dbpedia_language_tag(diq).
dbpedia_language_tag(dv).
dbpedia_language_tag(ee).
dbpedia_language_tag(el).
dbpedia_language_tag(eo).
dbpedia_language_tag(es).
dbpedia_language_tag(et).
dbpedia_language_tag(eu).
dbpedia_language_tag(fa).
dbpedia_language_tag(fi).
dbpedia_language_tag(fiu).
dbpedia_language_tag(fiu_vro).
dbpedia_language_tag(fj).
dbpedia_language_tag(fr).
dbpedia_language_tag(frp).
dbpedia_language_tag(frr).
dbpedia_language_tag(fy).
dbpedia_language_tag(ga).
dbpedia_language_tag(gan).
dbpedia_language_tag(gd).
dbpedia_language_tag(gl).
dbpedia_language_tag(gn).
dbpedia_language_tag(got).
dbpedia_language_tag(gu).
dbpedia_language_tag(gv).
dbpedia_language_tag(ha).
dbpedia_language_tag(hak).
dbpedia_language_tag(he).
dbpedia_language_tag(hi).
dbpedia_language_tag(hif).
dbpedia_language_tag(hr).
dbpedia_language_tag(hsb).
dbpedia_language_tag(ht).
dbpedia_language_tag(hu).
dbpedia_language_tag(hy).
dbpedia_language_tag(ia).
dbpedia_language_tag(id).
dbpedia_language_tag(ig).
dbpedia_language_tag(ilo).
dbpedia_language_tag(io).
dbpedia_language_tag(is).
dbpedia_language_tag(it).
dbpedia_language_tag(ja).
dbpedia_language_tag(jbo).
dbpedia_language_tag(jv).
dbpedia_language_tag(ka).
dbpedia_language_tag(kaa).
dbpedia_language_tag(kab).
dbpedia_language_tag(kbd).
dbpedia_language_tag(ki).
dbpedia_language_tag(kk).
dbpedia_language_tag(kl).
dbpedia_language_tag(km).
dbpedia_language_tag(kn).
dbpedia_language_tag(ko).
dbpedia_language_tag(koi).
dbpedia_language_tag(ks).
dbpedia_language_tag(ku).
dbpedia_language_tag(kv).
dbpedia_language_tag(la).
dbpedia_language_tag(lb).
dbpedia_language_tag(lbe).
dbpedia_language_tag(lez).
dbpedia_language_tag(li).
dbpedia_language_tag(lmo).
dbpedia_language_tag(ln).
dbpedia_language_tag(lt).
dbpedia_language_tag(lv).
dbpedia_language_tag(mg).
dbpedia_language_tag(mhr).
dbpedia_language_tag(mk).
dbpedia_language_tag(ml).
dbpedia_language_tag(mn).
dbpedia_language_tag(mr).
dbpedia_language_tag(mrj).
dbpedia_language_tag(ms).
dbpedia_language_tag(my).
dbpedia_language_tag(na).
dbpedia_language_tag(nah).
dbpedia_language_tag(nds).
dbpedia_language_tag(nds_nl).
dbpedia_language_tag(ne).
dbpedia_language_tag(new).
dbpedia_language_tag(nl).
dbpedia_language_tag(nn).
dbpedia_language_tag(no).
dbpedia_language_tag(nrm).
dbpedia_language_tag(nv).
dbpedia_language_tag(oc).
dbpedia_language_tag(or).
dbpedia_language_tag(pam).
dbpedia_language_tag(pcd).
dbpedia_language_tag(pms).
dbpedia_language_tag(pnb).
dbpedia_language_tag(pl).
dbpedia_language_tag(pt).
dbpedia_language_tag(qu).
dbpedia_language_tag(ro).
dbpedia_language_tag(ru).
dbpedia_language_tag(rw).
dbpedia_language_tag(sa).
dbpedia_language_tag(scn).
dbpedia_language_tag(sco).
dbpedia_language_tag(se).
dbpedia_language_tag(sh).
dbpedia_language_tag(si).
dbpedia_language_tag(simple).
dbpedia_language_tag(sk).
dbpedia_language_tag(sl).
dbpedia_language_tag(sm).
dbpedia_language_tag(sn).
dbpedia_language_tag(so).
dbpedia_language_tag(sq).
dbpedia_language_tag(sr).
dbpedia_language_tag(srn).
dbpedia_language_tag(su).
dbpedia_language_tag(sv).
dbpedia_language_tag(sw).
dbpedia_language_tag(szl).
dbpedia_language_tag(ta).
dbpedia_language_tag(te).
dbpedia_language_tag(tg).
dbpedia_language_tag(th).
dbpedia_language_tag(tl).
dbpedia_language_tag(to).
dbpedia_language_tag(tpi).
dbpedia_language_tag(tr).
dbpedia_language_tag(tt).
dbpedia_language_tag(tum).
dbpedia_language_tag(udm).
dbpedia_language_tag(ug).
dbpedia_language_tag(uk).
dbpedia_language_tag(ur).
dbpedia_language_tag(uz).
dbpedia_language_tag(vec).
dbpedia_language_tag(vi).
dbpedia_language_tag(vls).
dbpedia_language_tag(wa).
dbpedia_language_tag(war).
dbpedia_language_tag(wo).
dbpedia_language_tag(wuu).
dbpedia_language_tag(xal).
dbpedia_language_tag(yi).
dbpedia_language_tag(yo).
dbpedia_language_tag(yoh).
dbpedia_language_tag(zh).
dbpedia_language_tag('zh-min-nan').
dbpedia_language_tag(zh_min_nan).
dbpedia_language_tag('zh-yue').
dbpedia_language_tag(zh_yue).

% DBpedia ontology
:- rdf_register_prefix(dbo, 'http://dbpedia.org/ontology/').

% DBpedia property
:- rdf_register_prefix(dbp, 'http://dbpedia.org/property/').

% DBpedia resource
:- rdf_register_prefix(dbpedia, 'http://dbpedia.org/resource/').

% DBpedia Yago
:- rdf_register_prefix(dbyago, 'http://dbpedia.org/class/yago/').

% DCAT
:- rdf_register_prefix(dcat, 'http://www.w3.org/ns/dcat#').

% Dublin Core: elements
:- rdf_register_prefix(dc, 'http://purl.org/dc/elements/1.1/').
:- rdf_register_reduced_location(dc).

% Dublin Core: terms
:- rdf_register_prefix(dct, 'http://purl.org/dc/terms/').
:- rdf_register_reduced_location(dct).
:- rdf_register_prefix(dcterms, 'http://purl.org/dc/terms/').
:- rdf_register_reduced_location(dcterms).

% Dublin Core: types
:- rdf_register_prefix(dctype, 'http://purl.org/dc/dcmitype/').
:- rdf_register_reduced_location(dctype).

% Dublin core: ?
:- rdf_register_prefix(eor, 'http://dublincore.org/2000/03/13/eor#').

% Freebase
:- rdf_register_prefix(fb, 'http://rdf.freebase.com/ns/').

% Friend Of A Friend (FOAF)
:- rdf_register_prefix(foaf, 'http://xmlns.com/foaf/0.1/').
:- rdf_register_reduced_location(foaf).

% Functional Requirements for Bibliographic Records (FRBR)
:- rdf_register_prefix(frbr, 'http://purl.org/vocab/frbr/core#').

% Geo
:- rdf_register_prefix(geo, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

% OpenCyc
:- rdf_register_prefix(opencyc, 'http://sw.opencyc.org/2008/06/10/concept/').

% Web Ontology Language (OWL)
:- rdf_register_prefix(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_reduced_location(owl).
:- rdf_set_predicate(owl:sameAs, symmetric(true)).
:- rdf_set_predicate(owl:sameAs, transitive(true)).

% ?
:- rdf_register_prefix('powder-s', 'http://www.w3.org/2007/05/powder-s#').

% PROV
:- rdf_register_prefix(prov, 'http://www.w3.org/ns/prov#').

% RDF
:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_reduced_location(rdf).

% RDFS
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_reduced_location(rdfs).
:- rdf_set_predicate(rdfs:subClassOf, transitive(true)).
:- rdf_set_predicate(rdfs:subPropertyOf, transitive(true)).

% Schema
:- rdf_register_prefix(schema, 'http://schema.org/').

% SERQL
:- rdf_register_prefix(serql, 'http://www.openrdf.org/schema/serql#').

% SKOS
:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_reduced_location(skos).

% UMBEL
:- rdf_register_prefix(umbel, 'http://umbel.org/umbel#').
:- rdf_register_prefix('umbel-sc', 'http://umbel.org/umbel/sc/').
:- rdf_register_prefix(umbelrc, 'http://umbel.org/umbel/rc/').

% VCARD
:- rdf_register_prefix(vcard, 'http://www.w3.org/2006/vcard/ns#').

% VS
:- rdf_register_prefix(vs, 'http://www.w3.org/2003/06/sw-vocab-status/ns#').

% WordNet
:- rdf_register_prefix(wn, 'http://wordnet.princeton.edu/wn20/').
:- rdf_register_prefix('wn20:schema', 'http://www.w3.org/2006/03/wn/wn20/schema').

% XHTML Vocabulary.
:- rdf_register_prefix(xhv, 'http://www.w3.org/1999/xhtml/vocab#').

% XSD
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').

% YAGO resource
:- rdf_register_prefix(yago, 'http://yago-knowledge.org/resource/').

