:- module(q_prefix, []).

/** <module> Quine prefix handling

The following terminology is (mis-)used in the field:

  * **IRI prefix**

    The prefix of an IRI that is abbreviated by a *Prefix*.  (I would
    have prefered the name "prefix".)

  * **Local Name**

    For a prefixed IRI, the suffix that is not covered by the
    IRI-prefix.

  * **Alias**

    The custom string that stands for an *IRI prefix*.  (Erroneously
    called ‘prefix’ in the RDF 1.1 specification.)

This allows the following two IRI notations to be distinguished:

  * **Expanded**

    IRI notation where no prefix is used.

  * **Prefixed**

    IRI notation where a prefix alias is used.  Prefixed IRIs have the
    form `Alias:LocalName'.

---

@author Wouter Beek
@version 2015/07-2015/09, 2015/11-2016/01, 2016/03-2016/07, 2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- initialization(init_q_prefix).
init_q_prefix :-
  qb_alias(bf, 'http://bibframe.org/vocab/'),
  qb_alias(blog, 'http://quine.cc/blog/def#'),
  %qb_alias(data, 'http://lodlaundromat.org/data/'),
  qb_alias(dbc, 'http://dbpedia.org/resource/Category:'),
  qb_alias(dbr, 'http://dbpedia.org/resource/'),
  qb_alias(dc, 'http://purl.org/dc/elements/1.1/'),
  qb_alias(dct, 'http://purl.org/dc/terms/'),
  qb_alias(bf, 'http://bibframe.org/vocab/'),
  qb_alias(dct, 'http://purl.org/dc/terms/'),
  qb_alias(foaf, 'http://xmlns.com/foaf/0.1/'),
  qb_alias(http, 'http://www.w3.org/2011/http#'),
  qb_alias(ll, 'http://lodlaundromat.org/resource/'),
  qb_alias(llm, 'http://lodlaundromat.org/metrics/ontology/'),
  qb_alias(llo, 'http://lodlaundromat.org/ontology/'),
  qb_alias(llr, 'http://lodlaundromat.org/resource/'),
  %qb_alias(meta, 'http://lodlaundromat.org/meta/'),
  qb_alias(org, 'http://www.w3.org/ns/org#'),
  qb_alias(owl, 'http://www.w3.org/2002/07/owl#'),
  qb_alias(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'),
  qb_alias(rdfs, 'http://www.w3.org/2000/01/rdf-schema#'),
  qb_alias(sioc, 'http://rdfs.org/sioc/ns#'),
  qb_alias(skos, 'http://www.w3.org/2004/02/skos/core#'),
  qb_alias(vzm, 'https://vrijheidzondermaar.nl/'),
  qb_alias(vzma, 'https://vrijheidzondermaar.nl/article/'),
  qb_alias(vzmc, 'https://vrijheidzondermaar.nl/comment/'),
  qb_alias(vzmt, 'https://vrijheidzondermaar.nl/tag/'),
  qb_alias(vzmu, 'https://vrijheidzondermaar.nl/user/'),
  qb_alias(vzmv, 'https://vrijheidzondermaar.nl/vote/'),
  qb_alias(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#'),
  register_dbpedia_localizations.



register_dbpedia_localizations :-
  forall(
    dbpedia_language_tag(LTag),
    dbpedia_register(LTag)
  ).


%! dbpedia_language_tag(-LanguageTag) is multi.

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
dbpedia_language_tag(ext).
dbpedia_language_tag(eu).
dbpedia_language_tag(fa).
dbpedia_language_tag(fi).
dbpedia_language_tag(fiu).
dbpedia_language_tag(fiu_vro).
dbpedia_language_tag(fj).
dbpedia_language_tag(fo).
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
dbpedia_language_tag(map_bms).
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
dbpedia_language_tag(vo).
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



%! dbpedia_register(+LanguageTag) is det.

dbpedia_register(LTag) :-
  atomic_list_concat([LTag,dbpedia,org], ., Auth),

  % XML namespace for resources.
  atomic_list_concat([LTag,dbr], ., ResourceAlias),
  iri_comps(ResourcePrefix, uri_components(http,Auth,'/resource/',_,_)),
  q_reset_prefix(ResourceAlias, ResourcePrefix),

  % XML namespace for properties.
  atomic_list_concat([LTag,dbp], ., PropAlias),
  iri_comps(PropPrefix, uri_components(http,Auth,'/property/',_,_)),
  q_reset_prefix(PropAlias, PropPrefix).



%! q_reset_prefix(+Alias, +Prefix) is det.
%
% Sets or resets RDF prefixes (whatever is needed to effectuate the
% mapping from Alias onto Prefix), but shows a warning in the case of
% resetting.

q_reset_prefix(Alias, Prefix) :-
  with_mutex(q_reset_prefix, (
    (   q_alias_prefix(Alias, OldPrefix)
    ->  (   OldPrefix == Prefix
        ->  true
        ;   qb_alias(Alias, Prefix),
            print_message(warning, q_reset_prefix(Alias,OldPrefix,Prefix))
        )
    ;   qb_alias(Alias, Prefix)
    )
  )).


:- multifile
    prolog:message//1.

prolog:message(q_reset_prefix(Alias,FromPrefix,ToPrefix)) -->
  % Circumvent prefix abbreviation in ClioPatria.
  ["Alias ~a was reset from prefix ~a to prefix ~a."-[Alias,FromPrefix,ToPrefix]].
