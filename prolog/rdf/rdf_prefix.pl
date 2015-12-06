:- module(
  rdf_prefix,
  [
    rdf_maplist/2, % :Goal_1
                   % +Arguments
    rdf_member/2, % ?Term:rdf_term
                  % +Terms:list(rdf_term)
    rdf_memberchk/2, % ?Term:rdf_term
                     % +Terms:list(rdf_term)
    rdf_prefix_iri/2, % +Iri:atom
                      % -PrefixIri:atom
    rdf_prefixes/1, % -Prefixes:ordset(atom)
    rdf_reset_prefix/2 % +Prefix:atom
                       % +Iri:atom
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_current_prefix/2, % ?Alias:atom
                           % ?Iri:atom
     rdf_equal/2 as rdf_expand_ct, % ?Iri1:compound
                                   % ?Iri2:compound
     rdf_global_id/2 as rdf_expand_rt, % ?Prefixed:compound
                                       % ?Iri:atom
     rdf_global_object/2 as rdf_expand_term, % ?PrefixedTerm:compound
                                             % ?ExpandedTerm:compound
     rdf_meta/1, % +Heads:compound
     rdf_register_prefix/2, % +Alias, Iri
     rdf_register_prefix/3 % +Alias:atom
                           % +Iri:atom
                           % +Options:list(compound)
   ]).

/** <module> RDF prefix

@author Wouter Beek
@version 2015/07-2015/09, 2015/11-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(library(uri)).

:- initialization((assert_cc_prefixes,assert_dbpedia_localizations)).

:- meta_predicate(rdf_maplist(1,+)).

:- rdf_meta(rdf_expand_ct(r,r)).
:- rdf_meta(rdf_maplist(:,t)).
:- rdf_meta(rdf_member(r,t)).
:- rdf_meta(rdf_memberchk(r,t)).





assert_cc_prefixes:-
  absolute_file_name(library(rdf/prefixes), File, [access(read),extensions([csv])]),
  csv_read_file(File, Rows0),
  % Since the more popular prefixes are stored towards the top of the file,
  % we assert them in reverse order. This way the Semweb library will
  % (1) be able to interpret all CC-registered prefixes,
  % while at the same time
  % (2) using only the most popular prefix in writing.
  reverse(Rows0, Rows),
  maplist(assert_cc_prefix, Rows).

assert_cc_prefix(row(Prefix,Iri)):-
  rdf_reset_prefix(Prefix, Iri).



assert_dbpedia_localizations:-
  forall(
    dbpedia_language_tag(LTag),
    dbpedia_register(LTag)
  ).

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

%! dbpedia_register(+LanguageTag:atom) is det.

dbpedia_register(LTag):-
  atomic_list_concat([LTag,dbpedia,org], ., Authority),

  % XML namespace for resources.
  atomic_list_concat([LTag,dbr], ., ResourceNamespace),
  uri_components(
    ResourcePrefix,
    uri_components(http,Authority,'/resource/',_,_)
  ),
  rdf_reset_prefix(ResourceNamespace, ResourcePrefix),

  % XML namespace for properties.
  atomic_list_concat([LTag,dbp], ., PropertyNamespace),
  uri_components(
    PropertyPrefix,
    uri_components(http,Authority,'/property/',_,_)
  ),
  rdf_reset_prefix(PropertyNamespace, PropertyPrefix).



%! rdf_maplist(:Goal_1, +Arguments:list) is det.

rdf_maplist(Goal_1, L):-
  maplist(Goal_1, L).



%! rdf_member(+Term:rdf_term, +PrefixedTerms:list(rdf_term)) is semidet.
%! rdf_member(-Term:rdf_term, +PrefixedTerms:list(rdf_term)) is det.

rdf_member(X, L):-
  member(X, L).



%! rdf_memberchk(+Term:rdf_term, +PrefixedTerms:list(rdf_term)) is semidet.
%! rdf_memberchk(-Term:rdf_term, +PrefixedTerms:list(rdf_term)) is det.

rdf_memberchk(X, L):-
  memberchk(X, L).



%! rdf_prefix_iri(+Iri:atom, -PrefixIri:atom) is det.
% Returns the prefix of the given IRI that is abbreviated with a registered
% RDF prefix, if any.  If no registered RDF prefix occurs in Iri the full IRI
% is returned.

rdf_prefix_iri(Iri, PrefixIri):-
  rdf_expand_rt(Prefix:_, Iri), !,
  rdf_current_prefix(Prefix, PrefixIri).
rdf_prefix_iri(Iri, Iri).



%! rdf_prefixes(-Prefixes:ordset(atom)) is det.

rdf_prefixes(Prefixes):-
  aggregate_all(set(Prefix), rdf_current_prefix(Prefix, _), Prefixes).



%! rdf_reset_prefix(+Prefix:atom, +Iri:atom) is det.
% Sets or resets RDF prefixes (whatever is needed to effectuate the mapping
% from Prefix onto IRI), but shows a warning in the case of resetting.

rdf_reset_prefix(Prefix, Iri):-
  with_mutex(rdf_reset_prefix, (
    (   rdf_current_prefix(Prefix, Iri0)
    ->  (   Iri0 == Iri
        ->  true
        ;   rdf_register_prefix(Prefix, Iri, [force(true)]),
            print_message(warning, rdf_reset_prefix(Prefix,Iri0,Iri))
        )
    ;   rdf_register_prefix(Prefix, Iri)
    )
  )).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(rdf_reset_prefix(Prefix,From0,To0)) -->
  % Circumvent prefix abbreviation in ClioPatria.
  {maplist(atom_string, [From0,To0], [From,To])},
  ['RDF prefix ~a was reset from ~s to ~s.'-[Prefix,From,To]].
