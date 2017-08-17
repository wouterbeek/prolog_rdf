:- module(
  rdf_ext,
  [
    call_on_rdf/2,                   % +UriSpec, :Goal_2
    call_on_rdf/3,                   % +UriSpec, :Goal_2, +Options
    hdt_call_on_file/2,              % +FileSpec, :Goal_1
    hdt_call_on_graph/2,             % +G, :Goal_1
    hdt_graph/2,                     % ?Hdt, ?G
    hdt_graph_file/2,                % ?G, ?File
    hdt_init/0,
    hdt_init/1,                      % +FileSpec
    hdt_init/2,                      % +FileSpec, +G
    hdt_warm_index/1,                % +FileSpec
    graph_file/2,                    % ?G, ?File
    prefix_local_iri/3,              % ?Prefix, ?Local, ?Iri
    rdf/5,                           % +M, ?S, ?P, ?O, ?G
    rdf_agent_image/4,               % +M, +Agent, -Image, ?G
    rdf_agent_name/4,                % +M, +Agent, -Name, ?G
    rdf_aggregate_all/3,             % +Templ, :Goal_0, -Result
    rdf_assert/1,                    % +Tuple
    rdf_assert/2,                    % +Triple, +G
    rdf_assert/5,                    % +M, +S, +P, +O, +G
    rdf_assert_action/5,             % +M, +ActionClass, +Actor, -Action, +G
    rdf_assert_agent/7,              % +M, +User, +Group, +Image, +GivenName,
                                     % +FamilyName, +G
    rdf_assert_objects/5,            % +M, +S, +P, +Os, +G
    rdf_atom_to_term/2,              % +Atom, -Term
    rdf_bnode_iri/2,                 % +M, ?BNode
    rdf_bnode_iri/3,                 % +M, ?BNode, ?G
    rdf_call_update/2,               % :Find_0, Transform_0
    rdf_cbd_quad/3,                  % +M, +Node,     -Quad
    rdf_cbd_quads/3,                 % +M, +Node,     -Quads
    rdf_cbd_triple/3,                % +M, +Node,     -Triple
    rdf_cbd_triple/4,                % +M, +Node, ?G, -Triple
    rdf_cbd_triples/3,               % +M, ?Node,     -Triples
    rdf_cbd_triples/4,               % +M, ?Node, ?G, -Triples
    rdf_chk/4,                       % ?S, ?P, ?O, ?G
    rdf_chk/5,                       % +M, ?S, ?P, ?O, ?G
    rdf_chk_lexical_form/4,          % ?S, ?P, -Lex, ?G
    rdf_chk_lexical_form/5,          % +M, ?S, ?P, -Lex, ?G
    rdf_clean_tuple/2,               % +Tuple, -Quad
    rdf_create_bnode_iri/1,          % -BNode
    rdf_create_iri/3,                % +Prefix, +Path, -Iri
    rdf_creator/4,                   % +M, ?Resource, ?Agent, +G
    rdf_current_prefix/1,            % +Prefix
    rdf_deref_quad/2,                % +Uri, -Quad
    rdf_deref_quad/3,                % +Uri, -Quad, +Options
    rdf_deref_quads/2,               % +Uri, -Quads
    rdf_deref_quads/3,               % +Uri, -Quads, +Options
    rdf_deref_triple/2,              % +Uri, -Triple
    rdf_deref_triple/3,              % +Uri, -Triple, +Options
    rdf_endpoint_init/1,             % +Dict
    rdf_estimate/5,                  % +M, ?S, ?P, ?O, -NumTriples
    rdf_estimate/6,                  % +M, ?S, ?P, ?O, ?G, -NumTriples
    rdf_familyName/4,                % +M, +Agent, -FamilyName, ?G
    rdf_givenName/4,                 % +M, +Agent, -GivenName, ?G
    rdf_iri/2,                       % +M, ?Iri
    rdf_iri/3,                       % +M, ?Iri, ?G
    rdf_is_language_tagged_string/1, % @Term
    rdf_is_legacy_literal/1,         % @Term
    rdf_is_bnode_iri/1,              % @Term
    rdf_is_graph/1,                  % @Term
    rdf_list_member/4,               % +M, ?L, ?O, ?G
    rdf_list_member/5,               % +M, ?S, ?P, ?O, ?G
    rdf_list_memberchk/4,            % +M, ?L,  ?O,  ?G
    rdf_list_memberchk/5,            % +M, ?S,  ?P,  ?O, ?G
    rdf_literal/2,                   % +M, ?Literal
    rdf_literal/3,                   % +M, ?Literal, ?G
    rdf_literal/4,                   % ?Literal, ?D, ?Lexical, ?Lang
    rdf_literal_datatype/2,          % +Lit, ?D
    rdf_literal_language_tag/2,      % +Lit, -LTag
    rdf_literal_lexical_form/2,      % +Lit, ?Lex
    rdf_literal_value/2,             % +Lit, ?Val
    rdf_load2/1,                     % +UriSpec
    rdf_load2/2,                     % +UriSpec, +Options
    rdf_name/2,                      % +M, ?Name
    rdf_name/3,                      % +M, ?Name, ?G
    rdf_node/2,                      % +M, ?Node
    rdf_node/3,                      % +M, ?Node, ?G
    rdf_object/2,                    % +M, ?O
    rdf_object/3,                    % +M, ?O, ?G
    rdf_predicate/2,                 % +M, ?P
    rdf_predicate/3,                 % +M, ?P, ?G
    rdf_pref_string/4,               % ?S, ?P, -Lit, ?G
    rdf_pref_string/5,               % +M, ?S, ?P, -Lit, ?G
    rdf_pref_string/6,               % +M, ?S, ?P, +LRange, -Lit, ?G
    rdf_pref_string_lexical_form/4,  % ?S, ?P, -Lex, ?G
    rdf_pref_string_lexical_form/5,  % +M, ?S, ?P, -Lex, ?G
    rdf_pref_string_lexical_form/6,  % +M, ?S, ?P, +LRange, -Lex, ?G
    rdf_prefix_member/2,             % ?Elem, +L
    rdf_prefix_memberchk/2,          % ?Elem, +L
    rdf_query_term/2,                % +Term, -QueryTerm
    rdf_reserialize/2,               % +Uri, +FileSpec
    rdf_reserialize/3,               % +Kind, +Uri, +FileSpec
    rdf_reserialize/4,               % +Kind, +Uri, +FileSpec, +Options
    rdf_retractall/0,
    rdf_retractall/1,                % +Tuple
    rdf_reification/4,               % +M, ?S, ?P, ?O
    rdf_reification/5,               % +M, ?S, ?P, ?O, ?G
    rdf_reification/6,               % +M, ?S, ?P, ?O, ?G, -Stmt
    rdf_root/2,                      % +M, ?Root
    rdf_root/3,                      % +M, ?Root, ?G
    rdf_save/1,                      % +FileSpec
    rdf_save/2,                      % +Type, +FileSpec
    rdf_save/3,                      % +Type, +FileSpec, +Options
    rdf_save/6,                      % +Type, +FileSpec, ?S, ?P, ?O, ?G
    rdf_save/7,                      % +Type, +FileSpec, ?S, ?P, ?O, ?G, +Options
    rdf_scbd_quad/3,                 % +M, +Node,     -Quad
    rdf_scbd_quads/3,                % +M, +Node,     -Quads
    rdf_scbd_triple/3,               % +M, +Node,     -Triple
    rdf_scbd_triple/4,               % +M, +Node, ?G, -Triple
    rdf_scbd_triples/3,              % +M, ?Node,     -Triples
    rdf_scbd_triples/4,              % +M, ?Node, ?G, -Triples
    rdf_snap/1,                      % :Goal_0
    rdf_statistic/4,                 % +M, +Key, -Value, ?G
    rdf_subdatatype_of/2,            % ?Sub, ?Super
    rdf_subject/2,                   % +M, ?S
    rdf_subject/3,                   % +M, ?S, ?G
    rdf_term/2,                      % +M, ?Term
    rdf_term/3,                      % +M, ?Term, ?G
    rdf_term_to_atom/2,              % +Term, -Atom
    rdf_to_hdt/2,                    % +UriSpec, +HdtFile
    rdf_tuple_quad/3,                % +Tuple, +G, -Quad
    rdf_tuple_triple/2,              % +Tuple, ?Triple
    rdf_write_term/2,                % +Out, +Term
    write_literal/1,                 % +Literal
    write_literal/2,                 % +Out, +Literal
    write_nquad/1,                   % +Quad
    write_nquad/2,                   % +Out, +Quad
    write_nquad/3,                   % +Out, +Triple, +G
    write_nquad/5,                   % +Out, +S, +P, +O, +G
    write_ntriple/1,                 % +Triple
    write_ntriple/2,                 % +Out, +Triple
    write_ntriple/4,                 % +Out, +S, +P, +O
    write_ntuple/1,                  % +Tuple
    write_ntuple/2,                  % +Out, +Tuple
    write_ntuples/1,                 % +Tuples
    write_ntuples/2                  % +Out, +Tuples
  ]
).
:- reexport(library(hdt)).
:- reexport(library(semweb/rdf11), except([rdf_save/1,rdf_save/2])).

/** <module> RDF extensions

@author Wouter Beek
@version 2017/04-2017/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(default)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http/rfc7231)).
:- use_module(library(lists)).
:- use_module(library(nb_set)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_date_time)).
:- use_module(library(semweb/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin), []).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).
:- use_module(library(solution_sequences)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(uuid)).
:- use_module(library(xsd/xsd)).
:- use_module(library(yall)).

:- debug(clean_tuple).
:- debug(hdt_graph).

:- dynamic
    hdt_graph/2.

%mime_type_encoding('application/n-quads', utf8).
%mime_type_encoding('application/n-triples', utf8).
%mime_type_encoding('application/sparql-query', utf8).
%mime_type_encoding('application/trig', utf8).
%mime_type_encoding('text/turtle', utf8).

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

:- meta_predicate
    call_on_rdf(+, 2),
    call_on_rdf(+, 2, +),
    hdt_call_on_file(+, 1),
    hdt_call_on_graph(?, 1),
    rdf_call_update(0, 0),
    rdf_call_update(0, 0, +),
    rdf_aggregate_all(+, 0, -),
    rdf_snap(0).

:- multifile
    file_ext:media_type_extension/2,
    http:map_exception_to_http_status_hook/4,
    user:message_hook/3.

file_ext:media_type_extension_(media(application/'ld+json',[]), jsonld).
file_ext:media_type_extension_(media(application/'n-quads',[]), nq).
file_ext:media_type_extension_(media(application/'n-quads',[]),nquads).
file_ext:media_type_extension_(media(application/'n-triples',[]), nt).
file_ext:media_type_extension_(media(application/'rdf+xml',[]), rdf).
file_ext:media_type_extension_(media(application/trig,[]), trig).
file_ext:media_type_extension_(media(text/turtle,[]), ttl).

http:map_exception_to_http_status_hook(
  error(existence_error(hdt_graph,G),_),
  not_found(G),
  [connection(close)],
  []
).

user:message_hook(non_canonical_lexical_form('http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML',_,_), _, _).
user:message_hook(non_canonical_lexical_form('http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',_,_), _, _).
user:message_hook(non_canonical_lexical_form('http://www.w3.org/2001/XMLSchema#double',_,_), _, _).
user:message_hook(non_canonical_lexical_form('http://www.w3.org/2001/XMLSchema#float',_,_), _, _).

:- rdf_meta
   rdf_agent_image(+, r, -, r),
   rdf_agent_name(+, r, -, r),
   rdf_creator(+, r, r, r),
   rdf_familyName(+, r, -, r),
   rdf_givenName(+, r, -, r),
   graph_file(r, -),
   ntriples_to_nquads(+, t, +),
   hdt_call_on_graph(r, :),
   hdt_graph(?, r),
   hdt_init(+, r),
   prefix_local_iri(?, ?, r),
   rdf(+, r, r, o, r),
   rdf_aggregate_all(+, t, -),
   rdf_assert(t),
   rdf_assert(t, r),
   rdf_assert(+, r, r, o, r),
   rdf_assert_action(+, r, r, -, r),
   rdf_assert_agent(+, r, r, +, +, +, r),
   rdf_assert_objects(+, r, r, t, r),
   rdf_call_update(t, t),
   rdf_cbd_quad(?, o, -),
   rdf_cbd_quads(?, o, -),
   rdf_cbd_triple(?, o, -),
   rdf_cbd_triple(?, o, r, -),
   rdf_cbd_triples(?, o, -),
   rdf_cbd_triples(?, o, r, -),
   rdf_chk(r, r, o, r),
   rdf_chk(+, r, r, o, r),
   rdf_chk_lexical_form(r, r, -, r),
   rdf_chk_lexical_form(+, r, r, -, r),
   rdf_deref_quad(r, t),
   rdf_deref_quad(r, t, +),
   rdf_deref_quads(r, -),
   rdf_deref_quads(r, -, +),
   rdf_deref_triple(r, t),
   rdf_deref_triple(r, t, +),
   rdf_estimate(+, r, r, o, -),
   rdf_estimate(+, r, r, o, -, r),
   rdf_iri(?, r),
   rdf_iri(?, r, r),
   rdf_is_legacy_literal(o),
   rdf_is_bnode_iri(r),
   rdf_is_graph(r),
   rdf_is_language_tagged_string(o),
   rdf_is_real_iri(r),
   rdf_list_member(+, r, o, r),
   rdf_list_member(+, r, r, o, r),
   rdf_list_memberchk(+, r, o, r),
   rdf_list_memberchk(+, r, r, o, r),
   rdf_literal(?, o),
   rdf_literal(?, o, r),
   rdf_literal(o, r, ?, ?),
   rdf_literal_datatype(o, r),
   rdf_literal_language_tag(o, -),
   rdf_literal_lexical_form(o, -),
   rdf_literal_value(o, ?),
   rdf_load2(r),
   rdf_load2(r, +),
   rdf_name(?, o),
   rdf_name(?, o, r),
   rdf_node(?, o),
   rdf_node(?, o, r),
   rdf_object(?, o),
   rdf_object(?, o, r),
   rdf_predicate(?, r),
   rdf_predicate(?, r, r),
   rdf_pref_string(r, r, -, r),
   rdf_pref_string(+, r, r, -, r),
   rdf_pref_string(+, r, r, +, -, r),
   rdf_pref_string_lexical_form(r, r, -, r),
   rdf_pref_string_lexical_form(+, r, r, -, r),
   rdf_pref_string_lexical_form(+, r, r, +, -, r),
   rdf_prefix_member(t, t),
   rdf_prefix_memberchk(t, t),
   rdf_reification(+, r, r, o),
   rdf_reification(+, r, r, o, r),
   rdf_reification(+, r, r, o, r, r),
   rdf_retractall(t),
   rdf_root(+, r),
   rdf_root(+, r, r),
   rdf_save(+, +, r),
   rdf_save(+, +, r, +),
   rdf_save(+, +, r, r, o, r),
   rdf_save(+, +, r, r, o, r, +),
   rdf_scbd_quad(?, o, -),
   rdf_scbd_quads(?, o, -),
   rdf_scbd_triple(?, o, -),
   rdf_scbd_triple(?, o, r, -),
   rdf_scbd_triples(?, o, -),
   rdf_scbd_triples(?, o, r, -),
   rdf_statistic(+, +, -, r),
   rdf_subdatatype_of(r, r),
   rdf_subject(?, r),
   rdf_subject(?, r, r),
   rdf_term(?, o),
   rdf_term(?, o, r),
   rdf_tuple_quad(t, r, t),
   rdf_tuple_triple(t, t),
   write_literal(o),
   write_literal(+, o),
   write_nquad(t),
   write_nquad(+, t),
   write_nquad(+, t, r),
   write_nquad(+, r, r, o, r),
   write_ntriple(t),
   write_ntriple(+, t),
   write_ntriple(+, r, r, o),
   write_ntuple(t),
   write_ntuple(+, t).

:- rdf_register_prefix(bibframe, 'http://bibframe.org/vocab/').
:- rdf_register_prefix(bnode, 'https://example.org/.well-known/genid/').
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
:- rdf_register_prefix(sioc, 'http://rdfs.org/sioc/ns#').
:- rdf_register_prefix(skosxl, 'http://www.w3.org/2008/05/skos-xl#').
:- rdf_register_prefix(sparql, 'http://www.w3.org/2005/sparql-results#').
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





%! call_on_rdf(+UriSpec:term, :Goal_2) is nondet.
%! call_on_rdf(+UriSpec:term, :Goal_2, +Options:list(compound)) is nondet.
%
% Makes the call `Goal_2(+Tuple, ?G)'.
%
% The following options are supported:
%
%   * accept(+list(compound))
%
%     The value of the HTTP Accept header, from high to low
%     precedence.  The default value is a list of all and only
%     standardized Media Types.
%
%   * base_uri(+atom)
%
%     The default is the URI of the last metadata element.
%
%   * bnode_prefix(+atom)
%
%     The default is a well-known IRI as per RDF 1.1.
%
%   * Other options are passed to call_on_uri/3 and
%     rdf_process_ntriples/3.

call_on_rdf(UriSpec, Goal_2) :-
  call_on_rdf(UriSpec, Goal_2, []).


call_on_rdf(UriSpec, Goal_2, Options1) :-
  findall(MediaType, rdf_media_type(MediaType), DefaultMediaTypes),
  select_option(accept(MediaTypes), Options1, Options2, DefaultMediaTypes),
  atom_phrase(accept(MediaTypes), Accept),
  merge_options(Options2, [request_header('Accept'=Accept)], Options3),
  call_on_uri(UriSpec, call_on_rdf_stream(Goal_2, Options1), Options3).

call_on_rdf_stream(Goal_2, Options1, In, [Dict1|Metadata], [Dict2|Metadata]) :-
  % Guess the Media Type based on peeking inside the stream.
  rdf_guess(In, MediaTypes),
  (memberchk(MediaType1, MediaTypes) -> true ; throw(error(rdf_unknown,_))),

  % Compare the guessed Media Type to the value of the last
  % `Content-Type' header.
  (   metadata_content_type(Metadata, MediaType2)
  ->  media_type_warning(MediaType1, MediaType2)
  ;   true
  ),

  % Compare the guessed Media Type to the URI's path component.
  metadata_uri(Metadata, Uri),
  (   uri_media_type(Uri, MediaType3)
  ->  media_type_warning(MediaType1, MediaType3)
  ;   true
  ),
  
  % Determine the base URI.
  option(base_uri(BaseUri), Options1, Uri),
  
  % Determine the blank node prefix.
  call_default_option(
    bnode_prefix(BNodePrefix),
    Options1,
    rdf_create_bnode_iri
  ),
  
  % Parse according to the guessed Media Type.

  (   % N-Quads
      MediaType1 = media(application/'n-quads',_)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(nquads)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Goal_2, Options2)
  ;   % N-Triples
      MediaType1 = media(application/'n-triples',_)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(ntriples)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Goal_2, Options2)
  ;   % RDF/XML
      MediaType1 = media(application/'rdf+xml',_)
  ->  merge_options(
        [base_uri(BaseUri),blank_nodes(noshare)],
        Options1,
        Options2
      ),
      process_rdf(In, Goal_2, Options2)
  ;   % TriG
      MediaType1 = media(application/trig,_)
  ->  merge_options(
        [
          anon_prefix(BNodePrefix),
          base_uri(BaseUri),
          format(trig),
          resources(iri)
        ],
        Options1,
        Options2
      ),
      rdf_process_turtle(In, Goal_2, Options2)
  ;   % Turtle
      MediaType1 = media(text/turtle,_)
  ->  merge_options(
        [
          anon_prefix(BNodePrefix),
          base_uri(BaseUri),
          format(turtle),
          resources(iri)
        ],
        Options1,
        Options2
      ),
      rdf_process_turtle(In, Goal_2, Options2)
  ;   % RDFa
      memberchk(
        MediaType1,
        [media(application/'xhtml+xml',_),media(text/html,_)]
      )
  ->  merge_options(
        [anon_prefix(BNodePrefix),base(BaseUri)],
        Options1,
        Options2
      ),
      read_rdfa(In, Triples, Options2),
      maplist(Goal_2, Triples, _)
  ;   % An unsupported Media Type (e.g., JSON-LD).
      print_message(warning, unsupported_media_type(MediaType1))
  ),
  dict_put(media_type, Dict1, MediaType1, Dict2).

media_type_warning(MediaType1, MediaType2) :-
  'rdf_media_type_>'(MediaType1, MediaType2), !.
media_type_warning(MediaType1, MediaType2) :-
  print_message(warning, different_media_type(MediaType1,MediaType2)).

'rdf_media_type_>'(X, Y) :-
  'rdf_media_type_='(X, Y), !.
'rdf_media_type_>'(X, Z) :-
  'rdf_media_type_strict>'(X, Y),
  'rdf_media_type_>'(Y, Z).

'rdf_media_type_='(media(Supertype/Subtype,_),  media(Supertype/Subtype,_)).

'rdf_media_type_strict>'(media(application/trig,_), media(text/turtle,_)).
'rdf_media_type_strict>'(
  media(text/turtle,_),
  media(application/'n-triples',_)
).
'rdf_media_type_strict>'(
  media(application/'n-quads',_),
  media(application/'n-triples',_)
).

% Ordering represents precedence, from lower to hgiher.
rdf_media_type(media(application/'json-ld',[])).
rdf_media_type(media(application/'rdf+xml',[])).
rdf_media_type(media(text/turtle,[])).
rdf_media_type(media(application/'n-triples',[])).
rdf_media_type(media(application/trig,[])).
rdf_media_type(media(application/'n-quads',[])).



%! graph_file(+G, -File) is det.

graph_file(G, File) :-
  rdf_global_id(graph:Local, G),
  absolute_file_name(data(Local), File, [access(write),extensions([nt])]).



%! hdt_call_on_file(+FileSpec:term, :Goal_1) is det.

hdt_call_on_file(FileSpec, Goal_1) :-
  absolute_file_name(FileSpec, File, [access(read)]),
  setup_call_cleanup(
    hdt_open(Hdt, File),
    call(Goal_1, Hdt),
    hdt_close(Hdt)
  ).



%! hdt_call_on_graph(+G, :Goal_1) is det.

hdt_call_on_graph(G, Goal_1) :-
  hdt_graph(Hdt, G),
  call(Goal_1, Hdt).



%! hdt_graph_file(+G, -File) is det.
%! hdt_graph_file(-G, +File) is det.

hdt_graph_file(G, File) :-
  ground(G), !,
  rdf_global_id(graph:Base, G),
  file_name_extension(Base, hdt, Local),
  absolute_file_name(data(Local), File, [access(read)]).
hdt_graph_file(G, File) :-
  ground(File), !,
  file_base_name(File, Local),
  file_name_extension(Base, hdt, Local),
  rdf_global_id(graph:Base, G).



%! hdt_init is det.
%! hdt_init(+FileSpec:term) is det.
%! hdt_init(+FileSpec:term, +G:atom) is det.

hdt_init :-
  forall(
    (
      absolute_file_name(
        data(.),
        Dir,
        [access(read),file_errors(fail),file_type(directory),solutions(all)]
      ),
      directory_path_recursive(Dir, File),
      directory_file_path(Dir, Local, File),
      file_name_extension(Base, hdt, Local)
    ),
    hdt_init(File, Base)
  ).


hdt_init(FileSpec) :-
  hdt_init(FileSpec, graph:default).


hdt_init(FileSpec, G) :-
  absolute_file_name(FileSpec, File, [access(read)]),
  hdt_open(Hdt, File),
  assert(hdt_graph(Hdt, G)),
  debug(hdt_graph, "Open HDT: ~a", [File]).



%! hdt_warm_index(+FileSpec:term) is det.

hdt_warm_index(FileSpec) :-
  absolute_file_name(FileSpec, File, [access(read)]),
  hdt_call_on_file(File, hdt_warm_index1).

hdt_warm_index1(Hdt) :-
  once(hdt:hdt_search(Hdt, _, _, _)).



%! prefix_local_iri(-Prefix:atom, -Local:atom,   +Iri:atom) is det.
%! prefix_local_iri(+Prefix:atom, +Local:atom, -Iri:atom) is det.

prefix_local_iri(Prefix, Local, Iri) :-
  rdf_global_id(Prefix:Local, Iri).



%! rdf(+M, ?S, ?P, ?O, ?G) is nondet.

rdf(hdt, S, P, O, G) :-
  hdt_graph(Hdt, G),
  rdf(hdt0, S, P, O, Hdt).
rdf(hdt0, S, P, O, Hdt) :-
  (var(S) -> true ; \+ rdf_is_literal(S)),
  hdt_search(Hdt, S, P, O).
rdf(trp, S, P, O, G) :-
  rdf(S, P, O, G).



%! rdf_agent_gravatar(+M, +Agent, -Uri, +G) is det.

rdf_agent_gravatar(M, Agent, Uri, G) :-
  rdf_chk(M, Agent, foaf:mbox, EMail^^xsd:anyURI, G),
  downcase_atom(EMail, CanonicalEMail),
  md5(CanonicalEMail, Hash),
  uri_comps(Uri, uri(http,'www.gravatar.com',[avatar,Hash],_,_)).



%! rdf_agent_image(+M, +Agent, -Image, ?G) is det.

rdf_agent_image(M, Agent, Image, G) :-
  rdf_chk(M, Agent, foaf:depiction, Image^^xsd:anyURI, G), !.
rdf_agent_image(M, Agent, Image, G) :-
  rdf_agent_gravatar(M, Agent, Image, G).



%! rdf_agent_name(+M, +Agent, -Name, ?G) is det.

rdf_agent_name(M, Agent, Name, G) :-
  rdf_givenName(M, Agent, GivenName0, G),
  rdf_familyName(M, Agent, FamilyName0, G), !,
  maplist(rdf_literal_lexical_form, [GivenName0,FamilyName0], Names),
  atomics_to_string(Names, " ", Name).
rdf_agent_name(M, Agent, Name, G) :-
  rdf_pref_string(M, Agent, foaf:name, Name0, G),
  rdf_literal_lexical_form(Name0, Name).



%! rdf_aggregate_all(+Templ, :Goal_0, -Result) is det.
%
% @see aggregate_all/3 with RDF prefix expansion applied.

rdf_aggregate_all(Templ, Goal_0, Result) :-
  aggregate_all(Templ, Goal_0, Result).



%! rdf_assert(+Tuple) is det.
%! rdf_assert(+Triple, +G) is det.
%! rdf_assert(+M, +S, +P, +O, +G) is det.

rdf_assert(rdf(S,P,O)) :- !,
  rdf_assert(S, P, O).
rdf_assert(rdf(S,P,O,G)) :-
  rdf_assert(S, P, O, G).


rdf_assert(rdf(S,P,O), G) :-
  rdf_assert(S, P, O, G).


rdf_assert(stream(Out), S, P, O, G) :-
  ground(Out),
  (   is_uri(G)
  ->  write_nquad(Out, S, P, O, G)
  ;   write_ntriple(Out, S, P, O)
  ).
rdf_assert(trp, S, P, O, G) :-
  rdf_assert(S, P, O, G).



%! rdf_assert_action(+M, +ActionClass, +Actor, -Action, +G) is det.

rdf_assert_action(M, ActionClass, Actor, Action, G):-
  fresh_uri(Action, uri(_,_,[action],_,_)),
  rdf_assert(M, Action, rdf:type, ActionClass, G),
  rdf_assert_now(M, Action, prov:atTime, G),
  rdf_assert(M, Action, prov:wasAssociatedWith, Actor, G).



%! rdf_assert_agent(+M, +User, +Group, +Image, +GivenName, +FamilyName,
%!                  +G) is det.

rdf_assert_agent(M, User, Group, Image, GivenName, FamilyName, G) :-
  rdf_assert(M, User, rdf:type, Group, G),
  rdf_assert(M, User, foaf:depiction, Image^^xsd:anyURI, G),
  rdf_assert(M, User, foaf:familyName, FamilyName@nl, G),
  rdf_assert(M, User, foaf:givenName, GivenName@nl, G).



%! rdf_assert_objects(+M, +S, +P, +Os, +G) is det.

rdf_assert_objects(M, S, P, Os, G) :-
  maplist({M,S,P,G}/[O]>>rdf_assert(M, S, P, O, G), Os).



%! rdf_atom_to_term(+Atom:atom, -Term:term) is det.

rdf_atom_to_term(Atom, Literal) :-
  must_be(atom, Atom),
  atom_phrase(rdf_literal_(Literal), Atom), !.
rdf_atom_to_term(Iri, Iri).

rdf_literal_(Literal) -->
  "\"",
  ...(Codes1),
  "\"",
  (   "^^"
  ->  rdf_iri_(D)
  ;   "@"
  ->  rest(Codes2),
      {atom_codes(LTag, Codes2)}
  ),
  {
    string_codes(Lex, Codes1),
    rdf_literal(Literal, D, Lex, LTag)
  }.

rdf_iri_(Iri) -->
  "<",
  ...(Codes),
  ">", !,
  {atom_codes(Iri, Codes)}.



%! rdf_bnode_iri(+M, ?BNode) is nondet.
%! rdf_bnode_iri(+M, ?BNode, ?G) is nondet.

rdf_bnode_iri(M, BNode) :-
  rdf_iri(M, BNode),
  rdf_is_bnode_iri(BNode).


rdf_bnode_iri(M, BNode, G) :-
  rdf_iri(M, BNode, G),
  rdf_is_bnode_iri(BNode).



%! rdf_call_update(:Find_0, Transform_0) is det.
%
% Generic data transformation call:
%
%   - Find_0 matches a single candidate for transformation.
%
%   - Transform_0 acts on a single matched candidate to effectuate the
%     transformation.
%
% If Transform_0 fails the debugger is opened.

rdf_call_update(Find_0, Transform_0) :-
  rdf_transaction(
    rdf_call_update(Find_0, Transform_0, _{count: 0})
  ).

rdf_call_update(Find_0, Transform_0, State) :-
  Find_0, % NONDET
  (   Transform_0
  ->  true
  ;   print_message(warning, failed_transformation(State.count))
  ),
  dict_inc(count, State),
  fail.
rdf_call_update(_, _, State) :-
  debug(rdf_call_update, "~D updates were made.", [State.count]).



%! rdf_cbd_quad(+M, +Node, -Quad) is nondet.
%! rdf_cbd_quad(+M, +Node, -Quad) is nondet.

rdf_cbd_quad(M, Node, rdf(S,P,O,G)) :-
  rdf_cbd_triple(M, Node, G, rdf(S,P,O)).


rdf_cbd_quads(M, Node, Quads) :-
  rdf_subject(M, Node),
  aggregate_all(set(Quad), rdf_cbd_quad(M, Node, Quad), Quads).



%! rdf_cbd_triple(+M, +Node, -Triple) is nondet.
%! rdf_cbd_triple(+M, +Node, ?G, -Triple) is nondet.

rdf_cbd_triple(M, Node, Triple) :-
  rdf_cbd_triple(M, Node, _, Triple).


rdf_cbd_triple(M, Node, G, Triple) :-
  rdf_cbd_triple1(M, Node, [Node], G, Triple).

rdf_cbd_triple1(M, S, Hist1, G, Triple) :-
  rdf_is_subject(S),
  rdf(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode_iri(O),
      node_history1(Hist1, O, Hist2),
      rdf_cbd_triple1(M, O, Hist2, G, Triple)
  ;   rdf_reification(M, S, P, O, G, Stmt),
      node_history1(Hist1, Stmt, Hist2),
      rdf_cbd_triple1(M, Stmt, Hist2, G, Triple)
  ).

rdf_cbd_inv_triple1(M, O, Hist1, G, Triple) :-
  rdf(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode_iri(S),
      node_history1(Hist1, S, Hist2),
      rdf_cbd_inv_triple1(M, S, Hist2, G, Triple)
  ;   rdf_reification(M, S, P, O, G, Stmt),
      node_history1(Hist1, Stmt, Hist2),
      rdf_scbd_triple1(M, Stmt, Hist2, G, Triple)
  ).

node_history1(Hist, O, _) :-
  memberchk(O, Hist), !,
  fail.
node_history1(Hist1, O, Hist2) :-
  ord_add_element(Hist1, O, Hist2).



%! rdf_cbd_triples(+M, ?Node, -Triples) is det.
%! rdf_cbd_triples(+M, ?Node, ?G, -Triples) is det.

rdf_cbd_triples(M, Node, Triples) :-
  rdf_cbd_triples(M, Node, _, Triples).


rdf_cbd_triples(M, Node, G, Triples) :-
  rdf_subject(M, Node, G),
  aggregate_all(set(Triple), rdf_cbd_triple(M, Node, G, Triple), Triples).



%! rdf_chk(?S, ?P, ?O, ?G) is nondet.
%! rdf_chk(+M, ?S, ?P, ?O, ?G) is nondet.

rdf_chk(S, P, O, G) :-
  once(rdf(S, P, O, G)).


rdf_chk(M, S, P, O, G) :-
  once(rdf(M, S, P, O, G)).



%! rdf_chk_lexical_form(?S, ?P, -Lex, ?G) is nondet.
%! rdf_chk_lexical_form(+M, ?S, ?P, -Lex, ?G) is nondet.

rdf_chk_lexical_form(S, P, Lex, G) :-
  rdf_chk_lexical_form(trp, S, P, Lex, G).


rdf_chk_lexical_form(M, S, P, Lex, G) :-
  once((
    rdf(M, S, P, Lit, G),
    rdf_is_literal(Lit)
  )),
  rdf_literal_lexical_form(Lit, Lex).



%! rdf_clean_bnode(+BNode:atom, -Iri:atom) is det.

rdf_clean_bnode(BNode, Iri) :-
  atomic_list_concat(L1, '_:', BNode),
  append(L2, [Local], L1),
  md5(L2, Hash),
  atomic_list_concat([Hash,Local], ':', BNodeLabel),
  rdf_global_id(bnode:BNodeLabel, Iri).



%! rdf_clean_graph(+G1, -G2) is det.

rdf_clean_graph(G1, G3) :-
  rdf11:post_graph(G2, G1),
  (G2 == user -> G3 = default ; rdf_clean_iri(G2, G3)).



%! rdf_clean_iri(+Uri1, -Uri2) is det.

rdf_clean_iri(Uri1, Uri2) :-
  uri_resolve(Uri1, 'https://example.org/', Uri2).



%! rdf_clean_object(+O1:term, -O2:term) is semidet.

rdf_clean_object(Literal1, Literal2) :-
  legacy_literal_components(Literal1, D, Lex1, LTag1), !,
  (   rdf_equal(rdf:'HTML', D)
  ->  rdf11:write_xml_literal(html, Lex1, Lex2)
  ;   rdf_equal(rdf:'XMLLiteral', D)
  ->  rdf11:write_xml_literal(xml, Lex1, Lex2)
  ;   rdf_equal(xsd:decimal, D)
  ->  (   string_phrase(decimalLexicalMap(Val), Lex1)
      ->  atom_phrase(decimalCanonicalMap(Val), Lex2)
      ;   print_message(warning, invalid_decimal(Lex1)),
          fail
      )
  ;   Lex2 = Lex1
  ),
  catch(
    (
      rdf11:post_object(Literal2, Literal1),
      rdf11:pre_object(Literal2, Literal3),
      legacy_literal_components(Literal3, D, Lex3, LTag3)
    ),
    E,
    true
  ),
  (   % Warn and fail for an incorrect lexical form.
      var(E)
  ->  (   % Warn for a non-canonical lexical form.
          Lex2 \== Lex3
      ->  print_message(warning, non_canonical_lexical_form(D,Lex2,Lex3))
      ;   true
      ),
      (   % Warn for a non-canonical language tag.
          ground(LTag1),
          LTag1 \== LTag3
      ->  print_message(warning, non_canonical_language_tag(LTag1))
      ;   true
      )
  ;   print_message(warning, E),
      fail
  ).
rdf_clean_object(BNode, Iri) :-
  rdf_is_bnode(BNode), !,
  rdf_clean_bnode(BNode, Iri).
rdf_clean_object(Iri1, Iri2) :-
  rdf_clean_iri(Iri1, Iri2).



%! rdf_clean_predicate(+P1:atom, -P2:atom) is det.

rdf_clean_predicate(P1, P2) :-
  rdf_clean_iri(P1, P2).



%! rdf_clean_quad(+Quad1:compound, -Quad2:compound) is semidet.

rdf_clean_quad(rdf(S1,P1,O1,G1), rdf(S2,P2,O2,G2)) :-
  rdf_clean_subject(S1, S2),
  rdf_clean_predicate(P1, P2),
  rdf_clean_object(O1, O2),
  rdf_clean_graph(G1, G2).



%! rdf_clean_subject(+S1:atom, -S2:atom) is det.

rdf_clean_subject(BNode, Iri) :-
  rdf_is_bnode(BNode), !,
  rdf_clean_bnode(BNode, Iri).
rdf_clean_subject(Iri1, Iri2) :-
  rdf_clean_iri(Iri1, Iri2).



%! rdf_clean_triple(+Triple:term, -Quad:term) is semidet.

rdf_clean_triple(rdf(S,P,O), Quad) :-
  rdf_default_graph(G),
  rdf_clean_tuple(rdf(S,P,O,G), Quad).



%! rdf_clean_tuple(+Tuple:term, -Quad:term) is semidet.

rdf_clean_tuple(rdf(S,P,O), Quad) :- !,
  rdf_clean_triple(rdf(S,P,O), Quad).
rdf_clean_tuple(rdf(S,P,O,G), Quad) :-
  rdf_clean_quad(rdf(S,P,O,G), Quad).



%! rdf_create_bnode_iri(-BNode) is det.

rdf_create_bnode_iri(BNode) :-
  uuid(Uuid),
  rdf_global_id(bnode:Uuid, BNode).



%! rdf_create_iri(+Prefix:atom, +Segments:list(atom), -Iri:atom) is det.

rdf_create_iri(Prefix, Segments, Iri2) :-
  rdf_current_prefix(Prefix, Iri1),
  uri_comp_add(path, Iri1, Segments, Iri2).



%! rdf_creator(+M, ?Resource, ?Agent, +G) is nondet.
%
% @tbd Generatize to rdf_api as “direct resource or resource in list”.

% An RDF list of multiple creators.
rdf_creator(M, Resource, Agent, G) :-
  rdf_list_member(M, Resource, dc:creator, Agent, G).
% A single creator.
rdf_creator(M, Resource, Agent, G) :-
  rdf(M, Resource, dc:creator, Agent, G),
  \+ rdf_list_member(M, Agent, _, G).



%! rdf_current_prefix(+Prefix:atom) is semidet.

rdf_current_prefix(Prefix) :-
  rdf_current_prefix(Prefix, _).



%! rdf_deref_quad(+Uri, -Quad) is nondet.
%! rdf_deref_quad(+Uri, -Quad, +Options) is nondet.

rdf_deref_quad(Uri, Quad) :-
  rdf_deref_quad(Uri, Quad, []).


rdf_deref_quad(Uri, Quad, Options) :-
  rdf_deref_quads(Uri, Quads, Options),
  member(Quad, Quads).



%! rdf_deref_quads(+UriSpec:term, -Quads:list(compound)) is nondet.
%! rdf_deref_quads(+UriSpec:term, -Quads:list(compound),
%!                 +Options:list(compound)) is nondet.
%
% Options are passed to call_on_rdf/3.

rdf_deref_quads(UriSpec, Quads) :-
  rdf_deref_quads(UriSpec, Quads, []).


rdf_deref_quads(UriSpec, Quads, Options) :-
  empty_nb_set(Set),
  forall(
    call_on_rdf(UriSpec, rdf_deref_quads_(Set), Options),
    true
  ),
  nb_set_to_list(Set, Quads).

rdf_deref_quads_(Set, Tuples, G) :-
  maplist({G}/[Tuple,Quad]>>rdf_tuple_quad(Tuple, G, Quad), Tuples, Quads1),
  convlist(rdf_clean_quad, Quads1, Quads2),
  maplist({Set}/[Quad]>>add_nb_set(Quad, Set), Quads2).



%! rdf_deref_triple(+UriSpec:term, -Triple:compound) is nondet.
%! rdf_deref_triple(+UriSpec:term, -Triple:compound,
%!                  +Options:list(compound)) is nondet.
%
% Options are passed to rdf_deref_quad/3.

rdf_deref_triple(UriSpec, Triple) :-
  rdf_deref_triple(UriSpec, Triple, []).


rdf_deref_triple(UriSpec, Triple, Options) :-
  rdf_deref_quad(UriSpec, Quad, Options),
  rdf_tuple_triple(Quad, Triple).



%! rdf_endpoint_init(+Dict) is det.

rdf_endpoint_init(Dict) :-
  dict_get(graphs, Dict, [], Dicts),
  maplist(init_graph1, Dicts).

init_graph1(Dict) :-
  hdt_init(Dict.file, Dict.name).



%! rdf_estimate(+M, ?S, ?P, ?O, -NumTriples:nonneg) is det.
%! rdf_estimate(+M, ?S, ?P, ?O, -NumTriples:nonneg, ?G) is det.
%
% @tbd TRP support for rdf_estimate/6.

rdf_estimate(_, S, _, _, 0) :-
  ground(S),
  rdf_is_literal(S), !.
rdf_estimate(_, S, P, O, 1) :-
  ground(rdf(S,P,O)), !.
rdf_estimate(hdt, S, P, O, NumTriples) :- !,
  aggregate_all(
    sum(NumTriples),
    (
      hdt_graph(Hdt, _),
      rdf_estimate(hdt0, S, P, O, NumTriples, Hdt)
    ),
    NumTriples
  ).
rdf_estimate(trp, S, P, O, NumTriples) :-
  rdf_estimate_complexity(S, P, O, NumTriples).


rdf_estimate(_, S, _, _, 0, _) :-
  ground(S),
  rdf_is_literal(S), !.
rdf_estimate(_, S, P, O, 1, _) :-
  ground(rdf(S,P,O)), !.
rdf_estimate(hdt, S, P, O, NumTriples, G) :- !,
  hdt_call_on_graph(G, rdf_estimate(hdt0, S, P, O, NumTriples)).
rdf_estimate(hdt0, S, P, O, NumTriples, Hdt) :-
  hdt_search_cost(Hdt, S, P, O, NumTriples).



%! rdf_familyName(+M, +Agent, -FamilyName, ?G) is det.

rdf_familyName(M, Agent, FamilyName, G) :-
  rdf_pref_string(M, Agent, foaf:familyName, FamilyName, G).



%! rdf_givenName(+M, +Agent, -GivenName, ?G) is det.

rdf_givenName(M, Agent, GivenName, G) :-
  rdf_pref_string(M, Agent, foaf:givenName, GivenName, G).



%! rdf_iri(+M, ?Iri) is nondet.
%! rdf_iri(+M, ?Iri, ?G) is nondet.

rdf_iri(hdt, Iri) :-
  rdf_iri(hdt0, Iri).
rdf_iri(hdt0, Iri) :-
  distinct(Iri, rdf_iri(hdt0, Iri, _)).
rdf_iri(trp, Iri) :-
  rdf_iri(Iri).


rdf_iri(hdt, Iri, G) :-
  hdt_call_on_graph(G, rdf_iri(hdt0, Iri)).
rdf_iri(hdt0, Iri, Hdt) :-
  rdf_name(hdt, Iri, Hdt),
  rdf_is_iri(Iri).
rdf_iri(trp, Iri, G) :-
  rdf_iri(Iri),
  distinct(G, rdf_term(trp, Iri, G)).



%!  rdf_is_bnode_iri(@Term) is semidet.

rdf_is_bnode_iri(Term) :-
  rdf_global_id(bnode:_, Term).



%!  rdf_is_graph(@Term) is semidet.

rdf_is_graph(G) :-
  rdf_default_graph(G), !.
rdf_is_graph(G) :-
  rdf_is_iri(G).



%! rdf_is_language_tagged_string(@Term) is semidet.

rdf_is_language_tagged_string(Term) :-
  ground(Term),
  Term = _@_.



%!  rdf_is_legacy_literal(@Term) is semidet.

rdf_is_legacy_literal(literal(type(_,_))) :- !.
rdf_is_legacy_literal(literal(lang(_,_))) :- !.
rdf_is_legacy_literal(literal(_)).



%! rdf_language_tagged_string(+M, ?S, ?P, -Lit) is nondet.
%! rdf_language_tagged_string(+M, ?S, ?P, -Lit, ?G) is nondet.
%! rdf_language_tagged_string(+M, ?S, ?P, +LRange, -Lit, ?G) is nondet.
%
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.  Notice that results for
% each of the prioritized languages are given in arbitrary order.

rdf_language_tagged_string(M, S, P, Lit) :-
  rdf_language_tagged_string(M, S, P, Lit, _).


rdf_language_tagged_string(M, S, P, Lit, G) :-
  current_lrange(LRange),
  rdf_language_tagged_string(M, S, P, LRange, Lit, G).


rdf_language_tagged_string(M, S, P, LRange, Lit, G) :-
  rdf(M, S, P, Lex@LTag, G),
  basic_filtering(LRange, LTag),
  Lit = Lex@LTag.



%! rdf_list_member(+M, ?L, ?O, ?G) is nondet.
%! rdf_list_member(+M, ?S, ?P, ?O, ?G) is nondet.

rdf_list_member(M, L, O, G) :-
  rdf(M, L, rdf:first, O, G).
rdf_list_member(M, L, O, G) :-
  rdf(M, L, rdf:rest, T, G),
  rdf_list_member(M, T, O, G).


rdf_list_member(M, S, P, O, G) :-
  ground(O), !,
  rdf_list_member(M, L, O, G),
  rdf(M, S, P, L, G).
rdf_list_member(M, S, P, O, G) :-
  rdf(M, S, P, L, G),
  rdf_list_member(M, L, O, G).



%! rdf_list_memberchk(+M, ?L, ?O, ?G) is semidet.
%! rdf_list_memberchk(+M, ?S, ?P, ?O, ?G) is semidet.

rdf_list_memberchk(M, L, O, G) :-
  once(rdf_list_member(M, L, O, G)).


rdf_list_memberchk(M, S, P, O, G) :-
  once(rdf_list_member(M, S, P, O, G)).



%! rdf_literal(+M, ?Literal) is nondet.
%! rdf_literal(+M, ?Literal, ?G) is nondet.
%! rdf_literal(+Lit, -D, -Lex, -LTag) is det.
%! rdf_literal(-Lit, +D, +Lex, -LTag) is det.
%! rdf_literal(-Lit, +D, +Lex, +LTag) is det.

rdf_literal(M, Literal) :-
  rdf_object(M, Literal),
  rdf_is_literal(Literal).


rdf_literal(M, Literal, G) :-
  rdf_object(M, Literal, G),
  rdf_is_literal(Literal).


rdf_literal(Lit, D, Lex, LTag) :-
  var(Lit), !,
  legacy_literal_components(Lit0, D, Lex, LTag),
  rdf11:post_object(Lit, Lit0).
rdf_literal(Lit, D, Lex, LTag) :-
  rdf11:pre_ground_object(Lit, Lit0), !,
  legacy_literal_components(Lit0, D, Lex, LTag).
rdf_literal(Lit, _, _, _) :-
  type_error(rdf_literal, Lit).

% Legacy language-tagged string.
legacy_literal_components(literal(lang(LTag,Lex0)), D, Lex, LTag) :-
  rdf_equal(rdf:langString, D), !,
  atom_string(Lex0, Lex).
% Legacy literal with datatype IRI `rdf:HTML' or `rdf:XMLLiteral'.
% Has a non-atomic lexical form (a DOM compound term).
legacy_literal_components(literal(type(D,Dom)), D, Dom, _) :-
  rdf_prefix_memberchk(D, [rdf:'HTML',rdf:'XMLLiteral']), !.
% Legacy typed literal.
legacy_literal_components(literal(type(D,Lex0)), D, Lex, _) :- !,
  atom_string(Lex0, Lex).
% Legacy simple literal.
legacy_literal_components(literal(Lex0), D, Lex, _) :-
  rdf_equal(xsd:string, D),
  atom_string(Lex0, Lex).



%! rdf_literal_datatype(+Lit, +D) is semidet.
%! rdf_literal_datatype(+Lit, -D) is det.

rdf_literal_datatype(_^^D, D).
rdf_literal_datatype(_@_, rdf:langString).



%! rdf_literal_language_tag(+Lit, -LTag) is semidet.

rdf_literal_language_tag(_@LTag, LTag).



%! rdf_literal_lexical_form(+Lit, +Lex) is semidet.
%! rdf_literal_lexical_form(+Lit, -Lex) is det.
%
% The lexical form Lex is always a string.

rdf_literal_lexical_form(Val^^D, Lex) :- !,
  rdf11:rdf_lexical_form(Val^^D, Lex^^D).
rdf_literal_lexical_form(Val@_, Val).



%! rdf_literal_value(+Literal, +Value) is semidet.
%! rdf_literal_value(+Literal, -Value) is nondet.

rdf_literal_value(Val^^_, Val).
rdf_literal_value(Val@_, Val).



%! rdf_load2(+UriSpec:term) is det.
%! rdf_load2(+UriSpec:term, +Options:list(compound)) is det.
%
% Options are passed to call_on_rdf/3.

rdf_load2(In) :-
  rdf_load2(In, []).


rdf_load2(UriSpec, Options) :-
  forall(
    call_on_rdf(UriSpec, rdf_assert_clean_tuples1, Options),
    true
  ).

rdf_assert_clean_tuples1(Tuples, G) :-
  maplist(rdf_assert_clean_tuple1(G), Tuples).

rdf_assert_clean_tuple1(G, rdf(S,P,O)) :- !,
  rdf_assert_clean_quad1(S, P, O, G).
rdf_assert_clean_tuple1(_, rdf(S,P,O,G)) :-
  rdf_assert_clean_quad1(S, P, O, G).

rdf_assert_clean_quad1(S1, P1, O1, G1) :-
  rdf_clean_quad(rdf(S1,P1,O1,G1), rdf(S2,P2,O2,G2)), !,
  rdf_assert(S2, P2, O2, G2).
rdf_assert_clean_quad1(_, _, _, _).



%! rdf_name(+M, ?Name) is nondet.
%! rdf_name(+M, ?Name, ?G) is nondet.

rdf_name(hdt, Name) :-
  rdf_name(hdt0, Name).
rdf_name(hdt0, Name) :-
  distinct(Name, rdf_name(hdt0, Name, _)).
rdf_name(trp, Name) :-
  rdf_name(Name).


rdf_name(hdt, Name, G) :-
  hdt_call_on_graph(G, rdf_name(hdt0, Name)).
rdf_name(hdt0, Name, Hdt) :-
  (ground(Name) -> rdf_is_subject(Name) ; true),
  hdt_subject(Hdt, Name).
rdf_name(hdt0, Name, Hdt) :-
  (ground(Name) -> rdf_is_predicate(Name) ; true),
  hdt_predicate(Hdt, Name),
  \+ hdt_subject(Hdt, Name).
rdf_name(hdt0, Name, Hdt) :-
  hdt_object(Hdt, Name),
  \+ hdt_subject(Hdt, Name),
  \+ hdt_predicate(Hdt, Name).
rdf_name(trp, Name, G) :-
  rdf_term(trp, Name, G),
  \+ rdf_is_bnode_iri(Name).



%! rdf_node(+M, ?Node) is nondet.
%! rdf_node(+M, ?Node, ?G) is nondet.

rdf_node(hdt, Node) :-
  rdf_node(hdt0, Node).
rdf_node(hdt0, Node) :-
  distinct(Node, rdf_node(hdt0, Node, _)).
rdf_node(trp, Node) :-
  rdf_node(Node).


rdf_node(hdt, Node, G) :-
  hdt_call_on_graph(G, rdf_node(hdt0, Node)).
rdf_node(hdt0, S, Hdt) :-
  rdf_subject(hdt0, S, Hdt).
rdf_node(hdt0, O, Hdt) :-
  rdf_object(hdt0, O, Hdt),
  % Make sure there are no duplicates.
  \+ rdf_subject(hdt, O, Hdt).
rdf_node(trp, S, G) :-
  rdf_subject(trp, S, G).
rdf_node(trp, O, G) :-
  rdf_object(trp, O, G),
  % Make sure there are no duplicates.
  \+ rdf_subject(trp, O, G).



%! rdf_number_of(+M, +Witness, ?S, ?P, ?O, ?G, -NumWitnesses) is semidet.
%
% Master predicate that calculates how many instances match a
% 〈S,P,O,G〉 pattern WRT a given Witness (e.g., `S' or `rdf(S,P,O)').

rdf_number_of(M, Witness, S, P, O, G, NumWitnesses) :-
  aggregate_all(count, distinct(Witness, rdf(M, S, P, O, G)), NumWitnesses).



%! rdf_object(+M, ?O) is nondet.
%! rdf_object(+M, ?O, ?G) is nondet.

rdf_object(hdt, O) :-
  rdf_object(hdt0, O).
rdf_object(hdt0, O) :-
  distinct(O, rdf_object(hdt0, O, _)).
rdf_object(trp, O) :-
  rdf_object(O).


rdf_object(hdt, O, G) :-
  hdt_call_on_graph(G, rdf_object(hdt0, O)).
rdf_object(hdt0, O, Hdt) :-
  hdt_object(Hdt, O).
rdf_object(trp, O, G) :-
  % [O] In memory we can pre-enumerate (pre-check is idle).
  (var(O) -> rdf_object(O) ; true),
  distinct(G, rdf(_, _, O, G)).



%! rdf_predicate(+M, ?P) is nondet.
%! rdf_predicate(+M, ?P, ?G) is nondet.

rdf_predicate(hdt, P) :-
  rdf_predicate(hdt0, P).
rdf_predicate(hdt0, P) :-
  distinct(P, rdf_predicate(hdt0, P, _)).
rdf_predicate(trp, P) :-
  rdf_predicate(P).


rdf_predicate(hdt, P, G) :-
  hdt_call_on_graph(G, rdf_predicate(hdt0, P)).
rdf_predicate(hdt0, P, Hdt) :-
  (var(P) -> true ; rdf_is_predicate(P)),
  hdt_predicate(Hdt, P).
rdf_predicate(trp, P, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (var(P) -> rdf_predicate(P) ; rdf_is_iri(P)),
  distinct(G, rdf(_, P, _, G)).



%! rdf_pref_string(?S, ?P, -Lit, ?G) is nondet.
%! rdf_pref_string(+M, ?S, ?P, -Lit, ?G) is nondet.
%! rdf_pref_string(+M, ?S, ?P, +LanguageRange, -Lit, ?G) is nondet.
%
% Returns, in the following order:
%
%   1. Language-tagged strings that match the given language priority
%      list; returning results for higher priority language earlier.
%
%   2. Language-tagged strings that do not match the given language
%      priority list.
%
%   3. XSD strings.

rdf_pref_string(S, P, Lit, G) :-
  rdf_pref_string(trp, S, P, Lit, G).


rdf_pref_string(M, S, P, Lit, G) :-
  current_lrange(LRange),
  rdf_pref_string(M, S, P, LRange, Lit, G).


% Matching language-tagged strings.
rdf_pref_string(M, S, P, LRange, Lit, G) :-
  rdf_language_tagged_string(M, S, P, LRange, Lit, G).
% Non-matching language-tagged strings.
rdf_pref_string(M, S, P, LRange, Lit, G) :-
  rdf(M, S, P, Lex@LTag, G),
  % Avoid duplicates.
  \+ basic_filtering(LRange, LTag),
  Lit = Lex@LTag.
% Plain XSD strings.
rdf_pref_string(M, S, P, _, Value^^Datatype, G) :-
  % @bug RDF prefix expansion does not work here.
  rdf_equal(Datatype, xsd:string),
  rdf(M, S, P, Value^^Datatype, G).

%! basic_filtering(+Ranges:list, +LTag) is semidet.
%
% Succeeds if the LanguagePriorityList matches the LanguageTag
% according to the basic filtering algorithm described in RFC 4647,
% i.e., if the former is a case-insensitive prefix of the latter,
% while also treating the `*` sign as a wildcard.
%
% @compat RFC 4647

% Allow language priority lists of length 1 to be specified as atoms.
basic_filtering(Ranges, Tag):-
  % NONDET
  member(Range, Ranges),
  atomic_list_concat(Subtags1, -, Range),
  atomic_list_concat(Subtags2, -, Tag),
  basic_filtering1(Subtags1, Subtags2), !.

basic_filtering1(_, []).
basic_filtering1([H1|T1], [H2|T2]):-
  subtag_match(H1, H2),
  basic_filtering1(T1, T2).

%! subtag_match(+RangeSubtag, +Subtag) is semidet.
%
% Two subtags match if either they are the same when compared
% case-insensitively or the language range's subtag is the wildcard
% `*`

subtag_match(*, _):- !.
subtag_match(X1, X2):-
  downcase_atom(X1, X),
  downcase_atom(X2, X).



%! rdf_pref_string_lexical_form(?S, ?P, -Lex, ?G) is nondet.
%! rdf_pref_string_lexical_form(+M, ?S, ?P, -Lex, ?G) is nondet.
%! rdf_pref_string_lexical_form(+M, ?S, ?P, +LRange, -Lex, ?G) is nondet.
%
% Like rdf_pref_string/[4-6], but returns only the lexical form.

rdf_pref_string_lexical_form(S, P, Lex, G) :-
  rdf_pref_string_lexical_form(trp, S, P, Lex, G).


rdf_pref_string_lexical_form(M, S, P, Lex, G) :-
  current_lrange(LRange),
  rdf_pref_string_lexical_form(M, S, P, LRange, Lex, G).


rdf_pref_string_lexical_form(M, S, P, LRange, Lex, G) :-
  rdf_pref_string(M, S, P, LRange, Lit, G),
  rdf_literal_lexical_form(Lit, Lex).



%! rdf_prefix_member(?Elem, +L) is nondet.
%
% Calls member/2 under RDF prefix expansion.

rdf_prefix_member(Elem, L) :-
  member(Elem, L).



%! rdf_prefix_memberchk(?Elem, +L) is nondet.
%
% Calls memberchk/2 under RDF prefix expansion.

rdf_prefix_memberchk(Elem, L) :-
  memberchk(Elem, L).



%! rdf_query_term(+Term, -QueryTerm) is det.

rdf_query_term(Term, QueryTerm) :-
  Term =.. [Key,Value],
  ground(Value),
  rdf_term_to_atom(Value, Atom),
  QueryTerm =.. [Key,Atom].



%! rdf_reification(+M, ?S, ?P, ?O) is nondet.
%! rdf_reification(+M, ?S, ?P, ?O, ?G) is nondet.
%! rdf_reification(+M, ?S, ?P, ?O, ?G, -Stmt) is nondet.

rdf_reification(M, S, P, O) :-
  rdf_reification(M, S, P, O, _).


rdf_reification(M, S, P, O, G) :-
  rdf_reification(M, S, P, O, G, _).


rdf_reification(M, S, P, O, G, Stmt) :-
  rdf(M, Stmt, rdf:subject, S, G),
  rdf(M, Stmt, rdf:predicate, P, G),
  rdf(M, Stmt, rdf:object, O, G).



%! rdf_reserialize(+UriSpec:term, +FileSpec:term) is nondet.
%! rdf_reserialize(+Kind:oneof([quads,triples]), +UriSpec:term,
%!                 +FileSpec:term) is nondet.
%! rdf_reserialize(+Kind:oneof([quads,triples]), +UriSpec:term, +FileSpec:term,
%!                 +Options:list(compound)) is nondet.
%
% Reserializes RDF data from URI to N-Quads FileSpec.
%
% Options are passed to call_to_file/3 and call_on_rdf/3.

rdf_reserialize(UriSpec, FileSpec) :-
  rdf_reserialize(quads, UriSpec, FileSpec).


rdf_reserialize(Kind, UriSpec, FileSpec) :-
  rdf_reserialize(Kind, UriSpec, FileSpec, []).


rdf_reserialize(Kind, UriSpec, FileSpec, Options) :-
  call_to_file(FileSpec, rdf_reserialize_(Kind, UriSpec, Options), Options).

rdf_reserialize_(Kind, UriSpec, Options, Out, Meta, Meta) :-
  forall(call_on_rdf(UriSpec, write_ntuples_(Kind, Out), Options), true).

write_ntuples_(Kind, Out, Tuples, _) :-
  convlist(rdf_clean_tuple, Tuples, Quads),
  maplist(write_ntuple_(Kind, Out), Quads).

write_ntuple_(quads, Out, Quad) :- !,
  write_nquad(Out, Quad).
write_ntuple_(triples, Out, Quad) :-
  write_ntriple(Out, Quad).



%! rdf_retractall is det.
%! rdf_retractall(+Tuple:compound) is det.

rdf_retractall :-
  rdf_retractall(_, _, _, _).


rdf_retractall(rdf(S,P,O)) :- !,
  rdf_retractall(S, P, O, _).
rdf_retractall(rdf(S,P,O,G)) :-
  rdf_retractall(S, P, O, G).



%! rdf_root(+M, ?Root) is nondet.
%! rdf_root(+M, ?Root, ?G) is nondet.

rdf_root(M, Root) :-
  rdf_root(M, Root, _).


rdf_root(M, Root, G) :-
  rdf_subject(M, Root, G),
  \+ rdf(M, _, _, Root, G).



%! rdf_save(+FileSpec:term) is det.
%! rdf_save(+Type:oneof([nquads,ntriples]), +FileSpec:term) is det.
%! rdf_save(+Type:oneof([nquads,ntriples]), +FileSpec:term, +G:atom) is det.
%! rdf_save(+Type:oneof([nquads,ntriples]), +FileSpec:term, +G:atom,
%!          +Options:list(compound)) is det.
%! rdf_save(+Type:oneof([nquads,ntriples]), +FileSpec:term, ?S, ?P, ?O,
%!          ?G) is det.
%! rdf_save(+Type:oneof([nquads,ntriples]), +FileSpec:term, ?S, ?P, ?O, ?G,
%!          +Options:list(compound)) is det.
%
% Options are passed to call_to_file/3.

rdf_save(FileSpec) :-
  rdf_save(quads, FileSpec).


rdf_save(Type, FileSpec) :-
  rdf_save(Type, FileSpec, _).


rdf_save(Type, FileSpec, G) :-
  rdf_save(Type, FileSpec, G, []).


rdf_save(Type, FileSpec, G, Options) :-
  rdf_save(Type, FileSpec, _, _, _, G, Options).


rdf_save(Type, FileSpec, S, P, O, G) :-
  rdf_save(Type, FileSpec, S, P, O, G, []).


rdf_save(Type, FileSpec, S, P, O, G, Options) :-
  call_to_file(FileSpec, rdf_save_(Type, S, P, O, G), Options).

rdf_save_(quads, S, P, O, G, Out, Metadata, Metadata) :- !,
  forall(rdf(S, P, O, G), write_nquad(Out, rdf(S,P,O,G))).
rdf_save_(triples, S, P, O, G, Out, Metadata, Metadata) :-
  forall(rdf(S, P, O, G), write_ntriple(Out, rdf(S,P,O,G))).



%! rdf_scbd_quad(+M, +Node, -Quad) is nondet.
%! rdf_scbd_quads(+M, +Node, -Quads) is nondet.

rdf_scbd_quad(M, Node, rdf(S,P,O,G)) :-
  rdf_scbd_triple(M, Node, G, rdf(S,P,O)).


rdf_scbd_quads(M, Node, Quads) :-
  rdf_subject(M, Node),
  aggregate_all(set(Quad), rdf_scbd_quad(M, Node, Quad), Quads).



%! rdf_scbd_triple(+M, +Node, -Triple) is nondet.
%! rdf_scbd_triple(+M, +Node, ?G, -Triple) is nondet.

rdf_scbd_triple(M, Node, Triple) :-
  rdf_scbd_triple(M, Node, _, Triple).


rdf_scbd_triple(M, Node, G, Triple) :-
  rdf_scbd_triple1(M, Node, [Node], G, Triple).

rdf_scbd_triple1(M, O, Hist, G, Triple) :-
  rdf_cbd_inv_triple1(M, O, Hist, G, Triple).
rdf_scbd_triple1(M, S, Hist, G, Triple) :-
  rdf_cbd_triple1(M, S, Hist, G, Triple).



%! rdf_scbd_triples(+M, ?Node, -Triples) is det.
%! rdf_scbd_triples(+M, ?Node, ?G, -Triples) is det.

rdf_scbd_triples(M, Node, Triples) :-
  rdf_scbd_triples(M, Node, _, Triples).


rdf_scbd_triples(M, Node, G, Triples) :-
  rdf_term(M, Node, G),
  aggregate_all(set(Triple), rdf_scbd_triple(M, Node, G, Triple), Triples).



%! rdf_snap(:Goal_0) .
%
% Call Goal_0 inside a snapshot of the RDF store.

rdf_snap(Goal_0) :-
  rdf_transaction(Goal_0, _, [snapshot(true)]).



%! rdf_statistic(+M, +Key, -Value, +G) is det.
%
% @arg Key The following keys are supported:
%
%          * nodes(-nonneg)
%
%            The number of unique RDF nodes.
%
%          * objects(-nonneg)
%
%            The number of unique objects.
%
%          * predicates(-nonneg)
%
%            The number of unique predicates.
%
%          * subjects(-nonneg)
%
%            The number of unique subjects.
%
%          * terms(-nonneg)
%
%            The number of unique terms.
%
%          * triples(-nonneg)
%
%            The number of unique triples.

rdf_statistic(hdt, Key, N, G) :-
  hdt_call_on_graph(G, rdf_statistic(hdt0, Key, N)).
rdf_statistic(hdt0, nodes, N, Hdt) :-
  rdf_statistic(hdt0, subject_objects, N1, Hdt),
  rdf_statistic(hdt0, subjects, N2, Hdt),
  rdf_statistic(hdt0, objects, N3, Hdt),
  sum_list([N1,N2,N3], N).
rdf_statistic(hdt0, terms, N, Hdt) :-
  rdf_statistic(hdt0, predicates, N1, Hdt),
  rdf_statistic(hdt0, nodes, N2, Hdt),
  sum_list([N1,N2], N).
rdf_statistic(hdt0, Key, N, Hdt) :-
  hdt_header_property(Key, P),
  once(hdt_header(Hdt, _, P, N^^_)).
rdf_statistic(trp, nodes, N, G) :-
  aggregate_all(count, rdf_node(trp, _, G), N).
rdf_statistic(trp, objects, N, G) :-
  aggregate_all(count, rdf_object(trp, _, G), N).
rdf_statistic(trp, predicates, N, G) :-
  aggregate_all(count, rdf_predicate(trp, _, G), N).
rdf_statistic(trp, subjects, N, G) :-
  aggregate_all(count, rdf_subject(trp, _, G), N).
rdf_statistic(trp, terms, N, G) :-
  aggregate_all(count, rdf_term(trp, _, G), N).
rdf_statistic(trp, triples, N, G) :-
  rdf_graph_property(G, triples(N)).

hdt_header_property(objects, '<http://rdfs.org/ns/void#distinctObjects>').
hdt_header_property(predicates, '<http://rdfs.org/ns/void#properties>').
hdt_header_property(subject_objects, '<http://purl.org/HDT/hdt#dictionarynumSharedSubjectObject>').
hdt_header_property(subjects, '<http://rdfs.org/ns/void#distinctSubjects>').
hdt_header_property(triples, '<http://rdfs.org/ns/void#triples>').



%! rdf_subdatatype_of(?Sub, ?Super) is nondet.

rdf_subdatatype_of(X, Y) :-
  xsd_subtype_of(X, Y).



%! rdf_subject(+M, ?S) is nondet.
%! rdf_subject(+M, ?S, ?G) is nondet.

rdf_subject(hdt, S) :-
  rdf_subject(hdt0, S).
rdf_subject(hdt0, S) :-
  distinct(S, rdf_subject(hdt0, S, _)).
rdf_subject(trp, S) :-
  rdf_subject(S).


rdf_subject(hdt, S, G) :-
  hdt_call_on_graph(G, rdf_subject(hdt0, S)).
rdf_subject(hdt0, S, Hdt) :-
  (var(S) -> true ; rdf_is_subject(S)),
  hdt_subject(Hdt, S).
rdf_subject(trp, S, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (var(S) -> rdf_subject(S) ; rdf_is_subject(S)),
  (var(G) -> distinct(G, rdf(S, _, _, G)) ; once(rdf(S, _, _, G))).



%! rdf_term(+M, ?Term) is nondet.
%! rdf_term(+M, ?Term, ?G) is nondet.

rdf_term(hdt, Term) :-
  rdf_term(hdt0, Term).
rdf_term(hdt0, Term) :-
  distinct(Term, rdf_term(hdt0, Term, _)).
rdf_term(trp, Term) :-
  rdf_term(Term).


rdf_term(hdt, Name, G) :-
  hdt_call_on_graph(G, rdf_term(hdt0, Name)).
rdf_term(hdt0, Name, Hdt) :-
  rdf_name(hdt0, Name, Hdt).
rdf_term(hdt0, BNode, Hdt) :-
  rdf_bnode_iri(hdt0, BNode, Hdt).
rdf_term(trp, P, G) :-
  rdf_predicate(trp, P, G).
rdf_term(trp, Node, G) :-
  rdf_node(trp, Node, G),
  % Ensure there are no duplicates.
  \+ rdf_predicate(trp, Node, G).



%! rdf_term_to_atom(+Term, -Atom) is det.

rdf_term_to_atom(Val^^D, Atom) :- !,
  rdf11:in_ground_type(D, Val, Lex),
  format(atom(Atom), '"~a"^^<~a>', [Lex,D]).
rdf_term_to_atom(Lex@LTag, Atom) :- !,
  format(atom(Atom), '"~a"@~a', [Lex,LTag]).
rdf_term_to_atom(Atom, Atom) :-
  rdf_is_iri(Atom).



%! rdf_to_hdt(+UriSpec:term, +FileSpec:term) is det.

rdf_to_hdt(UriSpec, FileSpec) :-
  absolute_file_name(FileSpec, File, [access(write)]),

  % Convert to uncompressed N-Triples.
  debug(semweb(rdf_to_hdt), "Creating uncompressed N-Triples…", []),
  create_temporary_file1(TriplesFile),
  rdf_reserialize(triples, UriSpec, TriplesFile, [compression(none)]),
  debug(semweb(rdf_to_hdt), "…uncompressed N-Triples created.", []),

  % Create HDT file.
  debug(semweb(rdf_to_hdt), "Creating HDT…", []),
  file_name_extension(File, working, HdtFile),
  hdt:hdt_create_from_file(HdtFile, TriplesFile, []),
  with_mutex(rdf_to_hdt,
    (   % Somebody else was earlier.
        exists_file(File)
    ->  delete_file(HdtFile)
    ;   rename_file(HdtFile, File)
    )
  ),
  debug(semweb(rdf_to_hdt), "…HDT created.", []),

  % Create HDT index file.
  debug(semweb(rdf_to_hdt), "Creating HDT index…", []),
  hdt_warm_index(File),
  debug(semweb(rdf_to_hdt), "…HDT index created.", []).
  
create_temporary_file1(File) :-
  uuid(Uuid),
  absolute_file_name(Uuid, File, [access(write),extensions([tmp])]).



%! rdf_tuple_quad(+Tuple, +G, -Quad) is det.

rdf_tuple_quad(rdf(S,P,O), G, rdf(S,P,O,G)) :- !.
rdf_tuple_quad(Quad, _, Quad).



%! rdf_tuple_triple(+Tuple, ?Triple) is det.

rdf_tuple_triple(rdf(S,P,O), rdf(S,P,O)) :- !.
rdf_tuple_triple(rdf(S,P,O,_), rdf(S,P,O)).



%! rdf_write_term(+Out, +Term) is det.

rdf_write_term(Out, Term) :-
  write_object(Out, Term).



%! write_literal(+Literal:compound) is det.
%! write_literal(+Out:stream, +Literal:compound) is det.

write_literal(Literal) :-
  write_literal(current_output, Literal).


write_literal(Out, Val^^D) :- !,
  rdf_literal_lexical_form(Val^^D, Lex),
  turtle:turtle_write_quoted_string(Out, Lex),
  write(Out, '^^'),
  write_iri(Out, D).
write_literal(Out, Val@LTag) :- !,
  rdf_literal_lexical_form(Val@LTag, Lex),
  turtle:turtle_write_quoted_string(Out, Lex),
  write(Out, '@'),
  write(Out, LTag).
write_literal(Out, Val) :-
  rdf_equal(xsd:string, D),
  write_literal(Out, Val^^D).

write_object(Out, S) :-
  write_subject(Out, S), !.
write_object(Out, Lit) :-
  write_literal(Out, Lit).

write_predicate(Out, P) :-
  write_iri(Out, P).

write_subject(Out, Iri) :-
  rdf_is_iri(Iri), !,
  write_iri(Out, Iri).



%! write_nquad(+Quad) is det.
%! write_nquad(+Out, +Quad) is det.
%! write_nquad(+Out, +Triple, +G) is det.
%! write_nquad(+Out, +S, +P, +O, G) is det.
%
% Quad must be a quad (compound term rdf/4), i.e., triples (compound
% term rdf/3) are not accepted.

write_nquad(Quad) :-
  write_nquad(current_output, Quad).


write_nquad(Out, rdf(S,P,O,G)) :-
  write_nquad(Out, S, P, O, G).


write_nquad(Out, rdf(S,P,O), G) :- !,
  write_nquad(Out, rdf(S,P,O,G)).
write_nquad(Out, rdf(S,P,O,_), G) :-
  write_nquad(Out, rdf(S,P,O,G)).


write_nquad(Out, S, P, O, G) :-
  write_ntriple_part1(Out, S, P, O),
  write_graph(Out, G),
  write(Out, ' '),
  write_eot(Out).

write_ntriple_part1(Out, S, P, O) :-
  write_subject(Out, S),
  write(Out, ' '),
  write_predicate(Out, P),
  write(Out, ' '),
  write_object(Out, O),
  write(Out, ' ').

write_eot(Out) :-
  write(Out, '.\n').

write_graph(Out, G) :-
  write_iri(Out, G).

write_iri(Out, Iri) :-
  turtle:turtle_write_uri(Out, Iri).



%! write_nquads(+Quads) is det.
%! write_nquads(+Out, +Quads) is det.
%! write_nquads(+Out, +Tuples, +G) is det.

write_nquads(Quads) :-
  write_nquads(current_output, Quads).


write_nquads(Out, Quads) :-
  maplist(write_nquad(Out), Quads).


write_nquads(Out, Tuples, G) :-
  maplist(rdf_tuple_triple, Tuples, Triples),
  maplist({Out,G}/[Triple]>>write_nquad(Out, Triple, G), Triples).



%! write_ntriple(+Tuple) is det.
%! write_ntriple(+Out, +Tuple) is det.
%! write_ntriple(+Out, +S, +P, +O) is det.
%
% write_ntriple/2 also accepts quads (compound term rdf/4), but writes
% them as triples.

write_ntriple(Triple) :-
  write_ntriple(current_output, Triple).


write_ntriple(Out, rdf(S,P,O)) :- !,
  write_ntriple(Out, S, P, O).
write_ntriple(Out, rdf(S,P,O,_)) :-
  write_ntriple(Out, S, P, O).


write_ntriple(Out, S, P, O) :-
  write_ntriple_part1(Out, S, P, O),
  write_eot(Out).



%! write_ntuple(+Tuple:compound) is det.
%! write_ntuple(+Out:stream, +Tuple:compound) is det.
%
% If Tuple is a triple (compound term rdf/3), it is written as a
% triple.  If Tuple is a quad (compound term rdf/4), it is written as
% a quad.

write_ntuple(Tuple) :-
  write_ntuple(current_output, Tuple).


write_ntuple(Out, rdf(S,P,O)) :- !,
  write_ntriple(Out, S, P, O).
write_ntuple(Out, rdf(S,P,O,G)) :-
  rdf_default_graph(G), !,
  write_ntriple(Out, S, P, O).
write_ntuple(Out, rdf(S,P,O,G)) :-
  write_nquad(Out, S, P, O, G).



%! write_ntuples(+Tuples) is det.
%! write_ntuples(+Out, +Tuples) is det.

write_ntuples(Tuples) :-
  write_ntuples(current_output, Tuples).


write_ntuples(Out, Tuples) :-
  maplist(write_ntuple(Out), Tuples).
