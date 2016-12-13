:- module(
  iisg_api,
  [
    iisg_record2rdf/1 % +Record
  ]
).

/** <module> IISG API

@author Wouter Beek
@version 2016/06-2016/07
*/

:- use_module(library(conv/xml2rdf)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(http/http_io)).
:- use_module(library(os/io)).
:- use_module(library(print_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).
:- use_module(library(xpath)).





iisg_record2rdf(Record) :-
  atomic_list_concat(['',solr,all,oai], /, Path),
  atomic_list_concat([oai,'socialhistoryservices.org',Record], :, Id),
  uri_query_components(Query, [identifier(Id),metadataPrefix(marcxml),verb('GetRecord')]),
  uri_components(Iri, uri_components(http,'api.socialhistoryservices.org',Path,Query,_)),
  atomic_list_concat([iisg,Record], '_', Base),
  atom_concat(Base, '.nt.gz', Local),
  absolute_file_name(data(Local), File, [access(write)]),
  rdf_global_id(iisg:Record, S),
  xml2rdf(Iri, File, ['marc:record'], _{concept: S}, _{}).



%! iisg_record(Dom) is det.
%! iisg_record(Dom, +Opts) is det.
%
% Enumerates DOM describing IISG collection objects.
%
% The following options are supported:
%
%   * limit(+oneof([20,50]))
%
%     The number of search results per page.  The default is 50.
%
%   * page(+positive_integer)
%
%     The number of the first page that is retrieved.  The default is
%     1.
%
%   * search(+string)
%
%     The optional search string.
%
%   * sort(+oneof([author,callnumber,relevance,title,year,'year+asc']))
%
%     The way in which search results are ordered.  The default is
%     `relevance`.

iisg_record(Dom) :-
  iisg_record(Dom, _{}).


iisg_record(Dom3, Opts1) :-
  dict_put_def(limit, Opts1, 50, Opts2),
  dict_put_def(page, Opts2, 1, Opts3),
  dict_put_def(sort, Opts3, relevance, Opts4),
  between(1, inf, Opts4.page),
  (dict_has_key(lookfor, Opts4) -> T = [lookfor=Opts4.search] ; T = []),
  iisg_collection_iri(
    '/Search/Results',
    [limit=Opts4.limit,page=Opts4.page,sort=Opts4.sort,type='AllFields'|T],
    Iri1
  ),
  http_retry_until_success(html_download(Iri1, Dom1)),
  xpath(Dom1, //a(@class=title), A1),
  xpath_chk(A1, /self(@href), Path1),
  iisg_collection_iri(Path1, [], Iri2),
  http_retry_until_success(html_download(Iri2, Dom2)),
  xpath(Dom2, //a(@target='MARCXMLMain'), A2),
  xpath_chk(A2, /self(@href), Path2),
  iisg_collection_iri(Path2, [], Iri3),
  http_retry_until_success(xml_download(Iri3, Dom3)).

iisg_collection_iri(Path, QueryPairs, Iri) :-
  uri_query_components(Query, QueryPairs),
  uri_components(
    Iri,
    uri_components(https,'search.socialhistory.org',Path,Query,_)
  ).
