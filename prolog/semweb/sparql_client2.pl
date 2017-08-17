:- module(
  sparql_client2,
  [
    select_result/3,      % +Result, +Keys, -Values
    sparql_client/3,      % +Uri, +Query, -Result
    sparql_client/4,      % +Uri, +Query, -Result, +Options
    sparql_client_file/3, % +Uri, +File, -Result
    sparql_client_file/4  % +Uri, +File, -Result, +Options
  ]
).

/** <module> SPARQL Client

A client that performs SPARQL 1.1 HTTP requests and that interprets
SPARQL 1.1 HTTP responses (result sets).

---

@author Wouter Beek
@compat SPARQL 1.1 Protocol
@comapt SPARQL 1.1 Query Results CSV and TSV Formats
@compat SPARQL 1.1 Query Results JSON Format
@compat SPARQL Query Results XML Format (Second Edition)
@see https://www.w3.org/TR/2013/REC-sparql11-protocol-20130321/
@see https://www.w3.org/TR/2013/REC-sparql11-results-csv-tsv-20130321/
@see https://www.w3.org/TR/2013/REC-sparql11-results-json-20130321/
@see https://www.w3.org/TR/2013/REC-rdf-sparql-XMLres-20130321/
@tbd Fix streamed JSON parser.
@tbd Extract the order of the results set in the XML parsed (<head>).
@version 2017/03-2017/08
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(dcg/rfc7159)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pure_input)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(sgml)).
:- use_module(library(stream_ext)).
:- use_module(library(uri/uri_ext)).





%! select_result(+Result:compound, +Keys:list(atom),
%!               -Values:list(rdf_term)) is det.
%
% Hack that is currently needed to retrieve values in RDF/XML results
% in the appropriate order.  This can not only be used to enforce the
% order expressed in the `select' clauses, but also to apply an
% arbitrary order that is not in the query itself.

select_result(select(Keys0,Values0), Keys, Values) :-
  pairs_keys_values(Pairs, Keys0, Values0),
  select_result_pairs(Pairs, Keys, Values).

select_result_pairs([], [], []) :- !.
select_result_pairs(Pairs1, [Key|Keys], [Value|Values]) :-
  selectchk(Key-Value, Pairs1, Pairs2),
  select_result_pairs(Pairs2, Keys, Values).



%! sparql_client(+Uri:atom, +Query:string, -Result:compound) is nondet.
%! sparql_client(+Uri:atom, +Query:string, -Result:compound,
%!               +Options:list(compound)) is nondet.
%
% @arg Uri Either an atomic URI or a compound term uri/5 that is
%      supported by uri_comps/2.
%
% @arg Query A string that contains a syntactically valid SPARQL 1.1
%      query.
%
% @arg Result An atomic result for the given query.
%
%      * For `ask' queries the Boolean `true' or `false', and this
%        predicate is deterministic.
%
%      * For `construct' and `describe' queries a compound term rdf/3
%        representing an RDF triple.
%
%      * For `select' queries a list representing a row of the
%        solution set.
%
% @arg Options The following options are supported:
%
%      * default_graphs(+list(atom))
%
%        Default is `[]'.
%
%      * named_graphs(+list(atom))
%
%        Default is `[]'.
%
%      * result_set_format(+oneof([csv,json,tsv,xml]))
%
%        Default is `xml'.
%
%      * method(+oneof([get,post,url_encoded_post])
%
%        Default is `post'.
%
%      * type(+oneof([query,update]))
%
%        Default is `query'.
%
%      * Other options are passed to call_on_uri/3.

sparql_client(Uri, Query, Result) :-
  sparql_client(Uri, Query, Result, []).


sparql_client(Uri1, Query, Result, Options1) :-
  select_option(method(Method), Options1, Options2, post),
  select_option(type(Type), Options2, Options3, query),
  select_option(default_graphs(DefaultGraphs), Options3, Options4, []),
  select_option(named_graphs(NamedGraphs), Options4, Options5, []),
  (   Type == query
  ->  select_option(result_set_format(Format), Options5, Options6, xml),
      result_set_media_type(Format, MediaType),
      maplist(
        graph_option('default-graph-uri'),
        DefaultGraphs,
        DefaultGraphsQuery
      ),
      maplist(
        graph_option('named-graph-uri'),
        NamedGraphs,
        NamedGraphsQuery
      ),
      append(DefaultGraphsQuery, NamedGraphsQuery, GraphsQuery),
      (   % Query via GET
          Method == get
      ->  uri_comps(Uri1, uri(Scheme,Authority,Segments,QueryComps1,_)),
          append(QueryComps1, [query(Query)|GraphsQuery], QueryComps2),
          uri_comps(Uri2, uri(Scheme,Authority,Segments,QueryComps2,_)),
          Options7 = Options6
      ;   % Query via POST directly
          Method == post
      ->  uri_comps(Uri1, uri(Scheme,Authority,Segments,QueryComps1,_)),
          append(QueryComps1, GraphsQuery, QueryComps2),
          uri_comps(Uri2, uri(Scheme,Authority,Segments,QueryComps2,_)),
          merge_options(
            [post(string('application/sparql-query',Query))],
            Options6,
            Options7
         )
      ;   % Query via URL-encoded POST
          Method == url_encoded_post
      ->  uri_query_components(QueryComps, [query(Query)|GraphsQuery]),
          MediaType = 'application/x-www-form-urlencoded; charset=UTF-8',
          merge_options(
            [post(string(MediaType,QueryComps))],
            Options6,
            Options7
          ),
          Uri2 = Uri1
      )
  ;   Type == update
  ->  MediaType = '*/*; charset=UTF-8',
      maplist(
        graph_option('using-graph-uri'),
        DefaultGraphs,
        DefaultGraphsQuery
      ),
      maplist(
        graph_option('using-named-graph-uri'),
        NamedGraphs,
        NamedGraphsQuery
      ),
      append(DefaultGraphsQuery, NamedGraphsQuery, GraphsQuery),
      (   Method == post
      ->  uri_comps(Uri1, uri(Scheme,Authority,Segments,QueryComps1,_)),
          append(QueryComps1, GraphsQuery, QueryComps2),
          uri_comps(Uri2, uri(Scheme,Authority,Segments,QueryComps2,_)),
          MediaType = 'application/sparql-update; charset=UTF-8',
          merge_options([post(string(MediaType,Query))], Options5, Options7)
      ;   Method == url_encoded_post
      ->  uri_query_components(QueryComps, [update(Query)|GraphsQuery]),
          MediaType = 'application/x-www-form-urlencoded; charset=UTF-8',
          merge_options(
            [post(string(MediaType,QueryComps))],
            Options5,
            Options7
          ),
          Uri2 = Uri1
      )
  ),
  merge_options([request_header('Accept'=MediaType)], Options7, Options8),
  call_on_uri(Uri2, sparql_client_results(Result), Options8).

%! result_set_media_type(+Format:oneof([csv,json,tsv,xml]),
%!                        -MediaType:atom) is det.

result_set_media_type(csv, 'text/csv; charset=UTF-8').
result_set_media_type(json, 'application/sparql-results+json').
result_set_media_type(tsv, 'text/tab-separated-values; charset=UTF-8').
result_set_media_type(xml, 'application/sparql-results+xml; charset=UTF-8').

%! graph_option(+Key:atom, +Value:atom, -Option:compound) is det.
%
% @arg Key is either `default-graph-uri', `named-graph-uri',
%      `using-graph-uri', or `using-named-graph-uri'.

graph_option(Key, Value, Option) :-
  Option =.. [Key,Value].

%! sparql_client_results(-Result:compound, +In:stream, +Metadata1:list(dict),
%!                       -Metadata2:list(dict)) is nondet.

sparql_client_results(_, In, Metadata, Metadata) :-
  metadata_status_code(Metadata, Status),
  between(400, 599, Status), !,
  throw(error(http_error_code(Status))),
  copy_stream_data(In, error_output).
sparql_client_results(Result, In, Metadata, Metadata) :-
  metadata_content_type(Metadata, MediaType),
  call_cleanup(
    sparql_client_results(MediaType, In, Result),
    close(In)
  ).

%! sparql_client_results(+MediaType:compound, +In:stream,
%!                       -Result:compound) is nondet.

sparql_client_results(media(text/csv,_), In, Result) :-
  sparql_result_csv(In, Result).
sparql_client_results(media(application/'sparql-results+json',_), In, Result) :-
  sparql_result_json(In, Result).
sparql_client_results(media(text/'tab-separated-values',_), In, Result) :-
  sparql_result_tsv(In, Result).
sparql_client_results(media(application/'sparql-results+xml',_), In, Result) :-
  sparql_result_xml(In, Result).
sparql_client_results(MediaType, _, _) :-
  domain_error(sparql_media_type, MediaType).



%! sparql_client_file(+Uri:atom, +File:atom, -Result:compound) is nondet.
%! sparql_client_file(+Uri:atom, +File:atom, -Result:compound,
%!                    +Options:list(compound)) is nondet.
%
% Options are passed to sparql_client/4.

sparql_client_file(Uri, File, Result) :-
  sparql_client_file(Uri, File, Result, []).


sparql_client_file(Uri, File, Result, Options) :-
  file_to_string(File, Query),
  sparql_client(Uri, Query, Result, Options).





% SPARQL 1.1 QUERY RESULTS CSV AND TSV FORMATS %

%! sparql_result_csv(+In:stream, -Row:compound) is nondet.

sparql_result_csv(In, Row) :-
  csv_options(Options, []),
  csv:csv_read_stream_row(In, Row, _Line, Options).



%! sparql_result_tsv(+In:stream, -Row:compound) is nondet.

sparql_result_tsv(In, Row) :-
  csv_options(Options, [separator(0'\t)]),
  csv:csv_read_stream_row(In, Row, _Line, Options).





% SPARQL 1.1 QUERY RESULTS JSON FORMAT %

%! sparql_result_json(+In:stream, -Row:compound) is nondet.

sparql_result_json(In, Row) :-
  (   debugging(sparql_client)
  ->  peek_string(In, 1000, String),
      debug(sparql_client, "~s", [String])
  ;   true
  ),
  phrase_from_stream(result(Row), In).

result(Row) -->
  skip_until_bindings,
  binding(Row).

skip_until_bindings -->
  rfc7159:'begin-object',
  rfc7159:member(head-_),
  rfc7159:'value-separator',
  rfc7159:string("results"),
  rfc7159:'name-separator',
  rfc7159:'begin-object',
  rfc7159:string("bindings"),
  rfc7159:'name-separator',
  rfc7159:'begin-array'.

binding(Dict) -->
  rfc7159:object(Dict).



% SPARQL QUERY RESULTS XML FORMAT (SECOND EDITION) %

%! sparql_result_xml(+In:stream, -Result:compound) is nondet.
%
% Skip the following content:
%
% ```xml
% <?xml version='1.0' encoding='UTF-8'?>
% <sparql xmlns='http://www.w3.org/2005/sparql-results#'>
% <head>
%   <variable name='subject'/>
%   <variable name='predicate'/>
% </head>
% <results>
% ```
%
% or
%
% ```xml
% <boolean>true</boolean>
% ```

sparql_result_xml(In, Result) :-
  (   debugging(sparql_client)
  ->  peek_string(In, 500, Str),
      debug(sparql_client, Str, [])
  ;   true
  ),
  setup_call_cleanup(
    new_sgml_parser(Parser, []),
    (
      set_sgml_parser(Parser, space(remove)),
      sparql_result_xml0(In, Parser, Result)
    ),
    free_sgml_parser(Parser)
  ).

sparql_result_xml0(In, Parser, Result) :-
  skip_until_tag(In, Name),
  (   atom_prefix(Name, boolean)
  ->  sparql_result_xml_ask(In, Parser, Result)
  ;   atom_prefix(Name, results)
  ->  sparql_result_xml_select(In, Parser, Result)
  ;   sparql_result_xml0(In, Parser, Result)
  ).



%! sparql_result_xml_ask(+In:stream, +Parser:blob, -Result:compound) is det.

sparql_result_xml_ask(In, Parser, ask(Decision)) :-
  sgml_parse(
    Parser,
    [document(Dom),parse(element),source(In),syntax_errors(quiet)]
  ),
  Dom = [Decision].



%! sparql_result_xml_select(+In:stream, +Parser:blob,
%!                          -Result:compound) is nondet.

sparql_result_xml_select(In, Parser, select(VarNames,Row)) :-
  repeat,
  sgml_parse(Parser, [document(Dom),parse(element),source(In)]),
  (   Dom = [element(result,_,Bindings)]
  ->  maplist(xml_binding, Bindings, VarNames, Row)
  ;   !, fail
  ).



%! xml_binding(+Dom:list(compound), -VarName:atom, -Term:rdf_term) is det.

xml_binding(element(binding,[name=VarName],Term1), VarName, Term2) :-
  xml_term(Term1, Term2).



%! xml_term(+Dom:list(compound), -Term:rdf_term) is det.
%
% RDF URI Reference U
%   <binding><uri>U</uri></binding>
% RDF Literal S
%   <binding><literal>S</literal></binding>
% RDF Literal S with language L
%   <binding><literal xml:lang="L">S</literal></binding>
% RDF Typed Literal S with datatype URI D
%   <binding><literal datatype="D">S</literal></binding>
% Blank Node label I
%   <binding><bnode>I</bnode></binding>

xml_term([element(uri,_,[Iri])], Iri) :- !.
xml_term([element(literal,[datatype=DatatypeIri],[LexicalForm])], Literal) :- !,
  rdf_literal(Literal, DatatypeIri, LexicalForm, _).
xml_term([element(literal,['xml:lang'=LanguageTag],[LexicalForm])], Literal) :- !,
  rdf_literal(Literal, rdf:langString, LexicalForm, LanguageTag).
xml_term([element(literal,[],[LexicalForm])], Literal) :- !,
  rdf_literal(Literal, xsd:string, LexicalForm, _).
xml_term([element(bnode,[],[Label])], BNode) :-
  atom_concat('_:', Label, BNode).





% HELPERS %

%! get_until_code(+In:stream, +Code:code, -Codes:list(code)) is semidet.

get_until_code(In, Code, Codes):-
  get_code(In, H), !,
  (   H == -1
  ->  fail
  ;   H == Code
  ->  Codes = []
  ;   Codes = [H|T],
      get_until_code(In, Code, T)
  ).



%! skip_until_tag(+In:stream, -Name:atom) is semidet.

skip_until_tag(In, Name) :-
  get_until_code(In, 0'<, _),
  get_until_code(In, 0'>, Codes),
  atom_codes(Name, Codes).
