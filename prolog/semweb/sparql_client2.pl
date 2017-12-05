:- module(
  sparql_client2,
  [
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
@tbd Extract the order of the results set in the XML parse (<head>).
@version 2017/03-2017/11
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client2)).
:- use_module(library(http/http_header)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/sparql_parser)).
:- use_module(library(sgml)).
:- use_module(library(uri/uri_ext)).





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
%      * bugs(+oneof([none,virtuoso]))
%
%        Purposefully make bugs in performing the SPARQL request.
%        Supported values are `none' (default) for not making any bugs
%        on purpose and `virtuoso' for making bugs that are needed in
%        order to retrieve results from a Virtuoso endpoint.
%
%      * default_graphs(+list(atom))
%
%        Default is `[]'.
%
%      * format(+oneof([csv,json,tsv,xml]))
%
%        Default is `xml'.
%
%      * method(+oneof([get,post,url_encoded_post])
%
%        Default is `post'.
%
%      * named_graphs(+list(atom))
%
%        Default is `[]'.

sparql_client(Uri, Query, Result) :-
  sparql_client(Uri, Query, Result, []).


sparql_client(Uri1, Query, Result, Options1) :-
  select_option(bugs(Bugs), Options1, Options2, none),
  select_option(method(Method), Options2, Options3, post),
  (   Bugs == virtuoso,
      Method == post
  ->  print_message(warning, "Virtuoso cannot handle direct POST requests."),
      fail
  ;   true
  ),
  sparql_form(Query, Form),
  select_option(default_graphs(DefaultGraphs), Options3, Options4, []),
  select_option(named_graphs(NamedGraphs), Options4, Options5, []),
  (   sparql_is_query_form(Form)
  ->  select_option(format(Format), Options5, Options6, xml),
      (   Bugs == virtuoso,
          Format == tsv
      ->  print_message(warning, "Virtuoso does not emit valid TSV results."),
          fail
      ;   true
      ),
      result_set_media_type(Format, Bugs, ReplyMediaType1),
      maplist(
        graph_option('default-graph-uri'),
        DefaultGraphs,
        DefaultGraphsQuery
      ),
      maplist(graph_option('named-graph-uri'), NamedGraphs, NamedGraphsQuery),
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
          merge_options([post(string('application/sparql-query',Query))],
                        Options6, Options7)
      ;   % Query via URL-encoded POST
          Method == url_encoded_post
      ->  uri_query_components(QueryComps, [query(Query)|GraphsQuery]),
          RequestMediaType0 = 'application/x-www-form-urlencoded; charset=UTF-8',
          merge_options(
            [post(string(RequestMediaType0,QueryComps))],
            Options6,
            Options7
          ),
          Uri2 = Uri1
      )
  ;   sparql_is_update_form(Form)
  ->  ReplyMediaType1 = '*/*; charset=UTF-8',
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
          RequestMediaType0 = 'application/sparql-update; charset=UTF-8',
          merge_options(
            [post(string(RequestMediaType0,Query))],
            Options5,
            Options7
          )
      ;   Method == url_encoded_post
      ->  uri_query_components(QueryComps, [update(Query)|GraphsQuery]),
          RequestMediaType0 = 'application/x-www-form-urlencoded; charset=UTF-8',
          merge_options(
            [post(string(RequestMediaType0,QueryComps))],
            Options5,
            Options7
          ),
          Uri2 = Uri1
      )
  ),
  merge_options(
    [
      header(content_type,ContentType),
      request_header('Accept'=ReplyMediaType1),
      status_code(Status)
    ],
    Options7,
    Options8
  ),
  http_open2(Uri2, In, Options8),
  call_cleanup(
    (
      http_parse_header_value(content_type, ContentType, ReplyMediaType2),
      (   between(200, 299, Status)
      ->  sparql_client_results(Form, In, ReplyMediaType2, Result)
      ;   throw(error(http_error_code(Status))),
          copy_stream_data(In, error_output)
      )
    ),
    close(In)
  ).

%! result_set_media_type(+Format:oneof([csv,json,tsv,xml]),
%!                       +Bugs:oneof([none,virtuoso]),
%!                       -ReplyMediaType:atom) is det.

result_set_media_type(csv, none, 'text/csv; charset=UTF-8').
result_set_media_type(csv, virtuoso, 'text/csv').
result_set_media_type(json, _, 'application/sparql-results+json').
result_set_media_type(tsv, none, 'text/tab-separated-values; charset=UTF-8').
result_set_media_type(tsv, virtuoso, 'text/tab-separated-values').
result_set_media_type(xml, none, 'application/sparql-results+xml; charset=UTF-8').
result_set_media_type(xml, virtuoso, 'application/sparql-results+xml').

%! graph_option(+Key:atom, +Value:atom, -Option:compound) is det.
%
% @arg Key is either `default-graph-uri', `named-graph-uri',
%      `using-graph-uri', or `using-named-graph-uri'.

graph_option(Key, Value, Option) :-
  Option =.. [Key,Value].

%! sparql_client_results(+Form:oneof([ask,construct,describe,select]),
%!                       +In:stream, +MediaType:compound,
%!                       -Result:compound) is nondet.

sparql_client_results(Form, In, MediaType, Result) :-
  (   media_type_comps(MediaType, text, csv, Params)
  ->  % BUG: “Singleton variable in branch: Value”
      (memberchk(header=Value, Params) -> assertion(Value=present) ; true),
      sparql_result_csv(In, Result)
  ;   media_type_comps(MediaType, application, 'sparql-results+json', _)
  ->  sparql_result_json(Form, In, Result)
  ;   media_type_comps(MediaType, application, 'sparql-results+xml', _)
  ->  sparql_result_xml(Form, In, Result)
  ;   media_type_comps(MediaType, text, 'tab-separated-values', _)
  ->  sparql_result_tsv(In, Result)
  ;   domain_error(sparql_media_type, MediaType)
  ).



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

sparql_result_csv(In, select(VarNames,Terms)) :-
  csv_options(Options, []),
  once(csv:csv_read_stream_row(In, HeaderRow, _, Options)),
  HeaderRow =.. [row|VarNames],
  csv:csv_read_stream_row(In, DataRow, _, Options),
  DataRow =.. [row|Terms].



%! sparql_result_tsv(+In:stream, -Row:compound) is nondet.

sparql_result_tsv(In, select(VarNames2,Terms2)) :-
  csv_options(Options, [separator(0'\t)]),
  once(csv:csv_read_stream_row(In, HeaderRow, _, Options)),
  HeaderRow =.. [row|VarNames1],
  maplist(tsv_var_name, VarNames1, VarNames2),
  csv:csv_read_stream_row(In, DataRow, _, Options),
  DataRow =.. [row|Terms1],
  maplist(tsv_term, Terms1, Terms2).

tsv_var_name(row(VarName1), VarName2) :-
  atom_concat(?, VarName2, VarName1).

tsv_term(row(Atom), Term) :-
  atom_phrase(tsv_term(Term), Atom).

tsv_term(Iri) -->
  "<", ...(Codes), ">", !,
  {atom_codes(Iri, Codes)}.
tsv_term(Literal) -->
  "\"", ...(Codes), "\"", !,
  {string_codes(Lex, Codes)},
  (   "^^<"
  ->  ...(Codes), ">", !,
      {atom_codes(D, Codes)},
      {rdf_typed_literal(D, Lex, Literal)}
  ;   "@"
  ->  rest(Codes),
      {atom_codes(LTag, Codes)},
      {rdf_language_tagged_string(LTag, Lex, Literal)}
  ).
tsv_term(BNode) -->
  rest(Codes),
  {atom_codes(BNode, Codes)}.





% SPARQL 1.1 QUERY RESULTS JSON FORMAT %

%! sparql_result_json(+Form:oneof([ask,select]), +In:stream,
%!                    -Result) is nondet.
%
% Assumption: `head' appears before `results', if `Form = select'.

sparql_result_json(ask, In, ask(Result)) :- !,
  json_read_dict(In, Dict, [value_string_as(atom)]),
  _{boolean: Result, head: _} :< Dict.
sparql_result_json(select, In, select(VarNames,Result)) :-
  json_read_dict(In, Dict1, [value_string_as(atom)]),
  _{head: Head, results: Results} :< Dict1,
  _{vars: VarNames} :< Head,
  _{bindings: Bindings} :< Results,
  member(Binding1, Bindings),
  dict_pairs(Binding1, Binding2),
  select_result_order(Binding2, VarNames, Binding3),
  maplist(sparql_result_json_term, Binding3, Result).

% { "type": "uri", "value": "I" }
% { "type": "literal","value": "S" }
% { "type": "literal", "value": "S", "xml:lang": "L" }
% { "type": "literal", "value": "S", "datatype": "D" }
% { "type": "bnode", "value": "B" }
sparql_result_json_term(Dict, Term) :-
  (   _{type: uri, value: Term} :< Dict
  ->  true
  ;   _{type: literal, value: Lex} :< Dict
  ->  rdf_typed_literal(xsd:string, Lex, Term)
  ;   _{type: literal, value: Lex, 'xml:lang': LTag} :< Dict
  ->  rdf_language_tagged_string(LTag, Lex, Term)
  ;   _{datatype: D, type: literal, value: Lex} :< Dict
  ->  rdf_typed_literal(D, Lex, Term)
  ;   _{type: bnode, value: Label} :< Dict
  ->   atom_concat('_:', Label, Term)
  ).





% SPARQL QUERY RESULTS XML FORMAT (SECOND EDITION) %

%! sparql_result_xml(+Form:oneof([ask,select]), +In:stream,
%!                   -Result:compound) is nondet.

sparql_result_xml(ask, In, ask(Result)) :- !,
  NS = 'http://www.w3.org/2005/sparql-results#',
  load_structure(In, Dom1, [dialect(xmlns),space(remove)]),
  Dom1 = [
    element(NS:sparql,_,[
      element(NS:head,_,[]),
      element(NS:boolean,_,[Result])
    ])
  ].
sparql_result_xml(select, In, Result) :-
  setup_call_cleanup(
    new_sgml_parser(Parser, []),
    (
      maplist(set_sgml_parser(Parser), [dialect(xmlns),space(remove)]),
      sparql_result_xml_(Parser, In, Result)
    ),
    free_sgml_parser(Parser)
  ).

sparql_result_xml_(Parser, In, select(VarNames,Terms)) :-
  skip_until_tag(In, sparql),
  sgml_parse(Parser, [document(Dom1),parse(element),source(In)]),
  Dom1 = [element(head,_,Dom2)],
  maplist(xml_variable_name, Dom2, VarNames),
  skip_until_tag(In, results),
  repeat,
  sgml_parse(Parser, [document(Dom3),parse(element),source(In)]),
  (   Dom3 = [element(result,_,Dom4)]
  ->  maplist(xml_binding, Dom4, Pairs),
      select_result_order(Pairs, VarNames, Terms)
  ;   !, fail
  ).

xml_variable_name(element(variable,[name=VarName],[]), VarName).

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
xml_binding(element(binding,[name=VarName],[Dom]), VarName-Term) :-
  (   Dom = element(uri,_,[Term])
  ->  true
  ;   Dom = element(literal,[datatype=D],[Lex])
  ->  rdf_typed_literal(D, Lex, Term)
  ;   Dom = element(literal,['xml:lang'=LTag],[Lex])
  ->  rdf_language_tagged_string(LTag, Lex, Term)
  ;   Dom = element(literal,[],[Lex])
  ->  rdf_typed_literal(xsd:string, Lex, Term)
  ;   Dom = element(bnode,[],[Label])
  ->  atom_concat('_:', Label, Term)
  ).





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



%! select_result_order(+Pairs:list(pair(atom,dict)), +Keys:list(atom),
%!                     -Values:list(dict)) is det.

select_result_order([], [], []) :- !.
select_result_order(Pairs1, [Key|Keys], [Value|Values]) :-
  selectchk(Key-Value, Pairs1, Pairs2),
  select_result_order(Pairs2, Keys, Values).



%! skip_until_tag(+In:stream, -ElementName:atom) is semidet.

skip_until_tag(In, ElementName) :-
  get_until_code(In, 0'<, _),
  get_until_code(In, 0'>, Codes),
  atom_codes(Atom, Codes),
  (   atomic_list_concat([ElementName|_], ' ', Atom)
  ->  true
  ;   skip_until_tag(In, ElementName)
  ).
