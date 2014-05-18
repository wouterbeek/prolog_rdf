:- module(
  rdf_name,
  [
    rdf_graph_name//1, % +RdfGraph:atom
    rdf_term_name//1, % ?RdfTerm
    rdf_term_name//2, % +Options:list(nvpair)
                      % +RdfTerm
    rdf_triple_name//3, % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +Object:or([bnode,iri,literal])
    rdf_triple_name//4 % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +Object:or([bnode,iri,literal])
                       % +Graph:atom
  ]
).

/** <module> RDF name

Generates names for RDF terms and triples.

@author Wouter Beek
@version 2013/07-2013/09, 2014/01-2014/04
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_collection)).
:- use_module(generics(codes_ext)).
:- use_module(generics(typecheck)).
:- use_module(pl(pl_log)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- rdf_meta(rdf_term_name(r,?,?)).
:- rdf_meta(rdf_triple_name(r,r,r,?,?)).
:- rdf_meta(rdf_triple_name(r,r,r,+,?,?)).



% GRAPH %

%! rdf_graph_name(+RdfGraph:atom)// is det.

rdf_graph_name(G) --> {var(G)}, !, [].
rdf_graph_name(G) --> atom(G).



% TERM %

%! rdf_term_name(+RdfTerm:oneof([bnode,iri,literal]))// is det.
%! rdf_term_name(
%!   +Options:list(nvpair),
%!   +RdfTerm:oneof([bnode,iri,literal])
%!)// is det.
% Returns a display name for the given RDF term.
%
% The following options are supported:
%   * =|graph(+Graph:atom)|=
%     `TERM in GRAPH`
%   * =|language(+Language:atom)|=
%     The atomic language tag of the language that is preferred for
%     use in the RDF term's name.
%     The default value is `en`.
%   * =|literal_ellipsis(+MaximumLength:or([oneof([inf]),positive_integer]))|=
%     Ellipse long literal values.
%   * =|uri_desc(+DescriptionMode:oneof([
%       only_literals,
%       only_preferred_label,
%       uri_only,
%       with_literals,
%       with_preferred_label
%     ]))|=
%     Whether or not literals are included in the name of the RDF term.
%     The default value is `uri_only`.
%
% @arg Options A list of name-value pairs.
% @arg RdfTerm An RDF term.

rdf_term_name(RdfTerm) -->
  rdf_term_name([], RdfTerm).

rdf_term_name(O1, RdfTerm) -->
  {select_option(graph(Graph), O1, O2)}, !,
  rdf_term_name(O2, RdfTerm),
  ` in `,
  rdf_graph_name(Graph).
% RDF list.
% @tbd Fix this.
%rdf_term_name(O1, RDF_List) -->
%  {rdf_is_list(RDF_List)}, !,
%  rdf_list_name(O1, RDF_List).
% Blank node.
rdf_term_name(_, BNode) -->
  {rdf_is_bnode(BNode)}, !,
  rdf_bnode_name(BNode).
% Literal.
rdf_term_name(O1, Literal) -->
  {rdf_is_literal(Literal)}, !,
  rdf_literal_name(O1, Literal).
% IRI.
rdf_term_name(O1, IRI1) -->
  {(
    rdf_global_id(IRI2, IRI1), IRI2 = _:_
  ;
    is_of_type(iri, IRI1)
  )}, !,
  rdf_iri_name(O1, IRI1).
% Prolog term.
rdf_term_name(_, PL_Term) -->
  {with_output_to(codes(Codes), write_canonical_blobs(PL_Term))},
  codes(Codes).



% TRIPLE %

rdf_triple_name(S, P, O) -->
  tuple(ascii, rdf_term_name, [S,P,O]).

rdf_triple_name(S, P, O, G) -->
  tuple(ascii, rdf_term_name, [S,P,O,G]).



% BLANK NODE %

rdf_bnode_name(BNode) -->
  atom(BNode).



% LITERAL %

rdf_language_tag_name(Language) -->
  atom(Language).


% Typed literals must occur before plain literals.
rdf_literal_name(_, Literal) -->
  rdf_typed_literal_name(Literal).
rdf_literal_name(O1, Literal) -->
  rdf_plain_literal_name(O1, Literal).


% Non-simple plain literals must occur before simple literals.
rdf_plain_literal_name(O1, literal(lang(Language,Value))) --> !,
  rdf_simple_literal_name(O1, Value),
  "@",
  rdf_language_tag_name(Language).
rdf_plain_literal_name(O1, literal(Value)) -->
  rdf_simple_literal_name(O1, Value).


rdf_simple_literal_name(O1, Value) -->
  {option(literal_ellipsis(Ellipsis), O1, inf)},
  quoted(atom(Value, Ellipsis)).


rdf_typed_literal_name(literal(type(DatatypeIri,LexicalForm))) -->
  {(
    % The datatype is recognized, so we can display
    % the lexically mapped value.
    xsd_datatype(DatatypeIri)
  ->
    xsd_lexical_map(DatatypeIri, LexicalForm, Value0),
    with_output_to(atom(Value), write_canonical_blobs(Value0))
  ;
    Value = LexicalForm
  )},
  quoted(atom(Value)),
  `^^`,
  rdf_iri_name([], DatatypeIri).



% IRI %

% The options `only_preferred_label` and `with_preferred_label`.
rdf_iri_name(O1, RdfTerm) -->
  % Whether to include the RDF term itself or only its preferred RDFS label.
  (
    {option(uri_desc(with_preferred_label), O1)}
  ->
    rdf_iri_name([uri_desc(uri_only)], RdfTerm),
    nl
  ;
    {option(uri_desc(only_preferred_label), O1)}
  ), !,

  % See whether a preferred label can be found.
  {option(language(LanguageTag), O1, en)},
  (
    {rdfs_preferred_label(LanguageTag, RdfTerm, PreferredLabel, _, _)}
  ->
    atom(PreferredLabel)
  ;
    ``
  ).
% The RDF term is set to collate all literals that (directly) relate to it.
% These are options `only_literals` and `with_literals`.
rdf_iri_name(O1, RdfTerm) -->
  % The URI, if included.
  {(
    option(uri_desc(with_literals), O1)
  ->
    Elements = [RdfTerm|Literals2]
  ;
    option(uri_desc(only_literals), O1)
  ->
    Elements = Literals2
  )},

  {
    % Labels are treated specially: only the preferred label is included.
    option(language(LanguageTag), O1, en),
    rdfs_preferred_label(LanguageTag, RdfTerm, PreferredLabel, _, _),

    % All non-label literals are included.
    findall(
      Literal,
      (
        % Any directly related literal.
        rdf(RdfTerm, P, Literal),
        rdf_is_literal(Literal),
        % Exclude literals that are RDFS labels.
        \+ rdf_equal(rdfs:label, P)
      ),
      Literals1
    ),
    append(Literals1, [PreferredLabel], Literals2)
  },

  collection(``, ``, list_to_ord_set, nl, rdf_term_name, Elements).
% Only the URI is used. XML namespace prefixes are used when present.
% This appears last, since it is the default or fallback option.
% When option `uri_desc` is set to `uri_only` we end up here as well.
% Writes a given RDF term that is an IRI.
% This is the IRI ad verbatim, or a shortened version, if there is a
% registered XML namespace prefix for this IRI.
% We take the XML namespace prefix that results in the shortest output form.
% The IRI has at least one XML namespace prefix.
rdf_iri_name(_, IRI) -->
  % We take the prefix that stands for the longest IRI substring.
  {rdf_resource_to_namespace(IRI, Prefix, LocalName)}, !,
  atom(Prefix),
  `:`,
  atom(LocalName).
% An IRI without an XML namespace prefix.
rdf_iri_name(_, IRI) -->
  atom(IRI).

