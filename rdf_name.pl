:- module(
  rdf_name,
  [
    rdf_graph_name//1, % +Graph:atom
    rdf_quadruple_name//1, % +Quadruple:compound
    rdf_quadruple_name//2, % +Quadruple:compound
                           % +Options:list(nvpair)
    rdf_term_name//1, % ?Term:rdf_term
    rdf_term_name//2, % +Term:rdf_term
                      % +Options:list(nvpair)
    rdf_triple_name//1, % +Triple:compound
    rdf_triple_name//2 % +Triple:compound
                       % +Graph:atom
  ]
).

/** <module> RDF name

Generates names for RDF terms and triples.

@author Wouter Beek
@tbd Update to RDF 1,1,
@tbd Add support for RDF list printing.
@version 2013/07-2013/09, 2014/01-2014/04, 2014/07, 2014/10-2014/11, 2015/03
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_code)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_collection)).
:- use_module(plc(dcg/dcg_quote)).
:- use_module(plc(generics/typecheck)).
:- use_module(plc(prolog/pl_log)).

:- use_module(plXsd(xsd)).

:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(management/rdf_prefix)).
:- use_module(plRdf(term/rdf_datatype)).
:- use_module(plRdf(term/rdf_list)).
:- use_module(plRdf(term/rdf_literal)).

:- rdf_meta(rdf_quadruple_name(t,?,?)).
:- rdf_meta(rdf_quadruple_name(t,+,?,?)).
:- rdf_meta(rdf_term_name(+,r,?,?)).
:- rdf_meta(rdf_term_name(r,?,?)).
:- rdf_meta(rdf_triple_name(t,?,?)).
:- rdf_meta(rdf_triple_name(t,+,?,?)).

:- predicate_options(rdf_iri_name//2, 2, [
  iri_description(+oneof([
    iri_only,
    only_all_literals,
    only_preferred_label,
    with_all_literals,
    with_preferred_label
  ])),
  language_preferences(+list(list(atom)))
]).
:- predicate_options(rdf_literal_name//2, 2, [
  pass_to(rdf_plain_literal_name//2, 2)
]).
:- predicate_options(rdf_plain_literal_name//2, 2, [
  pass_to(rdf_simple_literal_name//2, 2)
]).
:- predicate_options(rdf_quadruple_name/2, 2, [
  pass_to(rdf_term_name0/2, 1)
]).
:- predicate_options(rdf_simple_literal_name//2, 2, [
  literal_ellipsis(+nonneg)
]).
:- predicate_options(rdf_term_name//2, 2, [
  collate_rdf_lists(+boolean),
  graph(+atom),
  pass_to(rdf_iri_name//2, 2),
  pass_to(rdf_literal_name//2, 2),
  pass_to(rdf_term_name/2, 2),
  pass_to(rdf_term_name0/2, 1)
]).
:- predicate_options(rdf_term_name0/2, 1, [
  pass_to(rdf_term_name/2, 2)
]).
:- predicate_options(rdf_triple_name/2, 2, [
  pass_to(rdf_term_name0/2, 1)
]).





%! rdf_bnode_name(+BNode:bnode)// is det.

rdf_bnode_name(BNode) -->
  atom(BNode).



%! rdf_graph_name(+Graph:atom)// is det.

rdf_graph_name(Graph) -->
  {var(Graph)}, !, [].
rdf_graph_name(Graph) -->
  atom(Graph).



%! rdf_iri_name(
%!   +Term:rdf_term,
%!   +Options:list(nvpair)
%! )// is det.
% The following options are supported:
%   - iri_description(+oneof([
%       iri_only,
%       only_all_literals,
%       only_preferred_label,
%       with_all_literals,
%       with_preferred_label
%     ]))
%   - language_preferences(+list(list(atom)))

% The options `only_preferred_label` and `with_preferred_label`.
rdf_iri_name(Iri, Options1) -->
  % Whether to include the RDF term itself or only its preferred RDFS label.
  (   {option(iri_description(with_preferred_label), Options1)}
  ->  rdf_iri_name([iri_description(iri_only)], Iri),
      nl
  ;  {option(iri_description(only_preferred_label), Options1)}
  ), !,

  % See whether a preferred label can be found.
  ({    option(preferred_languages(LanguageTags), Options1, [[en,'US']]),
        rdfs_label_value(Iri, PreferredLabel, LanguageTags, _)
  } ->  atom(PreferredLabel)
  ;     ""
  ).
% The IRI is set to collate all literals that (directly) relate to it.
% These are the options `only_all_literals` and `with_all_literals`.
rdf_iri_name(Iri, Options1) -->
  % The URI, if included.
  {(  option(iri_description(with_all_literals), Options1)
  ->  Elements = [Iri|Literals2]
  ;   option(iri_description(only_all_literals), Options1)
  ->  Elements = Literals2
  )},

  {
    % Labels are treated specially: only the preferred label is included.
    option(language_preferences(LanguageTags), Options1, [en]),
    rdfs_label_value(Iri, PreferredLabel, LanguageTags, _),

    % All non-label literals are included.
    findall(
      Literal,
      (
        % Any directly related literal.
        rdf(Iri, P, Literal),
        rdf_is_literal(Literal),
        % Exclude literals that are RDFS labels.
        \+ rdf_equal(rdfs:label, P)
      ),
      Literals1
    ),
    append(Literals1, [PreferredLabel], Literals2)
  },

  collection(``, ``, list_to_ord_set, nl, rdf_term_name, Elements).
% Only the IRI is used. XML namespace prefixes are used when present.
% This appears last, since it is the default or fallback option.
% When option `iri_description` is set to `iri_only` we end up here as well.
% Writes a given RDF term that is an IRI.
% This is the IRI ad verbatim, or a shortened version, if there is a
% registered XML namespace prefix for this IRI.
% We take the XML namespace prefix that results in the shortest output form.
% The IRI has at least one XML namespace prefix.
rdf_iri_name(Iri, _) -->
  % We take the prefix that stands for the longest IRI substring.
  {rdf_longest_prefix(Iri, LongestPrefix, ShortestLocalName)}, !,
  atom(LongestPrefix),
  ":",
  atom(ShortestLocalName).
% An IRI without an RDF prefix.
rdf_iri_name(Iri, _) -->
  atom(Iri).



%! rdf_language_tag_name(+LanguageTag:atom)// is det.

rdf_language_tag_name(LangTag) -->
  atom(LangTag).



%! rdf_literal_name(+Literal:compound, +Options:list(nvpair))// is det.

% Typed literals must be processed before plain literals.
rdf_literal_name(Literal, Options) -->
  (   {rdf_is_typed_literal(Literal)}
  ->  rdf_typed_literal_name(Literal)
  ;   rdf_plain_literal_name(Literal, Options)
  ).



%! rdf_plain_literal_name(
%!   +PlainLiteral:compound,
%!   +Options:list(nvpair)
%! )// is det.

% Non-simple plain literals must occur before simple literals.
rdf_plain_literal_name(literal(lang(LangTag,Value)), Options) --> !,
  rdf_simple_literal_name(Value, Options),
  "@",
  rdf_language_tag_name(LangTag).
rdf_plain_literal_name(literal(Value), Options) -->
  rdf_simple_literal_name(Value, Options).



%! rdf_quadruple_name(+Quadruple:compound)// is det.

rdf_quadruple_name(rdf(S,P,O,G)) -->
  tuple(ascii, rdf_term_name, [S,P,O,graph(G)]).

%! rdf_quadruple_name(+Quadruple:compound, +Options:list(nvpair))// is det.

rdf_quadruple_name(rdf(S,P,O,G), Options) -->
  tuple(ascii, rdf_term_name0(Options), [S,P,O,graph(G)]).



%! rdf_simple_literal_name(+Value:atom, +Options:list(nvpair))// is det.
% The following options are supported:
%   - literal_ellipsis(+or([oneof([inf]),positive_integer]))
%     The maximum length of a literal before ellipsis s used.

rdf_simple_literal_name(Value, Options) -->
  {option(literal_ellipsis(Ellipsis), Options, inf)},
  quoted(atom_ellipsis(Value, Ellipsis)).



%! rdf_term_name(+Term:oneof([bnode,iri,literal]))// is det.

rdf_term_name(Term) -->
  rdf_term_name(Term, []).

%! rdf_term_name(
%!   +Term:oneof([bnode,iri,literal]),
%!   +Options:list(nvpair)
%!)// is det.
% Returns a display name for the given RDF term.
%
% The following options are supported:
%   - collate_rdf_lists(+boolean)
%     Whether or not the name of an RDF list should consist of the names of
%     its elements.
%     Default: `true`.
%   - graph(+Graph:atom)
%     `TERM in GRAPH`
%   - language_preferences(+list(list(atom)))
%     The atomic language tag of the language that is preferred for
%     use in the RDF term's name.
%     The default value is `en`.
%   - literal_ellipsis(+or([oneof([inf]),positive_integer]))
%     The maximum length of a literal before ellipsis s used.
%   - iri_description(+DescriptionMode:oneof([
%       only_all_literals,
%       only_preferred_label,
%       iri_only,
%       with_all_literals,
%       with_preferred_label
%     ]))
%     Whether or not literals are included in the name of the RDF term.
%     The default value is `iri_only`.

% Graph.
rdf_term_name(graph(Graph), _) -->
  rdf_graph_name(Graph).
% In graph.
rdf_term_name(Term, Options1) -->
  {select_option(graph(Graph), Options1, Options2)}, !,
  rdf_term_name(Term, Options2),
  " in ",
  rdf_graph_name(Graph).
% RDF list.
rdf_term_name(RdfList, Options) -->
  {
    rdf_list(RdfList),
    \+ option(collate_rdf_lists(false), Options)
  }, !,
  % Recursively retrieve the contents of the RDF list.
  % This has to be done non-recursively, since the nested
  % Prolog list `[a,[b,c]]` would bring rdf_term_name//1 into
  % trouble when it comes accross `[b,c]`
  % (which fails the check for RDF list).
  {rdf_list(RdfList, PlList, _, [recursive(false)])},
  list(rdf_term_name0(Options), PlList).
% Blank node.
rdf_term_name(BNode, _) -->
  {rdf_is_bnode(BNode)}, !,
  rdf_bnode_name(BNode).
% Literal.
rdf_term_name(Literal, Options) -->
  {rdf_is_literal(Literal)}, !,
  rdf_literal_name(Literal, Options).
% IRI.
rdf_term_name(Iri, Options) -->
  {is_uri(Iri)}, !,
  rdf_iri_name(Iri, Options).
% Prolog term.
rdf_term_name(Term, _) -->
  {with_output_to(codes(Codes), write_canonical_blobs(Term))},
  codes(Codes).



%! rdf_triple_name(+Triple:compound)// is det.

rdf_triple_name(rdf(S,P,O)) -->
  tuple(ascii, rdf_term_name, [S,P,O]).

%! rdf_triple_name(+Triple:compound, +Options:list(nvpair))// is det.

rdf_triple_name(rdf(S,P,O), Options) -->
  tuple(ascii, rdf_term_name0(Options), [S,P,O]).



%! rdf_typed_literal_name(+TypedLiteral:compound)// is det.

rdf_typed_literal_name(literal(type(Datatype,LexicalForm))) -->
  quoted(double_quote, atom(LexicalForm)),
  "^^",
  rdf_iri_name(Datatype, []).





% HELPERS %

rdf_term_name0(Options, Term) -->
  rdf_term_name(Term, Options).
