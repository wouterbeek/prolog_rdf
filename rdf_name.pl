:- module(
  rdf_name,
  [
    rdf_dataset_name//1, % +RdfDataset:atom
    rdf_graph_name//1, % +RdfGraph:atom
    rdf_term_name//1, % ?Term:rdf_term
    rdf_term_name//2, % +Options:list(nvpair)
                      % +Term:rdf_term
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
@version 2013/07-2013/09, 2014/01-2014/04, 2014/07, 2014/10-2014/11
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

:- rdf_meta(rdf_term_name(+,r,?,?)).
:- rdf_meta(rdf_term_name(r,?,?)).
:- rdf_meta(rdf_triple_name(t,?,?)).
:- rdf_meta(rdf_triple_name(t,+,?,?)).

:- predicate_options(rdf_iri_name//2, 1, [
  iri_description(+oneof([
    iri_only,
    only_all_literals,
    only_preferred_label,
    with_all_literals,
    with_preferred_label
  ])),
  language_preferences(+list(list(atom)))
]).
:- predicate_options(rdf_literal_name//2, 1, [
  pass_to(rdf_plain_literal_name//2, 1)
]).
:- predicate_options(rdf_plain_literal_name//2, 1, [
  pass_to(rdf_simple_literal_name//2, 1)
]).
:- predicate_options(rdf_simple_literal_name//2, 1, [
  literal_ellipsis(+nonneg)
]).
:- predicate_options(rdf_term_name//2, 1, [
  collate_rdf_lists(+boolean),
  graph(+atom),
  pass_to(rdf_iri_name//2, 1),
  pass_to(rdf_literal_name//2, 1)
]).





%! rdf_bnode_name(+BNode:bnode)// is det.

rdf_bnode_name(BNode) -->
  atom(BNode).



%! rdf_dataset_name(+Dataset:atom)// is det.

rdf_dataset_name(Dataset) -->
  atom(Dataset).



%! rdf_graph_name(+Graph:atom)// is det.

rdf_graph_name(Graph) -->
  {var(Graph)}, !, [].
rdf_graph_name(Graph) -->
  atom(Graph).



%! rdf_iri_name(
%!   +Options:list(nvpair),
%!   +Term:rdf_term
%! )// is det.
% The following options are supported:
%   - `iri_description(+oneof([
%         iri_only,
%         only_all_literals,
%         only_preferred_label,
%         with_all_literals,
%         with_preferred_label
%     ]))`
%   - `language_preferences(+list(atom))`

% The options `only_preferred_label` and `with_preferred_label`.
rdf_iri_name(Options1, Iri) -->
  % Whether to include the RDF term itself or only its preferred RDFS label.
  (   {option(iri_description(with_preferred_label), Options1)}
  ->  rdf_iri_name([iri_description(iri_only)], Iri),
      nl
  ;  {option(iri_description(only_preferred_label), Options1)}
  ), !,

  % See whether a preferred label can be found.
  ({    option(prferred_languages(LanguageTags), Options1, [[en,'US']]),
        rdfs_label_value(Iri, PreferredLabel, LanguageTags, _)
  } ->  atom(PreferredLabel)
  ;     ""
  ).
% The IRI is set to collate all literals that (directly) relate to it.
% These are the options `only_all_literals` and `with_all_literals`.
rdf_iri_name(Options1, Iri) -->
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
rdf_iri_name(_, Iri) -->
  % We take the prefix that stands for the longest IRI substring.
  {rdf_longest_prefix(Iri, LongestPrefix, ShortestLocalName)}, !,
  atom(LongestPrefix),
  ":",
  atom(ShortestLocalName).
% An IRI without an RDF prefix.
rdf_iri_name(_, Iri) -->
  atom(Iri).



%! rdf_language_tag_name(+LanguageTag:atom)// is det.

rdf_language_tag_name(LanguageTag) -->
  atom(LanguageTag).



%! rdf_literal_name(+Options:list(nvpair), +Literal:compound)// is det.

% Typed literals must be processed before plain literals.
rdf_literal_name(_, Literal) -->
  rdf_typed_literal_name(Literal).
rdf_literal_name(Options1, Literal) -->
  rdf_plain_literal_name(Options1, Literal).



%! rdf_plain_literal_name(
%!   +Options:list(nvpair),
%!   +PlainLiteral:compound
%! )// is det.

% Non-simple plain literals must occur before simple literals.
rdf_plain_literal_name(Options1, literal(lang(LanguageTag,Value))) --> !,
  rdf_simple_literal_name(Options1, Value),
  "@",
  rdf_language_tag_name(LanguageTag).
rdf_plain_literal_name(Options1, literal(Value)) -->
  rdf_simple_literal_name(Options1, Value).



%! rdf_simple_literal_name(+Options:list(nvpair), +Value:atom)// is det.
% The following options are supported:
%   - `literal_ellipsis(+or([oneof([inf]),positive_integer]))`
%     The maximum length of a literal before ellipsis s used.

rdf_simple_literal_name(Options1, Value) -->
  {option(literal_ellipsis(Ellipsis), Options1, inf)},
  quoted(atom_ellipsis(Value, Ellipsis)).



%! rdf_term_name(+Term:oneof([bnode,iri,literal]))// is det.

rdf_term_name(Term) -->
  rdf_term_name([], Term).

%! rdf_term_name(
%!   +Options:list(nvpair),
%!   +Term:oneof([bnode,iri,literal])
%!)// is det.
% Returns a display name for the given RDF term.
%
% The following options are supported:
%   - `collate_rdf_lists(+boolean)`
%     Whether or not the name of an RDF list should consist of the names of
%     its elements.
%     Default: `true`.
%   - `graph(+Graph:atom)`
%     `TERM in GRAPH`
%   - `language(+Language:atom)`
%     The atomic language tag of the language that is preferred for
%     use in the RDF term's name.
%     The default value is `en`.
%   - `literal_ellipsis(+or([oneof([inf]),positive_integer]))`
%     The maximum length of a literal before ellipsis s used.
%   - `iri_description(+DescriptionMode:oneof([
%       only_all_literals,
%       only_preferred_label,
%       iri_only,
%       with_all_literals,
%       with_preferred_label
%     ]))`
%     Whether or not literals are included in the name of the RDF term.
%     The default value is `iri_only`.

rdf_term_name(Options1, Term) -->
  {select_option(graph(Graph), Options1, Options2)}, !,
  rdf_term_name(Options2, Term),
  " in ",
  rdf_graph_name(Graph).
% RDF list.
rdf_term_name(Options, RdfList) -->
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
  list(rdf_term_name(Options), PlList).
% Blank node.
rdf_term_name(_, BNode) -->
  {rdf_is_bnode(BNode)}, !,
  rdf_bnode_name(BNode).
% Literal.
rdf_term_name(Options1, Literal) -->
  {rdf_is_literal(Literal)}, !,
  rdf_literal_name(Options1, Literal).
% IRI.
rdf_term_name(Options1, Iri) -->
  {is_uri(Iri)}, !,
  rdf_iri_name(Options1, Iri).
% Prolog term.
rdf_term_name(_, PlTerm) -->
  {with_output_to(codes(Codes), write_canonical_blobs(PlTerm))},
  '*'(code, Codes, []).



%! rdf_triple_name(+Triple:compound)// is det.

rdf_triple_name(rdf(S,P,O)) -->
  tuple(ascii, rdf_term_name, [S,P,O]).

%! rdf_triple_name(+Triple:compound, +Graph:atom)// is det.

rdf_triple_name(rdf(S,P,O), Graph) -->
  tuple(ascii, rdf_term_name, [S,P,O,graph(Graph)]).



%! rdf_typed_literal_name(+TypedLiteral:compound)// is det.

rdf_typed_literal_name(literal(type(Datatype,LexicalForm))) -->
  {(  % The datatype is recognized, so we can display
      % the lexically mapped value.
      rdf_datatype(Datatype)
  ->  rdf_lexical_map(Datatype, LexicalForm, Value0),
      with_output_to(atom(Value), write_canonical_blobs(Value0))
  ;   Value = LexicalForm
  )},
  quoted(double_quote, atom(Value)),
  "^^",
  rdf_iri_name([], Datatype).

