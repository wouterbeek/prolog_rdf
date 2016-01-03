:- module(
  xml_to_rdf,
  [
    parse_file/4, % +File:atom
                  % +ParserVersion:oneof(['1.0','1.1'])
                  % +Namespace:atom
                  % ?Graph:rdf_graph
    create_resource/7, % +XML_DOM:list
                       % +XML_PrimaryProperties:list(atom)
                       % :XML2RDF_Translation
                       % +Class:iri
                       % +Graph:rdf_graph
                       % -Subject:or([bnode,iri])
                       % -XML_RemainingDOM:list
    create_triples/6 % +XML_DOM:list
                     % +XML_Properties:list(atom)
                     % :XML2RDF_Translation
                     % +Subject:or([bnode,iri])
                     % +Graph:rdf_graph
                     % -XML_RemainingDOM:list
  ]
).

/** <module> XML to RDF conversion

Converts XML DOMs to RDF graphs.

@author Wouter Beek
@version 2013/06, 2013/09-2013/11, 2014/01, 2014/03, 2014/10-2014/11, 2015/02,
         2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(pure_input)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(uri)).

:- meta_predicate(create_resource(+,+,3,+,+,-,-)).
:- meta_predicate(create_triples(+,+,3,+,+,-)).
:- meta_predicate(get_dom_value(+,3,+,-)).

:- rdf_meta(parse_file(+,+,+,r)).





%! ensure_graph_name(+File:atom, ?Graph:rdf_graph) is det.

ensure_graph_name(_, G) :-
  nonvar(G), !.
ensure_graph_name(File, G) :-
  uri_file_name(File, G).


parse_file(File1, Version, Prefix, G) :-
  ensure_graph_name(File1, G),
  phrase_from_file(xml_parse(Version, Prefix, G), File1),
  debug(xml_to_rdf, 'Done parsing file ~w', [File1]), %DEB
  file_kind_alternative(File1, turtle, File2),
  prolog_stack_property(global, limit(Limit)),
  debug(xml_to_rdf, 'About to save triples to file with ~:d global stack.',
      [Limit]),
  rdf_save(File2, [format(turtle),graph(G)]),
  rdf_unload_graph_deb(G).



%! xml_parse(
%!   +Version:oneof(['1.0','1.1']),
%!   +Prefix:atom,
%!   +Graph:rdf_graph
%! )// is det.
% Parses the root tag.

xml_parse(ParserVersion, Prefix, G) -->
  {xml_version_map(ParserVersion, DocumentVersion)},
  'XMLDecl'(ParserVersion, xml_decl(DocumentVersion,_,_)),
  'STag'(ParserVersion, RootName, _),
  *(ascii_white, []),
  {
    rdf_global_id(Prefix:RootName, C),
    rdf_create_iri(Prefix, [RootName], I),
    rdf_assert_instance(I, C, G)
  },
  xml_parses(ParserVersion, Prefix, Resource, G),
  'ETag'(RootName),
  dcg_done.



%! xml_parse(
%!   +Version:oneof(['1.0','1.1']),
%!   +Prefix:atom,
%!   +Resource:iri,
%!   +Graph:rdf_graph
%! )// is det.

% Non-tag content.
xml_parse(Version, Prefix, S, G) -->
  'STag'(Version, PTag, _), !,
  *(ascii_white, []),
  xml_content(Version, PTag, Cs),
  {
    string_codes(O, Cs),
    rdf_global_id(Prefix:PTag, P),
    rdf_assert(S, P, O, G)
  }.
% Skip short tags.
xml_parse(Version, _, _, _) -->
  'EmptyElemTag'(Version, _, _), !,
  *(ascii_white).
% Nested tag.
xml_parse(Version, Prefix, S, G) -->
  'STag'(Version, OTag, _), !,
  *(ascii_white),
  {
    rdf_global_id(Prefix:OTag, Class),
    rdf_create_next_resource(Prefix, [OTag], Class, G, O)
  },
  xml_parses(Version, Prefix, O, G),
  'ETag'(OTag),
  *(ascii_white, []),
  {
    rdf_assert_instance(S, rdf:'Bag', G),
    rdf_assert_collection_member(S, O, G)
  }.


xml_parses(Version, Prefix, S, G) -->
  xml_parse(Version, Prefix, S, G), !,
  xml_parses(Version, Prefix, S, G).
xml_parses(_, _, _, _) --> [], !.
xml_parses(_, _, _, _) -->
  dcg_all([output_format(atom)], Remains),
  {format(user_output, '~w', [Remains])}.


% The tag closes: end of content codes.
xml_content(_, Tag, []) -->
  'ETag'(Tag), !,
  *(ascii_white, []).
% Another XML tag starts, this is not XML content.
xml_content(Version, _, []) -->
  dcg_peek('STag'(Version, _, _)), !, {fail}.
% Parse a code of content.
xml_content(Version, Tag, [H|T]) -->
  [H],
  xml_content(Version, Tag, T).



% OLD APPROACH %

%! create_resource(
%!   +XML_DOM:list,
%!   +XML_PrimaryProperties:list(atom),
%!   :XML2RDF_Translation,
%!   +Class:iri,
%!   +Graph:atom,
%!   -Subject:or([bnode,iri]),
%!   -XML_RemainingDOM:list
%! ) is det.

create_resource(DOM1, XML_PrimaryPs, Trans, C, G, S, DOM2) :-
  rdf_global_id(Ns:Name1, C),
  findall(
    Value,
    (
      member(XML_PrimaryP, XML_PrimaryPs),
      get_dom_value(DOM1, Trans, XML_PrimaryP, Value)
    ),
    Values
  ),
  atomic_list_concat(Values, '_', Name2),
  atomic_list_concat([Name1,Name2], /, Name3),

  % Escape space (SPACE to `%20`) and grave accent (GRAVE-ACCENT -> `%60`).
  atom_phrase(
    *(dcg_replace, [[32],[96]], [[37,50,48],[37,54,48]], []),
    Name3,
    Name4
  ),

  rdf_global_id(Ns:Name4, S),

  rdf_assert_instance(S, C, G),

  create_triples(DOM1, XML_PrimaryPs, Trans, S, G, DOM2).



%! create_triple(
%!   +RDF_Subject:or([bnode,iri]),
%!   +RDF_Predicate:iri,
%!   +ObjectType:atom,
%!   +XML_Content,
%!   +RDF_Graph:atom
%! ) is det.

% Simple literal.
create_triple(S, P, literal, Content, G) :- !,
  rdf_assert_simple_literal(S, P, Content, G).
% Typed literal.
create_triple(S, P, D1, Content, G) :-
  xsd_datatype(D1, D2), !,
  rdf_assert_typed_literal(S, P, D2, Content, G).
% IRI.
create_triple(S, P, _, Content, G) :-
  % Spaces are not allowed in IRIs.
  rdf_assert(S, P, Content, G).



%! create_triples(
%!   +XML_DOM:list,
%!   +XML_Properties:list(atom),
%!   :XML2RDF_Translation,
%!   +RDF_Subject:or([bnode,iri]),
%!   +RDF_Graph:atom,
%!   -XML_RemainingDOM:list
%! ) is nondet.

% The XML DOM is fully processed.
create_triples([], _Ps, _Trans, _S, _G, []) :- !.
% The XML properties are all processed.
create_triples(DOM, [], _Trans, _S, _G, DOM) :- !.
% Process an XML element.
create_triples(DOM1, Ps1, Trans, S, G, RestDOM) :-
  % Process only properties that are allowed according to the filter.
  select(element(XML_P, _, Content1), DOM1, DOM2),
  update_property_filter(Ps1, XML_P, Ps2), !,

  (   % XML element with no content.
      Content1 == [], !
  ;   % XML element with content.
      Content1 = [Content2],
      call(Trans, XML_P, RDF_P, RDF_O_Type),
      create_triple(S, RDF_P, RDF_O_Type, Content2, G)
  ),

  create_triples(DOM2, Ps2, Trans, S, G, RestDOM).
% Neither the DOM nor the propery filter is empty.
% This means that some properties in the filter are optional.
create_triples(DOM, _Ps, _Trans, _S, _G, DOM).


%! get_dom_value(
%!   +DOM:list,
%!   :XML2RDF_Translation,
%!   +XML_Property:atom,
%!   -Value
%! ) is det.

get_dom_value(DOM, Trans, XML_P, Value) :-
  memberchk(element(XML_P, _, [LexicalForm]), DOM),
  call(Trans, XML_P, _, O_Type),
  (   O_Type == literal
  ->  Value = LexicalForm
  ;   xsd_datatype(O_Type, XSD_Datatype)
  ->  xsd_canonical_map(XSD_Datatype, LexicalForm, Value)
  ;   Value = LexicalForm
  ).

update_property_filter(Ps1, _, _) :-
  var(Ps1), !.
update_property_filter(Ps1, XML_P, Ps2) :-
  selectchk(XML_P, Ps1, Ps2).





% HELPERS

%! xml_version_map(
%!   ?ParserVersion:oneof(['1.0','1.1']),
%!   ?DocumentVersion:compound
%! ) .

xml_version_map('1.0', version(1,0)).
xml_version_map('1.1', version(1,1)).

