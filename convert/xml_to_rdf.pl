:- module(
  xml_to_rdf,
  [
    parse_file/3, % +File:atom
                  % +Namespace:atom
                  % ?RdfGraph:atom
    create_resource/7, % +XML_DOM:list
                       % +XML_PrimaryProperties:list(atom)
                       % :XML2RDF_Translation
                       % +Class:iri
                       % +Graph:atom
                       % -Subject:or([bnode,iri])
                       % -XML_RemainingDOM:list
    create_triples/6 % +XML_DOM:list,
                     % +XML_Properties:list(atom),
                     % :XML2RDF_Translation,
                     % +RDF_Subject:or([bnode,iri]),
                     % +RDF_Graph:atom,
                     % -XML_RemainingDOM:list
  ]
).

/** <module> XML to RDF conversion

Converts XML DOMs to RDF graphs.

@author Wouter Beek
@version 2013/06, 2013/09-2013/11, 2014/01, 2014/03, 2014/10
*/

:- use_module(library(debug)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(pure_input)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uri)).

:- use_module(os(file_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_meta)).
:- use_module(plDcg(dcg_peek)).
:- use_module(plDcg(dcg_replace)).

:- use_module(plXsd(xsd)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(debug/rdf_deb)).
:- use_module(plRdf(graph/rdf_graph_name)).
:- use_module(plRdf(term/rdf_container)).
:- use_module(plRdf(term/rdf_datatype)).

:- meta_predicate(create_resource(+,+,3,+,+,-,-)).
:- meta_predicate(create_triples(+,+,3,+,+,-)).
:- meta_predicate(get_dom_value(+,3,+,-)).

:- rdf_meta(create_resource(+,+,:,r,+,-,-)).
:- rdf_meta(create_triples(+,+,:,r,+,-)).



%! ensure_graph_name(+File:atom, ?Graph:atom) is det.

ensure_graph_name(_, Graph):-
  nonvar(Graph), !.
ensure_graph_name(File, Graph):-
  uri_file_name(File, Graph).


parse_file(File1, Prefix, G):-
  ensure_graph_name(File1, G),
  phrase_from_file(xml_parse(Prefix, G), File1),
  %%%%file_to_atom(File, Atom), %DEB
  %%%%dcg_phrase(xml_parse(el), Atom), %DEB
  debug(xml_to_rdf, 'Done parsing file ~w', [File1]), %DEB
  file_kind_alternative(File1, turtle, File2),
  prolog_stack_property(global, limit(Limit)),
  debug(xml_to_rdf, 'About to save triples to file with ~:d global stack.',
      [Limit]),
  rdf_save(File2, [format(turtle),graph(G)]),
  rdf_unload_graph_deb(G).


%! xml_parse(+Prefix:atom, +RdfGraph:atom)// is det.
% Parses the root tag.

xml_parse(Prefix, G) -->
  xml_declaration(_),
  'STag'(RootTag), skip_whites,
  {
    rdf_global_id(Prefix:RootTag, Class),
    rdf_create_next_resource(Prefix, [RootTag], Class, G, S)
  },
  xml_parses(Prefix, S, G),
  'ETag'(RootTag), dcg_done.


%! xml_parse(+RdfContainer:iri, +RdfGraph:atom)// is det.

% Non-tag content.
xml_parse(Prefix, S, G) -->
  'STag'(PTag), skip_whites,
  xml_content(PTag, Codes), !,
  {
    atom_codes(O, Codes),
    rdf_global_id(Prefix:PTag, P),
    rdf_assert_string(S, P, O, G)
  }.
% Skip short tags.
xml_parse(_, _, _) -->
  'EmptyElemTag'(_), !,
  skip_whites, !.
% Nested tag.
xml_parse(Prefix, S, G) -->
  'STag'(OTag), !,
  skip_whites, !,
  {
    rdf_global_id(Prefix:OTag, Class),
    rdf_create_next_resource(Prefix, [OTag], Class, G, O)
  },
  xml_parses(Prefix, O, G),
  'ETag'(OTag), skip_whites,
  {
    rdf_assert_instance(S, rdf:'Bag', G),
    rdf_assert_collection_member(S, O, G)
  }.


skip_whites -->
  '*'(white, []), !.


xml_parses(Prefix, S, G) -->
  xml_parse(Prefix, S, G), !,
  xml_parses(Prefix, S, G).
xml_parses(_, _, _) --> [], !.
xml_parses(_, _, _) -->
  dcg_all([output_format(atom)], Remains),
  {format(user_output, '~w', [Remains])}.


% The tag closes: end of content codes.
xml_content(Tag, []) -->
  'ETag'(Tag), skip_whites, !.
% Another XML tag starts, this is not XML content.
xml_content(_, []) -->
  dcg_peek('STag'(_)), !, {fail}.
% Parse a code of content.
xml_content(Tag, [H|T]) -->
  [H],
  xml_content(Tag, T).



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

create_resource(DOM1, XML_PrimaryPs, Trans, C, G, S, DOM2):-
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
  atomic_list_concat([Name1,Name2], '/', Name3),

  % Escape space (SPACE to `%20`) and grave accent (GRAVE-ACCENT -> `%60`).
  dcg_phrase(
    dcg_maplist(dcg_replace, [[32],[96]], [[37,50,48],[37,54,48]]),
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
create_triple(S, P, literal, Content, G):- !,
  rdf_assert_string(S, P, Content, G).
% Typed literal.
create_triple(S, P, D1, Content, G):-
  xsd_datatype(D1, D2), !,
  rdf_assert_datatype(S, P, D2, Content, G).
% IRI.
create_triple(S, P, _, Content, G):-
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
create_triples([], _Ps, _Trans, _S, _G, []):- !.
% The XML properties are all processed.
create_triples(DOM, [], _Trans, _S, _G, DOM):- !.
% Process an XML element.
create_triples(DOM1, Ps1, Trans, S, G, RestDOM):-
  % Process only properties that are allowed according to the filter.
  select(element(XML_P, _, Content1), DOM1, DOM2),
  update_property_filter(Ps1, XML_P, Ps2), !,

  (
    % XML element with no content.
    Content1 == [], !
  ;
    % XML element with content.
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

get_dom_value(DOM, Trans, XML_P, Value):-
  memberchk(element(XML_P, _, [LexicalForm]), DOM),
  call(Trans, XML_P, _, O_Type),
  (
    O_Type == literal
  ->
    Value = LexicalForm
  ;
    xsd_datatype(O_Type, XSD_Datatype)
  ->
    xsd_canonical_map(XSD_Datatype, LexicalForm, Value)
  ;
    Value = LexicalForm
  ).

update_property_filter(Ps1, _, _):-
  var(Ps1), !.
update_property_filter(Ps1, XML_P, Ps2):-
  selectchk(XML_P, Ps1, Ps2).



/*
In the past, I first used to check whether a resource already existed.
This meant that XML2RDF conversion would slow down over time
(increasingly longer query times for resource existence in the growing
RDF graph).

Nevertheless, some of these methods are quite nice.
They translate XML elements to RDF properties and use the object type
for converting XML content to RDF objects.

:- meta_predicate(rdf_property_trans(+,3,+,+,+)).
:- meta_predicate(rdf_subject_trans(+,+,3,+,+)).

:- rdf_meta(rdf_property_trans(+,:,r,+,+)).
:- rdf_meta(rdf_subject_trans(+,+,:,r,+)).

% The resource already exists.
create_resource(DOM, PrimaryPs, Trans, _C, G, S, DOM):-
  rdf_subject_trans(DOM, PrimaryPs, Trans, S, G), !,
  debug(
    xml_to_rdf,
    'The resource for the following XML DOM already exists: ~w',
    [DOM]
  ).
% The resource does not yet exist and is created.

%! rdf_object_trans(
%!   ?RDF_Subject:or([bnode,iri]),
%!   +RDF_Predicate:iri,
%!   +RDF_ObjectType:atom,
%!   +XML_Content:atom,
%!   +RDF_Graph:atom
%! ) is nondet.

% Succeeds if there is a subject resource with the given predicate term
% and an object term that is the literal value of the given XML content.
rdf_object_trans(S, P, literal, Content, G):- !,
  rdf_literal(S, P, Content, xsd:string, _, G).
% Succeeds if there is a subject resource with the given predicate term
% and an object term that translates to the same value as
% the given XML content.
rdf_object_trans(S, P, DName, Content, G):-
  rdf_datatype(DName), !,
  rdf_literal(S, P, Content, DName, _, G).

%! rdf_property_trans(
%!   +XML_DOM:list,
%!   :XML2RDF_Translation,
%!   +RDF_Subject:or([bnode,iri]),
%!   +XML_Property:atom,
%!   +RDF_Graph:atom
%! ) is nondet.
% Reads a predicate from the DOM and checks whether
% it is already present for some subject resource
% under the given translation.

rdf_property_trans(DOM, Trans, S, XML_P, G):-
  memberchk(element(XML_P, _, [Content]), DOM),
  call(Trans, XML_P, RDF_P, O_Type),
  rdf_object_trans(S, RDF_P, O_Type, Content, G).

%! rdf_subject_trans(
%!   +XML_DOM:list,
%!   +XML_Properties:list(atom),
%!   :XML2RDF_Translation,
%!   ?RDF_Subject:or([bnode,iri]),
%!   +RDF_Graph:atom
%! ) is nondet.
% Returns a resource that has the given XML properties set to
% the object values that occur in the XML DOM.

rdf_subject_trans(DOM, [P|Ps], Trans, S, G):-
  rdf_property_trans(DOM, Trans, S, P, G),
  forall(
    member(P_, Ps),
    rdf_property_trans(DOM, Trans, S, P_, G)
  ).
*/

