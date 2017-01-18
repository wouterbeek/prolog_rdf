:- module(media_type2rdf, []).

/** <module> Media Type to RDF

Transforms Media Types as recorded in the IANA registry to RDF.

@author Wouter Beek
@version 2015/03, 2016/05, 2016/11
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(http/http11)).
:- use_module(library(lists)).
:- use_module(library(os/io)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(semweb/rdfs), [
     rdfs_individual_of/2
   ]).
:- use_module(library(xpath)).
:- use_module(library(yall)).

:- rdf_create_alias(mto, 'http://mediatype.org/ontology/').
:- rdf_create_alias(mtr, 'http://mediatype.org/resource/').

:- rdf_meta
   media_type(+, -, r).

:- initialization(init_media_type).





%! assert_category_class(+M, +Cat, -C, +G) is det.

assert_category_class(M, Cat, C, G) :-
  rdf_global_id(mto:Cat, C),
  qb_label(M, C, Cat, G),
  qb_subclass(M, C, mto:'MediaType', G).



%! assert_media_type(+M, +MediaType, +DefExt, +G) is det.

assert_media_type(M, MediaType, DefExt, G):-
  Cat0 = MediaType.'llo:type',
  Subtype = MediaType.'llo:subtype',
  (get_dict('llo:parameters', MediaType, Params) -> true ; Params = []),
  % The table of the Website we scrape for file extensions
  % contains a typo: `applicaiton` i.o. `application`.
  (Cat0 == applicaiton -> Cat = application ; Cat = Cat0),
  % Class
  media_type_category(Cat),
  assert_category_class(M, Cat, C, G),
  % Subtype
  atomic_list_concat([Cat,Subtype], /, Name),
  rdf_global_id(mtr:Name, Res),
  string_phrase('media-type'(MediaType), S),
  qb_instance(M, Res, C, S),
  % Template
  qb(M, Res, mto:template, S),
  % Default extension
  qb(M, Res, mto:defaultFileExtension, DefExt, G),
  % Parameters
  maplist(assert_parameter(M, Res, G), Params),

  % Non-IANA.
  html_download(
    'http://www.webmaster-toolkit.com/mime-types.shtml',
    Dom,
    [dialect(html4)]
  ),
  forall(
    (
      member(Class, [tablerowdark,tablerowlight]),
      xpath(Dom, //tr(@class=Class), Tr),
      xpath(Tr, td(1,text), [DefExt]),
      xpath(Tr, td(2,text), [MediaType0])
    ),
    (
      atom_phrase('media-type'(MediaType), MediaType0),
      ignore(assert_media_type(M, MediaType, DefExt, G))
    )
  ).



%! assert_parameter(+M, +MediaType, +G, +Param) is det.

assert_parameter(M, MediaType, G, parameter{name:Name,value:Value}):-
  qb_bnode(Param),
  qb(M, MediaType, mto:parameter, Param, G),
  qb_instance(M, Param, mto:'Parameter', G),
  qb(M, Param, mto:name, Name, G),
  qb(M, Param, mto:value, Value, G).



init_media_type :-
  init_media_type(media_type).


init_media_type(G) :-
  absolute_file_name(G, File, [access(read),extensions([ttl]),file_errors(fail)]), !,
  rdf_load_file(File, [graph(G),format(turtle)]).
init_media_type(G) :-
  M = trp,
  rdf_assert_class(M, mto:'MediaType', rdfs:'Resource', "Media Type", _, G),
  forall(
    media_type_category(Cat0),
    (
      capitalize_atom(Cat0, Cat),
      assert_category_class(M, Cat, _, G),
      atomic_list_concat(
	['http://www.iana.org/assignments/media-types/',Cat,'.csv'],
	Iri
      ),
      csv2rdf(Iri, 'media_type.nt.gz', _{host: 'mediatype.org'}, _{})
    )
  ).



%! media_type(+M, -MT, +G) is nondet.
% Reads from the Media Types ontology.

media_type(M, media(Type/Subtype,[]), G) :-
  t(M, MediaType, mto:name, Subtype, G),
  rdfs_individual_of(MediaType, C),
  once(rdfs_pref_label(M, C, Type, G)).



%! media_type_category(+Cat) is semidet.
%! media_type_category(-Cat) is multi.

media_type_category(application).
media_type_category(audio).
media_type_category(image).
media_type_category(message).
media_type_category(model).
media_type_category(multipart).
media_type_category(text).
media_type_category(video).
