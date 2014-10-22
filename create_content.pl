:- module(
  create_content,
  [
    content_create_instance//1 % +Class:iri
  ]
).

/** <module> Create content

Content creation according to an RDF Schema.

@author Wouter Beek
@version 2014/10
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plXsd(xsd_html5)).

:- use_module(plTabular(rdf_term_html)).

:- rdf_meta(content_create_instance(r)).



%! content_create_instance(+Class:iri)// is det.

content_create_instance(Class) -->
  {
    aggregate_all(
      set(popair(Property,Range)),
      (
        instance_has_property(Class, Property),
        rdfs_range(Property, Range)
      ),
      POPairs
    )
  },
  html(
    form(
      [
        action=Class,
        class=['pure-form','pure-form-stacked'],
        id=contentCreateInstanceForm,
        method=post
      ],
      fieldset([
        legend(['Create ',\rdf_term_html(plTabular,Class)]),
        \property_fields(POPairs),
        button(
          [
            class=['pure-button','pure-button-primary'],
            id=contentCreateBtn,
            type=submit
          ],
          ['Create ',\rdf_term_html(plTabular,Class)]
        )
      ])
    )
  ).


property_field(popair(Property,Datatype)) -->
  {xsd_datatype_to_input_type(Datatype, Type)},
  html(
    p([
      label([], [\rdf_term_html(plTabular,Property)]),
      input([id=Property,type=Type], [])
    ])
  ).


property_fields([]) --> !, [].
property_fields([H|T]) -->
  property_field(H),
  property_fields(T).



% HELPERS

%! instance_has_property(+Class:iri, -Property:iri) is nondet.
% Potential properties of instances of the given class.

instance_has_property(Class, Property):-
  rdfs_subclass_of(Class, Superclass),
  rdf_has(Property, rdfs:domain, Superclass).


%! rdfs_domain(+Property:iri, -Domain:ordset(iri)) is det.

rdfs_domain(Property, Domain):-
  aggregate_all(
    set(DomainConjunct),
    rdf_has(Property, rdfs:domain, DomainConjunct),
    Domain
  ).


%! rdfs_range(+Property:iri, -Range:ordset(iri)) is det.

rdfs_range(Property, Range):-
  aggregate_all(
    set(RangeConjunct),
    rdf_has(Property, rdfs:domain, RangeConjunct),
    Range
  ).
