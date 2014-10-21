:- module(
  create_content,
  [
    content_create/2, % +Request:list(nvpair)
                      % +HtmlStyle
    register_content_class/1 % +Class:iri
  ]
).

/** <module> Create content

Content creation according to an RDF Schema.

@author Wouter Beek
@version 2014/10
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdfs)).

:- use_module(plDcg(dcg_generics)).

:- use_module(plRdf(rdf_name)).



content_create(Request, HtmlStyle):-
  {
    request_name_value(Request, class, Class), !,
    dcg_with_output_to(atom(ClassName), rdf_term_name(Class)),
  },
  reply_html_page(
    HtmlStyle,
    html(title(['Create content - ',ClassName])),
    html(\content_create_instance(Class))
  ).


content_create_instance(Class) -->
  {
    aggregate_all(
      set(Property-Range),
      (
        instance_has_property(Class, Property),
        rdfs_range(Property, Range)
      ),
      Pairs
    ),
    http_absolute_uri(content_create(.), AgentLocation)
  },
  html(
    form(
      [
        action=AgentLocation,
        class=['pure-form','pure-form-stacked'],
        id=agentDefinitionForm,
        method=post
      ],
      fieldset([
        legend('Agent definitions'),
        div(class='pure-g', [
          div([
            class=['pure-u-1','pure-u-md-1-3'],
            id=agentDefinitionsContainer
          ], []),
          div([
            class=['pure-u-1','pure-u-md-1-3'],
            id=agentDefinitionContainer
          ], [])
        ]),
        button(
          [
            class=['pure-button','pure-button-primary'],
            id=createBtn,
            type=submit
          ],
          ['Create agent']
        )
      ])
    ),
      \js_script({|javascript(AgentLocation,AgentDefinitionLocation,SparqlLocation)||

  content_create_properties(Pairs).


content_create_properties([]) --> !, [].
content_create_properties([H|T]) -->
  content_create_property(H),
  content_create_properties(T).


content_create_property(Property-Range) -->
  


%! register_content_class(+Class:iri) is det.
% For a given class:
%   - creates an HTTP handler on that class' IRI.

register_content_class(Class):-
  http_handler(



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
