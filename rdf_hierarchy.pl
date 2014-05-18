:- module(
  rdf_hierarchy,
  [
    rdf_export_hierarchy//2 % +Graphs:list(atom)
                            % +Predicate:iri
  ]
).

/** <module> RDF Hierarchy.

@author Wouter Beek
@version 2014/01
*/

:- use_module(gv(gv_file)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1.
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_reasoning(rdf_mat)).
:- use_module(xml(xml_dom)).

:- rdf_meta(rdf_export_hierarchy(+,r,?,?)).



%! rdf_export_hierarchy(+FromGraphs:list(atom), +Predicate:iri)// is det.
% Exports a Linked Data hierarchy to HTML.
%
% @arg Graph The atomic name of an RDF graph.
% @arg Predicate Typically `rdfs:subClassOf` or `rdfs:subPropertyOf`.

rdf_export_hierarchy(FromGs, Predicate) -->
  {
    % Materialize (transitivity of the subclass relation).
    maplist(
      materialize(
        [entailment_regimes([rdf,rdfs]),multiple_justifications(false)]
      ),
      FromGs
    ),
    
    % Copy to a temporary graph.
    setup_call_cleanup(
      rdf_new_graph(ToG),
      (
        forall(
          member(FromG, FromGs),
          rdf_copy(FromG, _, Predicate, _, ToG)
        ),
        
        export_rdf_graph([], ToG, GIF),
        graph_to_svg_dom([method(dot)], GIF, SvgDom)
      ),
      % Remove the temporary graph.
      rdf_unload_graph_debug(ToG)
    )
  },
  html(\xml_dom_as_atom(SvgDom)).

