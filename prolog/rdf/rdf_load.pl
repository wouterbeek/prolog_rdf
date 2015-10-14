:- module(
  rdf_load,
  [
    rdf_load_file/1, % +Spec
    rdf_load_file/2, % +Spec
                     % +Options:list(compound)
    rdf_load_triple/2 % +Spec
                      % :Goal_2
  ]
).

/** <module> RDF load

Support for loading RDF data.

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(rdf)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uuid_ext)).

:- meta_predicate(rdf_load_triple(+,2)).

:- predicate_options(rdf_load_file/2, 2, [
     pass_to(rdf_load_file0/4, 1),
     pass_to(rdf_stream_read/3, 3)
   ]).
:- predicate_options(rdf_load_file0/4, 1, [
     pass_to(rdf_load/2, 2)
   ]).





%! rdf_load_file(+Spec) is det.
% Wrapper around rdf_load_file/2 with default options.

rdf_load_file(Spec):-
  rdf_load_file(Spec, []).


%! rdf_load_file(+Spec, +Options:list(compound)) is det.

rdf_load_file(Spec, Opts1):-
  % Handle the graph name option.
  select_option(graph(G), Opts1, Opts2, _VAR),

  % In the absence of a graph name use the base IRI.
  (var(G), base_iri(Spec, BaseIri) -> G = BaseIri ; true),
  
  merge_options([graph(G)], Opts2, Opts3),
  rdf_stream_read(Spec, rdf_load_file0(Opts3), Opts2).

rdf_load_file0(Opts0, Read, M):-
  merge_options([base_iri(M.base_iri),format(M.rdf.format)], Opts0, Opts),
  rdf_load(Read, Opts).



%! rdf_load_triple(+Spec, :Goal_2) is nondet.

rdf_load_triple(Spec, Goal_2):-
  rdf_stream_read(Spec, rdf_load_triple0(Goal_2), []).

%! rdf_load_triple0(:Goal_2, +BaseIri:atom, +Format:atom, +Read:stream) is det.

rdf_load_triple0(Goal_2, BaseIri, Format, Read):-
  memberchk(Format, [nquads,ntriples]), !,
  rdf_process_ntriples(Read, Goal_2, [base_uri(BaseIri)]).
rdf_load_triple0(Goal_2, BaseIri, Format, Read):-
  memberchk(Format, [trig,turtle]), !,
  uuid_no_hyphen(Prefix0),
  atomic_list_concat(['__',Prefix0,':'], Prefix),
  rdf_process_turtle(Read, Goal_2, [anon_prefix(Prefix),base_uri(BaseIri)]).
rdf_load_triple0(Goal_2, BaseIri, xml, Read):- !,
  process_rdf(Read, Goal_2, [base_uri(BaseIri)]).
rdf_load_triple0(_, _, Format, _):-
  format(user_error, 'Unrecognized RDF serialization format: ~a~n', [Format]).
