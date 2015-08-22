:- module(
  rdf_stream,
  [
    rdf_load_any/1, % +Spec
    rdf_load_any/2, % +Spec
                    % +Options:list(compound)
    rdf_stream/2, % +Spec, :Goal_2
    rdf_stream/3 % +Spec
                 % :Goal_2
		 % +Options:list(compound)
  ]
).

/** <module> RDF load

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(archive)).
:- use_module(library(option)).
:- use_module(library(iostream)).
:- use_module(library(rdf/rdf_guess)).

:- meta_predicate(rdf_stream(+,2)).
:- meta_predicate(rdf_stream(+,2,+)).





%! rdf_load_any(+Spec) is det.

rdf_load_any(Spec):-
  rdf_load_any(Spec, []).

%! rdf_load_any(+Spec, +Options:list(compound)) is det.
% Options are passed to rdf_load/2.

rdf_load_any(Spec, Opts):-
  rdf_stream(Spec, rdf_load0(Opts), Opts).
rdf_load0(Opts0, Format, Read):-
  merge_options([format(Format)], Opts0, Opts),
  rdf_load(Read, Opts).



%! rdf_stream(+Spec, :Goal_2) is det.
% Wrapper around rdf_stream/3 with default options.

rdf_stream(Spec, Goal_1):-
  rdf_stream(Spec, Goal_1, []).

%! rdf_stream(+Spec, :Goal_2, +Options:list(compound)) is det.
% The following options are supported:
%    * format(?rdf_format)

rdf_stream(Spec, Goal_2, Opts):-
  ArchOpts = [close_parent(false),format(all),format(raw)],
  setup_call_cleanup(
    open_any(Spec, read, Read0, Close, []),
    setup_call_cleanup(
      archive_open(Read0, Arch, ArchOpts),
      (
        % NONDET
        archive_data_stream(Arch, Read, [meta_data(M)]),
        call_cleanup(
          (
            rdf_determine_format(Read, Opts, Format),
            call(Goal_2, Format, Read)
          ),
          close(Read)
        )
      ),
      archive_close(Arch)
    ),
    close_any(Close)
  ).





% HELPERS %

%! rdf_determine_format(
%!   +Read:stream,
%!   +Options:list(compound),
%!   -Format:rdf_format
%! ) is det.

rdf_determine_format(Read, Opts, Format):-
  option(format(Format), Opts),
  (   ground(Format)
  ->  true
  ;   rdf_guess_format(Read, Format)
  ).
rdf_determine_format(Read, _, Format):-
  rdf_guess_format(Read, Format).
