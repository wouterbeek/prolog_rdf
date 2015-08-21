:- module(
  rdf_load,
  [
    rdf_load_any/1 % +Spec
  ]
).

/** <module> RDF load

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(archive)).
:- use_module(library(option)).
:- use_module(library(iostream)).





rdf_load_any(Spec):-
  rdf_load_any(Spec, []).

rdf_load_any(Spec, Opts0):-
  setup_call_cleanup(
    open_any(Spec, read, Read0, Close, []),
    setup_call_cleanup(
      archive_open(Read0, Archive, [close_parent(Close),format(all),format(raw)]),
      (
        % NONDET
        archive_data_stream(Archive, Read, [meta_data(M)]),
        call_cleanup(
          rdf_set_format_option(Opts0, Read, Opts),
          rdf_load(Read, Opts),
          close(Read)
        )
      ),
      archive_close(Archive)
    ),
    close_any(Close)
  ).

rdf_set_format_option(Opts,Read, Opts):-
  option(format(Format), Opts),
  (   ground(Format)
  ->  true
  ;   rdf_guess_format(Read, Format)
  ).
rdf_set_format_option(Opts0, Read, Opts):-
  rdf_guess_format(Read, Format),
  merge_options([format(Format)], Opts0, Opts).
