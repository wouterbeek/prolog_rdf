:- module(
  rdf_file,
  [
    rdf_file_extension/1, % ?Extension:atom
    rdf_file_extension/2 % ?Extension:atom
                         % ?Format:atom
  ]
).

/** <module> RDF file

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(semweb/rdf_http_plugin)). % RDF serialization formats.
:- use_module(library(solution_sequences)).

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

user:prolog_file_type(nq, nquads).
user:prolog_file_type(nt, ntriples).
user:prolog_file_type(html, rdfa).
user:prolog_file_type(n3, n3).
user:prolog_file_type(trig, trig).
user:prolog_file_type(trix, trix).
user:prolog_file_type(ttl, turtle).
user:prolog_file_type(rdf, xml).





%! rdf_file_extension(+Extension:atom) is semidet.
% Succeeds for file extensions of RDF serializations.
%! rdf_file_extension(-Extension:atom) is multi.
% Enumerates file extensions RDF serializations.

rdf_file_extension(Ext):-
  distinct(Ext, rdf_file_extension(Ext, _)).



%! rdf_file_extension(+Extension:atom, +Format:atom) is semidet.
%! rdf_file_extension(+Extension:atom, -Format:atom) is semidet.
%! rdf_file_extension(-Extension:atom, +Format:atom) is det.
%! rdf_file_extension(-Extension:atom, -Format:atom) is multi.

rdf_file_extension(Ext, Format):-
  rdf_http_plugin:rdf_content_type(_, _, Format),
  user:prolog_file_type(Ext, Format).
