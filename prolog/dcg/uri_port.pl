:- module(
  uri_port,
  [
    port//1 % ?Port:nonneg
  ]
).

/** <module> URI: Port

Grammar for port subcomponent of URI.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_abnf_rules)).
:- use_module(library(math/positional)).





%! port(?Port:nonneg)// .
% ```abnf
% port = *DIGIT
% ```
%
% @compat RFC 3986
% @compat RFC 3987

port(Port) -->
  {clpfd_positional(Port, Ds)},
  '*'('DIGIT', Ds, []).
