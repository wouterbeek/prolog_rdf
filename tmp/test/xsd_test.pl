:- module(xsd_test, []).

/** <module> XSD: Unit tests

Unit testing for XSD support in PGC.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(plunit)).
:- use_module(library(xml/xsd_duration)).





datetimePlusDuration_test(
  'P1Y3M5DT7H10M3.3S',
  '2000-01-12T12:13:14Z',
  '2001-04-17T19:23:17.3Z'
).
datetimePlusDuration_test('-P3M', '2000-01', '1999-10').
datetimePlusDuration_test('PT33H', '2000-01-12', '2000-01-13').

% Test case for non-commutative nature of datetimePlusDuration/3.
% @tbd

% (2000-03-30 + P1D) + P1M = 2000-03-31 + P1M = 2000-04-30
% (2000-03-30 + P1M) + P1D = 2000-04-30 + P1D = 2000-05-01

