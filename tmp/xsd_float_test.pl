:- module(xsd_float_test, []).

:- use_module(library(plunit)).
:- use_module(library(xml/xsd)).



:- begin_tests(xsd_double).

test(double, true,  '-8.9884656743115785407263711865852e+307').
test(double, true,  '-2.2250738585072011360574097967091e-308').
test(double, true,  '2.2250738585072011360574097967091e-308').
test(double, true,  '8.9884656743115785407263711865852e+307').
test(double, true,  'NaN').
test(double, true,  '-INF').
test(double, true,  'INF').
test(double, true,  '+0').
test(double, true,  '-0').

test(xsd_double_parse_true, [forall(test(double,true,Lex))]):-
  xsd_lexical_map(xsd:double, Lex, N),
  format(user_output, "~w: ~w~n", [Lex,N]).
test(xsd_double_parse_false, [fail,forall(test(double,false,Lex))]):-
  xsd_lexical_map(xsd:double, Lex, _).

:- end_tests(xsd_double).



:- begin_tests(xsd_float).

% Specials.
test(float, true,  '+INF').
test(float, true,  'INF').
test(float, true,  '-INF').
test(float, true,  'NaN').
% Zeros
test(float, true,  '+0').
test(float, true,  '-0').
test(float, true,  '+0.0').
test(float, true,  '-0.0').
% Ones
test(float, true,  '1').
test(float, true,  '1.0').
test(float, true,  '-1.0').
test(float, true,  '+1').
test(float, true,  '1E2').
test(float, true,  '1e2').
test(float, true,  '-1E4').
% Pi
test(float, true,  '-3.14159').
test(float, true,  '3.14159').
% Decimal
test(float, true,  '23.456').
test(float, true,  '+1234.456').
% Scientific
test(float, true,  '-1.2344e56').
test(float, true,  '-.45E-6').
test(float, true,  '-3.4028234663852885981170418348452e+38').
test(float, true,  '-2.3509885615147285834557659820715e-38').
test(float, true,  '2.3509885615147285834557659820715e-38').
test(float, true,  '3.4028234663852885981170418348452e+38').

test(float, false, 'e').
test(float, false, 'E').
test(float, false, 'inf').
test(float, false, '+NaN').
test(float, false, '-NaN').
test(float, false, 'nan').
test(float, false, 'NAN').

test(xsd_float_parse_true, [forall(test(float,true,Lex))]):-
  xsd_lexical_map(xsd:float, Lex, N),
  format(user_output, "~w: ~w~n", [Lex,N]).
test(xsd_float_parse_false, [fail,forall(test(float,false,Lex))]):-
  xsd_lexical_map(xsd:float, Lex, _).

:- end_tests(xsd_float).
