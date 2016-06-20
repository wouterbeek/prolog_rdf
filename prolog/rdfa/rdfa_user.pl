:- module(
  rdfa_user,
  [
    rdfa_user_menu//0,
    rdfa_user_menu_button//1 % :Content_0
  ]
).

/** <module> RDFa user

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(html/html_bs)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_user)).
:- use_module(library(rdfa/rdfa_ext)).

:- html_meta
   rdfa_user_menu_button(html, ?, ?).





%! rdfa_user_menu// is det.

rdfa_user_menu -->
  user_menu(agent_name, agent_image).



%! rdfa_user_menu_button(:Content_0)// is det.
%
% The user menu shown as a button.

rdfa_user_menu_button(Content_0) -->
  {current_user(User)}, !,
  html(
    div([class=dropdown,id='user-menu'], [
      button([
        class=[btn,'btn-default','dropdown-toggle','navbar-btn'],
        'data-toggle'=dropdown,
        id='user-button',
        type=button
      ], [
        \agent_image(User),
        span(class=caret, [])
      ]),
      Content_0
    ])
  ).
rdfa_user_menu_button(_) -->
  {login_link(Link)},
  html(
    span(id='user-menu',
      \bs_icon_button(user, 'window.location="'+Link+'";')
    )
  ).
