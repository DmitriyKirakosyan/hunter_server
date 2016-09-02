%%%-------------------------------------------------------------------
%%% @author dima
%%% @copyright (C) 2016, nooneknows
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2016 1:26 PM
%%%-------------------------------------------------------------------
-module(hunter_admin).
-author("dima").

%% API
-export([]).
-compile(export_all).


ignore_action(Action) ->
    gen_server:call(hunter_game_controller, {admin_ignore_action, Action}).

clear_ignored_actions() ->
    gen_server:call(hunter_game_controller, admin_clear_ignored_actions).