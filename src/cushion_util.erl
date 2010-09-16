%%%-------------------------------------------------------------------
%%% Copyright 2010 Samuel Rivas <samuelrivas@gmail.com>
%%%
%%% This file is part of Cushion.
%%%
%%% Cushion is free software: you can redistribute it and/or modify it under
%%% the terms of the GNU General Public License as published by the Free
%%% Software Foundation, either version 3 of the License, or (at your option)
%%% any later version.
%%%
%%% Cushion is distributed in the hope that it will be useful, but WITHOUT ANY
%%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
%%% FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
%%% details.
%%%
%%% You should have received a copy of the GNU General Public License along with
%%% Cushion.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel Rivas
%%% @doc Common, useful functions
%%%
%%% <i>Don't use this module out of Cushion!</i> Most of these functions are
%%% here just temporarily. If they prove they usefulness,they'll graduate to a
%%% more universal utilities module.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cushion_util).

-export([start_app/1, format/2, untuple/1, get_value/2, get_value/3,
         app_modules/1]).

%%--------------------------------------------------------------------
%% @doc Starts an application and all its dependencies.
%%
%% Returns the list of actually started applications, in starting order.
%%
%% @spec start_app(atom()) -> [atom()]
%% @throws too_much_recursion
%% @end
%%--------------------------------------------------------------------
start_app(App) ->
    lists:flatten(start_app(App, 30)).

start_app(_, 0) ->
    throw(too_much_recursion);
start_app(App, N) ->
    case application:start(App) of
	{error, {already_started, App}} ->
	    [];
        {error, {not_started, OtherApp}} ->
            [start_app(OtherApp, N - 1),
             start_app(App, N - 1)];
        ok ->
            [App]
    end.

%%--------------------------------------------------------------------
%% @doc The same as io_lib:format/2 but flattening the result.
%%
%% Don't use this if you can use the deep list returned by io_lib:format, it is
%% more efficient than flattening each intermediate result this function does.
%% @spec format(string(), [term()]) -> string()
%% @end
%%--------------------------------------------------------------------
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%%--------------------------------------------------------------------
%% @doc Unwraps ok-tuples and throws error-tuples. It lets through any other
%% term.
%% @spec untuple(Tuple | term()) -> term()
%%      Tuple = {ok, Value} | {error, Reason}
%%      Value = term()
%%      Reason = term()
%% @throws term()
%% @end
%%--------------------------------------------------------------------
untuple({error, Reason}) -> throw(Reason);
untuple({ok, What}) -> What;
untuple(What) -> What.

%%--------------------------------------------------------------------
%% @doc Looks for the value associated to a key in a tuple list
%% @spec get_value(Key, [{key(), value()}]) -> value()
%% @throws not_found(Key, List)
%% @end
%%--------------------------------------------------------------------
get_value(Key, List) ->
    case lists:keysearch(Key, 1, List) of
        {value, {Key, Value}} ->
            Value;
        false ->
            throw({not_found, Key, List})
    end.

%%--------------------------------------------------------------------
%% @doc Looks for the value associated to a key in a tuple list, returning a
%% default value if  key is not found
%% @spec get_value(any(), [{key(), value()}], term()) -> value()
%% @end
%%--------------------------------------------------------------------
get_value(Key, List, Default) ->
    try
        get_value(Key, List)
    catch
        {not_found, Key, List} ->
            Default
    end.

%%--------------------------------------------------------------------
%% @doc Return the list of modules of an application
%%
%% The application app file must be in the code path and have the complete list
%% of modules.
%% @spec (atom()) -> [atom()]
%% @end
%%--------------------------------------------------------------------
app_modules(App) ->
    [{application, App, Params}] =
        untuple(
          file:consult(
            code:where_is_file(cushion_util:format("~p.app", [App])))),
    get_value(modules, Params).
