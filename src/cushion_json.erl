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
%%% @author Samuel <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel
%%% @doc Functions transform erlang terms into json streams and vice-versa
%%%
%%% @todo Write type specification
%%%
%%% @end
%%% Created : 31 Aug 2010 by Samuel <samuelrivas@gmail.com>
%%%-------------------------------------------------------------------
-module(cushion_json).
-export([erl2json/1, json2erl/1]).

json2erl(Binary) when is_binary(Binary) ->
    json2erl_priv(binary_to_list(Binary));
json2erl(IoList) ->
    json2erl_priv(lists:flatten(IoList)).

json2erl_priv(List) ->
    {Term, _, _} = ktj_parse:parse(List),
    Term.

erl2json(Term) ->
    ktj_encode:encode(Term).

