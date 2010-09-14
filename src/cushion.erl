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
%%% @doc This is the main module of the cushion application, containing the
%%% public API to talk to CouchDB instances
%%%
%%% @end
%%% Created :  7 Sep 2010 by Samuel Rivas <samuelrivas@gmail.com>
%%%-------------------------------------------------------------------
-module(cushion).

-export([new_access/2, get_dbs/1, create_db/2, delete_db/2, create_doc/3,
         delete_doc/3]).

new_access(Host, Port) ->
    {Host, Port}.

get_dbs({Host, Port}) ->
    [binary_to_list(Db)
     || Db <- cushion_json:json2erl(cushion_couch_api:get_dbs(Host, Port))].

create_db({Host, Port}, Name) ->
    unwrap_ok(
      cushion_json:json2erl(cushion_couch_api:create_db(Host, Port, Name))).

delete_db({Host, Port}, Name) ->
    unwrap_ok(
      cushion_json:json2erl(cushion_couch_api:delete_db(Host, Port, Name))).

create_doc({Host, Port}, Db, Doc) ->
    get_id_and_rev(
      cushion_json:json2erl(
        cushion_couch_api:create_doc(
          Host, Port, Db, cushion_json:erl2json(Doc)))).

delete_doc({Host, Port}, Db, {Id, Rev}) ->
    unwrap_ok(
      cushion_json:json2erl(
        cushion_couch_api:delete_doc(
          Host, Port, Db, binary_to_list(Id), binary_to_list(Rev)))).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------
%% Find the ok field in the result and see whether it's true
unwrap_ok({obj, Fields}) ->
    case lists:keysearch(<<"ok">>, 1, Fields) of
        {value, {<<"ok">>,true}} ->
            ok;
        Other ->
            % This shouldn't happen, cushion_couch_api throws errors when
            % couchdb reports them
            erlang:error({cushion_bug, {{?FILE, ?LINE}, {not_ok, Other}}})
    end.

get_id_and_rev({obj, Fields}) ->
    {get_field(<<"id">>, Fields), get_field(<<"rev">>, Fields)}.

get_field(Field, Fields) ->
    case lists:keysearch(Field, 1, Fields) of
        {value, {Field, Value}} ->
            Value;
        false ->
            erlang:error(
              {cushion_bug, {{?FILE, ?LINE}, {not_found, Field, Fields}}})
    end.
