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
         delete_doc/3, get_doc/3]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------
-type address() :: inet:ip_address() | inet:hostname().
-opaque db_access() :: {address(), inet:port_number()}.
-type json_term() :: json_string() | json_number() | json_array()
                   | json_object().
-type json_string() :: binary().
-type json_number() :: number().
-type json_array() :: [json_term()].
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type id_and_rev() :: {binary(), binary()}.

-export_type([address/0, json_term/0]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
-spec new_access(address(),inet:port_number()) -> db_access().
new_access(Host, Port) ->
    {Host, Port}.

-spec get_dbs(db_access()) -> [string()].
get_dbs({Host, Port}) ->
    [binary_to_list(Db)
     || Db <- cushion_json:json2erl(cushion_couch_api:get_dbs(Host, Port))].

-spec create_db(db_access(),string()) -> 'ok'.
create_db({Host, Port}, Name) ->
    unwrap_ok(
      cushion_json:json2erl(cushion_couch_api:create_db(Host, Port, Name))).

-spec delete_db(db_access(),string()) -> 'ok'.
delete_db({Host, Port}, Name) ->
    unwrap_ok(
      cushion_json:json2erl(cushion_couch_api:delete_db(Host, Port, Name))).

-spec create_doc(db_access(),string(),json_object()) -> id_and_rev().
create_doc({Host, Port}, Db, Doc) ->
    get_id_and_rev(
      cushion_json:json2erl(
        cushion_couch_api:create_doc(
          Host, Port, Db, cushion_json:erl2json(Doc)))).

-spec delete_doc(db_access(),string(),id_and_rev()) -> 'ok'.
delete_doc({Host, Port}, Db, {Id, Rev}) ->
    unwrap_ok(
      cushion_json:json2erl(
        cushion_couch_api:delete_doc(
          Host, Port, Db, binary_to_list(Id), binary_to_list(Rev)))).

-spec get_doc(db_access(),string(),binary()) -> json_object().
get_doc({Host, Port}, Db, Id) ->
    cushion_json:json2erl(
      cushion_couch_api:get_doc(Host, Port, Db, binary_to_list(Id))).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------
%% Find the ok field in the result and see whether it's true
-spec unwrap_ok(json_object()) -> 'ok'.
unwrap_ok({struct, Fields}) ->
    case lists:keysearch(<<"ok">>, 1, Fields) of
        {value, {<<"ok">>,true}} ->
            ok;
        Other ->
            % This shouldn't happen, cushion_couch_api throws errors when
            % couchdb reports them
            erlang:error({cushion_bug, {{?FILE, ?LINE}, {not_ok, Other}}})
    end.

-spec get_id_and_rev(json_object()) -> id_and_rev().
get_id_and_rev({struct, Fields}) ->
    {get_field(<<"id">>, Fields), get_field(<<"rev">>, Fields)}.

-spec get_field(binary(),[{binary(),json_term()}]) -> json_term().
get_field(Field, Fields) ->
    case lists:keysearch(Field, 1, Fields) of
        {value, {Field, Value}} ->
            Value;
        false ->
            erlang:error(
              {cushion_bug, {{?FILE, ?LINE}, {not_found, Field, Fields}}})
    end.
