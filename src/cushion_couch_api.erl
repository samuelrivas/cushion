%%%-------------------------------------------------------------------
%%% Copyright 2006, 2007, 2010 Samuel Rivas <samuelrivas@gmail.com>
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
%%% @doc This module contains functions that map directly to CouchDB api.
%%%
%%% See http://wiki.apache.org/couchdb/API_Cheatsheet for a quick reference.
%%%
%%% All functions in this module return the raw response from CouchDB.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cushion_couch_api).

-export([get_document/4]).

%%--------------------------------------------------------------------
%% @doc Retrieve a document.
%% @spec get_document(string(), int(), string(), string()) -> binary()
%% @end
%%--------------------------------------------------------------------
get_document(Couch, Port, Db, DocId) ->
    lhttpc:request(
      "http://" ++ Couch ++ ":" ++ integer_to_list(Port) ++ "/" ++ Db ++ "/"
      ++ DocId,
      "GET", [], infinity).
