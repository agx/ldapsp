%%-------------------------------------------------------------------
%% This file is part of ldapsp.
%%
%% Copyright (C) 2016 Guido Günther <agx@sigxcpu.org>
%%
%% ldapsp is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% ldapsp is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with ldapsp.  If not, see <http://www.gnu.org/licenses/>.
%%-------------------------------------------------------------------
-module(ldapsp_log).
-export([
	 err/2,
	 warn/2,
	 info/2,
	 debug/2
]).

-spec info(string(), list()) -> ok.
info(Fmt, Args) ->
    webmachine_log:log_info(io_lib:format(Fmt, Args)).

-spec warn(string(), list()) -> ok.
warn(Fmt, Args) ->
    webmachine_log:log_error(io_lib:format(Fmt, Args)).

-spec err(string(), list()) -> ok.
err(Fmt, Args) ->
    webmachine_log:log_error(io_lib:format(Fmt, Args)).

-spec debug(string(), list()) -> ok.
debug(Fmt, Args) ->
    webmachine_log:log_info(io_lib:format(Fmt, Args)).

