%%%-------------------------------------------------------------------
%%% File    : plObject.erl
%%% Author  : Peter Neumark
%%% Description : 
%%% Functions for dealing with postlock objects.
%%%
%%% Created :  17 Jan 2011 by Peter Neumark
%%%-------------------------------------------------------------------
-module(plObject).
-export([
    get_oid/1, 
    get_children/2,
    make_create_transformation/1]).
-include("plState.hrl").

get_oid(Obj) ->
    Obj#postlock_object.oid.

% Get children behaves differently based on the type of the object
get_children(#postlock_object{content=#postlock_content_data{}}, _StateServer)->
    [];

get_children(#postlock_object{content=#postlock_content_dict{children=Dict}}, StateServer) ->
    [gen_server:call(StateServer, {get_object, Oid}) || Oid <- gb_trees:values(Dict)].

make_spec(#postlock_content_dict{children=Dict}, Spec) ->
    Children = gb_trees:to_list(Dict),
    json:obj_store("type", "dict",
        json:obj_store("children", {struct, Children}, Spec)).

make_create_transformation(#postlock_object{
    oid=Oid,
    content=Content}) ->
    #postlock_transformation{
        oid="meta_object",
        cmd="create",
        parameters= {struct, [
            {"spec", make_spec(Content,{struct, [{"oid", Oid}]})}
        ]}
    }.
