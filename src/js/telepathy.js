/*
  telepathy.js - javascript client API for telepathy.
  author: neumark
*/

// mkTelepathy: constructor for the main telepathy module.
var mkTelepathy = function(websocket_url) {

    // shared state for all of telepathy
    var telepathy_shared = {
        config: {
            ws_url: websocket_url 
        },
        counters: {},
        master_dict: {}
    };

    // From Javascript - The Good Parts
    var is_array = function (value) {
        return value &&
            typeof value === 'object' &&
            value.constructor === Array;
    };
        
    var args2array = function(args) {
        args = args || [];
        else return Array.prototype.slice.apply(args);
    }

    var clone = function(state) {
        // NOTE: this is the worst solution performance-wise!
        return JSON.decode(JSON.encode(state));
    }

    var copy_methods = function(src, dest) {
        for (var i in src) dest[i] = src[i];
        return dest;
    }

    // from: http://stackoverflow.com/questions/1068834/object-comparison-in-javascript
    Object.prototype.equals = function(x) {
        for(p in this) {
            if(typeof(x[p])=='undefined') {return false;}
        }
        for(p in this) {
            if (this[p]) {
                switch(typeof(this[p])) {
                case 'object':
                        if (!this[p].equals(x[p])) { return false }; break;
                case 'function':
                        if (typeof(x[p])=='undefined' || (p != 'equals' && this[p].toString() != x[p].toString())) { return false; }; break;
                default:
                        if (this[p] != x[p]) { return false; }
                }
            }
            else {
                if (x[p]) {
                    return false;
                }
            }
        }
        for(p in x) {
            if(typeof(this[p])=='undefined') {return false;}
        }
        return true;
    }
     

    var mkCounter = function () {
    // From Javascript - The Good Parts
    // Produce an object that produces unique strings. A
    // unique string is made up of two parts: a prefix
    // and a sequence number. The object comes with
    // methods for setting the prefix and sequence
    // number, and a gensym method that produces unique
    // strings.
        var prefix = '';
        var seq = 0;
        return {
            set_prefix: function (p) {
                prefix = String(p);
            },
            set_seq: function (s) {
                seq = s;
            },
            gensym: function (  ) {
                var result = prefix + seq;
                seq += 1;
                return result;
            }
        };
    };

    //  --------------------
    //  - INTERNAL MODULES -
    //  --------------------

    // CallbackManager: Module responsible for getting/setting/calling
    // callback functions.
    var mkCallbackManager = function(spec) {
        // format of spec:
        // {
        //      string name (optional)
        // }
        // MODULE PRIVATE DATA:
        var my = {
            id: spec.name || "[unnamed object]",
            // debug state:
            debug: false,
            // callbacks holds 'signal' -> handler() entries.
            // telepathy.js internal callbacks
            internal_cb: {},
            // user-defined callbacks
            user_cb: {},
            // default callback function:
            default_callback: function(signal, args) {
                if (debug) {
                    console.log("Callback manager for " + id + " received signal " + signal + " with arguments " + args + ".");
                }
            },
            fire: function (signal, args) {
                // apply the internal callback first
                var args_array = args2array(args);
                var first_cb = ((signal in my.internal_cb)?my.internal_cb[signal]:my.default_callback);
                var internal_cb_result = first_cb.apply(this, args_array):
                // apply the user-defined cb if one exists
                if (signal in my.user_cb) my.user_cb[signal].apply(this,[args_array, internal_cb_result]);
            }
        }; // END my    

        // MODULE EXPORTS
        return {
            debug: function(new_debug_state) {my.debug = new_debug_state;},
            set_internal_cb: function(signal, handler) {my.internal_cb[signal] = handler;},
            get_internal_cb: function(signal) {return my.internal_cb[signal];}
            set_user_cb: function(signal, handler) {my.user_cb[signal] = handler;},
            get_user_cb: function(signal) {return my.user_cb[signal];}
            remove_internal_cb: function(signal) { delete my.internal_cb[signal];},
            remove_user_cb: function(signal) { delete my.user_cb[signal];},
            fire: my.fire, 
            fire_async: function(signal, args) {setTimeout(function() {my.fire(signal, args);}, 10);},
            // used when we need a function which fires a signal. For example,
            // ajax-based callbacks or ws calls.
            wrap_signal: function(signal) {
                return function() {my.fire(signal, arguments);}
            } 
        };
    };

    // TelepathicObject: All telepathy datatypes (dicts, lists, and data)
    // are similar in the sense that they receive change commands
    // from the server and the local client, manipulate the command queue,
    // and fire callback events.
    // This common functionality is factored into a base class, so
    // specialized classes (dict, list, data) can inherit these capabilities
    // throught the functional inheritance pattern.
    // Specific responsibilities of TelepathicObject:
    // - maintain a callback manager for user callback events
    // - receive server messages, forward these signals to handlers
    // - place local state modification commands in queue
    // - resolve discrepancies in client/server state (this is the BIG issue)
    var mkTelepathyObject = function (spec) {
        // spec format: {
        //      string oid: (optional)
        //      string type: "dict" || "list" || "data" (mandatory)
        //      function: process_server_message
        //      fucntion: apply_command
        // }
        var object_id = spec.oid || telepathy_shared.counters.object_id.gensym();

        // MODULE DATA:
        var my {
            // unique ID of object
            oid: object_id,
            // the last state which the server and client could agree on
            // for the current object
            // last_shared_state has form:
            // {
            //      opaque: data - state data of object
            //      string: hash - hash of contents of data
            // }
            last_shared_state: {
                server_message_id: 0,
                state: null
            },
            // current_local_state is the state according to the local client.
            // all local gets and sets are applied to this state.
            current_local_state: null,
            // local_stransformation_stack is the list of operations committed
            // locally not yet acknowledged by the server.
            //      transformation: {
            //                          string: command
            //                          opaque: parameters
            //                      }
            local_transformation_stack: [],
            // callback manager, which is shared with the descendant object
            cb: mkCallbackManager({name: spec.type+"#"+object_id}),
            // parent node
            parent_node: null,
            // default OT function is 'do nothing':
            process_server_message: spec.process_server_message,
            apply_command: spec.apply_command
        };

        my.cb.set_internal_cb("server_message", function(message) {
            return my.process_server_message(message, my);});

        // EXPORTS
        return {
            cb: my.cb,
            get_oid: function() {return my.oid+'';},
            get_type: function() {return spec.type+'';},
            get_parent: function() {return parent_node+'';},
            get_current_state: function() {
                return clone(my.current_local_state);
            },
            apply_local_transformation: function(transformation) {
                // transformation should be: {
                //      command: CMD,
                //      parameters: CMD PARAMS,
                // }
                var t = clone(transformation);
                // We extend transformation with:
                // ID
                // timestamp
                t.id = telepathy_shared.counters.client_msg_id.gensym();
                t.ts = +new Date();
                my.local_transformation_stack.push(t);
                my.current_local_state = my.apply_command(t,clone(my.current_local_state));
                // send transformation to server
                telepathy_shared.connection.send(JSON.stringify(t));
                // fire transformation events
                my.cb.fire(t.command, t.parameters);
            }
        };
    };

    // Data module for telepathy data nodes.
    var mkData = function(spec) {
        // MODULE FIELDS:
        var my = {
            process_server_message: null,
            apply_command: null
        }
        my.base = mkTelepathyObject({
            oid: spec.oid, 
            type: 'data',
            process_server_message: my.process_server_message,
            apply_command: my.apply_command
        });
        my.set = function(val, overwrite) {
            var transformation = {
                command: 'set',
                parameters: {
                    value: val,
                    overwrite: overwrite
                }
            };
            my.apply_local_transformation(transformation, val);
        };
        my.apply_command = function (transformation, state) {
            // in this case its trivial:
            return state;
        }
        my.process_server_message = function(message, base_data) {
            var pending_local_transformations = [];
            for (var i = 0; i < base_data.local_transformation_stack.length; i++) {
                if (base_data.local_transformation_stack[last_ack_index].id > message.last_client_message_id) {
                    pending_local_transformations.push(base_data.local_transformation_stack[last_ack_index]);
                }
            }
            base_data.local_transformation_stack = pending_local_transformations;
            // If there are local transformations which have not been
            // processed by the server, then current_local_state is not
            // modified.
            if (pending_local_transformations.length == 0) {
                // check if we need to fire update event.
                if (!base_data.current_local_state.equals(message.parameters.value)) {
                    my.base.cb.fire_async("set",[base_data.current_local_state, message.parameters.value]);
                }
            } 
            // update last_shared_state
            base_data.last_shared_state = {
                server_message_id: message.id,
                state: local_state
            };
        };
            
        return {
            exports: {
                set: function(val) {my.set(val,true);},
                safe_set: function(val) {my.set(val,false);},
                get: my.base.get_current_state,
                set_cb: my.base.cb.set_user_cb
            }
        };
    };



    // Dict module for telepathy dict nodes.
    var mkDict = function(spec) {
        // MODULE FIELDS:
        var my = {
            process_server_message: null,
            apply_command: null
        }
        my.base = mkTelepathyObject({
            oid: spec.oid, 
            type: 'dict',
            process_server_message: my.process_server_message,
            apply_command: my.apply_command
        });
        my.set = function(key, val, overwrite) {
            var transformation = {
                command: 'set',
                parameters: {
                    key: key,
                    value: val,
                    overwrite: overwrite
                }
            };
            my.apply_local_transformation(transformation, val);
        };
        my.remove = function(key) {
            var transformation = {
                command: 'remove',
                parameters: {
                    key: key,
                }
            };
            my.apply_local_transformation(transformation, val);
        };
        my.apply_command = function (transformation, state) {
            var s = clone(state);
            s[transformation.parameters.key] = transformation.parameters.value;
            return s;
        }
        my.process_server_message = function(message, base_data) {
            // Is the transformation related to a key which is modified on the local transformation stack?
            //  No:  CASE 1: Update last_shared_state, current_local_state & fire events.
            //  Yes: Have the pending transformations related to the key been processed by the server?
            //       No:  CASE 2: Update last_shared_state
            //       Yes: Is the latest pending event a duplicate of the received server command?
            //            Yes: CASE 3: update last_shared_state, local_transformation_stack
            //            No:  CASE 4: update last_shared_state, current_local_state & fire events.
            var acknowledged_pending_transformations_on_key = [];
            var unacknowledged_pending_transformations_on_key = [];

            for (var i = 0; i < base_data.local_transformation_stack.length; i++) {
                if (base_data.local_transformation_stack[i].parameters.key !== message.parameters.key) continue;
                if (base_data.local_transformation_stack[i].id <= message.last_client_message_id) {
                    acknowledged_pending_transformations_on_key.push(i);
                } else {
                    unacknowledged_pending_transformations_on_key.push(i);
                }
            }
            if (acknowledged_pending_transformations_on_key.length == 0 && 
                unacknowledged_pending_transformations_on_key.length == 0) {
                // CASE 1
                my.base.cb.fire_async("set",[message.parameters.key, base_data.current_local_state[message.parameters.key], message.parameters.value]);
            }
            base_data.local_transformation_stack = pending_local_transformations;
            // If there are local transformations which have not been
            // processed by the server, then current_local_state is not
            // modified.
            if (pending_local_transformations.length == 0) {
                // check if we need to fire update event.
                if (!base_data.current_local_state.equals(message.parameters.value)) {
                    my.base.cb.fire_async("set",[base_data.current_local_state, message.parameters.value]);
                }
            } 
            // update last_shared_state
            base_data.last_shared_state = {
                server_message_id: message.id,
                state: local_state
            };
        };
            
        return {
            cb: my.base.cb,
            exports: {
                set: function(key,val) {my.set(key,val,true);},
                safe_set: function(key,val) {my.set(key,val,false);},
                get: function(key) {return my.base.get_current_state()[key];},
                remove: my.remove,
                set_cb: my.base.cb.set_user_cb,
                keys: function() {
                    var keys = [];
                    var st = my.base.get_current_state();
                    var k;
                    for (k in st) {
                        if st.hasOwnProperty(k) keys.push(k);
                    }
                    return keys;
                },
                size: function() {return my.base.get_current_state().length;}
            }
        };
    };



    //  -------------------------
    //  - MAIN TELEPATHY MODULE -
    //  -------------------------
    var mkMain = function() {

        // MODULE FIELDS
        var my {
            // websocket object
            connection: null, 
            cb: mkCallbackManager(" main telepathy object "),
            state_list: {
                INACTIVE: 0,
                AUTH: 1,
                CONNECTED: 2
            },
            state: state_list.INACTIVE,
        };
        my.cb.debug(true); // for DEVEL: debug main CB manager.

        my.create_object = function(msg) {
            var new_obj = null;
            switch (msg.type) {
            case "data":
                new_obj = mkData({oid: msg.oid, value: msg.value});
                break;
            case "dict":
                new_obj = mkDict({oid: msg.oid});
                break;
            case "list":
                break;
                new_obj = mkList({oid: msg.oid});
            default:
                throw {msg: "Unknown data type: " + msg.type}
            };
            return new_obj;
        };

        my.transform_object = function(msg) {
            if (!(msg.oid in telepathy_shared.master_dict)) throw {msg: "Unknown object id: " + msg.oid};
            return telepathy_shared.master_dict[msg.oid].cb.fire("server_message", msg);
        };

        // Callback functions
        my.handle_incoming_message = [];
        my.handle_incoming_message[my.state_list.AUTH] = function(msg_obj) {
            // TODO: respond to authentication challenge
            // Right now, there's no authentication, we assume it succeeded:
            if ("client_id" in msg_obj && typeof msg_obj.client_id == "number") {
                telepathy_shared.config.client_id = msg_obj.client_id;
                telepathy_shared.counters.object_id.set_prefix(msg_obj.client_id + ".");
                my.state = my.state_list.CONNECTED;
            }
            else console.error("expected client id, received " + JSON.stringify(msg_obj));
        };
        my.handle_incoming_message[my.state_list.CONNECTED] = function(msg_obj) {
            // should we create an object?
            for (var i = 0; i < msg_obj.length; i++) {
                var msg = msg_obj[i];
                if (msg.action === "create") {
                    my.create_object(msg);
                    continue;
                }
                if (msg.action === "transform") {
                    my.transform_object(msg);
                    continue;
                }
                throw {msg: "Unknown message type received"};
            }
        };

        // handle incoming message from server
        my.cb.set_internal_cb("ws_onopen", function() {
            my.state = my.state_list.AUTH;
        });
        my.cb.set_internal_cb("ws_onmessage", function(msg) {
            console.log("ws_onmessage received: '" + msg+"'");
            var t = null;
            try {
                t = JSON.parse(msg);
            } catch (e) {
                console.log(e.message + "input: " + msg);
            }
            // apply server transformation
            if (t !== null) my.handle_incoming_message[my.state](t);
            else console.log("failed to parse incoming message '" + msg+"'");
        });
        my.cb.set_internal_cb("ws_onclose", function() {
            // TODO: attempt to re-open the connection
            console.log("ws_onclose fired, arguments: " + args2array(arguments));
        });
        my.cb.set_internal_cb("ws_error", function() {
            // TODO: attempt to handle the error
            console.log("ws_error fired, arguments: " + args2array(arguments));
        });

        // CREATE root element:
        my.root = mkDict({oid: '0.0'});
        // mkDict automatically adds this to master_dict.
        telepathy_shared.register_object = function(tobj) {
                telepathy_shared.master_dict[tobj.exports.get_id()] = tobj;
        };
        telepathy_shared.unregister_object = function(tobj) {
                delete telepathy_shared.master_dict[tobj.exports.get_id()];
        };
 
        // EXPORTS:
        // master dictionary manipulation (for internal use only):
        var exports = {
            // ---- exports methods ----
           connect: function() {
                telepathy_shared.connection=new WebSocket("ws://" + telepathy_shared.config.ws_url + ");
                // Set callbacks for websocket events.
                telepathy_shared.connection.onopen=my.cb.wrap_signal("ws_onopen");
                telepathy_shared.connection.onmessage=my.cb.wrap_signal("ws_onmessage");
                telepathy_shared.connection.onclose=my.cb.wrap_signal("ws_onclose");
                telepathy_shared.connection.onerror=my.cb.wrap_signal("ws_onerror");
                },
            disconnect: function() {}
            // ---- root callback manipulation ----
            set_callback: my.cb.set_user_cb,
            remove_callback: my.cb.remoe_user_cb
        };
        // ---- root node manipulation ----
        copy_methods(my.root.exports, exports);
        // RETURN EXPORTS
        return exports;
    }; // END of module Main

    // ---- Telepathy object creation ----
    // Now that we have defined all the modules we need,
    // we create a telepathy instance with mkMain, then
    // return a list of exports
    telepathy_shared.counters.object_id = mkCounter();
    telepathy_shared.counters.client_msg_id = mkCounter();
    var main = mkMain();
    return main;
}; // END mkTelepathy
