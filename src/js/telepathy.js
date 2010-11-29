/*
  telepathy.js - javascript client API for telepathy.
  author: neumark
*/
var debugvar;
// mkTelepathy: constructor for the main telepathy module.
var mkTelepathy = function(websocket_url) {

    // shared state for all of telepathy
    var telepathy_shared = {
        config: {
            ws_url: websocket_url 
        },
        counters: {},
        master_dict: {},
        transactions: {},
        // ---- telepathy shared functions ----
    
        get_object: function(oid) {
            return telepathy_shared.master_dict[oid];
        }
    };

    // ---------------------
    // - Utility functions -
    // ---------------------

    // From Javascript - The Good Parts
    var is_array = function (value) {
        return value &&
            typeof value === 'object' &&
            value.constructor === Array;
    };
       
    var args2array = function(args) {
        args = args || [];
        return Array.prototype.slice.apply(args);
    };

    var throw_ex = function(message, extra_data) {
        var e = new Error(message);
        if (extra_data) e.data = extra_data;
        throw e;
    };

    // from: http://stackoverflow.com/questions/122102/what-is-the-most-efficient-way-to-clone-a-javascript-object
    var clone = function(from) {
    if (from == null || typeof from != "object") return from;
    if (from.constructor != Object && from.constructor != Array) return from;
    if (from.constructor == Date || from.constructor == RegExp || from.constructor == Function ||
        from.constructor == String || from.constructor == Number || from.constructor == Boolean)
        return new from.constructor(from);
    to = new from.constructor();
    for (var name in from) {
        to[name] = typeof to[name] == "undefined" ? this.extend(from[name], null) : to[name];
    }
    return to;
    };

    var copy_methods = function(src, dest) {
        for (var i in src) dest[i] = src[i];
        return dest;
    };

    var get_timestamp = function() { return +new Date(); };

    var retry_until = function(condition, cb_success, cb_failure, num_retries, timeout) {
        var ms = timeout || 10;
        var retries = num_retries || 5;
        var on_failure = cb_failure || function() {throw_ex("retry_until failed", {args: args2array(arguments)});};
        var do_try = function(r) {
            if (condition()) return cb_success();
            else if (retries === 0) on_failure();
            else {
                setTimeout(function() {do_try(r-1);},ms);
            }
        };
        do_try(retries);
    }

    // from: http://stackoverflow.com/questions/1068834/object-comparison-in-javascript
    var equals = function(t,x) {
        for(p in t) {
            if(typeof(x[p])=='undefined') {return false;}
        }
        for(p in t) {
            if (t[p]) {
                switch(typeof(t[p])) {
                case 'object':
                        if (!equals(t[p],x[p])) { return false }; break;
                case 'function':
                        if (typeof(x[p])=='undefined' || (p != 'equals' && t[p].toString() != x[p].toString())) { return false; }; break;
                default:
                        if (t[p] != x[p]) { return false; }
                }
            } else { 
            if (x[p]) { return false; }
            }
        }
        for(p in x) {
            if(typeof(t[p])=='undefined') {return false;}
        }
        return true;
    };

    //  --------------------
    //  - INTERNAL MODULES -
    //  --------------------

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
            get_prefix: function() {return prefix + '';},
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
            },
            // Note: disregards prefix.
            getint: function() {
                var result = seq + 0;
                seq += 1;
                return result;
            }
        };
    };

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
                if (my.debug) {
                    console.log("Callback manager for " + my.id + " received signal " + signal + " with arguments " + args + ".");
                }
            },
            fire: function (signal, args) {
                // apply the internal callback first
                var args_array = args2array(args);
                var first_cb;
                if (signal in my.internal_cb) first_cb = my.internal_cb[signal];
                else {
                    args_array.unshift(signal);
                    first_cb = my.default_callback;
                }
                var internal_cb_result = first_cb.apply(this, args_array);
                // apply the user-defined cb if one exists
                if (signal in my.user_cb) my.user_cb[signal].apply(this,[args_array, internal_cb_result]);
            }
        }; // END my    

        // MODULE EXPORTS
        return {
            debug: function(new_debug_state) {my.debug = new_debug_state;},
            set_internal_cb: function(signal, handler) {my.internal_cb[signal] = handler;},
            get_internal_cb: function(signal) {return my.internal_cb[signal];},
            set_user_cb: function(signal, handler) {my.user_cb[signal] = handler;},
            get_user_cb: function(signal) {return my.user_cb[signal];},
            remove_internal_cb: function(signal) { delete my.internal_cb[signal];},
            remove_user_cb: function(signal) { delete my.user_cb[signal];},
            fire: my.fire, 
            fire_async: function(signal, args) {setTimeout(function() {my.fire(signal, args);}, 4);},
            // used when we need a function which fires a signal. For example,
            // ajax-based callbacks or ws calls.
            wrap_signal: function(signal) {
                return function() {my.fire(signal, arguments);}
            } 
        };
    };

   
    // Creates a transaction, which can be applied locally and sent to the
    // server.
    var mkTransaction = function(spec) {
        var my = {
            msg: telepathy_shared.create_message('t'), // a message of type t = transaction 
            // transaction status-es:
            // init - transaction is under composition
            // applying
            // applied - the transaction has been locally applied, ACK from server pending
            // acknowledged - the transaction has been acknowledged
            // aborting
            // aborted - the transaction has been rolled back
            current_status: "init"
        };

        my.msg.transformations = [];

        my.get_tid = function() {return my.msg.header.id+0;};

        my.make_t_map = function() {
            // rearrange transformation list into an
            // object id -> list of commands map.
            var t_map = {};
            var i;
            for (i = 0; i < my.msg.transformations; i++) {
                var oid = my.msg.transformations[i].oid || "none";
                if (!(oid in t_map)) t_map[oid] = [];
                t_map.oid.push(my.msg.transformations[i]);
            }
            return t_map;
        };

        my.rollback = function() {
            my.current_status = "aborting";
            var t_map = arguments[0] || my.make_t_map();
            var e = arguments[1] || {message: "no reason given", data: {}};
            console.log("starting rollback of transaction "+my.get_tid()+"; reason: "+e.message);
            var i;
            for (i in t_map) {
                if (!t_map.hasOwnProperty(i)) continue;
                if (i === "none") {
                    telepathy_shared.rollback_transaction(my.get_tid());
                    continue;
                }
                telepathy_shared.get_object(i).rollback_transaction(my.get_tid());
            }
            my.current_status = "aborted";
            // unregister transaction
            delete telepathy_shared.transactions[my.get_tid()];
        };

        my.apply_locally = function() {
           // apply transformations like 'create' which are not tied to an oid
            var i;
            var t_map = my.make_t_map();
            try {
                for (i in t_map) {
                    if (!t_map.hasOwnProperty(i)) continue;
                    if (i === "none") {
                        telepathy_shared.apply_transaction(my.get_tid(), t_map.none);
                        continue;
                    }
                    telepathy_shared.get_object(i).apply_transaction(my.get_tid(), t_map[i]);
                }
                my.current_status = "applied";
                return true;
            } catch (e) {
                my.rollback(t_map, e);
            }
            return false;
        };
        my.send = function() {
            telepathy_shared.outqueue.add(my.msg);
        };
        my.apply = function() {
            my.current_status = "applying";
            if (my.apply_locally()) my.send();
        };
        my.ack = function() {
            my.current_status = "acknowledged";
            var t_map = my.make_t_map();
            for (i in t_map) {
                if (!t_map.hasOwnProperty(i)) continue;
                if (i === "none") {
                    telepathy_shared.apply_transaction(my.get_tid(), t_map.none);
                    continue;
                }
                if (t_map.hasOwnProperty(i)) telepathy_shared.get_object(i).ack_transaction(my.get_tid());
            }
            // unregister transaction
            delete telepathy_shared.transactions[my.get_tid()];
        };
        // RETURN EXPORTS
        var ret = {
            set_current_state: function(new_status) {my.current_status = new_status;},
            get_transformations: function() {return my.transformations;},
            ack: my.ack,
            rollback: my.rollback,
            exports: {
                add_transformation: function(t) {
                    if (my.current_status !== "init") {
                        throw_ex("add_transformation failed: transaction not in init state", t);
                    }
                    my.msg.transformations.push(t);
                },
                get_tid: my.get_tid,
                get_current_status: function() {return my.current_status+"";},
                apply: my.apply
            }
        };
        // register this transaction
        telepathy_shared.transactions[my.get_tid()] = ret;
        return ret;
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
        //      object fun, where:
        //      fucntion: fun.remote_transformation
        // }
        var object_id = spec.oid;
        if (!object_id) {
            // throw exception if connection is not alive yet
            if (telepathy_shared.counters.object_id.get_prefix().length == 0) throw_ex("telepathy not yet connected!", {spec: spec}); 
            object_id = telepathy_shared.counters.object_id.gensym();
        }
        // MODULE DATA:
        var my = {
            // unique ID of object
            oid: object_id,
            // callback manager, which is shared with the descendant object
            cb: spec.cb || mkCallbackManager({name: spec.type+"#"+object_id}),
        };
        // --- callbacks ---
        my.cb.set_internal_cb("server_message", function(message) {
            // update last server mesasge id:
            telepathy_shared.counters.last_server_message_id = message.header.id;
            // if this object needs to be deleted, do it here
            if (message.header.action === 'e' && message.body.command === 'delete') {
                // TODO: We should check if this object is attached to a list/dict
                // and throw an execption if so (if parent_node !== null).
                delete telepathy_shared.master_dict[my.oid];
                // fire callback
                my.cb.fire("delete", message);
                return;
            }
            return spec.fun.remote_transformation(message);
        });
        // --- transaction handling ---
        // call the specialized functions which do the actual work, then
        // fire the callbacks
        my.apply_transaction = function(tid, transformations) {
                spec.fun.transformations.apply(tid, transformations);
                my.cb.fire("apply_transaction", {tid: tid, transformations: transformations});
        };
        my.ack_transaction = function(tid) {
                spec.fun.transformations.ack(tid);
                my.cb.fire("ack_transaction", {tid: tid});
        };
        my.rollback_transaction = function(tid) {
                spec.fun.transformations.rollback(tid);
                my.cb.fire("rollback_transaction", {tid: tid});
        };

        // EXPORTS
        my.exports = {
                get_oid: function() {return my.oid+'';},
                get_type: function() {return spec.type+'';},
                set_cb: my.cb.set_user_cb,
            };
        my.transactions = {
                apply: my.apply_transaction,
                ack: my.ack_transaction,
                rollback: my.rollback_transaction
            };
        return {
            cb: my.cb,
            finish_object: function(desc) {
                copy_methods(my.exports, desc.exports);
                copy_methods(my.transactions, desc.transactions);
                // add object to master_dict
                telepathy_shared.master_dict[desc.exports.get_oid()] = desc;
                // if necessary, create transaction
                if (!spec.local_only || spec.local_only === false) {
                    spec.transaction = spec.transaction || mkTransaction();
                    spec.transaction.add_command(
                        telepathy_shared.get_object("meta_object").create_command(
                            'create', 
                            {type: spec.type, oid: my.oid}));
                    spec.transaction.apply();
                }
                return desc;
            },
            create_command: function(command, parameters) {
                return {
                    oid: my.oid,
                    command: command,
                    parameters: parameters
                };
            }
        };
    };

    // Metaobject is the telepathy object that gets invoked if
    // no object id is given for a transaction (eg: object creation).
    var mkMetaObject = function(spec) {
        var my = {
            base: mkTelepathyObject({
                    oid: "meta_object", 
                    type: "meta",
                    fun: {
                        transactions: {
                            ack: function(tid) {
                            },
                            rollback: function(tid) {
                            },
                            apply: function(tid, transformations) {
                            }
                        }
                    }
                    local_only: true
                });
        };
        var ret = {
            create_command: function(command, parameters) {
                return {
                    command: command,
                    parameters: parameters
                };
            },
            exports: {},
            transactions: {}
        };
        return my.base.finish_object(ret);
   };
 
    // Data module for telepathy data nodes.
    var mkData = function(spec) {
        // MODULE FIELDS:
        var my = {
            // pending set (only the last pending set is of interest):
            pending_local_transformation: null,
            // fun is used by the base class's callback for 'server_message'
            fun: {}
        };
        my.base = mkTelepathyObject({
            oid: spec.oid, 
            state: spec.state,
            type: 'data',
            fun: my.fun
        });

        my.fun.remote_transformation = function(message) {
            // If pending_local_transformation has been processed by
            // the server, then update local state. Otherwise do nothing.
            if (pending_local_transformation 
                && pending_local_transformation.header.id 
                    <= message.header.last_client_message_id) {
                    my.base.update_current_state(message.parameters.value, message);
            }
        };

        my.fun.set = function(val, overwrite) {
            if (val === my.base.get_current_state()) return;
            var t = my.base.create_transaction('m', 'set', {
                value: val,
                overwrite: overwrite
            }); 
            my.pending_local_transformation = t;
            telepathy_shared.execute(t);
        };
        
        var ret = {
            cb: my.base.cb,
            exports: {
                set: function(val) {my.fun.set(val,true);},
                safe_set: function(val) {my.fun.set(val,false);},
                get: function() {return clone(my.base.get_current_state());},
            },
            transactions: {}
        };
        return return_telepathy_object(spec, my.base, ret);
   };

    // Dict module for telepathy dict nodes.
    var mkDict = function(spec) {
        // MODULE FIELDS:
        var my = {
            fun: {
                to_internal_key: function(v) {return "entry_" + v;},
                from_internal_key: function(v) {return v.slice(6);},
            },
            pending_transactions: {}
        }
        my.base = mkTelepathyObject({
            oid: spec.oid, 
            type: 'dict',
            fun: my.fun,
        });
        my.fun.existing_parent_ex = function(val) {
            throw_ex("Telepathy object cannot be attached to dictionary while attached to another object");
        };
        my.fun.do_set = function(key, child_oid) {
            // set newly acquired child's parent
            // check if val's parent is null.
            // if not, throw excpetion.
            if (telepathy_shared.master_dict[child_oid].exports.get_parent() !== null) {
                my.fun.existing_parent_ex(child_oid);
            }
            telepathy_shared.master_dict[child_oid].set_parent(my.base.exports.get_oid());
            var newstate = clone(my.base.get_current_state());
            newstate[my.fun.to_internal_key(key)] = child_oid;
            return newstate;
        };
        my.fun.do_remove = function(key) {
            var internal_key = my.fun.to_internal_key(key);
            // update parent of newly removed item:
            telepathy_shared.master_dict[my.base.get_current_state()[internal_key]].set_parent(null);
            var newstate = clone(my.base.get_current_state());
            if (internal_key in newstate) {
                delete newstate[internal_key];
            }
            return newstate;
        };
        my.fun.set = function(key, val, overwrite) {
            var newstate = my.fun.do_set(key, val.get_oid()); 
            // create 'set' transformation
            var transformation = my.base.create_transaction('m', 'set', {
                key: key,
                child_oid: val.get_oid(),
                overwrite: overwrite
            }); 
            my.base.update_current_state(newstate, transformation);
        };
        my.fun.get = function(key) {
            var k = my.fun.to_internal_key(key);
            var st = my.base.get_current_state();
            if ((k in st) &&
                (st[k] in telepathy_shared.master_dict)) {
                return telepathy_shared.master_dict[st[k]].exports;
            } else return undefined; // throw exception instead?
        };
        my.fun.remove = function(key, force) {
            // TODO: don't delete from master_dict, as safe_remove may fail.
            // we'll worry about this later...
            var transformation1 = my.base.create_transaction('m', 'remove', {
                    key: key,
                    force: force 
                }
            );
            var newstate = my.fun.do_remove(key);
            my.base.update_current_state(newstate, transformation1);
            var transformation2 = my.base.create_transaction('e', 'delete', {
                    key: key,
                }
            );
            // execute the delete on the element we just removed.
            telepathy_shared.execute(transformation2);
        };
        my.fun.keys = function() {
            var keys = [];
            var st = my.base.get_current_state();
            var k;
            for (k in st) {
                if (st.hasOwnProperty(k)) keys.push(my.fun.from_internal_key(k));
            }
            return keys;
        };
        my.base.cb.set_internal_cb("delete", function() {
            // delete all children as well
            var keys = my.fun.keys();
            for (var k = 0; k < keys.length; k++) {
                var child_oid = my.base.cb.get_current_state()[k];
                telepathy_shared.master_dict[child_oid].set_parent(null);
                telepathy_shared.master_dict[child_oid].cb.fire_async("delete");
            }
        });
       my.fun.remote_transformation = function(message) {
       // dict-s can receive two transformations:
       // set - due to another client's set, or the ack/nack this client's (safe)set
       // remove - remove an element from the dictionary
            var current_key = message.body.parameters.key;
            var newstate;
            switch(message.body.command) {
                case "set":
                    if (my.pending_transactions[my.fun.to_internal_key(current_key)].length == 0) {
                        newstate = my.fun.do_set(current_key, message.body.parameters.child_oid);
                    } else {
                        // there are pending transformations on the current key
                    }
                    break;
                case "remove":
                    if (my.pending_transactions[my.fun.to_internal_key(current_key)].length == 0) {
                        newstate = my.fun.do_remove(current_key, message);
                    } else {
                        // there are pending transformations on the current key
                    }
                    break;
                default:
                    throw_ex( "bad remote transformation on dict",
                        {transformation: message}
                    );
            };
            my.base.update_current_state(newstate, message);
       };
            
       var ret = {
           cb: my.base.cb,
           exports: {
                get: my.fun.get,
                set: function(key,val) {my.fun.set(key,val,true);},
                safe_set: function(key,val) {my.fun.set(key,val,false);},
                remove: function(key,val) {my.fun.remove(key,true);},
                safe_remove: function(key,val) {my.fun.remove(key,false);},
                keys: my.fun.keys, 
                size: function() {return my.base.get_current_state().length;}
           }
       };
       // register newly created object
       return my.base.finish_object(ret);
   };



    //  -------------------------
    //  - MAIN TELEPATHY MODULE -
    //  -------------------------
    var mkMain = function() {

        // MODULE FIELDS
        var my = {
            // websocket object
            connection: null, 
            cb: mkCallbackManager({name:"main telepathy object"}),
            state_list: {
                INACTIVE: 0,
                AUTH: 1,
                CONNECTED: 2
            },
            state: 0,
        };
        my.cb.debug(true); // for DEVEL: debug main CB manager.

        my.create_object = function(msg) {
            var new_obj = null;
            switch (msg.type) {
            case "data":
                new_obj = mkData({oid: msg.oid, state: msg.state});
                break;
            case "dict":
                new_obj = mkDict({oid: msg.oid});
                break;
            case "list":
                break;
                new_obj = mkList({oid: msg.oid});
            default:
                throw_ex ("Unknown data type: " + msg.type);
            };
            return new_obj;
        };

        my.transform_object = function(msg) {
            if (!(msg.oid in telepathy_shared.master_dict)) throw_ex("Unknown object id: " + msg.oid);
            return telepathy_shared.master_dict[msg.oid].cb.fire("server_message", msg);
        };

        my.websocket_safe_send = function(data) {
            retry_until(
                // condition
                function() {
                    return telepathy_shared.connection.readyState == 1;
                },
                // on success
                function() {
                    telepathy_shared.connection.send(data);
                }
            );
        };

        // Callback functions
        my.handle_incoming_message = [];
        my.handle_incoming_message[my.state_list.AUTH] = function(msg_obj) {
            // TODO: respond to authentication challenge
            // Right now, there's no authentication, we assume it succeeded:
            if ("client_id" in msg_obj) {
                telepathy_shared.config.client_id = msg_obj.client_id;
                telepathy_shared.counters.object_id.set_prefix(msg_obj.client_id + ".");
                my.state = my.state_list.CONNECTED;
                my.cb.fire("connection_state_change", [my.state]);
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
                throw_ex ("Unknown message type received");
            }
        };

        // handle incoming message from server
        my.cb.set_internal_cb("ws_onopen", function() {
            retry_until(
                // condition
                function() {
                    return telepathy_shared.connection.readyState == 1;
                },
                // on success
                function() {
                    telepathy_shared.connection.send("client-connected");
                    my.state = my.state_list.AUTH;
                }
            );
        });
        my.cb.set_internal_cb("ws_onmessage", function(msg) {
            console.log(msg.timeStamp + " - received: '" + msg.data + "'");
            var t = null;
            try {
                t = JSON.parse(msg.data);
            } catch (e) {
                console.log(e.message + "input: " + msg);
            }
            // apply server transformation
            if (t !== null)  {
                my.handle_incoming_message[my.state](t);
            }
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

        // CREATE root element and meta-object
        my.root = mkDict({oid: '0.0', local_only: true});
        telepathy_shared.master_dict.meta_object = mkMetaObject({cb: my.cb});

        telepathy_shared.create_message = function(message_type) {
            return {
                header: {
                    id: telepathy_shared.counters.client_message_id.getint(),
                    last_server_message_id: telepathy_shared.counters.last_server_message_id,
                    message_type: message_type,
                    ts: {
                        created:  get_timestamp()
                    }
                }
            };
        };
        telepathy_shared.outqueue = [];
        telepathy_shared.outqueue.add = function(t) {
            t.header.ts.queued = get_timestamp();
            telepathy_shared.outqueue.push(t);
        };
        telepathy_shared.outqueue.flush = function() {
            while (telepathy_shared.outqueue.length > 0) {
                var t = telepathy_shared.outqueue.shift();
                t.header.ts.sent = get_timestamp();
                my.websocket_safe_send(JSON.stringify(t));
            }
        };
 
        // EXPORTS:
        // master dictionary manipulation (for internal use only):
        var exports = {
            // ---- exports methods ----
           connect: function() {
                telepathy_shared.connection=new WebSocket("ws://" + telepathy_shared.config.ws_url);
                // Set callbacks for websocket events.
                telepathy_shared.connection.onopen=my.cb.wrap_signal("ws_onopen");
                telepathy_shared.connection.onmessage=my.cb.wrap_signal("ws_onmessage");
                telepathy_shared.connection.onclose=my.cb.wrap_signal("ws_onclose");
                telepathy_shared.connection.onerror=my.cb.wrap_signal("ws_onerror");
                },
           disconnect: function() {},
           root: my.root.exports,
           set_cb: my.cb.set_user_cb,
           make_data: function(initial_value) {return (mkData({state: initial_value})).exports;},
           make_dict: function() {return (mkDict()).exports;}
           // make_list
        };
        // RETURN EXPORTS
        return exports;
    }; // END of module Main

    // ---- Telepathy object creation ----
    // Now that we have defined all the modules we need,
    // we create a telepathy instance with mkMain, then
    // return a list of exports
    telepathy_shared.counters.object_id = mkCounter();
    telepathy_shared.counters.client_message_id = mkCounter();
    telepathy_shared.counters.last_server_msg_id = 0;
    var main = mkMain();
    return main;
}; // END mkTelepathy
