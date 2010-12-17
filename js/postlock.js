/*
  postlock.js - javascript client API for postlock.
  author: neumark
*/
var debugvar;
// mkPostlock: constructor for the main postlock module.
var mkPostlock = function (websocket_url) {

    // variable containing for all of postlock
    var PL = {
        config: {
            ws_url: websocket_url 
        },
        counters: {},
        objects: {
            get: function (oid) {
                return PL.objects[oid];
            },
            set: function (oid, obj) {
                PL.objects[oid] = obj;
            },
        },
        transactions: function() {
            var my = {
                s: {},
                c: {},
            };
            return {
                save: function (t) {
                    my[t.exports.get_tid().charAt(0)][t.exports.get_tid()]= t;
                },
                get: function(tid) {return my[tid.charAt(0)][tid];},
                remove: function(tid) {
                    console.log("deleteing " + tid);
                    delete my[tid.charAt(0)][tid];
                },
                list_acked_remote: function() {
                    var i, ackd = [];
                    for (i in my.s) {
                        if (my.s.hasOwnProperty(i) && my.s[i].exports.get_current_status() === "acknowledged") ackd.push(i);
                    }
                    return ackd;
                }
            };
        }(),
        modules: {},
        outqueue: [],
        // ---------------------
        // - Utility functions -
        // ---------------------
        util: {
            require_transaction: function (fun, t, t_spec) {
                // there may be a transaction in the arguments
                var apply_t = false;
                if (!PL.util.is_transaction(t)) {
                    apply_t = true;
                    t = PL.modules.mkTransaction(t_spec);
                }
                fun(t);
                if (apply_t) t.apply();
                return t;
            },
            // from: http://www.javascriptkit.com/javatutors/arraysort.shtml
            numeric_sort: function (a, b) {
                return (a - b);
            },
            create_message: function (message_type) {
                return {
                    header: {
                        id: PL.counters.client_message_id.getint(),
                        last_server_message_id: PL.counters.last_server_message_id,
                        message_type: message_type,
                        ts: {
                            created:  PL.util.get_timestamp()
                        }
                    }
                };
            },
            // Assembles ack's for applied remote transactions
            // based on the contents of PL.transactions.remote
            prepare_acks_of_remote_transactions: function () {
                var i, acknowledged_remote_transactions = PL.transactions.list_acked_remote();
                for (i=0; i < acknowledged_remote_transactions.length; i++) PL.transactions.remove(acknowledged_remote_transactions[i]);
                return acknowledged_remote_transactions;
            },
            // From Javascript - The Good Parts
            is_array: function (value) {
                return value &&
                    typeof value === 'object' &&
                    value.constructor === Array;
            },
            args2array: function (args) {
                args = args || [];
                return Array.prototype.slice.apply(args);
            },
            throw_ex: function (message, extra_data) {
                var e = new Error(message);
                if (extra_data) e.data = extra_data;
                throw e;
            },
            is_transaction: function (t) {
                return typeof t === "object" && typeof t.get_tid === "function";
            },
            // from: http://stackoverflow.com/questions/122102/what-is-the-most-efficient-way-to-clone-a-javascript-object
            clone: function (from) {
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
            },
            copy_methods: function (src, dest) {
                var i;
                for (i in src) dest[i] = src[i];
                return dest;
            },
            get_timestamp: function () { return +new Date(); },
            retry_until: function (condition, cb_success, cb_failure, num_retries, timeout) {
                var ms = timeout || 10, 
                    retries = num_retries || 5,
                    on_failure = cb_failure || function () {PL.util.throw_ex("retry_until failed", {args: PL.util.args2array(arguments)});},
                    do_try = function (r) {
                        if (condition()) return cb_success();
                        else if (retries === 0) on_failure();
                        else {
                            setTimeout(function () {do_try(r-1);},ms);
                        }
                    };
                do_try(retries);
            },
            // from: http://stackoverflow.com/questions/1068834/object-comparison-in-javascript
            equals: function (t,x) {
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
            }
        } // end PL.util
    }; // end PL

    //  --------------------
    //  - INTERNAL MODULES -
    //  --------------------

    PL.modules.mkCounter = function (initial_val) {
    // From Javascript - The Good Parts
    // Produce an object that produces unique strings. A
    // unique string is made up of two parts: a prefix
    // and a sequence number. The object comes with
    // methods for setting the prefix and sequence
    // number, and a gensym method that produces unique
    // strings.
        var prefix = '';
        var seq = initial_val || 0;
        return {
            get_prefix: function () {return prefix + '';},
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
            getint: function () {
                var result = seq + 0;
                seq += 1;
                return result;
            }
        };
    };

    // CallbackManager: Module responsible for getting/setting/calling
    // callback functions.
    PL.modules.mkCallbackManager = function (spec) {
        // format of spec:
        // {
        //      string name (optional)
        // }
        // MODULE PRIVATE DATA:
        var my = {
            async_delay: 4,
            id: spec.name || "[unnamed object]",
            // debug state:
            debug: true,
            // callbacks holds 'signal' -> handler() entries.
            // postlock.js internal callbacks
            internal_cb: {},
            // user-defined callbacks
            user_cb: {},
            // default callback function:
            default_callback: function (signal, args) {
                if (my.debug) {
                    console.log("Callback manager for " + my.id + " received signal " + signal + " with arguments " + args + ".");
                }
                return {fire_user_cb: true};
            },
            fire: function (signal, args) {
                // apply the internal callback first
                var result = {};
                var args_array = PL.util.args2array(args);
                var first_cb;
                if (signal in my.internal_cb) first_cb = my.internal_cb[signal];
                else {
                    first_cb = function(a) {return my.default_callback(signal, a);};
                }
                result.internal_result = first_cb.apply(this, args_array) || {fire_user_cb: true};
                // apply the user-defined cb if one exists
                if (result.internal_result.fire_user_cb && 
                    (signal in my.user_cb)) {
                    if (result.internal_result.user_cb_data) args_array.push(result.internal_result.user_cb_data);
                    result.user_result = my.user_cb[signal].apply(this,args_array);
                }
                return result;
            }
        }; // END my    

        // MODULE EXPORPL
        return {
            debug: function (new_debug_state) {my.debug = new_debug_state;},
            set_internal_cb: function (signal, handler) {my.internal_cb[signal] = handler;},
            get_internal_cb: function (signal) {return my.internal_cb[signal];},
            set_user_cb: function (signal, handler) {my.user_cb[signal] = handler;},
            get_user_cb: function (signal) {return my.user_cb[signal];},
            remove_internal_cb: function (signal) { delete my.internal_cb[signal];},
            remove_user_cb: function (signal) { delete my.user_cb[signal];},
            fire: my.fire, 
            fire_async: function (signal, args) {setTimeout(function () {my.fire(signal, args);}, my.async_delay);},
            // used when we need a function which fires a signal. For example,
            // ajax-based callbacks or ws calls.
            wrap_signal: function (signal) {
                return function () {my.fire(signal, arguments);}
            } 
        };
    };

   
    // Creates a transaction, which can be applied locally and sent to the
    // server.
    PL.modules.mkTransaction = function (spec) {
        spec = spec || {};
        var my = {
            msg: PL.util.create_message('t'), // a message of type t = transaction 
            // transaction status-es:
            // init - transaction is under composition (initial state)
            // applying
            // applied - the transaction has been locally applied, ACK from server pending
            // acknowledged - the transaction has been acknowledged
            // aborting
            // aborted - the transaction has been rolled back
            current_status: "init",
            is_remote: spec.is_remote || false
        };

        my.msg.transformations = [];

        my.get_tid = function () {return ((my.is_remote)?'s':'c') + my.msg.header.id;};

        my.make_t_map = function () {
            // rearrange transformation list into an
            // object id -> list of commands map.
            var i, oid, t_map = {};
            for (i = 0; i < my.msg.transformations.length; i++) {
                oid = my.msg.transformations[i].parameters.oid || "meta_object";
                if (!(oid in t_map)) t_map[oid] = [];
                t_map[oid].push(my.msg.transformations[i]);
            }
            return t_map;
        };

        my.rollback = function () {
            my.current_status = "aborting";
            var t_map = arguments[0] || my.make_t_map();
            var e = arguments[1] || {message: "no reason given", data: {}};
            console.log("starting rollback of transaction "+my.get_tid()+"; reason: "+e.message);
            var i;
            for (i in t_map) {
                if (!t_map.hasOwnProperty(i)) continue;
                PL.objects.get(i).cb.fire('rollback_transaction',[my.get_tid()]);
            }
            my.current_status = "aborted";
            // unregister transaction
            delete PL.transactions[my.get_tid()];
        };

        my.apply_locally = function () {
            var i, t_map = my.make_t_map();
            try {
                for (i in t_map) {
                    if (!t_map.hasOwnProperty(i)) continue;
                    // 'apply_transaction' handler will throw ex if transaction cannot be applied.
                    PL.objects.get(i).cb.fire("apply_transaction", [my.get_tid(), t_map[i]]);
                }
                my.current_status = "applied";
                return true;
            } catch (e) {
                my.rollback(t_map, e);
            }
            return true;
        };
        my.send = function () {
            PL.outqueue.add_transaction(my.msg);
        };
        my.apply = function () {
            my.current_status = "applying";
            if (my.apply_locally()) {
                if (my.is_remote === true) my.current_status = "acknowledged";
                else my.send();
                return true;
            }
            return false;
        };
        my.ack = function () {
            my.current_status = "acknowledged";
            var t_map = my.make_t_map();
            for (i in t_map) {
                if (!t_map.hasOwnProperty(i)) continue;
                PL.objects.get(i).cb.fire("ack_transaction", [my.get_tid()]);
            }
            // unregister transaction
            delete PL.transactions.local[my.get_tid()];
        };
        // RETURN EXPORPL
        var ret = {
            set_current_state: function (new_status) {my.current_status = new_status;},
            get_transformations: function () {return my.msg.transformations;},
            ack: my.ack,
            rollback: my.rollback,
            exports: {
                is_remote: function () {return my.is_remote === true;},
                add_transformation: function (t) {
                    if (my.current_status !== "init") {
                        PL.util.throw_ex("add_transformation failed: transaction not in init state", t);
                    }
                    my.msg.transformations.push(t);
                },
                get_tid: my.get_tid,
                get_current_status: function () {return my.current_status+"";},
                apply: my.apply
            }
        };
        // register this transaction
        PL.transactions.save(ret);
        return ret.exports;
    };

    // TelepathicObject: All postlock datatypes (dicts, lists, and data)
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
    PL.modules.mkPostlockObject = function (spec) {
        // spec format: {
        //      string oid: (optional)
        //      string type: "dict" || "list" || "data" (mandatory)
        //      object fun, where:
        //      fucntion: fun.remote_transformation
        // }
        var object_id = spec.oid;
        if (!object_id) {
            // throw exception if connection is not alive yet
            if (PL.counters.object_id.get_prefix().length == 0) PL.util.throw_ex("postlock not yet connected!", {spec: spec}); 
            object_id = PL.counters.object_id.gensym();
        }
        // MODULE DATA:
        var my = {
            // unique ID of object
            oid: object_id,
            // callback manager, which is shared with the descendant object
            cb: spec.cb || PL.modules.mkCallbackManager({name: spec.type+"#"+object_id}),
        };
        // --- callbacks ---
        my.cb.set_internal_cb("server_message", function (message) {
            // update last server mesasge id:
            PL.counters.last_server_message_id = message.header.id;
            // if this object needs to be deleted, do it here
            if (message.header.action === 'e' && message.body.command === 'delete') {
                // TODO: We should check if this object is attached to a list/dict
                // and throw an execption if so (if parent_node !== null).
                delete PL.objects[my.oid];
                // fire callback
                my.cb.fire("delete", message);
                return;
            }
            // TODO: fire event instead!
            return spec.fun.remote_transformation(message);
        });
        my.cb.set_internal_cb("apply_transaction", function(tid, transformations) {
            var i, results = [];
            for (i = 0; i < transformations.length; i++) {
                results.push(my.cb.fire(transformations[i].command, [tid, transformations[i].parameters]));
            }
            return results;
        });
 
        // EXPORPL
        my.exports = {
                get_oid: function () {return my.oid+'';},
                get_type: function () {return spec.type+'';},
                set_cb: my.cb.set_user_cb,
            };
        return {
            cb: my.cb,
            exports: my.exports,
            finish_object: function (desc, parameters, transaction) {
                desc.exports = desc.exports || {};
                PL.util.copy_methods(my.exports, desc.exports);
                // add object to objects
                PL.objects[desc.exports.get_oid()] = desc;
                // if no transaction was specified or transaction is local,
                // add 'create' transformation.
                if (!transaction.is_remote()) {
                    parameters = parameters || {};
                    parameters.type = spec.type;
                    parameters.oid = my.oid;
                    transaction.add_transformation(
                        PL.objects.get("meta_object").create_transformation(
                            'create', parameters));
                }
                return desc;
            },
            create_transformation: function (command, parameters, transaction) {
                parameters = parameters || {};
                parameters.oid = my.oid;
                return PL.util.require_transaction(
                    function (t) {
                        t.add_transformation({
                            command: command,
                            parameters: parameters});},
                            transaction);}
        };
    };

    // Metaobject is the postlock object that gets invoked if
    // no object id is given for a transaction (eg: object creation).
    PL.modules.mkMetaObject = function (spec, transaction) {
        var my = {
            unacked_objects: {},
            pending_transactions: {},
            base: PL.modules.mkPostlockObject({
                    transaction: spec.transaction,
                    oid: "meta_object", 
                    type: "meta",
                    fun: {
                        transactions: {
                            ack: function (tid) {
                            },
                            rollback: function (tid) {
                            },
                            apply: function (tid, transformations) {
                                // objects are added to objects when they are created,
                                // so apply simply adds them to unacked_objects if tid is local.
                                if (!PL.transactions.get(tid).is_remote()){ 
                                    for (var i = 0; i < transformations.length; i++) {
                                        // if the transaction is local, add newly created object to
                                        // pending objects
                                        if (transformations[i].command === "create") {
                                            my.unacked_objects[transformations[i].oid] = tid;
                                            if (!(tid in my.pending_transactions)) my.pending_transactions[tid] = [];
                                            my.pending_transactions[tid].push(transformations[i].oid);
                                        }
                                    }
                                }
                            }
                        }
                    }
                })
        };
        var ret = {
            create_transformation: function (command, parameters) {
                return {
                    command: command,
                    parameters: parameters
                };
            },
            exports: {},
            transactions: {}
        };
        PL.util.require_transaction(
            function (t) {
                my.base.finish_object(ret, {}, t)},
            transaction);
        return ret;
   };
 
    // Data module for postlock data nodes.
    PL.modules.mkData = function (spec, transaction) {
        // MODULE FIELDS:
        spec = spec || {};
        spec.type = 'data';
        var my = {
            base: PL.modules.mkPostlockObject(spec),
            pending_set: undefined, // pending transaction {tid: tid, value: value}
            value: spec.value,
            // fun is used by the base class's callback for 'server_message'
            fun: {}
        };

        my.fun.get = function() {
            if (my.pending_set) return my.pending_set.value;
            return my.value;
        };

        my.fun.do_set = function (val, force, transaction) {
            var t = my.base.create_transformation(
                'set', {value: val, force: force},
                transaction);
            my.pending_set = {tid: t.get_tid(), value: val};
            return t;
        };

        my.base.cb.set_internal_cb('ack_transaction', function(tid) {
            if (my.pending_set.tid === tid) {
                my.value = my.pending_set.value;
                my.pending_set = undefined;
            } // If condition is false, then an overridden set has been ack'd.
        });

        my.base.cb.set_internal_cb('set', function(tid, parameters) {
            var user_cb_data = {old_value: my.fun.get(), new_value: parameters.value};
            if (PL.transactions.get(tid).exports.is_remote()) {
                // remote case - only act if there are pending local set(s).
                if (!my.pending_set) {
                    my.value = parameters.value;
                    return {fire_user_cb: true, user_cb_data: user_cb_data};
                }
            } else {
                // local case - apply set & register pending transaction.
                my.pending_set = {tid: tid, value: parameters. value};
                return {fire_user_cb: true, user_cb_data: user_cb_data};
            }
            // Do nothing
            return {fire_user_cb: false};
        });
       
        var ret = {
            cb: my.base.cb,
            exports: {
                set: function (val, t) {my.fun.do_set(val,true, t);},
                safe_set: function (val, t) {my.fun.do_set(val,false, t);},
                get: my.fun.get,
            },
        };

        PL.util.require_transaction(
            function (t) {
                my.base.finish_object(ret, {value: my.value},t)},
            transaction);
        return ret;
   };

    // Dict module for postlock dict nodes.
    PL.modules.mkDict = function (spec, transaction) {
        // MODULE FIELDS:
        spec = spec || {};
        spec.type = 'dict';
        var my = {
            base: PL.modules.mkPostlockObject(spec),
            fun: {
                to_internal_key: function (v) {return "entry_" + v;},
                from_internal_key: function (v) {return v.slice(6);},
            },
            // acknowledged key, value entries live in data
            // format: key -> value
            data: {},
            // unacked remove's are stored in locally_removed
            // local sets after local removes result in the entry
            // for oid being removed.
            // format: key -> pending remove transaction's tid
            pending_remove: {},
            // pending updates contain sets on keys which exist
            // on the server, but where the set command has not
            // been acknoledged yet
            // format: key -> {tid, last pending set transform on key's parameters}
            pending_update: {},
            // pending_new_entries are sets on new keys which
            // have not yet been acknowledged
            // format: key -> {tid, last pending set transformation on key's parameters}
            pending_new: {}
        };
        my.fun.do_set = function (key, child_oid, force, transaction) {
            return my.base.create_transformation(
                'set', {
                        key: key,
                        value: child_oid,
                        force: force});};
        my.fun.do_remove = function (key, force, transaction) {
            return my.base.create_transformation(
                'remove', {
                        key: key,
                        force: force});};
        my.fun.keys = function () {
            var i, keys = {}, key_list = [];
            // add any ack'd data
            for (i in my.data) if (my.data.hasOwnProperty(i)) keys[i] = true;
            // add locally pending new keys
            for (i in my.pending_new) if (my.pending_new.hasOwnProperty(i)) keys[i] = true;
            // remove pending removes
            for (i in my.pending_remove) if (my.pending_remove.hasOwnProperty(i)) delete keys[i];
            // add all keys to list
            for (i in my.keys) if (my.keys.hasOwnProperty(i)) key_list.push(my.fun.from_internal_key(i));
            key_list.sort();
            return key_list;
        };

        my.fun.bad_key = function(key) {return undefined};

        my.fun.has_pending_set = function(int_key) {return (int_key in my.pending_update) || (int_key in my.pending_new);};

        my.fun.get = function(key) {
            var int_key = my.fun.to_internal_key(key);
            if (int_key in my.pending_remove) return my.fun.bad_key(key);
            if (int_key in my.pending_new) return PL.objects.get(my.pending_new[int_key].parameters.value);
            if (int_key in my.pending_update) return PL.objects.get(my.pending_update[int_key].parameters.value);
            if (int_key in my.data) return PL.objects.get(my.data[int_key]);
            return my.fun.bad_key(key);
        };

        my.fun.has_key = function(key) {
            var int_key = my.fun.to_internal_key(key);
            return !(int_key in my.pending_remove) &&
                ((int_key in my.data || int_key in my.pending_new));};

        my.base.cb.set_internal_cb("remove", function (tid, parameters) {
            var int_key = my.fun.to_internal_key(parameters.key), fire_user_cb = true;
            if (PL.transactions.get(tid).is_remote()) {
                // remote case - delete from my.data if there is no pending set on key
                if (!my.fun.has_pending_set(int_key) && (int_key in my.data)) delete my.data[int_key];
                // only execute user_cb if there is no pending local delete for key
                fire_user_cb = !(int_key in my.pending_remove);
            } else {
                // local case
                my.pending_remove[int_key] = tid;
            }
            return {fire_user_cb: fire_user_cb};
        });

        my.base.cb.set_internal_cb("set", function (tid, parameters) {
            var int_key = my.fun.to_internal_key(parameters.key), user_cb_data, fire_user_cb = true;
            if (my.fun.has_key(parameters.key)) user_cb_data.old_value = my.fun.get(parameters.key);
            user_cb_data.new_value = parameters.value;

            if (PL.transactions.get(tid).is_remote()) {
                // remote case
                my.data[int_key] = parameters.value;
                // fire user cb if no pending local sets exist for key
                fire_user_cb = !my.fun.has_pending_set(int_key);
            } else {
                // local case
                // undo pending removal of key (if applicable).
                if (int_key in my.pending_remove) delete my.pending_remove[int_key];
                if (my.fun.has_key(parameters.key)) {
                    // pending update
                    my.pending_update[int_key] = {tid: tid, parameters: parameters};
                } else {
                    // pending new
                    my.pending_new[int_key] = {tid: tid, parameters: parameters};
                }
            }
            return {fire_user_cb: fire_user_cb, user_cb_data: user_cb_data};
        });

        my.base.cb.set_internal_cb('ack_transaction', function(tid) {
            // TODO
            });


       var ret = {
           cb: my.base.cb,
           exports: {
                get: my.fun.get,
                set: function (key,val,t) {my.fun.do_set(key,val,true,t);},
                safe_set: function (key,val,t) {my.fun.do_set(key,val,false,t);},
                remove: function (key,val,t) {my.fun.do_remove(key,true,t);},
                safe_remove: function (key,val,t) {my.fun.do_remove(key,false,t);},
                keys: my.fun.keys, 
                size: function () {return my.fun.keys().length;},
                has_key: my.fun.has_key           
           }
       };
       // register newly created object
       PL.util.require_transaction(
            function (t) {
                my.base.finish_object(ret, {}, t)},
            transaction);
       return ret;
   };



    //  -------------------------
    //  - MAIN TELEPATHY MODULE -
    //  -------------------------
    PL.modules.mkMain = function () {

        // MODULE FIELDS
        var my = {
            // websocket object
            connection: null, 
            cb: PL.modules.mkCallbackManager({name:"main postlock object"}),
            state_list: {
                INACTIVE: 0,
                AUTH: 1,
                CONNECTED: 2
            },
            state: 0,
        };
        my.cb.debug(true); // for DEVEL: debug main CB manager.

        my.transform_object = function (msg) {
            if (!(msg.oid in PL.objects)) PL.util.throw_ex("Unknown object id: " + msg.oid);
            return PL.objects[msg.oid].cb.fire("server_message", msg);
        };

        my.websocket_safe_send = function (data) {
            PL.util.retry_until(
                // condition
                function () { return PL.connection.readyState == 1;},
                // on success
                function () {
                    console.log("just sent " + data);
                    PL.connection.send(data);
                }
            );
        };

        // Callback functions
        my.handle_incoming_message = [];
        my.handle_incoming_message[my.state_list.AUTH] = function (msg_obj) {
            // TODO: respond to authentication challenge
            // Right now, there's no authentication, we assume it succeeded:
            if ("client_id" in msg_obj) {
                PL.config.client_id = msg_obj.client_id;
                PL.counters.object_id.set_prefix(msg_obj.client_id + ".");
                my.state = my.state_list.CONNECTED;
                my.cb.fire("connection_state_change", [my.state]);
            }
            else console.error("expected client id, received " + JSON.stringify(msg_obj));
        };
        my.handle_incoming_message[my.state_list.CONNECTED] = function (msg_obj) {
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
                PL.util.throw_ex ("Unknown message type received");
            }
        };

        // handle incoming message from server
        my.cb.set_internal_cb("ws_onopen", function () {
            PL.util.retry_until(
                // condition
                function () {
                    return PL.connection.readyState == 1;
                },
                // on success
                function () {
                    PL.connection.send("client-connected");
                    my.state = my.state_list.AUTH;
                }
            );
        });
        my.cb.set_internal_cb("ws_onmessage", function (msg) {
            console.log(msg.timeStamp + " - received: '" + msg.data + "'");
            var t = null;
            try {
                t = JSON.parse(msg.data);
            } catch (e) {
                console.error("parse error: " + e.message + " input: " + msg);
            }
            // apply server transformation
            if (t !== null)  {
                my.handle_incoming_message[my.state](t);
            }
            else console.log("failed to parse incoming message '" + msg+"'");
        });
        my.cb.set_internal_cb("ws_onclose", function () {
            // TODO: attempt to re-open the connection
            var a = PL.util.args2array(arguments);
            console.log("ws_onclose fired, arguments: " + a);
        });
        my.cb.set_internal_cb("ws_error", function () {
            // TODO: attempt to handle the error
            console.log("ws_error fired, arguments: " + PL.util.args2array(arguments));
        });

        // CREATE root element and meta-object
        // my.root is created in a remote transaction so the server doesn't receive
        // the transaction.
        var mk_root_t = PL.modules.mkTransaction({is_remote: true});
        PL.modules.mkMetaObject({cb: my.cb}, mk_root_t);
        my.root = PL.modules.mkDict({oid: '0.0'}, mk_root_t);
        mk_root_t.apply();
        // meta-object and root's don't need to be acknowledged, so we delete these
        // transactions:
        

        PL.outqueue.add_transaction = function (t) {
            t.header.ts.queued = PL.util.get_timestamp();
            t.response = {
                ack: PL.util.prepare_acks_of_remote_transactions()
            };
            PL.outqueue.push(t);
            PL.outqueue.flush();
        };
        PL.outqueue.flush = function () {
            while (PL.outqueue.length > 0) {
                var t = PL.outqueue.shift();
                t.header.ts.sent = PL.util.get_timestamp();
                my.websocket_safe_send(JSON.stringify(t));
            }
        };
 
        // EXPORPL:
        // master dictionary manipulation (for internal use only):
        var exports = {
            // ---- exports methods ----
           connect: function () {
                PL.connection=new WebSocket("ws://" + PL.config.ws_url);
                // Set callbacks for websocket events.
                PL.connection.onopen=my.cb.wrap_signal("ws_onopen");
                PL.connection.onmessage=my.cb.wrap_signal("ws_onmessage");
                PL.connection.onclose=my.cb.wrap_signal("ws_onclose");
                PL.connection.onerror=my.cb.wrap_signal("ws_onerror");
                return exports;
                },
           disconnect: function () {},
           root: my.root.exports,
           set_cb: my.cb.set_user_cb,
           make_data: function (initial_value, transaction) {return (PL.modules.mkData({value: initial_value}, transaction)).exports;},
           make_dict: function (transaction) {return PL.modules.mkDict({}, transaction).exports;},
           make_transaction: function () {return PL.modules.mkTransaction().exports;}
           // make_list
        };
        // RETURN EXPORPL
        return exports;
    }; // END of module Main

    // ---- Postlock object creation ----
    // Now that we have defined all the modules we need,
    // we create a postlock instance with mkMain, then
    // return a list of exports
    PL.counters.object_id = PL.modules.mkCounter();
    PL.counters.client_message_id = PL.modules.mkCounter();
    PL.counters.last_server_msg_id = 0;
    var main = PL.modules.mkMain();
    return main;
}; // END mkPostlock
