/*
  telepathy.js - javascript client API for telepathy.
  author: neumark
*/

// mkTelepathy: constructor for the main telepathy module.
var mkTelepathy = function(connection_params) {

    //  ---------------------------------
    //  - UTILITY FUNCTIONS AND OBJECTS -
    //  ---------------------------------
    
    var args2array = function(args) {
        args = args || [];
        else return Array.prototype.slice.apply(args);
    }

    var clone_state = function(state) {
        // NOTE: this is the worst solution performance-wise!
        return JSON.decode(JSON.encode(state));
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

    var object_id_generator = mkCounter();

    //  --------------------
    //  - INTERNAL MODULES -
    //  --------------------

    // CallbackManager: Module responsible for getting/setting/calling
    // callback functions.
    var mkCallbackManager = function(object_id) {
    
        // MODULE FIELDS:
        var id = object_id || "[unnamed object]";
        // debug state:
        var debug = false;
        // callbacks holds 'signal' -> handler() entries.
        // telepathy.js internal callbacks
        var internal_cb = {};
        // user-defined callbacks
        var user_cb = {};

        // default callback function:
        var default_callback = function(signal, args) {
            if (debug) {
                console.log("Callback manager for " + id + " received signal " + signal + " with arguments " + args + ".");
            }
        }

        // RETURN EXPORTS
        var ret = {
            debug: function(new_debug_state) {debug = new_debug_state;},
            set_internal_cb: function(signal, handler) {internal_cb[signal] = handler;},
            set_user_cb: function(signal, handler) {user_cb[signal] = handler;},
            remove_internal_cb: function(signal) { delete internal_cb[signal];},
            remove_user_cb: function(signal) { delete user_cb[signal];},
            fire: function (signal, args) {
                // apply the internal callback first
                var args_array = args2array(args);
                var first_cb = ((signal in internal_cb)?callbacks[signal]:default_callback);
                var internal_cb_result = first_cb.apply(this, args_array):
                // apply the user-defined cb if one exists
                if (signal in user_cb) user_cb[signal].apply(this,[args_array, internal_cb_result]);
            },
            // used when we need a function which fires a signal. For example,
            // ajax-based callbacks or ws calls.
            wrapSignal: function(signal) {
                return function() {ret.fire(signal, arguments);}
            } 
        }
        return ret;
    };

    // Queue: structure for holding commands which change state.
    // operations:
    // enqueue: add a command to the end of the queue.
    // dequeue: pops the next element from the front of the queue.
    //          returns undefined if the queue is empty.
    var mkQueue = function() {
        // MODULE FIELDS:
        // queue holds a list of command objects.
        
        var queue = [];

        // MODULE METHODS
        var fun = {};
        fun.enqueue = function(command) {queue.push(command);};
        fun.dequeue = function() {return queue.shift();};
        fun.length = function() {return queue.length;};

        // RETURN EXPORTS
        return {
            enqueue: fun.enqueue,
            dequeue: fun.dequeue,
            length: fun.length
        }
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
    var mkTelepathyObject = function (obj_spec) {
        // obj_spec format: {
        //      type: "dict" || "list" || "data"
        // }

        var tobj = { // the telepathic object which will be returned
            public: {}, // methods directly available to the API user
            private: {} // methods for internal use only
        };

        // MODULE FIELDS:
        // unique ID of object
        var uid = object_id_generator.gensym();
        // the last state which the server and client could agree on
        // for the current object
        var last_shared_state;
        // pending state transformations on object committed since the
        // last_shared_state. history[history.length -1] is the current state.
        var history = [];
        // callback manager, which is used by the descendant object
        private.cb = mkCallbackManager(obj_spec.type+"#"+uid);
        // reference counting is necessary, because we have to garbage collect
        // nodes not used any more.
        var references = 0;

        // MODULE PRIVATE METHODS:
        tobj.private.get_id = function() {return uid;}
        tobj.private.get_type = function() {return obj_spec.type;}
        // --- reference counting --- 
        // should be called when this item is added to a collection
        tobj.private.inc_ref() { references++; }
        // should be called when this item is remove from a collection
        tobj.private.dec_ref() {
            references--;
            // if we have no more references to this object, delete it from
            // the global master dictionary, but call destroy callback first.
            private.cb.fire("destroy");
            if (references <= 0) tfun.unregister_object(tobj);
        }
        // --- manipulating object state ---
        tobj.private.apply_local_transformation = function(transformation) {
            // transformation should be: {
            //      command: CMD,
            //      param: CMD PARAMS,
            //      state: RESULTING STATE
            // }
            history.push(transformation);
        }
        tobj.private.get_state = function() {
            return ((history.length == 0)?last_shared_state:history[history.length -1].state);
        }
        // applying state updates sent by the server.
        tobj.private.apply_remote_transformation = function(remote_transformations) {
        }

        // MODULE PUBLIC METHODS:
        // --- applying state transformations ---


        // RETURN OBJECT:
        // register new object in global master dictionary
        tfun.register_object(tobj);
        return tobj;
    };

    // Data module for telepathy data nodes.
    var mkData = function() {
        // MODULE FIELDS:
        // start by inheriting from TelepathicObject
        var obj = mkTelepathyObject({
            'type':'data'
        });
        // stores key -> value entries
        
        // MODULE METHODS:
        obj.set = function(newValue) {
            // update local state & send update command to server
            var oldValue = tobj.private.get_state();
            tobj.private.apply_local_transformation({
                'command': 'set',
                'param': newValue,
                'state': newValue
            });
            // fire event 
            obj.private.cb.fire("set",[{old_value: oldValue, new_value: newValue}]);
        }
        obj.get = tobj.private.get_state;

        // RETURN OBJECT:
        return obj;
    };



    // mkDictionary: Module responsible for telepathy dictionaries.
    // The dictionary's entries are (key, value) pairs, where
    // keys are strings, and values can be opaque JS data, 
    // nested telepathy dictionaries, or telepathy lists.
    //
    // Signals which can be handled:
    // set(key, value) - add an entry to the dictionary
    // remove(key) - remove an entry from the dictionary 
    var mkDictionary = function() {
    
        // MODULE FIELDS:
        // start by inheriting from TelepathicObject
        var obj = mkTelepathyObject({
            'type':'dict'
        });
        // stores key -> value entries
        var dict = {};

        // PUBLIC MODULE METHODS
        // Each method updates dict first, since the users want to 'read their writes'.
        // afterwards, we queue the corresponding command to be sent to the server.
        obj.set = function(key, value) {
            // update local state
            dict[key] = value;
            // TODO: send update command to server
            // fire event 
            obj.private.cb.fire("set",arguments);
        }
        obj.del = function(key) {
            // update local state
            delete dict[key];
            // TODO: send update command to server
            // fire event
            obj.private.cb.fire("del",arguments);
        }

        // RETURN FINAL OBJECT
        return obj;
    }

    //  -------------------------
    //  - MAIN TELEPATHY MODULE -
    //  -------------------------

    // MODULE FIELDS
    var d = {};
    // master_dict contains all objects
    d.master_dict = {};
    // configuration for telepathy
    d.config = {};
    // websocket object
    d.connection = null; 
    // default callback functions
    d.cb = mkCallbackManager(" main telepathy object ");
    // command queues:
    d.q = {
        s2c: mkQueue(), // server to client
        c2s: mkQueue() // client to server
    };
    

    // MODULE METHODS
    var tfun = {};

    // websocket callback functions
    d.cb.set_internal_cb("onopen", function() {
        
    });
    d.cb.set_internal_cb("onmessage", function(msg) {
        
    });
    d.cb.set_internal_cb("onclose", function(msg) {
        
    });

    // master dictionary manipulation (for internal use only):
    tfun.register_object(tobj) {
        d.master_dict[tobj.private.get_id()] = tobj;
    }

    tfun.unregister_object(tobj) {
        delete d.master_dict[tobj.private.get_id()];
    }

    tfun.connect = function() {
        d.connection=new WebSocket("ws://" + connection_params.ws_uri + ");
        // Set callbacks for websocket events.
        d.connection.onopen=d.cb.wrapSignal("onopen");
        d.connection.onmessage=d.cb.wrapSignal("onmessage");
        d.connection.onclose=d.cb.wrapSignal("onclose");
    };

    tfun.disconnect = function() {
    };

    tfun.root = function() {};

    // RETURN EXPORTS
    return {
        connect: tfun.connect,
        disconnect: tfun.disconnect,
        root: tfun.root,
        set_cb: d.cb.set_user_cb,
    }
}; // END of module mkTelepathy
