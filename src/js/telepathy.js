/*
  telepathy.js - javascript client API for telepathy.
  author: neumark
*/

// mkTelepathy: constructor for the main telepathy module.
var mkTelepathy = function() {
    var d = {}; // declared here because used in internal modules.

    //  ---------------------
    //  - UTILITY FUNCTIONS -
    //  ---------------------

    var args2array = function(args) {return Array.prototype.slice.apply(args);}

    //  --------------------
    //  - INTERNAL MODULES -
    //  --------------------

    var mkCounter = function () {
    // Based on code from Javascript - The Good Parts
    var seq = 0;
    return {
       get: function () {
            var result = seq;
            seq += 1;
            return result;
            }
        };
    }();

    // CallbackManager: Module responsible for getting/setting/calling
    // callback functions.
    var mkCallbackManager = function() {
    
        // MODULE FIELDS:
        // debug state:
        var debug = false;
        // callbacks holds 'signal' -> handler() entries.
        var callbacks = {};

        // default callback function:
        var default_callback = function(signal, args) {
            if (debug) {
                console.log("Received signal " + signal + " with arguments " + args + ".");
            }
        }

        // RETURN EXPORTS
        return {
            debug: function(new_debug_state) {debug = new_debug_state;},
            set: function(signal, handler) {callbacks[signal] = handler;},
            remove: function(signal) { delete callbacks[signal];},
            fire: function (signal, args) {
                return ((signal in callbacks):?callbacks[signal].apply(this, args2array(arguments)):
                    default_callback(signal, args2array(arguments)));
            }
        }
    }();

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
    }();


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
        // dict holds the actual contents of the dictionary.
        // The items in dict are of the format
        // "key" -> value 
        var dict = {};
        // callbacks
        var cb = mkCallbackManager();

        // MODULE METHODS
        var fun = {};
        // Each method updates dict first, since the users want to 'read their writes'.
        // afterwards, we queue the corresponding command to be sent to the server.
        fun.set(key, value) {
            dict[key] = value
            d.q.c2s.enqueue();
            
        }

        // RETURN EXPORTS
        return {
            set: fun.set,
            remove: fun.remove,
            cb: cb, // export callback manager 'as is'.
            type: "dict"
        }
    }();

    //  -------------------------
    //  - MAIN TELEPATHY MODULE -
    //  -------------------------

    // MODULE FIELDS
    // NOTE: d has already been declared.
    // debug flag
    d.debug = false;
    // configuration for telepathy
    d.config = {};
    // websocket object
    d.connection = null; 
    // default callback functions
    d.cb = {
        // user-definable callbacks for websocket events
        ws: {
            onopen: function() {if(d.debug){console.log("d.cb.ws.onopen() called."}},
            onclose: function(m) {if(d.debug){console.log("d.cb.ws.onclose("+m+") called."}},
            onmessage: function(m) {if(d.debug){console.log("d.cb.ws.onmessage("+m+") called."}},
        }
    };
    // command queues:
    d.q = {
        s2c: mkQueue(), // server to client
        c2s: mkQueue() // client to server
    };
    

    // MODULE METHODS
    var fun = {};
    fun.connect = function(connection_params) {
        d.config.ws = connection_params;
        d.connection=new WebSocket("ws://" + d.config.ws.uri + ");
        // Set callbacks for websocket events.
        d.connection.onopen=this._onopen;
        d.connection.onmessage=this._onmessage;
        d.connection.onclose=this._onclose;
    };

    fun.disconnect = function() {
    };

    fun.debug = function(new_debug_state) {d.debug = new_debug_state};

    fun.root = function() {};

    // RETURN EXPORTS
    return {
        connect: fun.connect,
        disconnect: fun.disconnect,
        root: fun.root
    }
}(); // END of module mkTelepathy
