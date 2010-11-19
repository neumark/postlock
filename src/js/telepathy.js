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
        master_dict: {}
    };

    //  ---------------------
    //  - UTILITY FUNCTIONS -
    //  ---------------------
    var crc32 = function ( str ) {
        // Calculate the crc32 polynomial of a string  
        // 
        // version: 1008.1718
        // discuss at: http://phpjs.org/functions/crc32
        // +   original by: Webtoolkit.info (http://www.webtoolkit.info/)
        // +   improved by: T0bsn
        // -    depends on: utf8_encode
        // *     example 1: crc32('Kevin van Zonneveld');
        // *     returns 1: 1249991249
        // str = this.utf8_encode(str); <- already using utf8, right?
        var table = "00000000 77073096 EE0E612C 990951BA 076DC419 706AF48F E963A535 9E6495A3 0EDB8832 79DCB8A4 E0D5E91E 97D2D988 09B64C2B 7EB17CBD E7B82D07 90BF1D91 1DB71064 6AB020F2 F3B97148 84BE41DE 1ADAD47D 6DDDE4EB F4D4B551 83D385C7 136C9856 646BA8C0 FD62F97A 8A65C9EC 14015C4F 63066CD9 FA0F3D63 8D080DF5 3B6E20C8 4C69105E D56041E4 A2677172 3C03E4D1 4B04D447 D20D85FD A50AB56B 35B5A8FA 42B2986C DBBBC9D6 ACBCF940 32D86CE3 45DF5C75 DCD60DCF ABD13D59 26D930AC 51DE003A C8D75180 BFD06116 21B4F4B5 56B3C423 CFBA9599 B8BDA50F 2802B89E 5F058808 C60CD9B2 B10BE924 2F6F7C87 58684C11 C1611DAB B6662D3D 76DC4190 01DB7106 98D220BC EFD5102A 71B18589 06B6B51F 9FBFE4A5 E8B8D433 7807C9A2 0F00F934 9609A88E E10E9818 7F6A0DBB 086D3D2D 91646C97 E6635C01 6B6B51F4 1C6C6162 856530D8 F262004E 6C0695ED 1B01A57B 8208F4C1 F50FC457 65B0D9C6 12B7E950 8BBEB8EA FCB9887C 62DD1DDF 15DA2D49 8CD37CF3 FBD44C65 4DB26158 3AB551CE A3BC0074 D4BB30E2 4ADFA541 3DD895D7 A4D1C46D D3D6F4FB 4369E96A 346ED9FC AD678846 DA60B8D0 44042D73 33031DE5 AA0A4C5F DD0D7CC9 5005713C 270241AA BE0B1010 C90C2086 5768B525 206F85B3 B966D409 CE61E49F 5EDEF90E 29D9C998 B0D09822 C7D7A8B4 59B33D17 2EB40D81 B7BD5C3B C0BA6CAD EDB88320 9ABFB3B6 03B6E20C 74B1D29A EAD54739 9DD277AF 04DB2615 73DC1683 E3630B12 94643B84 0D6D6A3E 7A6A5AA8 E40ECF0B 9309FF9D 0A00AE27 7D079EB1 F00F9344 8708A3D2 1E01F268 6906C2FE F762575D 806567CB 196C3671 6E6B06E7 FED41B76 89D32BE0 10DA7A5A 67DD4ACC F9B9DF6F 8EBEEFF9 17B7BE43 60B08ED5 D6D6A3E8 A1D1937E 38D8C2C4 4FDFF252 D1BB67F1 A6BC5767 3FB506DD 48B2364B D80D2BDA AF0A1B4C 36034AF6 41047A60 DF60EFC3 A867DF55 316E8EEF 4669BE79 CB61B38C BC66831A 256FD2A0 5268E236 CC0C7795 BB0B4703 220216B9 5505262F C5BA3BBE B2BD0B28 2BB45A92 5CB36A04 C2D7FFA7 B5D0CF31 2CD99E8B 5BDEAE1D 9B64C2B0 EC63F226 756AA39C 026D930A 9C0906A9 EB0E363F 72076785 05005713 95BF4A82 E2B87A14 7BB12BAE 0CB61B38 92D28E9B E5D5BE0D 7CDCEFB7 0BDBDF21 86D3D2D4 F1D4E242 68DDB3F8 1FDA836E 81BE16CD F6B9265B 6FB077E1 18B74777 88085AE6 FF0F6A70 66063BCA 11010B5C 8F659EFF F862AE69 616BFFD3 166CCF45 A00AE278 D70DD2EE 4E048354 3903B3C2 A7672661 D06016F7 4969474D 3E6E77DB AED16A4A D9D65ADC 40DF0B66 37D83BF0 A9BCAE53 DEBB9EC5 47B2CF7F 30B5FFE9 BDBDF21C CABAC28A 53B39330 24B4A3A6 BAD03605 CDD70693 54DE5729 23D967BF B3667A2E C4614AB8 5D681B02 2A6F2B94 B40BBE37 C30C8EA1 5A05DF1B 2D02EF8D";
     
        var crc = 0;
        var x = 0;
        var y = 0;
     
        crc = crc ^ (-1);
        for (var i = 0, iTop = str.length; i < iTop; i++) {
            y = ( crc ^ str.charCodeAt( i ) ) & 0xFF;
            x = "0x" + table.substr( y * 9, 8 );
            crc = ( crc >>> 8 ) ^ x;
        }
     
        return crc ^ (-1);
    }

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
            }
        }; // END my    

        // MODULE EXPORTS
        var exports = {
            debug: function(new_debug_state) {my.debug = new_debug_state;},
            set_internal_cb: function(signal, handler) {my.internal_cb[signal] = handler;},
            get_internal_cb: function(signal) {return my.internal_cb[signal];}
            set_user_cb: function(signal, handler) {my.user_cb[signal] = handler;},
            get_user_cb: function(signal) {return my.user_cb[signal];}
            remove_internal_cb: function(signal) { delete my.internal_cb[signal];},
            remove_user_cb: function(signal) { delete my.user_cb[signal];},
            fire: function (signal, args) {
                // apply the internal callback first
                var args_array = args2array(args);
                var first_cb = ((signal in my.internal_cb)?my.internal_cb[signal]:my.default_callback);
                var internal_cb_result = first_cb.apply(this, args_array):
                // apply the user-defined cb if one exists
                if (signal in my.user_cb) my.user_cb[signal].apply(this,[args_array, internal_cb_result]);
            },
            // used when we need a function which fires a signal. For example,
            // ajax-based callbacks or ws calls.
            wrap_signal: function(signal) {
                return function() {exports.fire(signal, arguments);}
            } 
        };
        return exports;
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
        //      function server_command (optional)
        // }
        var object_id = spec.oid || telepathy_shared.object_id_generator.gensym();
        var default_server_command = function(command, last_shared_state, transformation_stack) {
        };

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
            last_shared_state: {}, 
            // pending state transformations on object committed since the
            // last_shared_state. history[history.length -1] is the current state.
            // transformation_stack has elements of the form:
            // {
            //      transformation: {
            //                          string: command
            //                          opaque: parameters
            //                      },
            //      resulting_state: same format as last_shared_state.
            // }
            transformation_stack: [],
            // callback manager, which is used by the descendant object
            cb: mkCallbackManager({name: spec.type+"#"+object_id}),
            // parent node
            parent_node: null,
            // default OT function is 'do nothing':
            server_command: spec.server_command ||  default_server_command
        };

        my.cb.set_internal_cb("server_command", function(messages) {
            // Function which applies state transformations sent by the server.
            // 1. Attempty to apply each of the commands in sequence by calling server_command.
            //      A.  All of the server's commands are executed successfully.
            //          Great! set the new state to 
            for (var i = 0; i < messages.length; i++) {
                // transform function
                try {
                    var res = my.execute_command(messages[i], my.last_shared_state, my.transformation_stack);
                } catch (e) {
                    // TODO: revert to stable state, send warning, do something!
                    console.error(e);
                }
            }
        });

        // EXPORTS
        var exports {
            cb: my.cb,
            get_oid: function() {return String(my.oid);},
            get_type: function() {return String(spec.type);},
            get_parent: function() {return parent_node;},
            get_current_state: function() {
                return ((history.length == 0)?last_shared_state:history[history.length -1]).state;
            },
            apply_local_transformation = function(transformation) {
                // transformation should be: {
                //      command: CMD,
                //      param: CMD PARAMS,
                //      state: RESULTING STATE
                // }
                // extend transformation with the hash of the new string.
                transformation.hash = crc32(JSON.stringify(transformation.state));
                transformation.oid = tobj.hidden.get_id();
                history.push(transformation);
                // send transformation to server
                d.connection.send(JSON.stringify(transformation)); // OPT: we could probably factor this second stringify out.
            }
        };
        return exports;
    };

    // Data module for telepathy data nodes.
    var mkData = function(spec) {
        // MODULE FIELDS:
        // start by inheriting from TelepathicObject
        spec.type = 'data';
        var my = mkTelepathyObject(spec);
        // stores key -> value entries
        
        // MODULE METHODS:
        obj.exports.set = function(val) {
            // update local state & send update command to server
            var oldValue = tobj.hidden.get_state();
            var newValue = clone(val);
            tobj.hidden.apply_local_transformation({
                'command': 'set',
                'param': newValue,
                'state': newValue
            });
            // fire event 
            obj.hidden.cb.fire("set",[{old_value: oldValue, new_value: newValue}]);
        }
        obj.exports.get = tobj.hidden.get_state;

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
            obj.hidden.cb.fire("set",arguments);
        }
        obj.del = function(key) {
            // update local state
            delete dict[key];
            // TODO: send update command to server
            // fire event
            obj.hidden.cb.fire("del",arguments);
        }

        // RETURN FINAL OBJECT
        return obj;
    }

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

        // Callback functions
        my.handle_incoming_message = [];
        my.handle_incoming_message[my.state_list.AUTH] = function(msg_obj) {
            // TODO: respond to authentication challenge
            // Right now, there's no authentication, we assume it succeeded:
            if ("client_id" in msg_obj && typeof msg_obj.client_id == "number") {
                telepathy_shared.config.client_id = msg_obj.client_id;
                telepathy_shared.object_id_generator.set_prefix(msg_obj.client_id + ".");
                my.state = my.state_list.CONNECTED;
            }
            else console.error("expected client id, received " + JSON.stringify(msg_obj));
        };
        my.handle_incoming_message[my.state_list.CONNECTED] = function(msg_obj) {
                    if ("oid" in msg_obj && msg_obj.oid in telepathy_shared.master_dict) telepathy_shared.master_dict[msg_obj.oid].hidden.cb.fire("onmessage", msg_obj);
                    else console.error("invalid or missing object id for command "+JSON.stringify(msg_obj));
        };

        // handle incoming message from server
        my.cb.set_internal_cb("ws_onopen", function() {
            my.state = my.state_list.AUTH;
        });
        my.cb.set_internal_cb("ws_onmessage", function(msg) {
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

        // EXPORTS:
        // master dictionary manipulation (for internal use only):
        var exports = {
            // ---- exports methods ----
            register_object: function(tobj) {
                telepathy_shared.master_dict[tobj.exports.get_id()] = tobj;
                },
            unregister_object: function(tobj) {
                delete telepathy_shared.master_dict[tobj.exports.get_id()];
                },
            connect: function() {
                my.connection=new WebSocket("ws://" + telepathy_shared.config.ws_url + ");
                // Set callbacks for websocket events.
                my.connection.onopen=d.cb.wrap_signal("ws_onopen");
                my.connection.onmessage=d.cb.wrap_signal("ws_onmessage");
                my.connection.onclose=d.cb.wrap_signal("ws_onclose");
                my.connection.onerror=d.cb.wrap_signal("ws_onerror");
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
    telepathy_shared.object_id_generator = mkCounter();
    var main = mkMain();
    return main;
}; // END mkTelepathy
