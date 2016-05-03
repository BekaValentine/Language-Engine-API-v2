// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("golem", function(_config, modeConfig) {
  
  var keywords = ["data","case","motive","of","end",
                  "where","let","module","open",
                  "opening","as","using","hiding",
                  "renaming","to",
                  "continue","shift","reset","from","in",
                  "require"
                  ];
  
  var switchState = function (stream, setState, f) {
    setState(f);
    return f(stream, setState);
  }
  
  var normal = function (stream, setState) {
    
    var tok;
    
    if (stream.match("{-")) {
      
      return switchState(stream, setState, blockCommentLevel(1));
      
    } else if (stream.match(/--.*/)) {
      
      return "comment";
      
    } else if (stream.eat("\"")) {
      
      return switchState(stream, setState, stringLiteral);
      
    } else if (tok = stream.match(/[A-Z][a-zA-Z0-9]*/)) {
      
      return "atom";
      
    } else if (tok = stream.match(/[a-z][a-zA-Z0-9]*/)) {
      
      return keywords.includes(tok[0]) ? "keyword" : "variable";
      
    } else {
      
      stream.next();
      
      return null;
      
    }
    
  };
  
  var stringLiteral = function (stream, setState) {
    
    while (!stream.eol()) {
      var ch = stream.next();
      
      if ("\\" === ch) {
        stream.next();
        
      } else if ("\"" === ch) {
        setState(normal);
        return "string";
      }
    }
    
    return "string";
    
  };
  
  var blockCommentLevel = function (depth) {
    if (0 === depth) {
      
      return normal;
      
    } else {
      
      return function (stream, setState) {
        var currentDepth = depth;
        
        while (!stream.eol()) {
          if (stream.match("{-")) {
            currentDepth++;
          } else if (stream.match("-}")) {
            currentDepth--;
            if (0 == currentDepth) {
              setState(normal);
              return "comment";
            }
          } else {
            stream.next();
          }
        }
        
        setState(blockCommentLevel(currentDepth));
        return "comment";
      };
      
    }
  }
  
  return {
    startState: function () { return { f: normal }; },
    copyState: function (s) { return { f: s.f }; },

    token: function(stream, state) {
      return state.f(stream, function(s) { state.f = s; });
    },
    
    lineComment: "--"
  };

});

});