/*

the LE interface is responsible for starting conversations, managing updates
to the world model, pushing world model updates to the server, resetting the
world model, sending a discourse move to the server, and handling/dispatching
actions. to do this, it uses an object-representation of prolog-esque queries.
instead of the prolog predicate

    pred(foo,X)
    
we would write

    pred("Foo", "x")

instead of

    rel(bar,x,b)
    
we would write

    rel("Bar", a, b)

there are a few tables/predicates, which correspond to the predicates of LE:

  Pred : Predicate -> Entity -> Type
  Rel : Relation -> Entity -> Entity -> Type
  Negative : Entity -> Type
  Foreground : Entity -> Type
  StringVal : Entity -> String -> Type
  
the tables in the interface have the types

  Pred : List (String * Entity)
  Rel : List (String * Entity * Entity)
  Negative : List Entity
  Foreground : List Entity
  StringVal : List (Entity * String)

an atomic query, build by the above functions, is something of the type

  { table : TableName, row : List (RowType table) }

where `RowType table` is the type of the row for the table of interest. so for
`table = "Pred"`, the row type is `String * Entity`, and so on.

given an atomic query  { table = t, row = r }, and a unifier u, the first step
in performing the atomic query is to substitute the unifier into the atomic
query's row, to get a row template. then we bind the rows of the table through
a function which maps each row to either the new unifier yielded by the row,
or nothing if the fixed columns dont match. this gives a set of unifiers.

given an atomic query and a set of unifiers, we get a new set of unifiers by
binding the quantifiers through the atomic query solver for each unifier.

given a set of atomic queries, and a set of unifiers, we can pass the unifiers
through the first query, to get a new set, then pass that through the next,
and so on. solving a whole query is done by starting with the set consist of
just the empty unifier.

a row in a query consists of a list of tagged items. they're tagged either as
variables, or as fixed values.

*/

'use strict';



function MakeLanguageEngine() {
  // zip : ([a],[b]) -> [(a,b)]
  let zip = function (as, bs) {
    var cs = [];
    
    for (var i = 0; i < Math.min(as.length, bs.length); i++) {
      cs.push([as[i],bs[i]]);
    }
    
    return cs;
  };
  
  // mjoin : [[a]] -> [a]
  let mjoin = function (xss) {
     return xss.reduce((acc,xs) => acc.concat(xs), []);
  };
  
  // mbind :: ([a],a -> [b]) -> [b]
  let mbind = function (xs,f) {
    return mjoin(xs.map(f));
  };
  
  // lookup :: (a, [(a,b)]) -> Nullable b
  let lookup = function (x, xys) {
    for (var i = 0; i < xys.length; i++) {
      if (x === xys[i][0]) {
        return xys[i][1];
      }
    }
    
    return null;
  };
  
  // withLookup :: (a, [(a,b)], () -> r, b -> r) -> r
  let withLookup = function (x, xys, n, j) {
    for (var i = 0; i < xys.length; i++) {
      if (x === xys[i][0]) {
        return j(xys[i][1]);
      }
    }
    
    return n();
  };
  
  
  
  let LanguageEngine = {};
  
  
  
  
  //
  // HTTP request handlers
  //
  
  // HTTP : HTTPInfo -> ()
  LanguageEngine.HTTP = function (info){
    let req = new XMLHttpRequest();
    
    req.onload = function () {
      if (200 === req.status) {
        if (info.success) { info.success(req); }
      } else {
        if (info.failure) { info.failure(req); }
      }
    };
    
    req.open(info.method, info.url, true);
    
    if (info.headers) {
      for (var i = 0; i < info.headers.length; i++) {
        req.setRequestHeader(info.headers[i][0], info.headers[i][1]);
      }
    }
    
    if (info.data) {
      req.setRequestHeader("Content-Type", "application/json");
      req.send(JSON.stringify(info.data));
    } else {
      req.send();
    }
  };
  
  //
  //  Query-builders
  //
  
  // atomicQuery : (TableName,QueryRow) -> AtomicQuery
  LanguageEngine.atomicQuery = function (table, row) {
    return { table: table, row: row };
  };
  
  // pred : (String|Var, Int|Var) -> AtomicQuery
  LanguageEngine.pred = function (p, x) {
    return LanguageEngine.atomicQuery("Pred", [p,x]);
  };
  
  // rel : (String, String|Int, String|Int) -> AtomicQuery
  LanguageEngine.rel = function (r, x, y) {
    return LanguageEngine.atomicQuery("Rel", [r,x,y]);
  };
  
  // nonexistant : String|Int -> AtomicQuery
  LanguageEngine.nonexistant = function (x) {
    return LanguageEngine.atomicQuery("Nonexistant", [x]);
  };
  
  // foreground : String|Int -> AtomicQuery
  LanguageEngine.foreground = function (x) {
    return LanguageEngine.atomicQuery("Foreground", [x]);
  };
  
  // stringVal : String|Int -> String -> AtomicQuery
  LanguageEngine.stringVal = function (x, s) {
    return LanguageEngine.atomicQuery("StringVal", [x,s]);
  };
  
  
  //
  //  Unifiers
  //
  
  // Unifier = [(String,Value)]
  
  // emptyU : Unifier
  LanguageEngine.emptyU = [];
  
  // extendU : (String,Value,Unifier) -> Unifier
  LanguageEngine.extendU = function (variable,value,tail) {
    
    return [{ variable: variable, value: value }].concat(tail);
    
  };
  
  // showUnifier : Unifier -> String
  LanguageEngine.showUnifier = function (u) {
    var acc = "{";
    
    for (var i = 0; i < u.length; i++) {
      acc += i == 0 ? " " : ", ";
      acc += u[i].variable + " = " + u[i].value.toString();
    }
    
    return acc + " }"
  };
  
  // showUnifiers : [Unifier] -> String
  LanguageEngine.showUnifiers = function (us) {
    return us.length == 0 ?
             "nothing" :
             us.map(LanguageEngine.showUnifier).join(" ; ");
  };
  
  
  //
  //  Query Rows and Query Values
  //
  
  // tag : (String,Value) -> QueryValue
  LanguageEngine.tag = function (t,x) {
    
    return { tag: t, value: x };
    
  };
  
  // variable : String -> QueryValue
  LanguageEngine.variable = function (x) {
    return LanguageEngine.tag("variable",x);
  };
  
  // value : Value -> QueryValue
  LanguageEngine.value = function (x) {
    return LanguageEngine.tag("value",x);
  };
  
  // substituteQueryValue : (QueryValue,Unifier) -> QueryValue
  LanguageEngine.substituteQueryValue = function (queryValue,unifier) {
    if ("variable" === queryValue.tag) {
      
      return withLookup(queryValue.value, unifier,
        () => queryValue,
        x => LanguageEngine.tag("value",x));
      
    } else {
      
      return queryValue;
      
    }
  };
  
  // matchQueryValue : (QueryValue,ColumnValue,Unifier) -> [Unifier]
  LanguageEngine.matchQueryValue = function (queryValue,columnValue,unifier) {
    
    let subQueryValue = LanguageEngine.
                          substituteQueryValue(queryValue,unifier);
    
    if ("variable" === subQueryValue.tag) {
      
      return [LanguageEngine.
                extendU(subQueryValue.value, columnValue, unifier)];
      
    } else if ("value" === subQueryValue.tag) {
      
      if (subQueryValue.value === columnValue) {
        return [unifier];
      } else {
        return [];
      }
        
    }
  };
  
  // matchQueryRow : (QueryRow,TableRow,Unifier) -> [Unifier]
  LanguageEngine.matchQueryRow = function (queryRow,tableRow,unifier) {
    return zip(queryRow,tableRow).
             reduce((us, p) =>
                      mbind(us, u =>
                        LanguageEngine.matchQueryValue(p[0], p[1], u)),
                    [unifier])
  };
  
  
  // DB = Map TableName [TableRow]
  // TableRow = [Value]
  
  
  // LanguageEngine.Conversation : (URL,AppID) -> Conversation
  LanguageEngine.Conversation = function (apiURL, appID, appToken, callback) {
    let self = this;
    
    self.apiURL = apiURL;
    self.appID = appID.toString();
    self.appToken = appToken;
    self.conversationID = null;
    
    self.eventDescriptions = {
      "Pred": [],
      "Rel": [],
      "Foreground": [],
      "Nonexistant": [],
      "StringVal": []
    };
    self.worldModel = {
      nextEntity: 0,
      facts: {
        "Pred": [],
        "Rel": [],
        "Foreground": [],
        "Nonexistant": [],
        "StringVal": []
      }
    };
    self.worldModelUpdate = {
      newNextEntity: 0,
      newFacts: {
        "Pred": [],
        "Rel": [],
        "Nonexistant": [],
        "StringVal": []
      }
    };
    self.entityData = [];
    
    self.defaultHandler = null;
    self.handlers = {};
    
    LanguageEngine.HTTP({
      method: 'post',
      synchronous: true,
      url: self.apiURL + '/apps/' + self.appID + '/conversations/',
      data: { "tokenStringConfig": self.appToken },
      success: function (req) {
        let convoInfo = JSON.parse(req.responseText);
        self.conversationID = convoInfo.conversationID.toString();
        
        if (callback) {
          callback();
        }
      }
    });
  };
  
  // runQuery : [AtomicQuery] -> [Unifier]
  LanguageEngine.Conversation.prototype.runQuery = function (atomicQueries, useEventDescriptions) {
    var self = this;
    
    return atomicQueries.
             reduce((us,q) => mbind(us, u => self.runAtomicQuery(q,u,useEventDescriptions)),
                    [LanguageEngine.emptyU]);
  };
  
  // runAtomicQuery : (AtomicQuery, Unifier, DB) -> [Unifier]
  LanguageEngine.Conversation.prototype.runAtomicQuery = function (atomicQuery, unifier, useEventDescriptions) {
    let dbTable = (useEventDescriptions ?
                     this.eventDescriptions[atomicQuery.table] :
                     this.worldModel.facts[atomicQuery.table]) ||
                  [];
    
    return mbind(dbTable, tableRow =>
             LanguageEngine.
               matchQueryRow(atomicQuery.row,tableRow,unifier));
  };
  
  // makeEntity : () -> Entity
  LanguageEngine.Conversation.prototype.makeEntity = function () {
    let e = this.worldModelUpdate.newNextEntity;
    this.worldModelUpdate.newNextEntity++;
    return e;
  };
  
  // setEntityData : (Entity,Value) -> ()
  LanguageEngine.Conversation.prototype.setEntityData = function (entity,data) {
    this.entityData.push([entity,data]);
  };
  
  // getEntityData : Entity -> Nullable Value
  LanguageEngine.Conversation.prototype.getEntityData = function (entity) {
    return lookup(entity, this.entityData);
  };
  
  // assertPred : (String,Int) -> ()
  LanguageEngine.Conversation.prototype.assertPred = function (p,x) {
    if (typeof p === "string" && typeof x === "number") {
      this.worldModelUpdate.newFacts["Pred"].push([p,x]);
    }
  };
  
  // assertRel : (String,Int,Int) -> ()
  LanguageEngine.Conversation.prototype.assertRel = function (r,x,y) {
    if (typeof r === "string" && typeof x === "number" && typeof y == "number") {
      this.worldModelUpdate.newFacts["Rel"].push([r,x,y]);
    }
  };
  
  // assertStringVal : (Int,String) -> ()
  LanguageEngine.Conversation.prototype.assertStringVal = function (x,s) {
    if (typeof x === "number" && typeof s === "string") {
      this.worldModelUpdate.newFacts["StringVal"].push([x,s]);
    }
  };
  
  // assertNonexistant : Int -> ()
  LanguageEngine.Conversation.prototype.assertNonexistant = function (x) {
    if (typeof x === "number") {
      this.worldModelUpdate.newFacts["Nonexistant"].push([x]);
    }
  };
  
  // pushUpdates : () -> ()
  LanguageEngine.Conversation.prototype.pushUpdates = function () {
    let self = this;
    
    LanguageEngine.HTTP({
      method: 'put',
      url: self.apiURL + '/apps/' + self.appID + '/conversations/' + self.conversationID,
      data: {
        "tokenStringUpdate": self.appToken,
        "updateInfo": {
          "tag": "ConversationUpdateWorldModel",
          "newNextEntity": self.worldModelUpdate.newNextEntity,
          "newFacts": LanguageEngine.databaseToEventDescriptions(self.worldModelUpdate.newFacts)
        }
      }
    });
  };
  
  // resetWorldModel : () -> ()
  LanguageEngine.Conversation.prototype.resetWorldModel = function () {
    let self = this;
    
    LanguageEngine.HTTP({
      method: 'put',
      url: self.apiURL + '/apps/' + self.appID + '/conversations/' + self.conversationID,
      data: {
        "tokenStringUpdate": self.appToken,
        "updateInfo": {
          "tag": "ConversationUpdateResetWorldModel"
        }
      }
    });
  };
  
  // processInput : (String, Callback) -> EventDescriptions
  LanguageEngine.Conversation.prototype.processInput = function (str) {
    let self = this;
    
    LanguageEngine.HTTP({
      method: 'put',
      url: self.apiURL + '/apps/' + self.appID + '/conversations/' + self.conversationID,
      data: {
        "tokenStringUpdate": self.appToken,
        "updateInfo": {
          "tag": "ConversationUpdateDiscourseMove",
          "move": str
        }
      },
      success: function (req) {
        
        let res = JSON.parse(req.responseText);
        let newEventDescriptions = LanguageEngine.eventDescriptionsToDatabase(res.change.facts);
        let newWorldModel = {
          nextEntity: res.change.worldModel.nextEntity,
          facts: LanguageEngine.eventDescriptionsToDatabase(res.change.worldModel.facts)
        };
        let newWorldModelUpdate = {
          newNextEntity: res.change.worldModel.nextEntity,
          newFacts: []
        };
        
        self.eventDescriptions = newEventDescriptions;
        self.worldModel = newWorldModel;
        self.worldModelUpdate = newWorldModelUpdate;
        
        self.dispatch();
      }
    });
  };
  
  // eventDescriptionsToDatabase : [EventDescription] -> DB
  LanguageEngine.eventDescriptionsToDatabase = function (eds) {
    let db = {};
    
    for (var i = 0; i < eds.length; i++) {
      let fact = eds[i];
      let tag = fact.tag;
      let args = fact.contents;
      
      if ("PredDesc" === tag) {
        
        db["Pred"] = db["Pred"] || [];
        db["Pred"].push(args);
        
      } else if ("RelDesc" === tag) {
        
        db["Rel"] = db["Rel"] || [];
        db["Rel"].push(args);
        
      } else {
        
        let p = args.shift();
        db[p] = db[p] || [];
        db[p].push(args);
        
      }
    }
    
    return db;
  };
  
  // databaseToEventDescriptions : DB -> [EventDescription]
  LanguageEngine.databaseToEventDescriptions = function (db) {
    let preds = db["Pred"].map(p => ({ "tag": "PredDesc", "contents": p }));
    let rels = db["Rel"].map(t => ({ "tag": "RelDesc", "contents": t }));
    let nonexs = db["Nonexistant"].map(s => ({ "tag": "UnaryDesc", "contents": ["Nonexistant", s[0]] }));
    let strvals = db["StringVal"].map(p => ({ "tag": "BinaryStringDesc", "contents": ["StringVal", p[0], p[1]] }));
    
    return preds.concat(rels,nonexs,strvals);
  };
  
  // dispatch : () -> ()
  LanguageEngine.Conversation.prototype.dispatch = function () {
    let q = [ LanguageEngine.foreground(LanguageEngine.variable("e")) ,
              LanguageEngine.pred(LanguageEngine.variable("p"),
                                  LanguageEngine.variable("e")) ];
    let res = this.runQuery(q, true);
    
    if (res.length === 1) {
      let event = lookup("e", res[0]);
      let pred = lookup("p", res[0]);
      
      let relq = [ LanguageEngine.rel(LanguageEngine.variable("r"),
                                      LanguageEngine.value(event),
                                      LanguageEngine.variable("x")) ];
      
      let relRes = this.runQuery(relq, true);
      
      let rels = [];
      let relArgs = {};
      
      for (var i = 0; i < relRes.length; i++) {
        var resi = relRes[i];
        var r = lookup("r", resi);
        var x = lookup("x", resi);
        
        rels.push(r)
        relArgs[r] = relArgs[r] || [];
        relArgs[r].push(x);
      }
      
      let handlerInfo = this.findHandler(pred, rels);
      
      if (handlerInfo) {
          handlerInfo.handler.apply(handlerInfo.map(r => relArgs[r]));
      }
    }
  };
  
  // setDefaultHandler : (Event -> ()) -> ()
  LanguageEngine.Conversation.prototype.setDefaultHandler = function (h) {
    this.defaultHandler = h;
  };
  
  // setHandler : (String,[String],Event -> ()) -> ()
  LanguageEngine.Conversation.prototype.setHandler = function (pred,rels,h) {
    this.handlers[pred] = this.handlers[pred] || [];
    
    let sortedRels = rels.sort();
    this.handlers[pred].push({ rels: rels, sortedRels: sortedRels, handler: h });
  };
  
  // findHandler : (String,[String]) -> Nullable (Event -> ())
  LanguageEngine.Conversation.prototype.findHandler = function (goalPred,goalRels) {
    let sortedGoalRels = goalRels.sort();
    
    let foundHandlers = (this.handlers[goalPred] || []).
                          filter(h => subset(h.sortedRels, sortedGoalRels)).
                          sort((h1,h2) => h1.sortedRels.length <= h2.sortedRels.length);
    
    return foundHandlers[0] || this.defaultHandler;
  };
  
  
  
  
  
  
  return LanguageEngine;
}


let LE = MakeLanguageEngine();
let pred = LE.pred;
let rel = LE.rel;
let vb = LE.variable;
let vl = LE.value;

let convo = new LE.Conversation("http://localhost:8080/api", 7, "m8RENOb3dhgj6fEdUyQWzTgLWAYJgm",
  function () {
    convo.processInput("a dog barked barked");
  }
);