'use strict';

Array.prototype.maximum = function (def) {
  if (0 === this.length) {
    return def;
  } else {
    var max = this[0];
    for (var i = 1; i < this.length; i++) {
      if (this[i] > max) {
        max = this[i];
      }
    }
    return max;
  }
};
Array.prototype.append = function (xs) {
  var copy = [];
  
  for (var i = 0; i < this.length; i++) {
    copy.push(this[i]);
  }
  for (var i = 0; i < xs.length; i++) {
    copy.push(xs[i]);
  }
  
  return copy;
};
Array.prototype.dropWhile = function (p) {
  var i;
  for (i = 0; i < this.length; i++) {
    if (!p(this[i])) { break; }
  }
  
  return this.slice(i);
};

var LanguageEngine = {
  sigils: {
    reverse: '-',
    event: '!'
  }
};

// World Model Controller
LanguageEngine.Conversation = function (apiURL, appID) {
  
  this.apiInfo = {
    apiURL: apiURL,
    appID: appID
  };
  
  this.convoID = null;
  this.worldModel = null;
  this.entityData = {};
  
  this.handlers = {};
  this.hasUpdates = false;
  this.worldModelUpdate = {
    newEntities: [],
    newEvents: [],
    newEventFacts: [],
    newEntityFacts: []
  };
  
};
LanguageEngine.Conversation.prototype.connect = function () {
  var promise = new LanguageEngine.Promise();
  var self = this;
  var req = new XMLHttpRequest();
  
  req.onload = function (data) {
    if (201 === req.status) {
      var convoInfo = JSON.parse(req.responseText);
      if (convoInfo) {
        self.convoID = convoInfo.conversationID;
        self.worldModel = {
          entities: [],
          nextEntity: 0,
          events: [],
          nextEvent: 0,
          eventFacts: {},
          eventReverse: {},
          entityReverse: {},
          entityFacts: {}
        };
        promise.succeed();
      } else {
        promise.fail();
      }
    } else {
      promise.fail();
    }
  };
  
  req.open('post', this.apiInfo.apiURL + '/conversations', true);
  req.setRequestHeader('Content-Type', 'application/json');
  
  req.send(JSON.stringify({
    appIDConfig: this.apiInfo.appID
  }));
  
  
  return promise;
};
LanguageEngine.Conversation.prototype.makeEntity = function (options) {
  var ent = new LanguageEngine.Entity(this, this.worldModel.nextEntity);
  
  this.worldModel.nextEntity++;
  this.worldModel.entities.push(ent.id);
  this.worldModelUpdate.newEntities.push(ent.id);
  
  this.entityData[ent.id] = options.data;
  
  this.ensureEntity(ent);
  if (options) {
    
    if (options.coargs) {
      for (var rel in options.coargs) {
        if ('stringVal' === rel) {
          this.assertStringVal(ent, options.coargs[rel]);
        } else {
          var coargs = options.coargs[rel] instanceof Array ?
                         options.coargs[rel] :
                         [options.coargs[rel]];
          for (var i = 0; i < coargs.length; i++) {
            this.assertRelation(rel, ent, coargs[i]);
          }
        }
      }
    }
    
  }
  return ent;
};
LanguageEngine.Conversation.prototype.makeEvent = function (options) {
  var ev = new LanguageEngine.Event(this, this.worldModel.nextEvent);
  this.worldModel.nextEvent++;
  this.worldModel.events.push(ev.id);
  this.worldModelUpdate.newEvents.push(ev.id);
  
  this.ensureEvent(ev);
  
  if (options) {
    
    if (options.pred) {
      this.assertPredicate(options.pred, ev);
    }
    
    if (options.args) {
      for (var rel in options.args) {
        var args = options.args[rel] instanceof Array ?
                     options.args[rel] :
                     [options.args[rel]];
        for (var i = 0; i < args.length; i++) {
          this.assertRelation(rel, ev, args[i]);
        }
      }
    }
    
  }
  
  return ev;
};
LanguageEngine.Conversation.prototype.ent = function (coargs) {
  return this.makeEntity({ coargs: coargs })
};
LanguageEngine.Conversation.prototype.dataEnt = function (data,coargs) {
  return this.makeEntity({ data: data, coargs: coargs });
};
LanguageEngine.Conversation.prototype.ev = function (pred, args) {
  return this.makeEvent({ pred: pred, args: args });
};
LanguageEngine.Conversation.prototype.ensureEntity = function (ent) {
  this.worldModel.entityFacts[ent.id] = this.worldModel.entityFacts[ent.id] || {
    stringVal: null
  };
  this.worldModel.entityReverse[ent.id] = this.worldModel.entityReverse[ent.id] || {};
};
LanguageEngine.Conversation.prototype.ensureEvent = function (ev) {
  this.worldModel.eventFacts[ev.id] = this.worldModel.eventFacts[ev.id] || {
    pred: null,
    args: {}
  };
  this.worldModel.eventReverse[ev.id] = this.worldModel.eventReverse[ev.id] || {};
}
LanguageEngine.Conversation.prototype.assertStringVal = function (ent,s) {
  this.hasUpdates = true;
  this.ensureEntity(ent);
  this.worldModel.entityFacts[ent.id].stringVal = s;
  
  var entrep = {
    tag: -1 === this.worldModelUpdate.newEntities.indexOf(ent.id) ?
           'OldEntity' :
           'NewEntity' ,
    contents: ent.id
  };
  
  this.worldModelUpdate.newEntityFacts.push({
    tag: 'StringVal',
    entityRep: entrep,
    strVal: s
  });
};
LanguageEngine.Conversation.prototype.assertPredicate = function (pred,ev) {
  this.hasUpdates = true;
  this.ensureEvent(ev);
  this.worldModel.eventFacts[ev.id].pred = pred;
  
  var evrep = {
    tag: -1 === this.worldModelUpdate.newEvents.indexOf(ev.id) ?
           'OldEvent' :
           'NewEvent' ,
    contents: ev.id
  };
  
  this.worldModelUpdate.newEventFacts.push({
    tag: 'EventPredicate',
    eventPred: pred,
    eventPredEventRep: evrep
  });
};
LanguageEngine.Conversation.prototype.assertRelation = function (rel,ev,other) {
  this.hasUpdates = true;
  this.ensureEvent(ev);
  
  var reverse = rel.charAt(0) === LanguageEngine.sigils.reverse;
  if (reverse) { rel = rel.slice(1); }
  
  var eventEvent = rel.charAt(0) === LanguageEngine.sigils.event;
  if (eventEvent) {
    // rel : Event -> Event -> Prop
    this.ensureEvent(ev);
    this.ensureEvent(other);
    if (reverse) {
      // e <--rel-- o
      this.worldModel.eventReverse[ev.id][rel] = this.worldModel.eventReverse[ev.id][rel] || [];
      this.worldModel.eventReverse[ev.id][rel].push(other.id);
      // o --rel--> e
      this.worldModel.eventFacts[other.id].args[rel] = this.worldModel.eventFacts[other.id].args[rel] || [];
      this.worldModel.eventFacts[other.id].args[rel].push(ev.id);
    } else {
      // e --rel--> o
      this.worldModel.eventFacts[ev.id].args[rel] = this.worldModel.eventFacts[ev.id].args[rel] || [];
      this.worldModel.eventFacts[ev.id].args[rel].push(other.id)
      // o <--rel-- e
      this.worldModel.eventReverse[other.id][rel] = this.worldModel.eventReverse[other.id][rel] || [];
      this.worldModel.eventReverse[other.id][rel].push(ev.id);
    }
  } else {
    // rel : Event -> Entity -> Prop
    if (reverse) {
      this.ensureEntity(ev);
      this.ensureEvent(other);
      // e <--rel-- o
      this.worldModel.entityReverse[ev.id][rel] = this.worldModel.entityReverse[ev.id][rel] || []
      this.worldModel.entityReverse[ev.id][rel].push(other.id)
      // o --rel--> e
      this.worldModel.eventFacts[other.id].args[rel] = this.worldModel.eventFacts[other.id].args[rel] || [];
      this.worldModel.eventFacts[other.id].args[rel].push(ev.id)
    } else {
      this.ensureEvent(ev);
      this.ensureEntity(other);
      // e --rel--> o
      this.worldModel.eventFacts[ev.id].args[rel] = this.worldModel.eventFacts[ev.id].args[rel] || [];
      this.worldModel.eventFacts[ev.id].args[rel].push(other.id);
      // o <--rel-- e
      this.worldModel.entityReverse[other.id][rel] = this.worldModel.entityReverse[other.id][rel] || [];
      this.worldModel.entityReverse[other.id][rel].push(ev.id);
    }
  }
  
  if (eventEvent) {
    var evrep = {
      tag: -1 === this.worldModelUpdate.newEvents.indexOf(ev.id) ?
             'OldEvent' :
             'NewEvent' ,
      contents: ev.id
    };
    
    var otherrep = {
      tag: -1 === this.worldModelUpdate.newEvents.indexOf(other.id) ?
             'OldEvent' :
             'NewEvent' ,
      contents: other.id
    };
    
    this.worldModelUpdate.newEventFacts.push({
      tag: 'EventEventRelation',
      eventEventRel: rel.slice(1), // slice 1 because ev-ev's start with =
      eventEventRelEventRep: reverse ? otherrep : evrep,
      eventEventRelArg: reverse ? evrep : otherrep
    });
  } else {
    if (reverse) {
      var evrep = {
        tag: -1 === this.worldModelUpdate.newEntities.indexOf(ev.id) ?
               'OldEntity' :
               'NewEntity' ,
        contents: ev.id
      };
      
      var otherrep = {
        tag: -1 === this.worldModelUpdate.newEvents.indexOf(other.id) ?
               'OldEvent' :
               'NewEvent' ,
        contents: other.id
      };
      
      this.worldModelUpdate.newEventFacts.push({
        tag: 'EventEntityRelation',
        eventEntityRel: rel,
        eventEntityRelEventRep: otherrep,
        eventEntityRelArg: evrep
      });
    } else {
      var evrep = {
        tag: -1 === this.worldModelUpdate.newEvents.indexOf(ev.id) ?
               'OldEvent' :
               'NewEvent' ,
        contents: ev.id
      };
      
      var otherrep = {
        tag: -1 === this.worldModelUpdate.newEntities.indexOf(other.id) ?
               'OldEntity' :
               'NewEntity' ,
        contents: other.id
      };
      
      this.worldModelUpdate.newEventFacts.push({
        tag: 'EventEntityRelation',
        eventEntityRel: rel,
        eventEntityRelEventRep: evrep,
        eventEntityRelArg: otherrep
      });
    }
  }
};
LanguageEngine.Conversation.prototype.pushUpdates = function () {
  var updateInfo = {
    tag: 'ConversationUpdateWorldModel',
    newEntities: this.worldModelUpdate.newEntities,
    newEvents: this.worldModelUpdate.newEvents,
    newEntityFacts: this.worldModelUpdate.newEntityFacts,
    newEventFacts: this.worldModelUpdate.newEventFacts
  };
  var promise = new LanguageEngine.Promise();
  var req = new XMLHttpRequest();
  req.onload = function () {
    if (200 === req.status) {
      promise.succeed();
    } else {
      promise.fail();
    }
  };
  req.open('put', this.apiInfo.apiURL + '/conversations/' + this.convoID.toString(), true);
  req.setRequestHeader("Content-Type", "application/json");
  req.send(JSON.stringify(updateInfo));
  
  var self = this;
  
  promise.success(function () {
    self.hasUpdates = false;
    self.worldModelUpdate.newEntities = [];
    self.worldModelUpdate.newEvents = [];
    self.worldModelUpdate.newEntityFacts = [];
    self.worldModelUpdate.newEventFacts = [];
    
    var wm = JSON.parse(req.responseText).newWorldModel;
    console.log(LanguageEngine.Conversation.convertWorldModel(wm));
  });
  
  
  return promise;
};
LanguageEngine.Conversation.prototype.resetWorldModel = function () {
  var updateInfo = {
    tag: 'ConversationUpdateWorldModel',
    contents: []
  };
  var promise = new LanguageEngine.Promise();
  var req = new XMLHttpRequest();
  req.onload = function () {
    if (200 === req.status) {
      promise.succeed();
    } else {
      promise.fail();
    }
  };
  req.open('put', this.apiInfo.apiURL + '/conversations/' + this.convoID.toString(), true);
  req.setRequestHeader("Content-Type", "application/json");
  req.send(JSON.stringify(updateInfo));
  
  var self = this;
  
  promise.success(function () {
    self.hasUpdates = false;
    self.worldModelUpdate.newEntities = [];
    self.worldModelUpdate.newEvents = [];
    self.worldModelUpdate.newEntityFacts = [];
    self.worldModelUpdate.newEventFacts = [];
    
    var wm = JSON.parse(req.responseText).newWorldModel;
    console.log(LanguageEngine.Conversation.convertWorldModel(wm));
  });
  
  
  return promise;
};
LanguageEngine.Conversation.prototype.processInput = function (str) {
  var discourseMoveInfo = {
    tag: 'ConversationUpdateDiscourseMove',
    move: str
  };
  var self = this;
  var promise = new LanguageEngine.Promise();
  
  var req = new XMLHttpRequest();
  req.onload = function () {
    if (200 === req.status) {
      promise.succeed();
    } else {
      promise.fail();
    }
  };
  req.open('put', this.apiInfo.apiURL + '/conversations/' + this.convoID, true);
  req.setRequestHeader("Content-Type", "application/json");
  req.send(JSON.stringify(discourseMoveInfo));
  
  promise.success(function () {
    
    var parsed = JSON.parse(req.responseText);
    
    self.worldModel = LanguageEngine.Conversation.convertWorldModel(parsed.newWorldModel);
    parsed.eventDescriptions.
      map(function (ed) {
        return self.convertEventDescription(ed);
      }).
      forEach(function (ed) {
        self.dispatch(ed);
      });
    
  });
  
  return promise;
};
LanguageEngine.Conversation.prototype.setDefaultHandler = function (callback) {
  this.defaultHandler = callback;
};
LanguageEngine.Conversation.prototype.setHandler = function (pred, argRels, callback) {
  this.handlers[pred] = this.handlers[pred] || new LanguageEngine.Trie();
  var sortedArgRels = argRels.splice(0,[]).sort();
  this.handlers[pred].insert(sortedArgRels, {
    argRels: argRels,
    callback: callback
  });
};
LanguageEngine.Conversation.prototype.dispatch = function (desc) {
  if (desc.foreground) {
    var pred = desc.pred;
    
    if (this.handlers[pred]) {
      var handlerInfo = this.handlers[pred].
                          follow(Object.keys(desc.args).sort()).
                          sort(function (a,b) { return b.value.argRels.length - a.value.argRels.length; }).
                          shift();
      if (handlerInfo) {
        var args = [desc];
        for (var i = 0; i < handlerInfo.value.argRels.length; i++) {
          args.push(desc.args[handlerInfo.value.argRels[i]]);
        }
        
        handlerInfo.value.callback.apply(null, args);
      } else {
        this.defaultHandler(desc);
      }
      
    } else if (this.defaultHandler) {
      this.defaultHandler(desc);
    }
  }
};
LanguageEngine.Conversation.convertWorldModel = function (wm) {
  var worldModel = {
    entities: wm.entities,
    nextEntity: wm.entities.maximum(-1) + 1,
    events: wm.events,
    nextEvent: wm.events.maximum(-1) + 1,
    eventFacts: {},
    eventReverse: {},
    entityReverse: {},
    entityFacts: {}
  };
  
  for (var i = 0; i < wm.entities.length; i++) {
    var entID = wm.entities[i];
    worldModel.entityFacts[entID] = {
      stringVal: null
    };
    worldModel.entityReverse[entID] = {};
  }
  
  for (var i = 0; i < wm.events.length; i++) {
   var evID = wm.events[i];
   worldModel.eventFacts[evID] = { pred: null, args: {} }; 
   worldModel.eventReverse[evID] = {};
  }
  
  for (var i = 0; i < wm.eventFacts.length; i++) {
    var evFact = wm.eventFacts[i];
    
    if ('EventPredicate' === evFact.tag) {
      
      var evID = evFact.eventPredEventRef;
      worldModel.eventFacts[evID].pred = evFact.eventPred;
      
    } else if ('EventEntityRelation' === evFact.tag) {
      
      var evID = evFact.eventEntityRelEventRef;
      
      var rel = evFact.eventEntityRel;
      var entID = evFact.eventEntityRelEntityArgRef;
      
      worldModel.eventFacts[evID].args[rel] = worldModel.eventFacts[evID].args[rel] || [];
      worldModel.eventFacts[evID].args[rel].push(entID);
      
      if (entID >= 0) { // entID < 0 means its a purely internal ent, e.g. the user or computer
        worldModel.entityReverse[entID][rel] = worldModel.entityReverse[entID][rel] || [];
        worldModel.entityReverse[entID][rel].push(evID);
      }
      
    } else if ('EventEventRelation' === evFact.tag) {
      
      var evID = evFact.eventEventRelEventRef;
      
      var rel = LanguageEngine.sigils.event + evFact.eventEventRel;
      var ev2ID = evFact.eventEventRelEventArgRef;
      
      worldModel.eventFacts[evID].args[rel] = worldModel.eventFacts[evID].args[rel] || [];
      worldModel.eventFacts[evID].args[rel].push(ev2ID);
      
      worldModel.eventReverse[ev2ID][rel] = worldModel.eventReverse[ev2ID][rel] || [];
      worldModel.eventReverse[ev2ID][rel].push(evID);
      
    }
  }
  
  for (var i = 0; i < wm.entityFacts.length; i++) {
    var entFact = wm.entityFacts[i];
    
    var entID = entFact.entityRef;
    
    if (entID >= 0) { // entID < 0 means its a purely internal ent, e.g. the user or computer
      worldModel.entityFacts[entID].stringVal = entFact.stringVal;
    }
  }
  
  return worldModel;
};
LanguageEngine.Conversation.prototype.convertEventDescription = function (desc) {
  var self = this;
  var args = {};
  
  for (var rel in desc.eventArguments) {
    args[rel] = desc.eventArguments[rel].map(function (evID) {
      return new LanguageEngine.Event(self, evID);
    });
  }
  for (var rel in desc.entityArguments) {
    args[rel] = desc.entityArguments[rel].map(function (entID) {
      return new LanguageEngine.Entity(self, entID);
    });
  }
  
  return {
    event: new LanguageEngine.Event(this, desc.eventRef),
    pred: desc.predicate,
    foreground: desc.foreground,
    nonexistent: desc.nonexistent,
    args: args
  };
};

// Entity
LanguageEngine.Entity = function (convoCtrl,id) {
  this.conversationController = convoCtrl;
  this.id = id;
};
LanguageEngine.Entity.prototype.isUser = function () {
  return this.id === -1;
};
LanguageEngine.Entity.prototype.isComputer = function () {
  return this.id === -2;
};
LanguageEngine.Entity.prototype.data = function () {
  return this.conversationController.entityData[this.id];
};
LanguageEngine.Entity.prototype.setData = function (d) {
  this.conversationController.entityData[this.id] = d;
};
LanguageEngine.Entity.prototype.stringVal = function () {
  return this.conversationController.worldModel.entityFacts[this.id].stringVal;
};
LanguageEngine.Entity.prototype.any = function (rel, p) {
  var reverse = rel.charAt(0) === LanguageEngine.sigils.reverse;
  if (reverse) {
    rel = rel.slice(1);
  }
  
  if (!reverse) {
    console.error('Entities to not have arguments.');
  } else if ('stringVal' === rel) {
    console.error('Entities are not the string values of anything.');
  }
  
  if (!this.conversationController.worldModel.entityReverse[this.id][rel]) { return false; }
  var toTest = this.conversationController.worldModel.entityReverse[this.id][rel];
  
  for (var i = 0; i < toTest.length; i++) {
    if(p(new LanguageEngine.Event(this.conversationController, toTest[i]))) { return true; }
  }
  
  return false;
};
LanguageEngine.Entity.prototype.all = function (rel, p) {
  return !this.any(rel, function (x) { return !p(x); });
};

// Event
LanguageEngine.Event = function (convoCtrl,id) {
  this.conversationController = convoCtrl;
  this.id = id;
};
LanguageEngine.Event.prototype.pred = function () {
  return this.conversationController.worldModel.eventFacts[this.id].pred;
};
LanguageEngine.Event.prototype.any = function (rel, p) {
  var reverse = rel.charAt(0) === LanguageEngine.sigils.reverse;
  if (reverse) {
    rel = rel.slice(1);
  }
  
  var eventEvent = rel.charAt(0) === LanguageEngine.sigils.event;
  
  var toTest;
  if (eventEvent) {
    // rel : Event -> Event -> Prop
    if (reverse) {
      if (!this.conversationController.worldModel.eventReverse[this.id][rel]) { return false; }
      toTest = this.conversationController.worldModel.eventReverse[this.id][rel];
    } else {
      if (!this.conversationController.worldModel.eventFacts[this.id].args[rel]) { return false; }
      toTest = this.conversationController.worldModel.eventFacts[this.id].args[rel];
    }
    
    for (var i = 0; i < toTest.length; i++) {
      if(p(new LanguageEngine.Event(this.conversationController, toTest[i]))) { return true; }
    }
  } else {
    // rel : Event -> Entity -> Prop
    if (reverse) {
      // e <--rel-- x
      console.error('Can\'t test a reverse event-entity relation on an event.');
    } else {
      // e --rel--> x
      if (!this.conversationController.worldModel.eventFacts[this.id].args[rel]) { return false; }
      toTest = this.conversationController.worldModel.eventFacts[this.id].args[rel];
    }
    
    for (var i = 0; i < toTest.length; i++) {
      if(p(new LanguageEngine.Entity(this.conversationController, toTest[i]))) { return true; }
    }
  }
  
  return false;
};
LanguageEngine.Event.prototype.all = function (rel, p) {
  return !this.any(rel, function (x) { return !p(x); });
};

// Promise
LanguageEngine.Promise = function () {
  this.isReady = false;
  this.didSucceed = false;
  this.onSuccess = [];
  this.onError = [];
};
LanguageEngine.Promise.prototype.succeed = function () {
  this.isReady = true;
  this.didSucceed = true;
  for (var i = 0; i < this.onSuccess.length; i++) {
    setTimeout(this.onSuccess[i]);
  }
};
LanguageEngine.Promise.prototype.fail = function () {
  this.isReady = true;
  this.didSucceed = false;
  for (var i = 0; i < this.onError.length; i++) {
    setTimeout(this.onError[i]);
  }
};
LanguageEngine.Promise.prototype.success = function (callback) {
  if (this.isReady) {
    if (this.didSucceed) {
      setTimeout(callback);
    }
  } else {
    this.onSuccess.push(callback);
  }
  
  return this;
};
LanguageEngine.Promise.prototype.error = function (callback) {
  if (this.isReady) {
    if (!this.didSucceed) {
      setTimeout(callback);
    }
  } else {
    this.onError.push(callback);
  }
  
  return this;
};

// Trie
LanguageEngine.Trie = function (value) {
  this.value = value;
  this.daughters = {};
};
LanguageEngine.Trie.prototype.insert = function (path, value) {
  if (0 === path.length) {
    this.value = value;
  } else {
    var key = path.shift()
    var next = this.daughters[key];
    if (next) {
      next.insert(path, value);
    } else {
      this.daughters[key] = new LanguageEngine.Trie();
      this.daughters[key].insert(path, value)
    }
  }
};
LanguageEngine.Trie.prototype.follow = function (sortedPath) {
  var matches = [];
  
  if (this.value) {
    matches = matches.append([{ keys: [], value: this.value }]);
  }
  
  if (0 !== sortedPath.length) {
    for (var key in this.daughters) {
      var nextPath = sortedPath.dropWhile(function (ps) { return ps < key; });
      var pathSegment = nextPath.shift();
      if (key === pathSegment) {
        var recs = this.daughters[key].follow(nextPath);
        matches = matches.append(recs.map(function (rec) {
          rec.keys.unshift(key);
          return rec;
        }));
      }
    }
  }
  
  return matches;
};

