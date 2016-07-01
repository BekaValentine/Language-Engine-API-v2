let Prop = {};

Prop.Background = function (x) {
  this.tag = "Background";
  this.variable = x;
};

Prop.Nonexistent = function (x) {
  this.tag = "Nonexistent";
  this.variable = x;
};

Prop.Pred = function (p,x) {
  this.tag = "Pred";
  this.predicate = p;
  this.variable = x;
};

Prop.Rel = function (r,x,y) {
  this.tag = "Rel";
  this.relation = r;
  this.variable1 = x;
  this.variable2 = y;
};

Prop.Act = function (a,p) {
  this.tag = "Act";
  this.act = a;
  this.proposition = p;
};

Prop.Speaker = function (x) {
  this.tag = "Speaker";
  this.variable = x;
};

Prop.Addressee = function (x) {
  this.tag = "Addressee";
  this.variable = x;
};

Prop.And = function (p,q) {
  this.tag = "And";
  this.leftConjunct = p;
  this.rightConjunct = q;
};

Prop.Sigma = function (sc) {
  this.tag = "Sigma";
  this.scope = sc;
};

Prop.Quant = function (q,e,rs,sc) {
  this.tag = "Quant";
  this.quantifier = q;
  this.variable = e;
  this.restrictor = rs;
  this.scope = sc;
};

Prop.pretty = function (p) {
  
  if ("Background" === p.tag) {
    
    return "Background(" + p.variable.toString() + ")";
    
  } else if ("Nonexistent" === p.tag) {
    
    return "Nonexistent(" + p.variable.toString() + ")";
    
  } else if ("Pred" === p.tag) {
    
    return "Pred(" + p.predicate + "," + p.variable.toString() + ")";
    
  } else if ("Rel" === p.tag) {
    
    return "Rel(" + p.relation + "," + p.variable1.toString() + "," + p.variable2.toString() + ")";
    
  } else if ("Act" === p.tag) {
    
    return "Act(" + p.act + "," + Prop.pretty(p.proposition) + ")";
    
  } else if ("Speaker" === p.tag) {
    
    return "Speaker(" + p.variable.toString() + ")";
    
  } else if ("Addressee" === p.tag) {
    
    return "Addressee(" + p.variable.toString() + ")";
    
  } else if ("And" === p.tag) {
    
    return "And(" + Prop.pretty(p.leftConjunct) + "," + Prop.pretty(p.rightConjunct) + ")";
    
  } else if ("Sigma" === p.tag) {
    
    return "Sigma(" + Prop.pretty(p.scope) + ")";
    
  } else if ("Quant" === p.tag) {
    
    return "Quant(" + p.quantifier + "," + p.variable.toString() + "," + Prop.pretty(p.restrictor) + "," + Prop.pretty(p.scope) + ")";
    
  }
};






function getProp(deserializer) {
  let tag = deserializer.get();
  
  if (0 === tag) {
    
    return new Prop.Background(DataBinary.getInt(deserializer));
    
  } else if (1 === tag) {
    
    return new Prop.Nonexistent(DataBinary.getInt(deserializer));
    
  } else if (2 === tag) {
    
    return new Prop.Pred(DataBinary.getString(deserializer),
                         DataBinary.getInt(deserializer));
    
  } else if (3 === tag) {
    
    return new Prop.Rel(DataBinary.getString(deserializer),
                        DataBinary.getInt(deserializer),
                        DataBinary.getInt(deserializer));
    
  } else if (4 === tag) {
    
    return new Prop.Act(DataBinary.getString(deserializer),
                        getProp(deserializer));
    
  } else if (5 === tag) {
    
    return new Prop.Speaker(DataBinary.getInt(deserializer));
    
  } else if (6 === tag) {
    
    return new Prop.Addressee(DataBinary.getInt(deserializer));
    
  } else if (7 === tag) {
    
    return new Prop.And(getProp(deserializer), getProp(deserializer));
    
  } else if (8 === tag) {
    
    return new Prop.Sigma(getProp(deserializer));
    
  } else if (9 === tag) {
    
    return new Prop.Quant(DataBinary.getString(deserializer),
                          DataBinary.getInt(deserializer),
                          getProp(deserializer),
                          getProp(deserializer));
    
  }
  
}