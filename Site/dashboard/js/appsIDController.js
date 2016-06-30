'use strict';

function subset(xs,ys) {
  return xs.every(function (x) {
    return ys.some(function (y) {
      return x == y;
    });
  });
}


function groupN(xs, n) {
  let groups = [];
  
  for (var i = 0; i < xs.length; i += n) {
    groups.push(xs.slice(i,i+n));
  }
  
  return groups;
}
    
function showFact(fact) {
  let p = fact.contents[0];
  let args = fact.contents.slice(1);
  
  return p + "(" + args.map(x => x.toString()).join(",") + ")";
}
    
function showFacts(facts) {
  return "\n  " +
         groupN(facts.reverse(), 5).
           map(group => group.map(showFact).join(", ")).
           join("\n  ");
}
    
function showREPLChange(change) {
  let factsResponse = showFacts(change.facts);
  let worldModelResponse = showFacts(change.worldModel.facts);
  
  return "response facts:" + factsResponse + "\n\nnew world model:" + worldModelResponse;
}
    
function showREPLError(err) {
  if ("MiscError" === err.tag) {
    
    return "Could not interpret input."
    
  } else if ("UnknownWord" === err.tag) {
    
    return "unknown word: " + err.replWord;
    
  } else if ("IncompleteParse" === err.tag) {
    
    let response = "incomplete parse:";
    
    err.replChart.
      sort((x,y) => x.length > y.length).
      forEach(function (bracketted) {
        response += "\n\n ";
        bracketted.forEach(function (sequence) {
          response += " [" + sequence.sequenceLabel.substring(4) +
                      " " + sequence.sequenceWords + "]";
        });
      });
    
    return response;
    
  }
}
    
function showREPLResponse(data) {
  if ("REPLConversationChange" === data.tag) {
    console.log(data.replChange);
    return data.replChange; //showREPLChange(data.replChange);
  } else if ("REPLConversationError" === data.tag) {
    return showREPLError(data.replError);
  }
}

function showGraphQLField(field) {
  var shownF = "";
  
  var f;
  
  if ("id" === field.fieldName.contents) {
    f = "id";
  } else {
    if ("Forward" === field.fieldName.tag) {
      f = "fwd_" + field.fieldName.contents;
    } else if ("Backward" === field.fieldName.tag) {
      f = "bwd_" + field.fieldName.contents;
    }
  }
  
  shownF += f;
  
  var params = [];
  
  if (field.fieldID) {
    params.push("id = " + field.fieldID);
  }
  
  if (field.fieldIsA) {
    params.push("isA = " + field.fieldIsA);
  }
  
  if (0 !== params.length) {
    shownF += "(" + params.join(", ") + ")";
  }
  
  if (field.fieldSubquery) {
    shownF += " " + showSimpleGraphQLQuery(field.fieldSubquery);
  }
  
  return shownF;
}

function showSimpleGraphQLQuery(fields) {
  return "{ " + fields.map(f => showGraphQLField(f)).join(", ") + " }";
}

function showQuantifiedGraphQLQuery(query) {
  return query.quantifier + "[" + query.scopeVar + "](" +
           showGraphQLQuery(query.restrictorQuery) + ", " +
           showGraphQLQuery(query.scopeQuery) + ")";
}

function showMixedGraphQLQuery(query) {
  
}

function showGraphQLQuery(query) {
  console.log("showing ",query);
  if ("Simple" === query.tag) {
    return showSimpleGraphQLQuery(query.contents);
  } else if ("Quantified" === query.tag) {
    return showQuantifiedGraphQLQuery(query.contents);
  }
  
  const fn = {
    Simple: showSimpleGraphQLQuery,
    Quantified
  }
  
  return fn[query.tag](query.contents)
  
  
  x.match(
    "Simple", f,
    "Quantifiction", g
  );
}




angular.module('leApp').
  controller('AppsIDController', ['$scope', '$route', '$location', 'apiService',
  function ($scope, $route, $location, apiService) {
    
    $scope.cmOption = {
      lineWrapping: true,
      readOnly: 'nocursor',
      theme: 'base16-light',
      onLoad: function (cm) {
        cm.setSize(null, "200px");
        cm.on('change', function () {
          cm.execCommand('goDocEnd');
        });
      }
    };
    
    $scope.appInfo = null;
    
    apiService.apps.id.read($route.current.params.id).success(function (data) {
      $scope.appInfo = data;
    });
    
    $scope.deleteApp = function () {
      if (confirm("Delete this app?")) {
        apiService.apps.id.destroy($scope.appInfo.appID).
          success(function () {
            $location.path('/apps');
          });
      }
    };
    
    $scope.appEditing = {
      editing: false,
      name: "",
      description: "",
      hasError: false,
      error: ""
    };
    
    $scope.editApp = function () {
      $scope.appEditing.editing = true;
      $scope.appEditing.hasError = false;
      $scope.appEditing.name = $scope.appInfo.appName;
      $scope.appEditing.description = $scope.appInfo.appDescription;
    };
    $scope.cancelEditingApp = function () {
      $scope.appEditing.editing = false;
    };
    $scope.saveAppChanges = function () {
      var appUpdate = {};
      var needsUpdate = false;
      
      if ($scope.appInfo.appName !== $scope.appEditing.name) {
        needsUpdate = true;
        appUpdate.name = $scope.appEditing.name;
      }
      
      if ($scope.appInfo.appDescription !== $scope.appEditing.description) {
        needsUpdate = true;
        appUpdate.description = $scope.appEditing.description;
      }
      
      if (needsUpdate) {
        apiService.apps.id.update($scope.appInfo.appID, appUpdate).
          success(function (data) {
            $scope.appEditing.editing = false;
            $scope.appInfo.appName = $scope.appEditing.name;
            $scope.appInfo.appDescription = $scope.appEditing.description;
          }).
          error(function (response) {
            $scope.appEditing.hasError = true;
            $scope.appEditing.error = response.statusText;
          });
      } else {
        $scope.appEditing.editing = false;
      }
    };
    
    
    
    $scope.makeNewToken = function () {
      apiService.apps.id.tokens.create($scope.appInfo.appID).
        success(function(data) {
          $scope.appInfo.tokens.push(data);
        });
    };
    $scope.deleteToken = function (tok) {
      if (confirm("Delete this token?")) {
        apiService.apps.id.tokens.id.destroy($scope.appInfo.appID, tok.tokenID).
          success(function () {
            $scope.appInfo.tokens = $scope.appInfo.tokens.filter(function (tok2) {
              return tok !== tok2;
            });
          });
      }
    };
    
    
    
    $scope.appPackagesEditing = {
      editing: false,
      packages: [],
      candidatePackages: [],
      hasError: false,
      error: ""
    };
    
    $scope.editPackages = function () {
      $scope.appPackagesEditing.editing = true;
      $scope.appPackagesEditing.hasError = false;
      $scope.appPackagesEditing.packages = $scope.appInfo.packageSummaries.map(function (summary) {
        return summary.packageIDSummary;
      });
      $scope.appPackagesEditing.candidatePackages = [];
      
      apiService.packages.read().
        success(function (data) {
          data.forEach(function (summary) {
            if (!summary.packageIsPreludeSummary) {
              summary.isUsed = $scope.appPackagesEditing.packages.includes(summary.packageIDSummary);
              $scope.appPackagesEditing.candidatePackages.push(summary);
            }
          });
        });
    };
    $scope.savePackageChanges = function () {
      let newPackages = $scope.appPackagesEditing.candidatePackages.filter(function (info) {
        return info.isUsed && !info.packageIsPreludeSummary;
      });
      
      let newPackageIDs = newPackages.map(function (info) {
        return info.packageIDSummary;
      });
      
      let needsUpdate =
        !subset(newPackageIDs, $scope.appPackagesEditing.packages) ||
        !subset($scope.appPackagesEditing.packages, newPackageIDs);
      
      if (needsUpdate) {
        var overlap1;
        var overlap2;
        let overlappingModuleNames = newPackages.some(function (p) {
          return newPackages.some(function (p2) {
            return p.packageIDSummary !== p2.packageIDSummary &&
                   p.packageModuleNamesSummary.some(function (mn) {
                     overlap1 = p;
                     overlap2 = p2;
                     return p2.packageModuleNamesSummary.includes(mn);
                   });
          });
        });
        
        if (overlappingModuleNames) {
          
          $scope.appPackagesEditing.hasError = true;
          $scope.appPackagesEditing.error =
            "The packages " + overlap1.packageNameSummary + " and " +
            overlap2.packageNameSummary + " cannot be used together " +
            " because they have some module names in common.";
          
        } else {
          apiService.apps.id.updatePackages($scope.appInfo.appID, newPackageIDs).
            success(function () {
              $scope.appInfo.packageSummaries = newPackages;
              $scope.appPackagesEditing.editing = false;
              $scope.appPackagesEditing.hasError = false;
            }).
            error(function (response) {
              $scope.appPackagesEditing.hasError = true;
              $scope.appPackagesEditing.error = response.statusText;
            });
        }
      } else {
        $scope.appPackagesEditing.editing = false;
        $scope.appPackagesEditing.hasError = false;
      }
    };
    $scope.cancelEditingPackages = function () {
      $scope.appPackagesEditing.editing = false;
    };
    
    
    
    
    
    $scope.replInfo = {
      needsInitialization: true,
      replConvoID: null,
      input: "",
      output: ""
    };
    
    
    
    let performSendREPL = function () {
      let input = $scope.replInfo.input;
      
      if ("" !== input) {
        apiService.apps.id.replConversations.id.
          updateDiscourseMove($scope.appInfo.appID, $scope.replInfo.replConvoID, input).
          success(function (data) {
            $scope.replInfo.input = "";
            $scope.replInfo.output += ">>> " + input + "\n\n" + showREPLResponse(data) + "\n\n";
          });
      }
    };
    
    $scope.sendREPL = function () {
      if ($scope.replInfo.needsInitialization) {
        
        apiService.apps.id.replConversations.create($scope.appInfo.appID).
          success(function (data) {
            $scope.replInfo.needsInitialization = false;
            $scope.replInfo.replConvoID = data.conversationID;
            performSendREPL();
          });
          
      } else {
        performSendREPL();
      }
    };
    
    $scope.resetREPL = function () {
      $scope.replInfo.needsInitialization = true;
      $scope.replInfo.input = "";
      $scope.replInfo.output = "";
    };
    
    
    
    
    $scope.loadChart = function (errorID) {
      apiService.apps.id.parseErrorCharts.id.read($scope.appInfo.appID, errorID).
        success(function (data) {
          data.chart.forEach(function (bracketting) {
            bracketting.forEach(function (bracket) {
              bracket.sequenceLabel = bracket.sequenceLabel.substring(4);
            });
          });
          
          $scope.appInfo.parseErrorSummaries.forEach(function (err) {
            if (errorID === err.errorIDSummary) {
              err.hasChartInfo = true;
              err.chartInfo = data.chart.sort((x,y) => x.length > y.length);
            }
          });
        });
    };
    
    $scope.deleteParseError = function (errorID) {
      apiService.apps.id.parseErrors.id.destroy($scope.appInfo.appID, errorID).
        success(function () {
          $scope.appInfo.parseErrorSummaries = $scope.appInfo.parseErrorSummaries.
            filter(err => errorID !== err.errorIDSummary);
        });
    };
    
  }]);