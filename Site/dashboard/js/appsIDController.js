'use strict';

function subset(xs,ys) {
  return xs.every(function (x) {
    return ys.some(function (y) {
      return x == y;
    });
  });
}

angular.module('leApp').
  controller('AppsIDController', ['$scope', '$route', '$location', 'apiService',
  function ($scope, $route, $location, apiService) {
    
    $scope.appInfo = null;
    
    apiService.apps.id.read($route.current.params.id).success(function (data) {
      console.log(data);
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
      apiService.apps.id.tokens.id.destroy($scope.appInfo.appID, tok.tokenID).
        success(function () {
          $scope.appInfo.tokens = $scope.appInfo.tokens.filter(function (tok2) {
            return tok !== tok2;
          });
        });
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
      
      apiService.packages.read().
        success(function (data) {
          data.forEach(function (summary) {
            summary.isUsed = $scope.appPackagesEditing.packages.includes(summary.packageIDSummary);
            $scope.appPackagesEditing.candidatePackages.push(summary);
          });
        });
    };
    $scope.savePackageChanges = function () {
      var newPackages = $scope.appPackagesEditing.candidatePackages.filter(function (info) {
        return info.isUsed;
      });
      
      var newPackageIDs = newPackages.map(function (info) {
        return info.packageIDSummary;
      });
      
      var needsUpdate =
        !subset(newPackageIDs, $scope.appPackagesEditing.packages) ||
        !subset($scope.appPackagesEditing.packages, newPackageIDs);
      
      if (needsUpdate) {
        apiService.apps.id.updatePackages($scope.appInfo.appID, newPackageIDs).
          success(function () {
            $scope.appInfo.packageSummaries = newPackages;
            $scope.appPackagesEditing.editing = false;
          }).
          error(function (response) {
            $scope.appPackagesEditing.hasError = true;
            $scope.appPackagesEditing.error = response.statusText;
          });
      } else {
        $scope.appPackagesEditing.editing = false;
      }
    };
    $scope.cancelEditingPackages = function () {
      $scope.appPackagesEditing.editing = false;
    };
    
  }]);