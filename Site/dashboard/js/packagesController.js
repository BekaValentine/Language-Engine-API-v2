'use strict';

angular.module('leApp').
  controller('PackagesController', ['$scope','apiService',
    function ($scope, apiService) {
      
      // get the current apps
      apiService.packages.read().success(function (data) {
        var ownPackages = [];
        var publicPackages = [];
        
        data.forEach(function (pkg) {
          if (pkg.packageIsOwnSummary) {
            ownPackages.push(pkg);
          } else {
            publicPackages.push(pkg);
          }
        });
        
        $scope.packageSummaries = {
          ownPackages: ownPackages,
          publicPackages: publicPackages
        };
      });
      
      
      // create a new package
      $scope.newPackageInfo = {
        name: "",
        hasResponse: false,
        response: ""
      };
      
      $scope.packageInfoIsValid = function () {
        return $scope.newPackageInfo.name !== "";
      };
      $scope.createPackage = function () {
        apiService.packages.
          create($scope.newPackageInfo.name).
          success(function (data) {
            $scope.packageSummaries.ownPackages.push(data);
            $scope.newPackageInfo.hasResponse = true;
            $scope.newPackageInfo.response = "Your package has been created.";
            $scope.newPackageInfo.name = "";
          }).
          error(function (response) {
            $scope.newPackageInfo.hasResponse = true;
            $scope.newPackageInfo.response = "Error: " + response.statusText;
          });
      };
      
      
    }]);