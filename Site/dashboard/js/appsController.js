'use strict';

angular.module('leApp').
  controller('AppsController', ['$scope','apiService',
    function ($scope, apiService) {
      
      // get the current apps
      apiService.apps.read().success(function (data) {
        $scope.appSummaries = data;
      });
      
      
      // create a new app
      $scope.newAppInfo = {
        name: "",
        hasResponse: false,
        response: ""
      };
      
      $scope.appInfoIsValid = function () {
        return $scope.newAppInfo.name !== "";
      };
      $scope.addApp = function () {
        apiService.apps.
          create($scope.newAppInfo.name).
          success(function (data) {
            $scope.appSummaries.push(data);
            $scope.newAppInfo.name = "";
            $scope.newAppInfo.hasResponse = true;
            $scope.newAppInfo.response = "Your app has been created."
          }).
          error(function (response) {
            $scope.newAppInfo.hasResponse = true;
            $scope.newAppInfo.response = "Error: " + response.statusText;
          });
      };
      
    }]);