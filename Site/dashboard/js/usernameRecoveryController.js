'use strict';

angular.module('leApp').
  controller('UsernameRecoveryController', ['$scope','apiService',
    function ($scope, apiService) {
      
      $scope.recoveryInfo = {
        email: ""
      };
      
      $scope.recoveryResponse = {
        hasResponse: false,
        response: "",
        clicked: false
      };
      
      $scope.recoverUsername = function () {
        $scope.recoveryResponse.clicked = true;
        apiService.login.recoverUsername($scope.recoveryInfo.email).
          success(function () {
            $scope.recoveryResponse.hasResponse = true;
            $scope.recoveryResponse.response = "Your username has been sent to your email.";
          }).
          error(function (data) {
            $scope.recoveryResponse.hasResponse = true;
            $scope.recoveryResponse.response = apiService.error(data);
            $scope.recoveryResponse.clicked = false;
          });
      };
      
    }]);