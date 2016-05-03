'use strict';

angular.module('leApp').
  controller('PasswordResetController', ['$scope', '$location', 'apiService',
    function ($scope, $location, apiService) {
      
      $scope.resetInfo = {
        username: "",
        resetCode: "",
        newPassword: "",
        confirmNewPassword: ""
      };
      
      $scope.requestResponse = {
        hasResponse: false,
        response: "",
        clicked: false
      };
      
      $scope.resetResponse = {
        hasResponse: false,
        response: "",
        clicked: false,
        successful: false
      };
      
      $scope.requestIsValid = function () {
        var un = $scope.resetInfo.username;
        
        return !!un && '' !== un;
      };
      
      $scope.requestReset = function () {
        $scope.requestResponse.clicked = true;
        apiService.login.passwordResetRequest($scope.resetInfo.username).
          success(function () {
            $scope.requestResponse.hasResponse = true;
            $scope.requestResponse.response = "Your password reset code has been sent.";
          }).
          error(function (data) {
            $scope.requestResponse.hasResponse = true;
            $scope.requestResponse.response = apiService.error(data);
            $scope.requestResponse.clicked = false;
          });
      };
      
      $scope.resetValid = function () {
        var i = $scope.resetInfo;
        
        return !!i.resetCode && '' !== i.resetCode &&
               !!i.newPassword && '' !== i.newPassword &&
               i.newPassword === i.confirmNewPassword;
      };
      
      $scope.resetValidityError = function () {
        var i = $scope.resetInfo;
        
        if (!i.resetCode || '' === i.resetCode) {
          return "No reset code entered."
        } else if (!i.newPassword || '' === i.newPassword) {
          return "No new password entered."
        } else if (i.newPassword !== i.confirmNewPassword) {
          return "Passwords do not match."
        }
      };
      
      $scope.completeReset = function () {
        $scope.resetResponse.clicked = true;
        apiService.login.passwordResetConfirm($scope.resetInfo.username, $scope.resetInfo.resetCode, $scope.resetInfo.newPassword).
          success(function () {
            $scope.resetResponse.hasResponse = true;
            $scope.resetResponse.response = "Your password has been successfully changed.";
            $scope.resetResponse.successful = true;
          }).
          error(function (data) {
            $scope.resetResponse.hasResponse = true;
            $scope.resetResponse.response = apiService.error(data);
            $scope.resetResponse.clicked = false;
          });
      };
      
    }]);