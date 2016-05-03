'use strict';

angular.module('leApp').
  controller('SettingsController', ['$scope', 'apiService',
    function ($scope, apiService) {
      
      $scope.emailInfo = {
        email: ""
      };
      
      apiService.users.id.read().
        success(function (data) {
          $scope.emailInfo.email = data.emailAddress;
        });
      
      $scope.newEmailInfo = {
        newEmail: "",
        hasResponse: false,
        response: ""
      };
      
      $scope.newEmailIsValid = function () {
        return $scope.newEmailInfo.newEmail !== "";
      };
      
      $scope.setNewEmail = function () {
        apiService.users.id.updateEmail($scope.newEmailInfo.newEmail).
          success(function () {
            $scope.emailInfo.email = $scope.newEmailInfo.newEmail;
            $scope.newEmailInfo.newEmail = "";
            $scope.newEmailInfo.hasResponse = true;
            $scope.newEmailInfo.response = "Your email has been changed.";
          }).
          error(function (response) {
            $scope.newEmailInfo.hasResponse = true;
            $scope.newEmailInfo.response = "Error: " + response.statusText;
          });
      };
      
      $scope.newPasswordInfo = {
        newPassword: "",
        confirmNewPassword: ""
      };
      
      $scope.newPasswordResponse = {
        hasResponse: false,
        response: ""
      };
      
      $scope.newPasswordIsValid = function () {
        var i = $scope.newPasswordInfo;
        
        return !!i.newPassword && '' !== i.newPassword &&
               i.newPassword === i.confirmNewPassword;
      };
      
      $scope.setNewPassword = function () {
        $scope.newPasswordResponse.hasResponse = false;
        
        apiService.users.id.updatePassword($scope.newPasswordInfo.newPassword).
          success(function () {
            $scope.newPasswordResponse.hasResponse = true;
            $scope.newPasswordResponse.response = "Your password has been changed.";
            $scope.newPasswordInfo.newPassword = "";
            $scope.newPasswordInfo.confirmNewPassword = "";
          }).
          error(function (data) {
            $scope.newPasswordResponse.hasResponse = true;
            $scope.newPasswordResponse.response = apiService.error(data);
          });
      };
      
    }]);