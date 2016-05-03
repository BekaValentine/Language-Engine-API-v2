'use strict';

angular.module('leApp').
  controller('LoginController', ['$scope', '$location', 'apiService', function ($scope, $location, apiService) {
    
    
    $scope.loginInfo = {
      username: "",
      password: ""
    };
    
    $scope.loginError = {
      hasError: false,
      error: ""
    };
    
    $scope.isLoggedIn = function () {
      return apiService.login.isLoggedIn();
    };
    
    $scope.login = function () {
      apiService.login.
        login($scope.loginInfo.username, $scope.loginInfo.password).
        success(function () {
          $location.path('/app');
        }).
        catch(function (response) {
          $scope.loginError.hasError = true;
          $scope.loginError.error = response.statusText;
        });
    };
    
    $scope.logout = function () {
      apiService.login.logout();
      $scope.loginError.hasError = false;
      $location.path('/');
    };
    
    
    
    $scope.signupInfo = {
      username: "",
      email: "",
      password: "",
      passwordConfirm: ""
    };
    
    $scope.signupResult = {
      hasResult: false,
      result: ""
    };
    
    $scope.signupValid = function (info) {
      var i = $scope.signupInfo;
      return !!i.username && '' !== i.username &&
             info.email.$valid &&
             !!i.username && '' !== i.password &&
             i.password === i.passwordConfirm;
    };
    
    $scope.signupValidityError = function (info) {
      var i = $scope.signupInfo;
      if (!i.username || '' === i.username) {
        return "Username cannot be empty.";
      } else if (!info.email.$valid) {
        return "Not a valid email address.";
      } else if (!i.password || '' === i.password) {
        return "Password cannot be empty.";
      } else if (i.password !== i.passwordConfirm) {
        return "Passwords don't match.";
      } else {
        return "";
      }
    };
    
    $scope.signup = function () {
      var i = $scope.signupInfo;
      apiService.users.create(i.username, i.email, i.password).
        success(function () {
          $scope.signupResult.hasResult = true;
          $scope.signupResult.result = "You have successfully signed up. You will be redirected momentarily."
          apiService.login.login(i.username, i.password).
            success(function () {
              $location.path('/apps');
            });
        }).
        error(function (data) {
          $scope.signupResult.hasResult = true;
          $scope.signupResult.result = apiService.error(data);
        });
    };
  }]);