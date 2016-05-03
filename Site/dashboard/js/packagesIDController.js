'use strict';

angular.module('leApp').
  controller('PackagesIDController', ['$scope', '$route', '$location', 'apiService',
  function ($scope, $route, $location, apiService) {
    
    $scope.cmOption = {
      lineNumbers: true,
      mode: 'golem',
      theme: 'base16-light'
    };
    
    $scope.packageInfo = null;
    
    apiService.packages.id.read($route.current.params.id).success(function (data) {
      $scope.packageInfo = data;
      $scope.packageInfo.userCanMakePrelude = apiService.login.isAdmin();
    });
    
    $scope.packageEditing = {
      editing: false,
      name: "",
      description: "",
      hasError: false,
      error: ""
    };
    
    $scope.editPackage = function () {
      $scope.packageEditing.editing = true;
      $scope.packageEditing.hasError = false;
      $scope.packageEditing.name = $scope.packageInfo.packageName;
      $scope.packageEditing.description = $scope.packageInfo.packageDescription;
      $scope.packageEditing.isPrelude = $scope.packageInfo.packageIsPrelude;
      $scope.packageEditing.usesPrelude = $scope.packageInfo.packageUsesPrelude;
      $scope.packageEditing.isPublic = $scope.packageInfo.packageIsPublic;
    };
    $scope.cancelEditingPackage = function () {
      $scope.packageEditing.editing = false;
    };
    $scope.savePackageChanges = function () {
      var packageUpdate = {};
      var needsUpdate = false;
      
      if ($scope.packageInfo.packageName !== $scope.packageEditing.name) {
        needsUpdate = true;
        packageUpdate.name = $scope.packageEditing.name;
      }
      
      if ($scope.packageInfo.packageDescription !== $scope.packageEditing.description) {
        needsUpdate = true;
        packageUpdate.description = $scope.packageEditing.description;
      }
      
      if ($scope.packageInfo.packageIsPrelude !== $scope.packageEditing.isPrelude) {
        needsUpdate = true;
        packageUpdate.isPrelude = $scope.packageEditing.isPrelude;
      }
      
      if ($scope.packageInfo.packageUsesPrelude !== $scope.packageEditing.usesPrelude) {
        needsUpdate = true;
        packageUpdate.usesPrelude = $scope.packageEditing.usesPrelude;
      }
      
      if ($scope.packageInfo.packageIsPublic !== $scope.packageEditing.isPublic) {
        needsUpdate = true;
        packageUpdate.isPublic = $scope.packageEditing.isPublic;
      }
      
      if (needsUpdate) {
        var onSuccess = function () {
            $scope.packageInfo.packageName = $scope.packageEditing.name;
            $scope.packageInfo.packageDescription = $scope.packageEditing.description;
            $scope.packageInfo.packageIsPrelude = $scope.packageEditing.isPrelude;
            $scope.packageInfo.packageUsesPrelude = $scope.packageEditing.usesPrelude;
            $scope.packageInfo.packageIsPublic = $scope.packageEditing.isPublic;
            $scope.packageEditing.editing = false;
        };
        
        var onError = function (response) {
          $scope.packageEditing.hasError = true;
          $scope.packageEditing.error = response.statusText;
        };
        
        apiService.packages.id.update($scope.packageInfo.packageID, packageUpdate).
          then(onSuccess, onError);
      } else {
        $scope.packageEditing.editing = false;
      }
    };
    
    $scope.enforceIsPrelude = function () {
      $scope.packageEditing.usesPrelude = false;
    };
    $scope.enforceUsesPrelude = function () {
      $scope.packageEditing.isPrelude = false;
    };
    
    $scope.packageNameEditing = {
      editing: false,
      name: "",
      hasError: false,
      error: ""
    };
    
    $scope.buildInfo = {
      hasError: false,
      error: ""
    };
    
    $scope.buildPackage = function () {
      apiService.packages.id.build.create($scope.packageInfo.packageID).
        success(function (data) {
          if (data.errorMessage && data.errorMessage !== "") {
            $scope.buildInfo.hasError = true;
            $scope.buildInfo.error = data.errorMessage;
          } else {
            $scope.packageInfo.packageNeedsBuild = false;
            $scope.buildInfo.hasError = false;
          }
        });
    };
    
    $scope.deletePackage = function () {
      apiService.packages.id.destroy($scope.packageInfo.packageID).
        success(function () {
          $location.path('/packages');
        });
    };
    
    $scope.newFileInfo = {
      name: "",
      hasResponse: false,
      response: ""
    };
    
    $scope.fileInfoIsValid = function () {
      return $scope.newFileInfo.name !== "";
    };
    $scope.createFile = function () {
      apiService.packages.id.files.create($scope.packageInfo.packageID, $scope.newFileInfo.name).
        success(function (data) {
          $scope.packageInfo.fileSummaries.push(data);
          $scope.newFileInfo.name = "";
          $scope.newFileInfo.hasResponse = true;
          $scope.newFileInfo.response = "Your file has been created.";
        }).
        error(function (response) {
          $scope.newFileInfo.hasResponse = true;
          $scope.newFileInfo.response = "Error: " + response.statusText;
        });
    };
    
    $scope.fileInfo = {
      fileSelected: false,
      id: null,
      name: "",
      description: "",
      sourceCode: "",
      hasError: false,
      error: ""
    };
    
    $scope.fileEditing = {
      name: "",
      description: "",
      sourceCode: "",
      hasError: false,
      error: ""
    };
    
    $scope.$on('$routeChangeStart', function (ev) {
      if ($scope.fileChanged()) {
        var message = "The file that is currently open has been changed but" +
                      " has not been saved. Click OK to discard the changes" +
                      " and proceed off this page. Click cancel to remain" +
                      " this page.";
        
        if (!confirm(message)) {
          ev.defaultPrevented = true;
        }
      }
    });
    
    $scope.viewFile = function (fid) {
      $scope.sourceCodeIsSomething = false;
      
      var shouldView = true;
      
      if ($scope.fileInfo.fileSelected && $scope.fileChanged()) {
        shouldView = confirm("The current file has not been saved. Do you wan't to switch files anyway?")
      }
      
      if (shouldView) {
        apiService.packages.id.files.id.read($scope.packageInfo.packageID, fid).
          success(function (data) {
            $scope.fileInfo.fileSelected = true;
            $scope.sourceCodeIsSomething = true;
            $scope.fileInfo.id = fid;
            $scope.fileInfo.name = data.fileName;
            $scope.fileInfo.description = data.fileDescription;
            $scope.fileInfo.sourceCode = data.fileSourceCode;
            $scope.fileInfo.hasError = false;
            
            $scope.fileEditing.name = data.fileName;
            $scope.fileEditing.description = data.fileDescription;
            $scope.fileEditing.sourceCode = data.fileSourceCode;
            $scope.fileEditing.hasError = false;
          }).
          error(function (response) {
            $scope.fileInfo.hasError = true;
            $scope.fileInfo.error = response.statusText;
          });
      }
    };
    
    $scope.fileChanged = function () {
      return $scope.fileInfo.name !== $scope.fileEditing.name ||
             $scope.fileInfo.description !== $scope.fileEditing.description ||
             $scope.fileInfo.sourceCode !== $scope.fileEditing.sourceCode;
    };
    
    $scope.saveFileChanges = function () {
      var updateInfo = {
        name: $scope.fileEditing.name,
        description: $scope.fileEditing.description,
        sourceCode: $scope.fileEditing.sourceCode
      };
      
      apiService.packages.id.files.id.update($scope.packageInfo.packageID, $scope.fileInfo.id, updateInfo).
        success(function () {
          $scope.packageInfo.packageNeedsBuild = true;
          $scope.fileInfo.name = $scope.fileEditing.name;
          $scope.fileInfo.description = $scope.fileEditing.description;
          $scope.fileInfo.sourceCode = $scope.fileEditing.sourceCode;
          
          $scope.packageInfo.fileSummaries.forEach(function (summary) {
            if (summary.fileIDSummary === $scope.fileInfo.id) {
              summary.fileNameSummary = $scope.fileEditing.name;
              summary.fileDescriptionSummary = $scope.fileEditing.description;
            }
          });
        }).
        error(function (response) {
          $scope.fileEditing.hasError = true;
          $scope.fileEditing.error = response.statusText;
        });
    };
    
    $scope.deleteFile = function () {
      if (confirm("Delete this file?")) {
        
        apiService.packages.id.files.id.destroy($scope.packageInfo.packageID,$scope.fileInfo.id).
          success(function () {
            $scope.fileEditing.hasError = false;
            $scope.fileInfo.fileSelected = false;
            $scope.packageInfo.fileSummaries = $scope.packageInfo.fileSummaries.filter(function (summary) {
              return summary.fileIDSummary !== $scope.fileInfo.id
            });
            $scope.packageInfo.packageNeedsBuild = true;
          }).
          error(function (response) {
            $scope.fileEditing.hasError = true;
            $scope.fileEditing.error = response.statusText;
          });
        
      }
    };
    
  }]);