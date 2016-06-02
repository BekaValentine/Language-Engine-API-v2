'use strict';

angular.module('leApp', ['ngRoute','ngResource','ngCookies','ngSanitize','ui.codemirror']).
  
  config(['$routeProvider', function ($routeProvider) {
    
    //var dashboardURL = 'https://calm-eyrie-6520.herokuapp.com/site/dashboard';
    var dashboardURL = '/site/dashboard';
    
    $routeProvider.
      when('/', { templateUrl: dashboardURL + '/partials/login-signup.html', noLogin: true }).
      
      when('/apps', {
        templateUrl: dashboardURL + '/partials/apps.html',
        controller: 'AppsController'
      }).
      when('/apps/:id', {
        templateUrl: dashboardURL + '/partials/apps-id.html',
        controller: 'AppsIDController'
      }).
      
      when('/packages', {
        templateUrl: dashboardURL + '/partials/packages.html',
        controller: 'PackagesController'
      }).
      when('/packages/:id', {
        templateUrl: dashboardURL + '/partials/packages-id.html',
        controller: 'PackagesIDController'
      }).
      
      /*
      when('/public-modules', {
        templateUrl: dashboardURL + '/partials/public-modules.html',
        controller: 'PublicModulesController'
      }).
      when('/public-modules/:id', {
        templateUrl: dashboardURL + '/partials/public-modules-id.html',
        controller: 'PublicModulesIDController'
      }).
      when('/public-concepts/:id', {
        templateUrl: dashboardURL + '/partials/public-concepts-id.html',
        controller: 'PublicConceptsIDController'
      }).
      when('/public-lexical-items/:id', {
        templateUrl: dashboardURL + '/partials/public-lexical-items-id.html',
        controller: 'PublicLexicalItemsIDController'
      }).
      
      when('/private-modules', {
        templateUrl: dashboardURL + '/partials/private-modules.html',
        controller: 'PrivateModulesController'
      }).
      when('/private-modules/:id', {
        templateUrl: dashboardURL + '/partials/private-modules-id.html',
        controller: 'PrivateModulesIDController'
      }).
      when('/private-concepts/:id', {
        templateUrl: dashboardURL + '/partials/private-concepts-id.html',
        controller: 'PrivateConceptsIDController'
      }).
      when('/private-lexical-items/:id', {
        templateUrl: dashboardURL + '/partials/private-lexical-items-id.html',
        controller: 'PrivateLexicalItemsIDController'
      }).
      
      when('/lexical-templates', {
        templateUrl: dashboardURL + '/partials/lexical-templates.html',
        controller: 'LexicalTemplatesController'
      }).
      when('/lexical-templates/:id', {
        templateUrl: dashboardURL + '/partials/lexical-templates-id.html',
        controller: 'LexicalTemplatesIDController'
      }).
      */
      
      when('/settings', {
        templateUrl: dashboardURL + '/partials/settings.html',
        controller: 'SettingsController'
      }).
      when('/password-reset', {
        templateUrl: dashboardURL + '/partials/password-reset.html',
        controller: 'PasswordResetController',
        noLogin: true
      }).
      when('/username-recovery', {
        templateUrl: dashboardURL + '/partials/username-recovery.html',
        controller: 'UsernameRecoveryController',
        noLogin: true
      }).
      otherwise('/');
  }]).
  run(['$rootScope', '$location', 'apiService', function ($rootScope, $location, apiService) {
    
    $rootScope.$on('$routeChangeStart', function (event,next) {
      if (!next.noLogin && !apiService.login.isLoggedIn()) {
        $location.path('/');
      } else if (next.noLogin && apiService.login.isLoggedIn()) {
        $location.path('/apps');
      }
    });
  }]);