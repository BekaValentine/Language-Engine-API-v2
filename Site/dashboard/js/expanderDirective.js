'use strict';

angular.module('leApp').
  directive('leExpander', function () {
    
    //var dashboardURL = 'https://calm-eyrie-6520.herokuapp.com/site/dashboard';
    var dashboardURL = '/site/dashboard'
    
    return {
      restrict: 'E',
      templateUrl: dashboardURL + '/partials/expander.html',
      transclude: true,
      scope: {}
    };
  });