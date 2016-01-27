
var module = angular.module('Agon', ['ngRoute', 'ngCookies', 'ui.bootstrap', 'facebook', 'uiGmapgoogle-maps']);

module.factory('ApiClient', ApiClient);

module.directive('agLoading', agLoading);
module.directive('agEmptyList', agEmptyList);
module.directive('agDialog', agDialog);
module.directive('agEvent', agEvent);
module.directive('agWorkout', agWorkout);
module.directive('agUser', agUser);
module.directive('agTeam', agTeam);

module.controller('HomeController', HomeController);
module.controller('EventsController', EventsController);
module.controller('AthletesController', AthletesController);
module.controller('TeamsController', TeamsController);
module.controller('AboutController', AboutController);

module.controller('UpdateEventController', UpdateEventController);

module.config(configF);
module.run(initF);

function HomeController($scope, ApiClient) {
    $scope.op = {};
}

function AthletesController($scope, $location, ApiClient) {
    $scope.op = {};
    ApiClient.listUsers(false, function (rsp) { $scope.op.users = rsp; });
}

function TeamsController($scope, $location, ApiClient) {
    $scope.op = {};
    ApiClient.listTeams(false, function (rsp) { $scope.op.teams = rsp; });
}

function AboutController() { }
