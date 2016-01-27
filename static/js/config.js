
var configF = function ($routeProvider, $httpProvider, $sceProvider, FacebookProvider, uiGmapGoogleMapApiProvider) {
    $routeProvider.when('/', { templateUrl: 'tmpl/home.html', controller: 'HomeController' });
    $routeProvider.when('/events', { templateUrl: 'tmpl/events.html', controller: 'EventsController' });
    $routeProvider.when('/athletes', { templateUrl: 'tmpl/athletes.html', controller: 'AthletesController' });
    $routeProvider.when('/teams', { templateUrl: 'tmpl/teams.html', controller: 'TeamsController' });
    $routeProvider.when('/about', { templateUrl: 'tmpl/about.html', controller: 'AboutController' });

    $routeProvider.when('/update-event/:eventID?', { templateUrl: 'tmpl/update-event.html', controller: 'UpdateEventController' });

    $httpProvider.interceptors.push(function($q, $rootScope, $location) {
        return {
            responseError: function (rejection) {
                $location.path('/error/' + rejection.status);
                return $q.reject(rejection);
            }
        };
    });

    $sceProvider.enabled(false);

    FacebookProvider.init('552436188236935');

    uiGmapGoogleMapApiProvider.configure({
        key: 'AIzaSyA0kfNJBkAnwyLCCSlowHqlFPtOuHpFOOc',
        v: '3.20',
        libraries: 'weather,geometry,visualization'
    });
}
