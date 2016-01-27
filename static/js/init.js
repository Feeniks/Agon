
var initF = ['$rootScope', '$cookies', '$location', '$window', 'Facebook', 'ApiClient', function ($rootScope, $cookies, $location, $window, Facebook, ApiClient) {

    var cachedLoginInfo = $cookies.getObject('loginInfo');
    if (cachedLoginInfo) {
        $rootScope.loginInfo = cachedLoginInfo;
    }

    var loginFailed = function() { $location.path('/loginfailed'); };

    var doLogin = function (token, then) {
        ApiClient.authenticate(
            token,
            function (info) {
                $rootScope.loginInfo = info;
                var dt = new Date();
                dt.setHours(dt.getHours() + 12);
                $cookies.putObject('loginInfo', info, { expires: dt });
                $window.location = '/';
            },
            function (err) { loginFailed(); },
            function () { then(); }
        );
    };

    var doFBLogin = function (then) {
        Facebook.login(function(rsp) {
            if (rsp.status === 'connected') {
                doLogin(rsp.authResponse.accessToken, then);
            } else {
                then();
                loginFailed();
            }
        });
    };

    $rootScope.login = function (laddaID) {
        var ladda = null;
        if (laddaID) {
            ladda = Ladda.create( $(laddaID)[0] );
            ladda.start();
        }

        var stopLadda = function () { if (ladda) ladda.stop(); }

        Facebook.getLoginStatus(function (rsp) {
            if (rsp.status === 'connected') {
                doLogin(rsp.authResponse.accessToken, stopLadda);
            } else {
                doFBLogin(stopLadda);
            }
        });
    };

    $rootScope.logout = function () {
        $rootScope.loginInfo = null;
        $cookies.remove('loginInfo');
        $location.path('/');
    }

    $rootScope.asDate = function (utcSecs) {
        return new Date(utcSecs * 1000);
    };

    $rootScope.$on(
        '$routeChangeSuccess',
        function (evt, next, current) {
            if (next && next.$$route) {
                var op = next.$$route.originalPath;

                if (op.indexOf('/update-event') === 0) {
                    $rootScope.activeMenuItem = 'events';
                } else if (op.indexOf('/events') === 0) {
                    $rootScope.activeMenuItem = 'events';
                } else if (op.indexOf('/athletes') === 0) {
                    $rootScope.activeMenuItem = 'athletes';
                } else if (op.indexOf('/teams') === 0) {
                    $rootScope.activeMenuItem = 'teams';
                } else if (op.indexOf('/about') === 0) {
                    $rootScope.activeMenuItem = 'about';
                } else {
                    $rootScope.activeMenuItem = 'home';
                }
            }
        }
    );
}];
