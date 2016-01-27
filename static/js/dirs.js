
var agLoading = function () {
    return {
        restrict: 'E',
        scope: {},
        controller: [function () { }],
        templateUrl: '/tmpl/ag-loading.html',
        link: function (scope, el, attrs) { }
    };
};

var agEmptyList = function () {
    return {
        restrict: 'E',
        scope: {},
        controller: [function () { }],
        templateUrl: '/tmpl/ag-empty-list.html',
        link: function (scope, el, attrs) { }
    };
};

var agDialog = function () {
    return {
        restrict: 'E',
        scope: {},
        controller: ['$rootScope', '$scope', function ($rootScope, $scope) {
            var el = $('#mod-dialog');

            $rootScope.$on(
                'agDialog.show',
                function (evt, action, onSuccess, onFinally, title, text, btnText) {
                    $scope.action = action;
                    $scope.onSuccess = onSuccess;
                    $scope.onFinally = onFinally;
                    $scope.title = title;
                    $scope.text = text;
                    $scope.btnText = btnText;
                    el.modal('show');
                }
            );

            $scope.cancel = function() {
                el.modal('hide');
            };

            $scope.act = function() {
                var ladda = Ladda.create($('#btnDialogSubmit')[0]);
                ladda.start();

                $scope.action(
                    $scope.onSuccess,
                    function () {
                        $scope.cancel();
                        ladda.stop();
                        if ($scope.onFinally) $scope.onFinally();
                    }
                );
            };
        }],
        templateUrl: '/tmpl/ag-dialog.html',
        link: function (scope, el, attrs) { }
    };
};

var agEvent = function () {
    return {
        restrict: 'E',
        scope: {
            event: "=",
            updateF: "=",
            deleteF: "="
        },
        controller: ['$scope', function ($scope) {
            $scope.$watch(
                'event',
                function () {
                    if ($scope.event) {
                        var tomorrow = new Date();
                        tomorrow.setHours(tomorrow.getHours() + 24);

                        $scope.eventStartTime = new Date($scope.event.startTimeUTC * 1000);
                        $scope.eventEndTime = new Date($scope.event.endTimeUTC * 1000);
                        $scope.isFuture = $scope.eventStartTime.getTime() > tomorrow.getTime();
                    }
                }
            );
        }],
        templateUrl: '/tmpl/ag-event.html',
        link: function (scope, el, attrs) { }
    };
};

var agWorkout = function () {
    return {
        restrict: 'E',
        scope: {
            workout: "=",
            updateF: "=",
            deleteF: "="
        },
        controller: ['$scope', function ($scope) {
            $scope.$watch(
                'workout',
                function () {
                    if ($scope.workout) {
                        $scope.workoutTime = new Date($scope.workout.timeUTC * 1000);
                    }
                }
            );
        }],
        templateUrl: '/tmpl/ag-workout.html',
        link: function (scope, el, attrs) { }
    };
};


var agUser = function () {
    return {
        restrict: 'E',
        scope: {
            user: "="
        },
        controller: [function () { }],
        templateUrl: '/tmpl/ag-user.html',
        link: function (scope, el, attrs) { }
    };
};

var agTeam = function () {
    return {
        restrict: 'E',
        scope: {
            team: "="
        },
        controller: [function () { }],
        templateUrl: '/tmpl/ag-team.html',
        link: function (scope, el, attrs) { }
    };
};
