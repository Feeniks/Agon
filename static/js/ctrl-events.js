
function EventsController($rootScope, $scope, $location, ApiClient) {
    $scope.op = {};
    ApiClient.listEvents(false, function (rsp) { $scope.op.events = rsp; });

    if ($rootScope.loginInfo && $rootScope.loginInfo.user.isAdmin) {
        $scope.updateF = function (e) { $location.path('/update-event/' + e._id) };
        $scope.deleteF = function (e) {
            $rootScope.$broadcast(
                'agDialog.show',
                function (succ, fin) {
                    ApiClient.deleteEvent(e, succ, null, fin);
                },
                fetchWorkouts,
                null,
                'Delete Event: ' + e.name,
                'Are you sure you want to delete this event? Deleted events cannot be restored.',
                'Delete'
            );
        };
    }
}
