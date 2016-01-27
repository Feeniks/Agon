
var UpdateEventController = function($rootScope, $scope, $location, $routeParams, ApiClient) {
    $scope.op = {
        map: {
            center: { latitude: -17.8254055, longitude: 31.0441506 },
            zoom: 10,
            events: {
                click: function (map, eventName, originalEventArgs) {
                    var e = originalEventArgs[0];
                    var lat = e.latLng.lat()
                    var lon = e.latLng.lng();
                    $scope.op.marker.coords.latitude = lat;
                    $scope.op.marker.coords.longitude = lon;
                    $scope.$apply();
                }
            }
        },
        marker: {
            id: 0,
            coords: { latitude: -17.8254055, longitude: 31.0441506 }
        },
        eventID: $routeParams.eventID,
        entryTypes: [{ type: 'individual', label: 'Individual Event' }, { type: 'team', label: 'Team Event' }],
        scoreTypes: [{ type: 'time', label: 'Score Type: Time' }, { type: 'reps', label: 'Score Type: Reps' }, { type: 'weight', label: 'Score Type: Weight' }]
    };

    var fetchWorkouts = function () {
        ApiClient.listWorkouts($scope.op.eventID, function (rsp) {
            for (i = 0; i < rsp.length; i++) {
                rsp[i].time = new Date(rsp[i].timeUTC * 1000);
            }
            $scope.op.workouts = rsp;
        });
    };

    $scope.setUpdateWorkout = function(w) { $scope.op.updateWorkout = w; }
    $scope.resetUpdateWorkout = function() {
        $scope.setUpdateWorkout({
            scoreType: 'time',
            time: new Date()
        });
    }
    $scope.resetUpdateWorkout();

    $scope.updateEvent = function () {
        var ladda = Ladda.create($('#btnUpdateEvent')[0]);
        ladda.start();

        $scope.op.event.startTimeUTC = $scope.op.event.startTime.getTime() / 1000;
        $scope.op.event.endTimeUTC = $scope.op.event.endTime.getTime() / 1000;
        $scope.op.event.venueLatitude = $scope.op.marker.coords.latitude;
        $scope.op.event.venueLongitude = $scope.op.marker.coords.longitude;

        var func = $scope.op.event._id ? ApiClient.updateEvent : ApiClient.createEvent;

        func(
            $scope.op.event,
            function (rsp) {
                $scope.op.event = rsp;
                $scope.op.event.startTime = new Date($scope.op.event.startTimeUTC * 1000);
                $scope.op.event.endTime = new Date($scope.op.event.endTimeUTC * 1000);
                $scope.op.eventID = rsp._id;
            },
            null,
            function () { ladda.stop(); }
        );
    };

    $scope.updateWorkout = function () {
        var ladda = Ladda.create($('#btnUpdateWorkout')[0]);
        ladda.start();

        $scope.op.updateWorkout.timeUTC = $scope.op.updateWorkout.time.getTime() / 1000;
        $scope.op.updateWorkout.eventID = $scope.op.eventID;

        var func = $scope.op.updateWorkout._id ? ApiClient.updateWorkout : ApiClient.createWorkout;

        func(
            $scope.op.updateWorkout,
            function (rsp) {
                fetchWorkouts();
            },
            null,
            function () { ladda.stop(); }
        );
    }

    $scope.startDeleteWorkout = function (workout) {
        $scope.resetUpdateWorkout();

        $rootScope.$broadcast(
            'agDialog.show',
            function (succ, fin) {
                ApiClient.deleteWorkout(workout, succ, null, fin);
            },
            fetchWorkouts,
            null,
            'Delete Workout: ' + workout.name,
            'Are you sure you want to delete this workout? Deleting a workout that has been scored will cause ranking errors.',
            'Delete'
        );
    };

    if ($scope.op.eventID) {
        ApiClient.listEvents(false, function (rsp) {
            for (i = 0; i < rsp.length; i++) {
                if (rsp[i]._id == $scope.op.eventID) {
                    $scope.op.event = rsp[i];
                    $scope.op.event.startTime = new Date($scope.op.event.startTimeUTC * 1000);
                    $scope.op.event.endTime = new Date($scope.op.event.endTimeUTC * 1000);
                }
            }
        });

        fetchWorkouts(false);
    } else {
        $scope.op.event = {
            entryType: { type: 'individual', minTeamSize: '3' },
            isInvitational: false,
            visible: false
        };

        $scope.op.workouts = [];
    }
};
