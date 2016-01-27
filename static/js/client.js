
var ApiClient = function ($rootScope, $http, $cacheFactory) {
    var cache = $cacheFactory('agon');
    var usersCacheKey = "users";
    var teamsCacheKey = "teams";
    var venuesCacheKey = "venues";
    var eventsCacheKey = "events";

    function getToken() {
        var info = $rootScope.loginInfo;
        return (info ? info.token : null);
    }

    function req(method, path, data, success, failure, finallycb) {
        $http({
            method: method,
            url: path,
            data: data,
            headers: { 'A-Auth': getToken() }
        }).then(
            function (response) { success(response); },
            function (err) { if (failure) failure(err); }
        ).then(
            function () { if (finallycb) { finallycb(); } }
        );
    }

    function get(invalidateCache, cacheKey, path, success, failure, finallycb) {
        if (invalidateCache && cache.get(cacheKey)) { cache.remove(cacheKey); }

        var cached = cache.get(cacheKey);

        if (cached) {
            success(cached);
            if (finallycb) finallycb();
        } else {
            req(
                "GET",
                path,
                null,
                function (resp) {
                    cache.put(cacheKey, resp.data);
                    success(resp.data);
                },
                failure,
                finallycb
            );
        }
    }

    function upd(method, cacheKey, path, data, success, failure, finallycb) {
        req(
            method,
            path,
            data,
            function (resp) {
                var resd = resp.data;
                if (cache.get(cacheKey)) {
                    var items = cache.get(cacheKey);
                    var itemsp = [];
                    for (i = 0; i < items.length; i++) {
                        var item = items[i];
                        var updated = item._id === resd._id;
                        itemsp.push(updated ? resd : item);
                    }
                    cache.put(cacheKey, itemsp);
                }

                success(resd);
            },
            failure,
            finallycb
        );
    }

    function del(cacheKey, item, path, data, success, failure, finallycb) {
        req(
            "DELETE",
            path + '/' + item._id + '/' + item._rev,
            data,
            function (resp) {
                var resd = resp.data;
                if (cache.get(cacheKey)) {
                    var items = cache.get(cacheKey);
                    var itemsp = [];
                    for (i = 0; i < items.length; i++) {
                        var item = items[i];
                        if (item._id !== item._id) {
                            itemsp.push(item);
                        }
                    }
                    cache.put(cacheKey, itemsp);
                }

                success(resd);
            },
            failure,
            finallycb
        );
    }

    function authenticate(fbToken, success, failure, finallycb) { get(false, "", "api/auth/" + fbToken, success, failure, finallycb); }

    function listUsers(invalidateCache, success, failure, finallycb) { get(invalidateCache, usersCacheKey, "api/users", success, failure, finallycb); }
    function listEvents(invalidateCache, success, failure, finallycb) { get(invalidateCache, eventsCacheKey, "api/events", success, failure, finallycb); }
    function listWorkouts(eventID, success, failure, finallycb) { get(true, "", 'api/workouts/childrenof/"' + eventID + '"', success, failure, finallycb); }
    function listEntrants(eventID, success, failure, finallycb) { get(true, "", 'api/entrants/childrenof/"' + eventID + '"', success, failure, finallycb); }

    function listTeams(invalidateCache, success, failure, finallycb) {
        var __success = function (users, teams) {
            var umap = [];
            for (i = 0; i < users.length; i++) {
                umap[users[i]._id] = users[i];
            }

            for (i = 0; i < teams.length; i++) {
                teams[i]._userMap = umap;
            }

            success(teams);
        };

        var _success = function (users) {
            get(invalidateCache, teamsCacheKey, "api/teams", function (teams) { __success(users, teams); }, failure, finallycb);
        };

        listUsers(invalidateCache, _success, failure, finallycb);
    }

    function createTeam(name, success, failure, finallycb) { upd("PUT", teamsCacheKey, "api/teams/" + name, null, success, failure, finallycb); };
    function createEvent(event, success, failure, finallycb) { event._id = ''; upd("PUT", eventsCacheKey, "api/events", event, success, failure, finallycb); }
    function createWorkout(workout, success, failure, finallycb) { workout._id = ''; upd("PUT", "", "api/workouts", workout, success, failure, finallycb); }
    function createEntrant(entrant, success, failure, finallycb) { entrant._id = ''; upd("PUT", "", "api/entrants", entrant, success, failure, finallycb); }

    function updateEvent(event, success, failure, finallycb) { upd("POST", eventsCacheKey, "api/events", event, success, failure, finallycb); }
    function updateWorkout(workout, success, failure, finallycb) { upd("POST", "", "api/workouts", workout, success, failure, finallycb); }
    function updateEntrant(entrant, success, failure, finallycb) { upd("POST", "", "api/entrants", entrant, success, failure, finallycb); }

    function deleteEvent(event, success, failure, finallycb) { del(eventsCacheKey, event, "api/events", null, success, failure, finallycb); };
    function deleteWorkout(workout, success, failure, finallycb) { del("", workout, "api/workouts", null, success, failure, finallycb); };

    function inviteToTeam(teamID, userID, success, failure, finallycb) { upd("POST", teamsCacheKey, "api/teams/" + teamID + "/" + userID, null, success, failure, finallycb); }
    function acceptTeamInvite(teamID, success, failure, finallycb) { upd("POST", teamsCacheKey, "api/teams/" + teamID, null, success, failure, finallycb); }
    function removeFromTeam(teamID, userID, success, failure, finallycb) { upd("DELETE", teamsCacheKey, "api/teams/" + teamID, null, success, failure, finallycb); }

    return {
        authenticate: authenticate,
        listUsers: listUsers,
        listTeams: listTeams,
        listEvents: listEvents,
        listWorkouts: listWorkouts,
        listEntrants: listEntrants,
        createTeam: createTeam,
        createEvent: createEvent, //admin only
        createWorkout: createWorkout, //admin only
        createEntrant: createEntrant,
        updateEvent: updateEvent, //admin only
        updateWorkout: updateWorkout, //admin only
        updateEntrant: updateEntrant, //admin only
        deleteEvent: deleteEvent, //admin only
        deleteWorkout: deleteWorkout, //admin only
        inviteToTeam: inviteToTeam,
        acceptTeamInvite: acceptTeamInvite,
        removeFromTeam: removeFromTeam
    };
};
