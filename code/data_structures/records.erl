-module(records).
-compile(export_all).
-include("records.hrl").

% definition
-record(robot, {name,
  type=industrial, % default value
  hobbies,
  details=[]}). % default value

% declaration
first_robot() ->
  #robot{name="Mechatron",
    type=handmade,
    details=["Moved by a small man inside"]}.

car_factory(CorpName) ->
  #robot{name=CorpName, hobbies="building cars"}.

% using in function heads and guards
-record(user, {id, name, group, age}).

%% use pattern matching to filter
admin_panel(#user{name=Name, group=admin}) ->
  Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
  Name ++ " is not allowed".

%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
  %% Show stuff that can't be written in such a text
  allowed;
adult_section(_) ->
  %% redirect to sesame street site
  forbidden.

% updating a record
repairman(Robot) ->
  Details = Robot#robot.details,
  NewRobot = Robot#robot{details=["Repaired by repairman"|Details]},
  {repaired, NewRobot}.

% using record from header file
included() ->
  #included{some_field="Some value"}.
