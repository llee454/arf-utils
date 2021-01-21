/*
  This module defines auxiliary functions that appear to be missing
  in the Prolog standard libraries.
*/
:- module(aux, []).

/*
  Accepts one argument: -Command, a string that represents a BASH
  command; and returns -Output, the text written by the command to
  STDOUT.
*/
bash_command(Command, Output) :-
  process_create(path(bash), ['-c', Command], [stdout(pipe(Out))]),
  read_string(Out, "\n", "\n", _, Output),
  close(Out).

/*
  Accepts two arguments: `+Date`, a string that represents a date; and
  `+Timestamp`, a timestamp; and returns true iff `Timestamp` occured
  on `Date`.

  Example onDate('Jan 1 2021', 123456678.0).
*/
onDate(Date, Timestamp) :-
  string_concat(Date, ' 0:00', StartTimeStr),
  string_concat(Date, ' 11:59:59 pm', StopTimeStr),
  convertESTToUTC(StartTimeStr, StartTime),
  convertESTToUTC(StopTimeStr, StopTime),
  StartTime =< Timestamp,
  Timestamp =< StopTime.

/*
  Accepts one argument: +Timestamp, a timestamp; and succeeds iff
  the given time overlaps with today.
*/
timestampToday(Timestamp) :-
  convertESTToUTC('0:00', StartTime),
  convertESTToUTC('11:59:59 pm', StopTime),
  StartTime =< Timestamp,
  Timestamp =< StopTime.

/*
  Accepts three arguments: -Hour, -Min, and -Sec; and returns a
  timestamp, +Timestamp, that represents the unix timestamp for the
  given time on the current day.

  Note: all times are in UTC time.

  Note: Timestamp is the number of seconds since Jan 1 1970.

  Example: getTimestamp (6, 0, 0, T).
*/
getTimestamp(Hour, Min, Sec, Timestamp) :-
  get_time(CurrTime),
  stamp_date_time(CurrTime, date(Year, Month, Day, _, _, _, 0, _, _), 0),
  date_time_stamp(date(Year, Month, Day, Hour, Min, Sec, 0, _, _), Timestamp).

/*
  Accepts one argument: -LocalTime, a string that represents a time in
  EST using the format HH:MM:SS; and returns a timestamp in UTC,
  +Timestamp.

  Note: Timestamp is the number of seconds since Jan 1 1970.

  Example: convertESTToUTC('Jan 5 2020 6:00 am', T).
*/
convertESTToUTC(LocalTime, Timestamp) :-
  string_concat("printf \"%.2f\" $(date +%s --date='TZ=\"EST\" ", LocalTime, CommandPrefix),
  string_concat(CommandPrefix, "')", Command),
  bash_command(Command, Result),
  number_string(Timestamp, Result).

/*
  Accepts three arguments: -Hour, -Min, and -Sec; and returns the
  number of hours that have elapsed from the current time to the given
  time today, +Hours. 
*/
hoursSince(Hour, Min, Sec, Hours) :-
  get_time(CurrTime),
  getTimestamp(Hour, Min, Sec, StartTime),
  Hours is (CurrTime - StartTime)/3600.
