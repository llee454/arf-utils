/*
  This module defines auxiliary functions that appear to be missing
  in the Prolog standard libraries.
*/
:- module(aux, []).

/*
  Accepts one argument: +Timestamp, a timestamp; and succeeds iff
  the given time overlaps with today.
*/
timestampToday(Timestamp) :-
  get_time(CurrTime),
  stamp_date_time(CurrTime, CurrDateTime, 0),
  stamp_date_time(Timestamp, DateTime, 0),
  date_time_value(date, CurrDateTime, CurrDate),
  date_time_value(date, DateTime, Date),
  CurrDate = Date.
