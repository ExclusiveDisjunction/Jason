#include "DateTime.h"

#include "Errors.h"

#include <sstream>
#include <chrono>
#include <fstream>

using namespace std;
using namespace std::chrono;

DateTime::DateTime() : _Dur(0) { }
DateTime::DateTime(int Month, int Day, int Year)
{
	if (Month < 1 || Month > 12 || Day < 1 || Day > 31)
		throw std::logic_error("Out of bounds.");

	Month--;
	Day--;

	years Y(Year);
	months M(Month);
	days D(Day);

	_Dur = Y + M + D;
}
DateTime::DateTime(int Hour, int Minute, int Second, int Millisecond)
{
	if (Hour > 23 || Hour < 0 || Minute > 59 || Minute < 0 || Second > 59 || Second < 0)
		throw std::logic_error("Out of bounds.");

	hours H(Hour);
	minutes Mi = minutes(Minute);
	seconds S(Second);
	milliseconds Ms(Millisecond);

	_Dur = H + Mi + S + Ms;
}
DateTime::DateTime(int Month, int Day, int Year, int Hour, int Minute, int Second, int Millisecond)
{
	try 
	{
		DateTime DayPart(Month, Day, Year);
		DateTime TimePart(Hour, Minute, Second, Millisecond);

		this->_Dur = DayPart._Dur + TimePart._Dur;
	}
	catch (std::logic_error& e)
	{
		throw e;
	}
}
DateTime::DateTime(duration<double> Dur) : _Dur(std::move(Dur)) { }

DateTime DateTime::Today()
{
	return Now().DayParts();
}
DateTime DateTime::Now()
{
	DateTime Return;

	auto Time = time(nullptr);
	auto Conv = mktime(localtime(&Time));

	Return._Dur = std::chrono::seconds(Conv);

	return Return;
}

long DateTime::Year() const
{
	duration<double> This = this->_Dur;
	return Year(This) + 1970;
}
long DateTime::Year(duration<double>& Remain) const
{
	years Return = duration_cast<years>(Remain);
	Remain -= Return;

	return Return.count();
}
long DateTime::Month() const
{
	duration<double> This = this->_Dur;
	Year(This);
	return Month(This) + 1;
}
long DateTime::Month(std::chrono::duration<double>& Remain) const
{
	months Return = duration_cast<months>(Remain);
	Remain -= Return;

	return Return.count();
}
long DateTime::Day() const
{
	duration<double> This = this->_Dur;
	Month(This);
	return Day(This) + 1;
}
long DateTime::Day(std::chrono::duration<double>& Remain) const
{
	days Return = duration_cast<days>(Remain);
	Remain -= Return;

	return Return.count();
}
long DateTime::Hour() const
{
	duration<double> This = this->_Dur;
	Day(This);
	return Hour(This);
}
long DateTime::Hour(std::chrono::duration<double>& Remain) const
{
	hours Return = duration_cast<hours>(Remain);
	Remain -= Return;

	return Return.count();
}
long DateTime::Minute() const
{
	duration<double> This = this->_Dur;
	Hour(This);
	return Minute(This);
}
long DateTime::Minute(std::chrono::duration<double>& Remain) const
{
	minutes Return = duration_cast<minutes>(Remain);
	Remain -= Return;

	return Return.count();
}
long DateTime::Second() const
{
	duration<double> This = this->_Dur;
	Minute(This);
	return Second(This);
}
long DateTime::Second(std::chrono::duration<double>& Remain) const
{
	seconds Return = duration_cast<seconds>(Remain);
	Remain -= Return;

	return static_cast<int>(Return.count());
}
long DateTime::Millisecond() const
{
	duration<double> This = this->_Dur;
	Second(This);
	return Millisecond(This);
}
long DateTime::Millisecond(duration<double>& Remain) const
{
	milliseconds Return = duration_cast<milliseconds>(Remain);
	Remain -= Return;

	return static_cast<int>(Return.count());
}

DateTime DateTime::DayParts() const
{
	DateTime New;

	duration<double>& Dur = New._Dur;
	Dur = _Dur;
	years Y = duration_cast<years>(Dur);
	Dur -= Y;
	months M = duration_cast<months>(Dur);
	Dur -= M;
	days D = duration_cast<days>(Dur);
	Dur -= D;

	Dur = Y + M + D; //Once Y:M:D is extracted, Dur is set to the sum of these three parts, excluding H:M:S:MI
	return New;
}
DateTime DateTime::TimeParts() const
{
	DateTime New;

	duration<double>& Dur = New._Dur;
	Dur = _Dur;
	years Y = duration_cast<years>(Dur);
	Dur -= Y;
	months M = duration_cast<months>(Dur);
	Dur -= M;
	days D = duration_cast<days>(Dur);
	Dur -= D;

	return New; //All that is left is simply H:M:S:MS. No further action must take place.
}

std::string DateTime::ToString(DateStringFormat date) const
{
	auto Year = this->Year(), Month = this->Month(), Day = this->Day();

	std::stringstream Return;
	switch (date)
	{
		case DateStringFormat::None:
			return std::string();
		case DateStringFormat::ShortDate: //MM-DD-YYY
		{
			if (Day != 0 && Year != 0 && Month != 0)
				Return << Month << "/" << Day << "/" << Year;
			else
				return "No Data";
			break;
		}
		case DateStringFormat::EuroShortDate: //DD-MM-YYY
		{
			if (Day != 0 && Year != 0 && Month != 0)
				Return << Day << "/" << Month << "/" << Year;
			else
				return "No Data";
			break;
		}
		case DateStringFormat::LongDate: //Month DD(th, rd, st), YYYY
		{
			if (Month != 0 && Year != 0 && Day != 0)
			{
				switch (Month)
				{
				case 1:
					Return <<  "January";;
					break;
				case 2:
					Return << "February";
					break;
				case 3:
					Return << "March";
					break;
				case 4:
					Return << "April";
					break;
				case 5:
					Return << "May";
					break;
				case 6:
					Return << "June";
					break;
				case 7:
					Return << "July";
					break;
				case 8:
					Return << "August";
					break;
				case 9:
					Return << "September";
					break;
				case 10:
					Return << "October";
					break;
				case 11:
					Return << "November";
					break;
				case 12:
					Return << "December";
					break;
				}

				Return << ' ' << Day;
				switch (Day)
				{
				case 1:
				case 21:
				case 31:
					Return << "st";
					break;

				case 2:
				case 22:
					Return << "nd";
					break;

				case 3:
				case 23:
					Return << "rd";
					break;

				default:
					Return << "th";
					break;
				}

				Return << ", " << Year;
			}
			else
				return "No Data";

			break;
		}
		case DateStringFormat::Duration: //## Years, ## Months, ## Days
		{
			if (Year == 0 && Month == 0 && Day == 0)
				return "No Data";

			std::string Y = Year == 0 ? "" : to_string(Year) + (Year == 1 ? "Year" : "Years"),
						M = Month == 0 ? "" : to_string(Month) + (Month == 1 ? "Month" : "Months"),
						D = Day == 0 ? "" : to_string(Day) + (Day == 1 ? "Day" : "Days");

			Return << Y << (Y.length() == 0 ? "" : ", ") <<
				M << (!M.length() ? "" : ", ") <<
				D << (!D.length() ? "" : ", ");
			break;
		}
	}

	return Return.str();
}
std::string DateTime::ToString(TimeStringFormat time) const
{
	auto Hour = this->Hour(), Minute = this->Minute(), Second = this->Second(), Millisecond = this->Millisecond();
	std::stringstream Return;

	switch (time)
	{
		case TimeStringFormat::None:
			return std::string();
		case TimeStringFormat::ShortTime12H:
		{
			if (Hour || Minute )
				Return << (Hour > 12 ? Hour - 12 : Hour) << ':' << Minute << ' ' << (Hour < 12 ? "AM" : "PM");
			else
				return "No Time";
			break;
		}
		case TimeStringFormat::ShortTime24H: //HH:MM 
		{
			if (Hour || Minute)
				Return << Hour << ':' << Minute;
			else
				return "No Time";
			break;
		}
		case TimeStringFormat::ExtendedTime: //HH:MM:SS:MS
		{
			if (Hour || Minute || Second || Millisecond)
				Return << Hour << ':' << Minute << ':' << Second << ':' << Millisecond;
			else
				return "No Time";
			break;
		}
		case TimeStringFormat::Duration: //## Hours, ## Minutes
		{
			if (Hour || Minute)
			{	
				std::string H = Hour == 0 ? "" : to_string(Hour) + (Hour == 1 ? "Hour" : "Hours"),
						Mi = Minute == 0 ? "" : to_string(Minute) + (Minute == 1 ? "Minute" : "Minutes");

				Return << H << (!H.length() ? "" : ", ") <<
							Mi << (!Mi.length() ? "" : ", ");
			}
			else
				return "No Data";
			break;	
		}
		case TimeStringFormat::ExtendedDuration: //## Hours, ## Minutes, ## Seconds, ## Miliseconds
		{
			if (Hour || Minute || Second || Millisecond)
			{	
				std::string H = Hour == 0 ? "" : to_string(Hour) + (Hour == 1 ? "Hour" : "Hours"),
						Mi = Minute == 0 ? "" : to_string(Minute) + (Minute == 1 ? "Minute" : "Minutes"),
						S = Second == 0 ? "" : to_string(Second) + (Second == 1 ? "Second" : "Seconds"),
						Ml = Millisecond == 0 ? "" : to_string(Millisecond) + (Millisecond == 1 ? "Millisecond" : "Milliseconds");

				Return << H << (!H.length() ? "" : ", ") <<
							Mi << (!Mi.length() ? "" : ", ") <<
							S << (!S.length() ? "" : ", ") <<
							Ml << (!Ml.length() ? "" : ", ");
			}
			else
				return "No Data";

			break;	
		}
	}

	return Return.str();
}
std::string DateTime::ToString(DateStringFormat date, TimeStringFormat time, bool JoinStrings) const
{
	std::string datePart = ToString(date), timePart = ToString(time);

	//Four possibilities:
	/*
		1. None, None - return Nothing
		2. None, * - return Time
		3. *, None - return Date
		4. *, * - return Date + "join" + Time
	*/

	if (date == DateStringFormat::None && time == TimeStringFormat::None)
		return std::string(); //Empty string
	else if (date == DateStringFormat::None && time != TimeStringFormat::None)
		return timePart;
	else if (date != DateStringFormat::None && time == TimeStringFormat::None)
		return datePart;
	else //Neither is empty.
	{
		if (!JoinStrings)
			return datePart + ' ' + timePart;

		//We have to join with the word 'at', 'and', or ','.

		if (date == DateStringFormat::Duration && time == TimeStringFormat::Duration) //Use comma
			return datePart + ", " + timePart;
		else if (date == DateStringFormat::Duration || time == TimeStringFormat::Duration) //Use and
			return datePart + " and " + timePart;
		else //No durations are present, so use at
			return datePart + " at " + timePart;
	}
}

void DateTime::str_serialize(std::ostream &out) const noexcept
{
    out << _Dur.count();
}
void DateTime::str_deserialize(std::istream &in)
{
    std::chrono::duration<double>::rep data;
    in >> data;
    this->_Dur = std::chrono::duration<double>(data);
}

void DateTime::dbg_fmt(std::ostream& out) const noexcept
{
    out << this->ToString(DateStringFormat::ShortDate, TimeStringFormat::ExtendedTime, false);
}
void DateTime::dsp_fmt(std::ostream& out) const noexcept
{
    out << this->ToString(DateStringFormat::ShortDate, TimeStringFormat::ShortTime12H, true);
}
void DateTime::ui_dsp_fmt(std::ostream& out) const noexcept
{
    out << this->ToString(DateStringFormat::LongDate, TimeStringFormat::ShortTime12H, true);
}

std::partial_ordering DateTime::operator<=>(const DateTime& two) const noexcept
{
    return this->_Dur <=> two._Dur;
}
bool DateTime::operator==(const DateTime& Two) const noexcept
{
	return _Dur == Two._Dur;
}
bool DateTime::operator!=(const DateTime& Two) const noexcept
{
	return _Dur != Two._Dur;
}
bool DateTime::operator<(const DateTime& Two) const noexcept
{
	return _Dur < Two._Dur;
}
bool DateTime::operator<=(const DateTime& Two) const noexcept
{
    return _Dur <= Two._Dur;
}
bool DateTime::operator>(const DateTime& Two) const noexcept
{
	return _Dur > Two._Dur;
}
bool DateTime::operator>=(const DateTime& Two) const noexcept
{
    return _Dur >= Two._Dur;
}

DateTime DateTime::operator+(const DateTime& Two) const
{
	return DateTime(_Dur + Two._Dur);
}
DateTime DateTime::operator-(const DateTime& Two) const
{
	return DateTime(_Dur - Two._Dur);
}
DateTime& DateTime::operator+=(const DateTime& Two)
{
	_Dur += Two._Dur;
	return *this;
}
DateTime& DateTime::operator-=(const DateTime& Two)
{
	_Dur -= Two._Dur;
	return *this;
}
