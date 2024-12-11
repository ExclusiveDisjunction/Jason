#ifndef DATETIME_H
#define DATETIME_H

#include <chrono>
#include <string>

enum class DateStringFormat
{
	None, //This means there is nothing here.
	ShortDate, //MM-DD-YYY
	EuroShortDate, //DD-MM-YYY
	LongDate, //Month DD(th, rd, st), YYYY
	Duration //## Years, ## Months, ## Days
};
enum class TimeStringFormat
{
	None,
	ShortTime12H, //HH:MM (AM/PM)
	ShortTime24H, //HH:MM 
	ExtendedTime, //HH:MM:SS:MS
	Duration, //## Hours, ## Minutes
	ExtendedDuration, //## Hours, ## Minutes, ## Seconds, ## Miliseconds
};

class DateTime
{
private:
	std::chrono::duration<double> _Dur;

public:
	static DateTime Today();
	static DateTime Now();

	DateTime();
	DateTime(int Month, int Day, int Year);
	DateTime(int Hour, int Minute, int Second, int Millisecond);
	DateTime(int Month, int Day, int Year, int Hour, int Minute, int Second, int Millisecond);
	DateTime(const std::string& BackString);
	DateTime(const std::chrono::duration<double>& Tm);

	//bool HasValue = true, HasDay = true, HasTime = true;

	int Year() const;
	int Year(std::chrono::duration<double>& Remain) const;
	int Month() const;
	int Month(std::chrono::duration<double>& Remain) const;
	int Day() const;
	int Day(std::chrono::duration<double>& Remain) const;
	int Hour() const;
	int Hour(std::chrono::duration<double>& Remain) const;
	int Minute() const;
	int Minute(std::chrono::duration<double>& Remain) const;
	int Second() const;
	int Second(std::chrono::duration<double>& Remain) const;
	int Millisecond() const;
	int Millisecond(std::chrono::duration<double>& Remain) const;

	DateTime DayParts() const;
	DateTime TimeParts() const;

	std::string ToString(DateStringFormat date) const;
	std::string ToString(TimeStringFormat time) const;
	std::string ToString(DateStringFormat date, TimeStringFormat time, bool JoinStrings = true) const;
	std::string Sterilize() const;
	static DateTime FromSterilize(const std::string& in);

	friend std::ostream& operator<<(std::ostream& out, const DateTime& Obj); //Uses ToString(ShortDate, ShortTime12HR)

	bool operator==(const DateTime& Obj) const;
	bool operator!=(const DateTime& Obj) const;
	bool operator<(const DateTime& Obj) const;
	bool operator>(const DateTime& Obj) const;

	DateTime operator-(const DateTime& Obj) const;
	DateTime operator+(const DateTime& Obj) const;
	DateTime& operator+=(const DateTime& Obj);
	DateTime& operator-=(const DateTime& Obj);
};

#endif
