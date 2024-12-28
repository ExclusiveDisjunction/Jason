#ifndef DATETIME_H
#define DATETIME_H

#include <chrono>
#include <string>

#include "Printing.h"
#include "Serialize.h"

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

class DateTime : public DebugPrint, public DisplayPrint, public UIDisplayPrint, public StringSerializable
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
	DateTime(std::chrono::duration<double> Tm);

	//bool HasValue = true, HasDay = true, HasTime = true;

    long Year() const;
    long Year(std::chrono::duration<double>& Remain) const;
    long Month() const;
    long Month(std::chrono::duration<double>& Remain) const;
    long Day() const;
    long Day(std::chrono::duration<double>& Remain) const;
    long Hour() const;
    long Hour(std::chrono::duration<double>& Remain) const;
    long Minute() const;
    long Minute(std::chrono::duration<double>& Remain) const;
    long Second() const;
    long Second(std::chrono::duration<double>& Remain) const;
    long Millisecond() const;
    long Millisecond(std::chrono::duration<double>& Remain) const;

	DateTime DayParts() const;
	DateTime TimeParts() const;

	std::string ToString(DateStringFormat date) const;
	std::string ToString(TimeStringFormat time) const;
	std::string ToString(DateStringFormat date, TimeStringFormat time, bool JoinStrings = true) const;
    
    void dbg_fmt(std::ostream& out) const noexcept override;
    void dsp_fmt(std::ostream& out) const noexcept override;
    void ui_dsp_fmt(std::ostream& out) const noexcept override;
    
    void str_serialize(std::ostream& out) const noexcept override;
    void str_deserialize(std::istream& in) override;

    std::partial_ordering operator<=>(const DateTime& obj) const noexcept;
    bool operator==(const DateTime& Obj) const noexcept;
    bool operator!=(const DateTime& Obj) const noexcept;
    bool operator<(const DateTime& obj) const noexcept;
    bool operator<=(const DateTime& obj) const noexcept;
    bool operator>(const DateTime& obj) const noexcept;
    bool operator>=(const DateTime& obj) const noexcept;

	DateTime operator-(const DateTime& Obj) const;
	DateTime operator+(const DateTime& Obj) const;
	DateTime& operator+=(const DateTime& Obj);
	DateTime& operator-=(const DateTime& Obj);
};

#endif
