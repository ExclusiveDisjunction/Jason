//
// Created by Hollan on 5/23/24.
//

#include "Log.h"

Logger logging = Logger();

Logger::Logger() : Out(), State(LoggerLevel::LL_None), Level(LoggerLevel::LL_None)
{
    
}
Logger::~Logger()
{
    if (IsInLog())
        EndLog();

    Out.close();
}

bool Logger::Open(const std::string& path, LoggerLevel level) 
{
    Out.open(path, std::ios::trunc);
    State = LoggerLevel::LL_None;
    Level = level;

    if (Out.bad())
    {
        Out.close();
        Level = LL_None;
        return false;
    }
    else 
        return true;
}
bool Logger::IsOpen() const noexcept 
{
    return Out.good();
}

bool Logger::StartLog(LoggerLevel level)
{
    if (level == LoggerLevel::LL_None)
        return EndLog();

    if (this->State != LoggerLevel::LL_None)
        return false;

    this->State = level;

    if (!CurrentLogIsIgnored())
    {
        //We need to write the begining of the level.

        Out << DateTime::Now().ToString(DateStringFormat::ShortDate, TimeStringFormat::ExtendedTime, false) << ' ';
        switch (level)
        {
            case LoggerLevel::LL_None:
                return false;
            case LoggerLevel::LL_Debug:
                Out << "DEBUG ";
                break;
            case LoggerLevel::LL_Info:
                Out << "INFO ";
                break;
            case LoggerLevel::LL_Warning:
                Out << "WARNING ";
                break;
            case LoggerLevel::LL_Error:
                Out << "ERROR ";
                break;
            case LoggerLevel::LL_Critical:
                Out << "CRITICAL ";
                break;
        }
    }

    return true;
}
bool Logger::EndLog()
{
    if (!IsInLog())
        return false;

    Out << '\n';
    this->State = LoggerLevel::LL_None;
    return true;
}

bool Logger::IsInLog() const
{
    return State != LoggerLevel::LL_None;
}
bool Logger::CurrentLogIsIgnored() const
{
    return State < Level;
}

Logger& Logger::operator<<(const std::string& obj)
{
    this->Write(obj);
    return *this;
}
Logger& Logger::operator<<(const DebugFormat& obj) 
{
    this->Write(obj);
    return *this;
}
Logger& Logger::operator<<(Logger& (*func)(Logger&))
{
    return func(*this);
}

Logger& Debug(Logger& in)
{
    if (!in.StartLog(LoggerLevel::LL_Debug))
        throw std::logic_error("Cannot start log line because another line has not been completed. Please finish that line first.");

    return in;
}
Logger& Info(Logger& in)
{
    if (!in.StartLog(LoggerLevel::LL_Info))
        throw std::logic_error("Cannot start log line because another line has not been completed. Please finish that line first.");

    return in;
}
Logger& Warning(Logger& in)
{
    if (!in.StartLog(LoggerLevel::LL_Warning))
        throw std::logic_error("Cannot start log line because another line has not been completed. Please finish that line first.");

    return in;
}
Logger& Error(Logger& in)
{
    if (!in.StartLog(LoggerLevel::LL_Error))
        throw std::logic_error("Cannot start log line because another line has not been completed. Please finish that line first.");

    return in;
}
Logger& Critical(Logger& in)
{
    if (!in.StartLog(LoggerLevel::LL_Critical))
        throw std::logic_error("Cannot start log line because another line has not been completed. Please finish that line first.");

    return in;
}

Logger& EndLog(Logger& in)
{
    in.EndLog();
    return in;
}
