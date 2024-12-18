//
// Created by Hollan on 5/23/24.
//

#include "Log.h"
#include "Errors.h"

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

void Logger::StartLog(LoggerLevel level)
{
    if (level == LoggerLevel::LL_None)
    {
        EndLog();
        return;
    }

    if (this->State != LoggerLevel::LL_None)
        throw OperationError("start log", "line already started");

    this->State = level;

    if (!CurrentLogIsIgnored())
    {
        //We need to write the beginning of the level.

        Out << DateTime::Now().ToString(DateStringFormat::ShortDate, TimeStringFormat::ExtendedTime, false) << ' ';
        switch (level)
        {
            case LoggerLevel::LL_None:
                throw OperationError("start log", "cannot start a log at level 'NONE'");
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
}
void Logger::EndLog()
{
    if (!IsInLog())
        throw OperationError("end log", "line not started");

    Out << '\n';
    this->State = LoggerLevel::LL_None;
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
Logger& Logger::operator<<(const DebugPrint& obj) 
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
    in.StartLog(LoggerLevel::LL_Debug);
    return in;
}
Logger& Info(Logger& in)
{
    in.StartLog(LoggerLevel::LL_Info);
    return in;
}
Logger& Warning(Logger& in)
{
    in.StartLog(LoggerLevel::LL_Warning);
    return in;
}
Logger& Error(Logger& in)
{
    in.StartLog(LoggerLevel::LL_Error);
    return in;
}
Logger& Critical(Logger& in)
{
    in.StartLog(LoggerLevel::LL_Critical);
    return in;
}

Logger& EndLog(Logger& in)
{
    in.EndLog();
    return in;
}
