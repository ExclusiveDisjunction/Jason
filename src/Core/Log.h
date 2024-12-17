#ifndef LOG_H
#define LOG_H

#include <iostream>
#include <fstream>
#include <climits>
#include <type_traits>
#include <concepts>

#include "DateTime.h"

enum LoggerLevel
{
    LL_None = INT_MAX,
    LL_Debug = 1,
    LL_Info = 2,
    LL_Warning = 3,
    LL_Error = 4,
    LL_Critical = 5
};

/// @brief A class that allows specific debugging strings for logging
class DebugFormat
{
public:
    virtual void dbg_fmt(std::ostream& obj) const noexcept = 0;
    
    [[nodiscard]] std::string dbg_fmt_string() const noexcept
    {
        std::stringstream ss;
        this->dbg_fmt(ss);
        return ss.str();
    }
};

std::ostream& operator<<(std::ostream& out, const DebugFormat& obj) noexcept
{
    obj.dbg_fmt(out);
    return out;
}

class Logger
{
private:
    void StartLog(LoggerLevel level);
    void EndLog();

    std::ofstream Out;
    LoggerLevel State;
    LoggerLevel Level;

public:
    Logger();
    Logger(const Logger& obj) = delete;
    Logger(Logger&& obj) noexcept = default;
    ~Logger();

    bool Open(const std::string& path, LoggerLevel level = LoggerLevel::LL_Info);
    bool IsOpen() const noexcept;

    Logger& operator=(const Logger& obj) = delete;
    Logger& operator=(Logger&& obj) noexcept = default;

    friend Logger& Debug(Logger&);
    friend Logger& Info(Logger&);
    friend Logger& Warning(Logger&);
    friend Logger& Error(Logger&);
    friend Logger& Critical(Logger&);
    friend Logger& EndLog(Logger&);

    /// @brief Returns true if the logger has started, but not ended a log.
    /// @return A boolean containing the state.
    bool IsInLog() const;
    bool CurrentLogIsIgnored() const;

    template<typename T>
    void Write(const T& obj)
    {
        if (!IsOpen())
            throw std::logic_error("logger not open");
        if (!IsInLog())
            throw std::logic_error("Cannot insert into log when a level has not been specified.");

        if (!CurrentLogIsIgnored())
            this->Out << obj;
    }

    Logger& operator<<(const std::string& obj);
    Logger& operator<<(const DebugFormat& obj);
    Logger& operator<<(Logger& (*func)(Logger&));
    template<typename T>
    requires (std::is_arithmetic_v<T> && !std::is_pointer_v<T>)
    Logger& operator<<(const T& obj)
    {
        this->Write(obj);
        return *this;
    }
};

Logger& Debug(Logger& in);
Logger& Info(Logger& in);
Logger& Warning(Logger& in);
Logger& Error(Logger& in);
Logger& Critical(Logger& in);

Logger& EndLog(Logger& in);

extern Logger logging;

#endif