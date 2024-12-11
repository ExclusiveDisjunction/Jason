#include <iostream>

#include "IOTester.h"

int main(int, char**)
{
    std::cout << " Welcome to Jason " << '\n'
              << "------------------" << '\n'
              << "   Version " << JASON_CURRENT_VERSION << "  " << '\n' << '\n';

    if (!logging.Open("run.log", LoggerLevel::LL_Debug))
    {
        std::cout << "Could not open log, aborting";
        return 0;
    }

    logging << Info << "Starting up Jason" << EndLog;

    //IOTester();

    logging << Info << "Exiting Jason, Exit Code 0" << EndLog;
    return 0;
}
