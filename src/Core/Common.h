//
// Created by exdisj on 8/10/24.
//

#ifndef JASON_COMMON_H
#define JASON_COMMON_H

#include <string>
#include <fstream>
#include <cstring>

void trim_left(std::string& item) noexcept;
void trim_right(std::string& item) noexcept;
void trim(std::string& item) noexcept;

void readFileBlock(std::ostream& target, std::istream& input, std::streampos dest);

#endif //JASON_COMMON_H
