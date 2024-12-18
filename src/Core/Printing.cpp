//
// Created by Hollan on 12/17/24.
//

#include "Printing.h"

#include <sstream>

std::string DebugPrint::dbg_fmt_string() const noexcept
{
    std::stringstream ss;
    this->dbg_fmt(ss);
    return ss.str();
}
std::string DisplayPrint::dsp_to_string() const noexcept
{
    std::stringstream ss;
    this->dsp_fmt(ss);
    return ss.str();
}
std::string UIDisplayPrint::ui_dsp_to_string() const noexcept
{
    std::stringstream ss;
    this->ui_dsp_fmt(ss);
    return ss.str();
}

std::ostream& operator<<(std::ostream& out, const DebugPrint& obj) noexcept
{
    obj.dbg_fmt(out);
    return out;
}
std::ostream& operator<<(std::ostream& out, const DisplayPrint& obj) noexcept
{
    obj.dsp_fmt(out);
    return out;
}
std::ostream& operator<<(std::ostream& out, const UIDisplayPrint& obj) noexcept
{
    obj.ui_dsp_fmt(out);
    return out;
}

depm debug_print(const DebugPrint& obj) noexcept
{
    return depm(obj);
}
dipm display_print(const DisplayPrint& obj) noexcept
{
    return dipm(obj);
}
udipm ui_display_print(const UIDisplayPrint& obj) noexcept
{
    return udipm(obj);
}

std::ostream& operator<<(std::ostream& out, const depm& obj) noexcept
{
    return out << obj.target;
}
std::ostream& operator<<(std::ostream& out, const dipm& obj) noexcept
{
    return out << obj.target;   
}
std::ostream& operator<<(std::ostream& out, const udipm& obj) noexcept
{
    return out << obj.target;
}

