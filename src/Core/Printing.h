//
// Created by Hollan on 12/17/24.
//

#ifndef JASON_PRINTING_H
#define JASON_PRINTING_H

/*
 * PRINTING
 * 
 * This header defines three types:
 * 
 * 1. DebugPrint
 * 2. DisplayPrint
 * 3. UIDisplayPrint
 * 
 * DebugPrint is used to denote a type that has developer facing outputs. This is, for example, used by the Error classes and Logging functionality.
 * DisplayPrint is used for user facing contexts, or for console outputs.
 * UIDisplayPrint is used for user facing contexts, but specifically for UI. 
 * 
 * Each object supports outputs to std::ostream, and provides a to_string functionality. 
 */

#include <iostream>
#include <string>

/// @brief A class that allows specific debugging strings for logging
class DebugPrint
{
public:
    DebugPrint() = default;
    DebugPrint(const DebugPrint& obj) = default;
    DebugPrint(DebugPrint&& obj) noexcept = default;
    virtual ~DebugPrint() = default;
    
    DebugPrint& operator=(const DebugPrint& obj) = default;
    DebugPrint& operator=(DebugPrint&& obj) noexcept = default;
    
    virtual void dbg_fmt(std::ostream& obj) const noexcept = 0;

    [[nodiscard]] std::string dbg_fmt_string() const noexcept;
};
class DisplayPrint
{
public:
    DisplayPrint() = default;
    DisplayPrint(const DisplayPrint& obj) = default;
    DisplayPrint(DisplayPrint&& obj) noexcept = default;
    virtual ~DisplayPrint() = default;

    DisplayPrint& operator=(const DisplayPrint& obj) = default;
    DisplayPrint& operator=(DisplayPrint&& obj) noexcept = default;
    
    virtual void dsp_fmt(std::ostream& out) const noexcept = 0;
    
    [[nodiscard]] std::string dsp_to_string() const noexcept;
};
class UIDisplayPrint
{
public:
    UIDisplayPrint() = default;
    UIDisplayPrint(const UIDisplayPrint& obj) = default;
    UIDisplayPrint(UIDisplayPrint&& obj) noexcept = default;
    virtual ~UIDisplayPrint() = default;

    UIDisplayPrint& operator=(const UIDisplayPrint& obj) = default;
    UIDisplayPrint& operator=(UIDisplayPrint&& obj) noexcept = default;
    
    virtual void ui_dsp_fmt(std::ostream& out) const noexcept = 0;
    
    [[nodiscard]] std::string ui_dsp_to_string() const noexcept;
};

std::ostream& operator<<(std::ostream& out, const DebugPrint& obj) noexcept;
std::ostream& operator<<(std::ostream& out, const DisplayPrint& obj) noexcept;
std::ostream& operator<<(std::ostream& out, const UIDisplayPrint& obj) noexcept;

/// @brief Debug print manifest
struct depm {
private:
    const DebugPrint& target;
    
public:
    explicit depm(const DebugPrint& target) : target(target) {}
    friend std::ostream& operator<<(std::ostream&, const depm&) noexcept;
};
/// @brief Display print manifest
struct dipm {
private:
    const DisplayPrint& target;

public:
    explicit dipm(const DisplayPrint& target) : target(target) {}
    friend std::ostream& operator<<(std::ostream&, const dipm&) noexcept;
};
/// @brief UI Display print manifest
struct udipm {
private:
    const UIDisplayPrint& target;

public:
    explicit udipm(const UIDisplayPrint& target) : target(target) {}
    friend std::ostream& operator<<(std::ostream&, const udipm&) noexcept;
};

std::ostream& operator<<(std::ostream& out, const depm& obj) noexcept;
std::ostream& operator<<(std::ostream& out, const dipm& obj) noexcept;
std::ostream& operator<<(std::ostream& out, const udipm& obj) noexcept;

// These functions provide short hands for outputing objects that inherit from more than just one of the Printing classes
depm debug_print(const DebugPrint& obj) noexcept;
dipm display_print(const DisplayPrint& obj) noexcept;
udipm ui_display_print(const UIDisplayPrint& obj) noexcept;

#endif //JASON_PRINTING_H
