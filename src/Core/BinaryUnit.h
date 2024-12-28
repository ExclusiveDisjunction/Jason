//
// Created by exdisj on 10/21/24.
//

#ifndef JASON_BINARYUNIT_H
#define JASON_BINARYUNIT_H

#include <vector>
#include <utility>
#include <memory.h>
#include <stdexcept>

/*
class BinaryUnit
{
private:
    char* Data = nullptr;
    unsigned char blockSize = 0;

    /// @brief Clears out internal data
    void Deallocate();
    /// @brief Moves or copies the data provided at a specific size
    /// @param data The data to copy or move
    /// @param size How big the data is
    /// @param copy If true, the unit will contain a copy of the data, otherwise, it will assume ownership
    void Allocate(char* data, unsigned char size, bool copy);
    /// @brief Allocates based on a specific type
    /// @tparam T The type to convert from
    /// @param obj The object to convert from
    template<typename T>
    void Allocate(const T& obj);

public:
    /// @brief Constructs an empty unit with no data or block size.
    BinaryUnit();
    /// @brief Constructs a Unit from the data stored in the other Unit
    /// @param obj The object to copy
    BinaryUnit(const BinaryUnit& obj) noexcept;
    /// @brief Constructs a Unit from the data stored in another unit
    /// @param obj The data to move
    BinaryUnit(BinaryUnit&& obj) noexcept;
    /// @brief Cleans up internal data
    ~BinaryUnit();

    /// @brief Constructs a `Unit` from a specific variable data. NOTE: Do not use with types that include internal pointers, as pointers may be changed when the data is converted out of the `Unit`.
    /// @tparam T The type being inputted
    /// @param item The data being inputted
    /// @return A unit containing a copy of that object's memory map
    template<typename T>
    static BinaryUnit FromVar(const T& item)
    {
        BinaryUnit result;
        result.Allocate(item);
        return result;
    }
    /// @brief Constructs a unit with a specific size, but no actual data contained (full of zeroes)
    /// @param size The size to construct
    /// @return A unit with a specific size
    static BinaryUnit FromSize(unsigned char size) noexcept;
    /// @brief If copy is true, copies the data provided. Otherwise, it will assume ownership of the pointer & will handle its memory accordingly.
    /// @param data The data to copy or move
    /// @param size The size of the data
    /// @param copy If we are copying or not
    /// @return The unit with the data contained
    static BinaryUnit FromCharPtr(char* data, unsigned char size, bool copy) noexcept;
    
    void Resize(unsigned char amount_to_add, bool packEnd = true);

    BinaryUnit& operator=(const BinaryUnit& obj) noexcept;
    BinaryUnit& operator=(BinaryUnit&& obj) noexcept;

    /// @brief Attempts to convert the unit into a specific output type. Throws `bad_cast` if the conversion is invalid (size mismatch)
    /// @tparam T The type to convert into
    /// @return A copy of the data, converted into `T`
    template<typename T>
    [[nodiscard]] T Convert() const;
    /// @brief Attempts to convert the unit into a specific output type. Throws `bad_cast` if the conversion is invalid (size mismatch)
    /// @tparam T The type to convert into
    /// @param result The output destination of the copied data.
    template<typename T>
    void ConvertTo(T& result) const;

    /// @brief Gets the raw pointer contained
    /// @return The internal pointer
    [[nodiscard]] const char* Expose() const noexcept;
    /// @brief Gets the internal size
    /// @return The internal size
    [[nodiscard]] unsigned char GetSize() const noexcept;
};
 */

#include "BinaryUnit.tpp"

#endif //JASON_BINARYUNIT_H
