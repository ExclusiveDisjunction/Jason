//
// Created by exdisj on 10/21/24.
//

#ifndef JASON_BINARYUNIT_H
#define JASON_BINARYUNIT_H

#include <vector>
#include <utility>
#include <memory.h>
#include <stdexcept>

template <typename T, typename U>
concept is_different_v = !std::is_same_v<T, U>;

class Unit
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
    /// @brief Allocates based on a list of a specific type
    /// @tparam T The type to convert from
    /// @param obj The objects to convert from
    template<typename T>
    void AllocateList(const std::vector<T>& obj);

public:
    /// @brief Constructs an empty unit with no data or block size.
    Unit();
    /// @brief Constructs a Unit from the data stored in the other Unit
    /// @param obj The object to copy
    Unit(const Unit& obj) noexcept;
    /// @brief Constructs a Unit from the data stored in another unit
    /// @param obj The data to move
    Unit(Unit&& obj) noexcept;
    /// @brief Cleans up internal data
    ~Unit();

    /// @brief Constructs a `Unit` from a specific variable data. NOTE: Do not use with types that include internal pointers, as pointers may be changed when the data is converted out of the `Unit`.
    /// @tparam T The type being inputted
    /// @param item The data being inputted
    /// @return A unit containing a copy of that object's memory map
    template<typename T>
    static Unit FromVar(const T& item)
    {
        Unit result;
        result.Allocate(item);
        return result;
    }
    /// @brief Constructs a `Unit` from a list of variable data. The block size will be the `sizeof(T)` * `item.length()`. NOTE: Do not use with types that include internal pointers, as pointers may be changed when the data is converted out of the `Unit`.
    /// @tparam T The type being inputted
    /// @param item The objects being inputted
    /// @return A unit containing a copy of all elements memory map
    template<typename T>
    static Unit FromList(const std::vector<T>& item)
    {
        Unit result;
        result.AllocateList(item);
        return result;
    }
    /// @brief Constructs a unit with a specific size, but no actual data contained (full of zeroes)
    /// @param size The size to construct
    /// @return A unit with a specific size
    static Unit FromSize(unsigned char size) noexcept;
    /// @brief If copy is true, copies the data provided. Otherwise, it will assume ownership of the pointer & will handle its memory accordingly.
    /// @param data The data to copy or move
    /// @param size The size of the data
    /// @param copy If we are copying or not
    /// @return The unit with the data contained
    static Unit FromCharPtr(char* data, unsigned char size, bool copy) noexcept;

    Unit& operator=(const Unit& obj) noexcept;
    Unit& operator=(Unit&& obj) noexcept;

    /// @brief Attempts to convert the unit into a specific output type. Throws `bad_cast` if the conversion is invalid (size mismatch)
    /// @tparam T The type to convert into
    /// @return A copy of the data, converted into `T`
    template<typename T>
    [[nodiscard]] T Convert() const;
    /// @brief Attempts to convert the unit into a specific output type. Throws `bad_cast` if the conversion is invalid (size mismatch)
    /// @tparam T The type to convert into
    /// @param result The output destination of the copied data.
    template<typename T>
    void Convert(T& result) const;
    /// @brief Converts the unit into a list of `T`. Throws `bad_cast` if `blockSize % sizeof(T) != 0`
    /// @tparam T The target output type
    /// @return A list containing the copied converted data
    template<typename T>
    [[nodiscard]] std::vector<T> ConvertMany() const;

    /// @brief Gets the raw pointer contained
    /// @return The internal pointer
    [[nodiscard]] const char* Expose() const noexcept;
    /// @brief Gets the internal size
    /// @return The internal size
    [[nodiscard]] unsigned char GetSize() const noexcept;
};

#include "BinaryUnit.tpp"

#endif //JASON_BINARYUNIT_H
