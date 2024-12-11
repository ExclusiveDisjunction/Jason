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

public:
    Unit();
    Unit(unsigned char Size);
    Unit(char* Data, unsigned char Size, bool Copy);
    Unit(const Unit& obj) noexcept;
    Unit(Unit&& obj) noexcept;
    ~Unit();

    template<typename T>
    static Unit FromVar(const T& item)
    {
        Unit result;
        result.Allocate(item);
        return result;
    }
    template<typename T>
    static Unit FromList(const std::vector<T>& item)
    {
        Unit result;
        result.AllocateList(item);
        return result;
    }

    void Deallocate();
    void Allocate(char* data, unsigned char size, bool copy);
    template<typename T>
    void Allocate(const T& obj);
    template<typename T>
    void AllocateList(const std::vector<T>& obj);

    Unit& operator=(const Unit& obj) noexcept;
    Unit& operator=(Unit&& obj) noexcept;
    template<typename T> requires is_different_v<T, Unit>
    Unit& operator=(const T& obj) noexcept;
    template<typename T> requires is_different_v<T, Unit>
    Unit& operator=(const std::vector<T>& items);

    template<typename T>
    [[nodiscard]] T Convert() const;
    template<typename T>
    void Convert(T& result) const;
    template<typename T>
    [[nodiscard]] std::vector<T> ConvertMany() const;

    [[nodiscard]] const char* Expose() const noexcept;
    [[nodiscard]] unsigned char GetSize() const noexcept;
};

#include "BinaryUnit.tpp"

#endif //JASON_BINARYUNIT_H
