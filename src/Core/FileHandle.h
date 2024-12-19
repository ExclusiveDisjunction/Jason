//
//  FileHandle.h
//  jason-cli
//
//  Created by Hollan on 12/18/24.
//

#ifndef FileHandle_h
#define FileHandle_h

#include "Printing.h"

#include <fstream>
#include <filesystem>

class FileHandle : public DebugPrint
{
private:
    std::fstream file;
    std::filesystem::path path;
    
public:
    FileHandle();
    FileHandle(std::filesystem::path path, std::ios::openmode flags = (std::ios::in | std::ios::out))
    
    static std::ios::openmode StandardIO;
    static std::ios::openmode Trunc;
    static std::ios::openmode Append;
    static std::ios::openmode Binary;
    
    void Close();
    void Open(std::filesystem::path, std::ios::openmode flags = FileHandle::StandardIO);
    void Reopen(std::ios::openmode flags = FileHandle::StandardIO);
    
    [[nodiscard]] bool IsOpen() const noexcept;
    
    void dbg_fmt(std::ostream& out) const noexcept override;
}

#endif /* FileHandle_h */
