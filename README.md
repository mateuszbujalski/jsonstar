# jsonstar
JSON Serialization library for F*

# Setup
This library depends currently on changes to F* that have not yet been merged to master (https://github.com/mateuszbujalski/FStar/tree/matbuj_fsharp_extraction) and requires a nuget created from the contents of FStar repository, Z3 and mingw binaries. Nuget is included in "nugets" directory in the repository and the sources for it's creation can be found inside "external/FStar.Windows" directory.

# Usage
Reference jsonstar nuget from your project and add "--include $(JSONSTAR_INCLUDE)" to your FSTAR_FLAGS property in your project. 
