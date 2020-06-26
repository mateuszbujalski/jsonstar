# jsonstar
JSON Serialization library for F*

# Setup
This library depends currently on changes to F* that have not yet been merged to master (https://github.com/mateuszbujalski/FStar/tree/matbuj_fsharp_extraction) and require some special setup. 

The code assumes that in the root of the repository there is a directory external with following contents:

```
external
    fstar
        libs -- contains ulibfs.dll
	    targets -- contains fsharp.extraction.targets from F* repository
	    tools 
	        bin -- contains fstar.exe and everything needed to run it
	        ulib -- contains the F* standard library (sources) as they are needed for F* to work (F* assumes that ulib is in ..\ulib)
```

In the future the external\fstar directory should be converted into a nuget package. 
