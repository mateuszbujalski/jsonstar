Nuget was created using following steps:
1. git clone https://github.com/mateuszbujalski/FStar.git -b matbuj_fsharp_extraction
	a) change to https://github.com/FStarLang/FStar.git once https://github.com/FStarLang/FStar/pull/2159 is merged
2. Inside cygwin terminal run `make -C src -j6 ocaml-fstar-ocaml` from <FStar_repo_root> to build ocaml version of F* (requires being able to build F* from sources on windows - check everest script)
3. From <FStar_repo_root> run `src\VS\nuget-restore.bat`
4. Open <FStar_repo_root>/src/VS/FStar.sln
5. Change project dependencies for ulib and uncheck dependency on fstar project.
6. Build ulib project
7. Copy <FStar_repo_root>/bin/fstar.exe to tools/bin
8. Copy <FStar_repo_root>/ulib to tools/ulib
9. Copy <FStar_repo_root>/bin/ulib.[dll|pdb|xml] to lib
10. Copy *.dll from mingw/bin (your installation) into tools/bin
11. Copy *.* from z3-4.8.5-x64-win/bin into tools/bin
12. Copy <FStar_repo_root>/fsharp.extraction.targets to targets
    a) rename it to FStar.Windows.targets
	b) If you need to work with sdk projects change TaskFactory="RoslynCodeTaskFactory" and AssemblyFile="$(MSBuildToolsPath\Microsoft.Build.Tasks.Code.dll"
13. Bump version in FStar.Windows.nuspec - the format is 1.YYYYMMDD.X, where YYYYMMDD is a date when nuget was created and X is a build number on that date. 
14. .\nuget.exe pack FStar.Windows.nuspec
15. Create some local nuget repository and add it as a source for your repo (<LOCAL_NUGET>)
16. Put created nuget in your local nuget repository: `nuget add FStar.Windows.1.YYYYMMDD.X.nupkg -source <LOCAL_NUGET>`
