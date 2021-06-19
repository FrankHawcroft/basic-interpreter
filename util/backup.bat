for /f "tokens=2-4 delims=/ " %%a in ('date /t') do (set budate=%%c-%%b-%%a)
"C:\Program Files\7-Zip\7z" a -tzip -xr!*.o -xr!*.obj -xr!.vs -xr!Debug -xr!Release -xr!amiga-obj -xr!x64 -xr!*.vcxproj* -xr!*.sln -xr!*.vspx -xr!*.diagsession -xr!CppProperties.json -xr!_removed -xr!*.DS_Store \Backup\Bas-016A-%budate%.zip %~dp0..\
