nmake /f Makefile-MSVC.mak
if %errorlevel% equ 0 (cd ..\test && call runtests.cmd && cd ..\src)
