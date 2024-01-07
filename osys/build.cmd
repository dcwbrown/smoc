@mkdir build >NUL 2>NUL
@copy ..\source\Boot.mod   . >NUL
@copy ..\source\Kernel.mod . >NUL
@copy ..\source\Files.mod  . >NUL
@del *.smb >NUL
@del *.code >NUL
@..\source\Build /v /b build /s . OC2
@if errorlevel 1 goto end
@build\OC2
@if errorlevel 1 goto end
@start.exe
:end
