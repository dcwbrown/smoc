@copy ..\source\Boot.mod   . >NUL
@copy ..\source\Kernel.mod . >NUL
@copy ..\source\Writer.mod . >NUL
@copy ..\source\Files.mod  . >NUL
@del *.smb >NUL
@..\source\Build /v /b build /s . OC2
@if errorlevel 1 goto end
@build\OC2
:end
