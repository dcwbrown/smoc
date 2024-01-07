:: Build bootstrap compiler
::
@cd bootstrap
@mkdir build >NUL 2>NUL
@..\..\source\Build /v /b build /s ./;.. obuild
@if errorlevel 1 goto end
@cd ..
::
:: use obuild to build linktest2
::
@del *.smb >NUL
@del *.code >NUL
@bootstrap\build\obuild
@if errorlevel 1 goto end
::
:: run linktest2
::
@linktest2
@rem :end
