:: Build bootstrap compiler
::
@cd bootstrap
@mkdir build >NUL 2>NUL
@..\..\source\Build /v /b build /s ./;.. bootbuild
@if errorlevel 1 goto end
@cd ..
::
:: use bootstrap compiler to build linktest2
::
@del *.smb >NUL
@del *.code >NUL
::
@bootstrap\build\bootbuild
@if errorlevel 1 goto end
::
:: run linktest2
::
@linktest2
::
:end
