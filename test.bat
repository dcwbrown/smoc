@echo --- Build test ---
@cd build\test
@del /q *.*
@copy ..\..\test\*.mod >nul
@copy ..\..\test\all >nul
@copy ..\boot2\*.sym >nul
@copy ..\boot2\*.dll >nul
@copy ..\boot2\*.exe >nul
smoc test.mod
@if errorlevel 1 goto fail
@cd ..\..
@build\test\test
@goto end
@
@:fail
@cd ..\..
@:end
