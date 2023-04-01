@echo --- Build with old compiler ---
@md build\boot1 >nul
@cd build\boot1
@del /q *.*
@copy ..\..\source\*.mod >nul
@copy ..\..\source\all >nul
@
..\boot0\smoc /b all
@if errorlevel 1 goto fail
@
@cd ..\..
@
@echo;
@echo --- Build with newly built compiler ---
@md build\boot2 >nul
@cd build\boot2
@del /q *.*
@copy ..\..\source\*.mod >nul
@copy ..\..\source\all >nul
@
..\boot1\smoc /b all
@if errorlevel 1 goto fail
@
@cd ..\..
@
@echo;
@echo --- Build test ---
@md build\test >nul
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
:fail
@cd ..\..
:end
