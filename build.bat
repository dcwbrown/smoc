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
@build\boot2\test
@goto end
:fail
@cd ..\..
:end
