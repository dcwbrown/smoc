@echo --- Build with old compiler ---
@rd /s /q build1
@md build1 >nul
@source\ORB /s source /b build1 ORB
@if errorlevel 1 goto end
@
@echo --- Build with new compiler ---
@rd /s /q build2
@md build2 >nul
@build1\ORB /s source /b build2 ORB
@if errorlevel 1 goto end
@
@echo --- Compiler build successful, copying ORB.EXE to source ---
@copy /Y source\ORB0.exe source\ORB1.exe >NUL
@copy /Y source\ORB.exe source\ORB0.exe >NUL
@copy /Y build2\ORB.exe source\ORB.exe >NUL
@
:end
