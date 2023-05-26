@echo --- Build with old compiler ---
@rd /s /q build1
@md build1 >nul
@source\Build /s source /b build1 Build
@if errorlevel 1 goto end
@
@echo --- Build with new compiler ---
@rd /s /q build2
@md build2 >nul
@build1\Build /s source /b build2 Build
@if errorlevel 1 goto end
@
@echo --- Compiler build successful, copying Build.exe to source ---
@copy /Y source\Build0.exe source\Build1.exe >NUL
@copy /Y source\Build.exe source\Build0.exe >NUL
@copy /Y build2\Build.exe source\Build.exe >NUL
@
:end
