@rd /s /q build
@md build >nul
@..\source\ORB /b build TestWindows
@if errorlevel 1 goto end
@
@build\TestWindows
:end
