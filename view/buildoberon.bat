@rd /s /q build
@md build >nul
@..\source\ORB /v /b build TestOberon
@if errorlevel 1 goto end
@
@build\TestOberon
:end
