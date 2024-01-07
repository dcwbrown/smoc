@rem Build bootstrap compiler
@cd bootstrap
@mkdir build >NUL 2>NUL
@del *.smb >NUL
@del *.code >NUL
@..\..\source\Build /v /b build /s ./;.. OC2
@rem @if errorlevel 1 goto end
@rem @build\OC2
@rem @if errorlevel 1 goto end
@rem @start.exe
@rem :end
