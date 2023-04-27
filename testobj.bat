@echo off
rem testobj.bat
rem was: del /q *.* build\*.* output\*.* && copy ..\source\*.mod && copy ..\build\boot2\ORB.exe && copy ..\build\boot2\*.dll

if not exist object (
  echo No object directory - are you in the right place?
) else (
  rd /s /q object
  md object\build
  md object\output
  copy source\*.mod object >NUL
  build\boot2\orb /object /source object /build object\build /output object\output Objtest
  if errorlevel 1 goto end
  object\output\objtest
)
:end