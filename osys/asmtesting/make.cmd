@echo off
bin\fasm -d OS=windows a.fasm -s a.fas
if errorlevel 1 goto :end
echo ** Generating a.fasm listing **
bin\fasmlist -i a.fas >a.lst
if not errorlevel 1 goto :end
echo ** Non zero return code: %ERRORLEVEL%. **
:end