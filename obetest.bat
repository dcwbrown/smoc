cd /d c:\Users\dave\projects\oberon\smoc
rd /s /q obetest >NUL 2>NUL
mkdir obetest >NUL 2>NUL
copy source\*.mod obetest
build\boot2\obe Smoc /search obetest
