@echo off
echo Building project...
call rsvars.bat
msbuild /t:build /p:config=Debug "mockjsonapi.dproj"
if errorlevel 1 exit 1 