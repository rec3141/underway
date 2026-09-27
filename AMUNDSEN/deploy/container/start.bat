@echo off
rem Start the underway dashboard on Windows (Docker Desktop). Double-click this file.
cd /d "%~dp0" 2>nul || goto :copyfirst
rem The dashboard writes its database and site into data\ all the time: it must run
rem from the computer's own disk, not from the USB drive or share the kit came on.
if "%~d0"=="\\" goto :copyfirst
fsutil fsinfo drivetype %~d0 2>nul | findstr /i /c:"Removable" /c:"Remote" /c:"Network" >nul && goto :copyfirst
docker info >nul 2>&1 || (echo Docker Desktop is not running. Start it from the Start menu, wait for the whale icon, then try again. & pause & exit /b 1)
for /f "tokens=1,* delims==" %%a in (.env) do if "%%a"=="UNDERWAY_VERSION" set UNDERWAY_VERSION=%%b
docker image inspect underway:%UNDERWAY_VERSION% >nul 2>&1 || (echo Loading the dashboard image, a few minutes, once... & docker load -i underway-%UNDERWAY_VERSION%.tar.gz)
if not exist data\config mkdir data\config
if exist seed\*.tar if not exist data\.seeded (
  echo Unpacking the starting data, once; it can take half an hour...
  for %%t in (seed\*.tar) do (
    echo    %%~nt
    if not exist data\%%~nt mkdir data\%%~nt
    tar -xf %%t -C data\%%~nt || (echo Unpacking %%~nt failed. & pause & exit /b 1)
  )
  echo.> data\.seeded
  echo    Done. The seed folder is no longer needed and may be deleted to free space.
)
docker compose up -d
if not exist data\config\admin-password (
  echo Making the password for the settings page...
  timeout /t 10 >nul
  for /f "tokens=2 delims=:" %%p in ('docker exec underway python3 -m dashboard set-admin-password --random ^| findstr /b /c:"The /settings password is now"') do set PW=%%p
)
if defined PW (
  echo Settings page password: %PW%> ADMIN-PASSWORD.txt
  echo Settings page password: %PW%   ^(also saved in ADMIN-PASSWORD.txt^)
)
echo.
echo The dashboard is starting. On this computer open http://localhost/
echo From other computers use this computer's address (ipconfig lists it), e.g. http://10.0.0.x/
pause
exit /b 0

:copyfirst
echo This folder is on a USB drive or a network share.
echo Copy the whole folder onto this computer first (for example C:\underway),
echo then double-click start.bat in the copy. See HOW-TO.txt, step 2.
pause
exit /b 1
