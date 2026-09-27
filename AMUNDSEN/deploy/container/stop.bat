@echo off
rem Stop the underway dashboard. Nothing is lost; start.bat brings it back.
cd /d "%~dp0"
docker compose down
pause
