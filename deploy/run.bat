@echo %off
setlocal enabledelayedexpansion

set "RootDir=%~dp0app_files\"
set "RPathFound=false"

echo "The app will open in your browser shortly"
echo "If you encounter an error send the contents of log.out to the app developers for assistance"

:: First, try to find R in the portable directory
if exist "%RootDir%R\bin\x64\R.exe" (
    set "R=%RootDir%R\bin\x64\R.exe"
    set "RPathFound=true"
)

:: If not found, search for R in default installation paths
if not !RPathFound! == true (
    for %%d in ("C:\Program Files\R" "C:\Program Files (x86)\R") do (
        for /d %%i in (%%d\R-*) do (
            if exist "%%i\bin\x64\R.exe" (
                set "R=%%i\bin\x64\R.exe"
                set "RPathFound=true"
                goto :runApp
            )
        )
    )
)

:: If R is still not found, prompt the user
:promptRPath
if not !RPathFound! == true (
    set /p "R=Cannot detect R installation location. Please specify the full path to your R installation (R.exe): "
    if not exist "!R!" (
        echo The specified path does not exist. Please try again.
        goto :promptRPath
    ) else (
        set "RPathFound=true"
        echo.
        goto :runApp
    )
)

:runApp
"%R%" --no-save --slave -f "%RootDir%run.R"> log.out 2>&1

exit /b 0
