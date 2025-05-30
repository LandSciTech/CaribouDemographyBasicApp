@echo %on
setlocal enabledelayedexpansion

set "RootDir=%~dp0app_files\"
set "RPathFound=false"

echo "Updating the app, this will take > 5 mins"
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
                goto :buildApp
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
        goto :buildApp
    )
)

:buildApp
"%R%" --no-save -f "%RootDir%build.R" > log.out 2>&1

:packApp
echo "Packaging the app into caribou_demography_app.tar, this will take several minutes"
tar --exclude rtools --exclude build.R -cf caribou_demography_app.tar app_files run.bat README-USER.txt

exit /b 0
