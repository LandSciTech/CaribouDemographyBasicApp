# Deploy Shiny app(s) locally on Windows

I have modified this from https://github.com/derryleng/Shiny_Desktop_App to work 
for installing shiny apps stored as R packages on GitHub


This will deploy a Shiny app as a standalone Windows desktop application.

- Optionally bundle in a portable R installation ([see below](#bundle-a-portable-r-installation)).
- Optionally bundle in a web browser ([see below](#bundle-a-portable-web-browser)).
- Edit build.R to install the app and its dependencies and bundle the files in a tar.
- Edit run.R to run the app.
- Double click build.bat to install the app and bundle the files.
- Double click run.bat to launch the app.

## Bundle a portable R installation

There are two options:

1. Install [R Portable](https://sourceforge.net/projects/rportable/), then copy the contents of **R-Portable\App\R-Portable\\** into the **R** folder.
2. Install a fresh installation of R (to avoid including packages the end-user may not use) and copy into the **R** folder.

## Bundle a portable web browser

Download and install [Google Chrome Portable](https://portableapps.com/apps/internet/google_chrome_portable), then copy the contents of **GoogleChromePortable\App\Chrome-bin\\** into the **chrome** folder.

Other suitable browsers may be also be used, but make sure to change *browser_path* in **run.R**.

## Extra tips

### Terminating app on window close

To allow the app to terminate when the browser window is closed, the following should be added to the *server* function:

> Note that *session* must be added as an argument to the *server* function

``` R
server <- function(input, output, session) {

  ...

  session$onSessionEnded(function() {
    stopApp()
  })

}
```
