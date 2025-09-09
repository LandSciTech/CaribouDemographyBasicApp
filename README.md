
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CaribouDemographyBasicApp

<!-- badges: start -->
<!-- badges: end -->
<h5 id="intro-purpose"><strong>Purpose</strong></h5>
<img align="right" src="C:/Users/EndicottS/AppData/Local/Programs/R/R-4.4.1/library/CaribouDemographyBasicApp/www/Female_caribou_and_calves.jpg" width="500">
The purpose of this application is to allow users to explore projections of the
population dynamics of boreal caribou. 
<p>Projections include uncertainty about the
current state of boreal caribou populations, and how those populations will
change in the future.</p>
<p>Caribou demographic model parameters, and uncertainty
about those parameter values, are estimated from available data on the survival
of collared caribou and the numbers of calves observed per cow in aerial surveys
using a modified version of the <a href="https://poissonconsulting.github.io/bboutools/" target="_blank">bboutools</a>
R package.</p>
<p>These parameter estimates are used to project the <strong>likely changes in the population
over time if there are no future changes in average survival and recruitment</strong>.</p>
<p>We project a mean value for the population over time and a number of plausible
trajectories created by randomly selecting parameter values from the estimated
uncertainty distributions.</p>
<p>Variation among plausible trajectories indicates
uncertainty in projections informed only by local available survey data. In this
version of the application, <strong>uncertainty can be reduced by acquiring more data</strong>.</p>
<p>We are also working on methods for reducing uncertainty using knowledge of
relationships between caribou demographic rates and the level of disturbance
in the landscape that have been observed among populations across the country.
We plan to incorporate these methods into future versions of the app.</p>
<p>The app allows users to add and analyze data for their populations of interest.
<strong>Users can then explore how changing survival and recruitment rates would change
expected outcomes for caribou</strong>.</p>
<h5 id="authors"><strong>Authors</strong></h5>
<p>This app was created by Sarah Endicott and Josie Hughes (Wildlife and Landscape
Science Division, Environment and Climate Change Canada), and Danielle Cantin
(Canadian Wildlife Service, Environment and Climate Change Canada).</p>
<h5 id="acknowlegments"><strong>Acknowlegments</strong></h5>
<p>Guy Larocque and Rebecca Viejou (Canadian Forest Service, Natural Resources Canada)<br />
participated in conceptualizing the app and compiling available data.</p>

## Installation

There are two options available for installing the app. Users who do not
have R installed and are not familiar with R packages can follow the
instructions on the
[Releases](https://github.com/LandSciTech/CaribouDemographyBasicApp/releases)
page to download a version of the app that is packaged with a version of
R. Please note, you will not be able to update the caribou demographic
data stored with the app when it is installed this way.

R users or those who wish to update the caribou demographic data used in
the app can follow the instructions below to install the R package.

To run the app you will need to install R and we recommend RStudio as
well. Follow the instructions
[here](https://posit.co/download/rstudio-desktop/) to install both.

In addition, you will need to install the Rtools application. You can do
this by selecting the version corresponding to your version of R
[here](https://cran.r-project.org/bin/windows/Rtools/). Or you can run
the following code in the R console which should prompt Rtools to be
installed.

``` r
install.packages("pkgbuild")
pkgbuild::check_build_tools()
```

This will check if you already have Rtools installed and if not it will
ask you if you would like to install it. Wait for the tool to download
and install, following the instructions that pop-up. When installation
is complete close R or RStudio and open it again in a fresh session for
the changes to take effect. You can run `pkgbuild::check_build_tools()`
again to confirm that the installation was successful.

You can install the CaribouDemographyBasicApp R package by running the
following in the R console:

``` r
install.packages("remotes")
remotes::install_github("LandSciTech/CaribouDemographyBasicApp")
```

Note this may take a long time the first time you install it since it
will also install all the R packages that CaribouDemographyBasicApp
depends on.

## Launching the Shiny app

To launch the app in your default browser run the following lines:

``` r
library(CaribouDemographyBasicApp)
run_caribou_demog_app()
```

To launch the app in French include the lang = “fr” argument:

``` r
run_caribou_demog_app(lang = "fr")
```

## Updating the R package and app

If you want to see the latest changes made to the app you will need to
re-install the package from GitHub. Run the code below in the R console:

``` r
remotes::install_github("LandSciTech/CaribouDemographyBasicApp",
                        type = "binary", upgrade = "always")
```

This will install the most recent version of the app, upgrade any
dependencies that have updates available and it will only use binary
package versions. This avoids the need for any user input during the
installation.
