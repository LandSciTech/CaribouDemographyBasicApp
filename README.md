
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CaribouDemographyBasicApp

<!-- badges: start -->
<!-- badges: end -->
<h2 id="intro-purpose">Purpose</h2>
<p>The purpose of this application is to allow users to explore projections of the
population dynamics of boreal caribou. Projections include uncertainty about the
current state of boreal caribou populations, and how those populations will
change in the future. Caribou demographic model parameters, and uncertainty
about those parameter values, are estimated from available data on the survival
of collared caribou and the numbers of calves observed per cow in aerial surveys
using a modified version of the
<a href="https://poissonconsulting.github.io/bboutools/">bboutools</a> R package. These
parameter estimates are used to project the likely changes in the population
over time if there are no future changes in average survival and recruitment. We
project a mean value for the population over time and a number of plausible
trajectories created by randomly selecting parameter values from the estimated
uncertainty distributions. Variation among plausible trajectories indicates
uncertainty in projections informed only by local available survey data. In this
version of the application, uncertainty can be reduced by acquiring more data.
We are also working on methods for reducing uncertainty using knowledge of the
state of a landscape and outcomes for caribou populations across the country,
and plan to incorporate these methods into future versions of the app.</p>
<p>The app allows users to add and analyze data for their populations of interest.
Users can then explore how changing survival and recruitment rates would change
expected outcomes for caribou.</p>
<h2 id="authors">Authors</h2>
<p>This app was created by Sarah Endicott and Josie Hughes (Wildlife and Landscape
Science Division, Environment and Climate Change Canada), and Danielle Cantin
(Canadian Wildlife Service, Environment and Climate Change Canada).</p>
<h2 id="acknowlegments">Acknowlegments</h2>
<p>Guy Larocque (Canadian Forest Service, Natural Resources Canada) and Rebecca
Viejou (affiliation?) participated in conceptualizing the app and compiling
available data.</p>

## Installation

To run the app you will need to install R and we recommend RStudio as
well. Follow the instructions
[here](https://posit.co/download/rstudio-desktop/) to install both.

You can install the CaribouDemographyBasicApp R package by running the
following in the R console:

``` r
install.packages("remotes")
remotes::install_github("LandSciTech/CaribouDemographyBasicApp")
```

## Launching the Shiny app

To launch the app in your default browser run the following lines:

``` r
library(CaribouDemographyBasicApp)
run_caribou_demog_app()
```
