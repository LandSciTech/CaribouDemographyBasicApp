---
output:
  word_document: default
  html_document: default
---
##### **Purpose** {#intro-purpose}

<img align="right" src="www/Female_caribou_and_calves.jpg" width="500">
The purpose of this application is to allow users to explore projections of the
population dynamics of boreal caribou. 

Projections include uncertainty about the
current state of boreal caribou populations, and how those populations will
change in the future. 

Caribou demographic model parameters, and uncertainty
about those parameter values, are estimated from available data on the survival
of collared caribou and the numbers of calves observed per cow in aerial surveys using the
<a href="https://LandSciTech.github.io/bboutools/" target="_blank">bboutools</a>
R package. 

If the level of disturbance in the landscape is provided then knowledge of 
relationships between caribou demographic rates and the level of disturbance 
in the landscape that have been observed among populations across the country is
used to inform the model in addition to the local survey data using the
<a href="https://LandSciTech.github.io/caribouMetrics/" target="_blank">caribouMetrics</a> 
R package.

These parameter estimates are used to project the **likely changes in the population
over time if there are no future changes in average survival and recruitment or disturbance**. 

We project a mean value for the population over time and a number of plausible
trajectories created by randomly selecting parameter values from the estimated
uncertainty distributions. 

If disturbance information is not provided variation among plausible trajectories indicates
uncertainty in projections informed only by local available survey data.

The app allows users to add and analyze data for their populations of interest.
**Users can then explore how changing survival and recruitment rates would change 
expected outcomes for caribou**.

##### **Authors**
This app was created by Sarah Endicott and Josie Hughes (Wildlife and Landscape
Science Division, Environment and Climate Change Canada), and Danielle Cantin
(Canadian Wildlife Service, Environment and Climate Change Canada).

##### **Acknowledgments**
Guy Larocque and Rebecca Viejou (Canadian Forest Service, Natural Resources Canada)  
participated in conceptualizing the app and compiling available data.


