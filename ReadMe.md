#  Fish-Zooplankton Risk Explorer

Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches

---

##  Overview
The Fish-Zooplankton Risk Explorer Explorer is an interactive R Shiny application designed to visualize and explore ecological risk data from the San Francisco Estuary as calculated in Fournier et al. 2025, Ecological Applications. It explores how phenological patterns of age-0 fishes and their prey might inform our understanding of critical decline risk throughout the region.

This app accompanies the manuscript  
 Fournier, R.J., Marino, T.C., Carlson, S.M., & Ruhí, A. “Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches.” Ecological Applications, in press.

---

##  Key Features

###  Map  
Visualize fish and zooplankton sampling stations within the San Francisco Estuary. Fish sampling is undertaken by the CDFW Bay study, while zooplankton sampling is performed by the IEP EMP. 

- Fish stations shown in black  
- Zooplankton stations shown in maroon  
- Interactive region and taxa filters  
- Hover tooltips reveal station metadata
  #### SideBar Selections:
   - Display Fish Stations(checkbox TRUE/FALSE)
   - Display Zooplankton Stations(checkbox TRUE/FAlSE)
   - Select Regions(SelectInputs multiple = TRUE)
   
   

### Phenology of Risk  
This visualizes how the probability of a critical population decline (≥90% drop) changes month by month for each fish species across estuarine regions. Critical decline risk is calculated using intrinsic growth rate and process error variance estimated using multivariate autoregressive (MAR) models fitted to fish and zooplankton abundance time series. 

- Points scale by the percentage of annual catch  
- Saturated colors = high-abundance months (80% annual catch window)  
- Desaturated colors = low-abundance months
    #### SideBar Selections:
   - Select Regions(checkbox multiple = TRUE)
   - Select Fish(checkbox multiple = TRUE)
   - Months(SliderInput multiple = TRUE)


###  1-Year Risk Predictions  
This plot compares present-day (near-term) decline risk between fish predators and their zooplankton prey within their shared high-abundance windows. High-abundance windows are defined as clusters of months which contain 80% of yearly catch across the time series.

Each point with associated uncertainty bars pairs a fish with its co-occurring prey community.
- Error bars represent “best-case” and “worst-case” model uncertainty bounds
    #### SideBar Selections:
   - Select Regions(checkbox multiple = TRUE)
   - Select Fish(checkbox multiple = TRUE)
   - Select Zooplankton(checkbox multiple = TRUE)
   - Years From Present(Single Value SliderInput multiple = TRUE)

###  10-Year Risk Projections  
A 10-year projection of critical decline risk for fish and zooplankton assemblage, based on time-series population trends and variability.

- Fish = solid lines  
- Zooplankton = dashed lines  
- Shaded areas = uncertainty range  
- Asterisks denote significant differences in mean risk
    #### SideBar Selections:
   - Select Regions(checkbox multiple = TRUE)
   - Select Fish(checkbox multiple = TRUE)
   - Select Zooplankton(checkbox multiple = TRUE)
   - Years From Present(Range with End Value SliderInput multiple = TRUE)

###  Station Data  
Tabular metadata summarizing the datasets used in modeling.  
Includes:
- Taxa  
- Region  
- StartEnd collection years  
- Total collection years  
- Mean CPUE (Catch Per Unit Effort)  
- Number of non-zero occurrences
 #### SideBar Selections:
   - Display Fish Stations(checkbox TRUE/FALSE)
   - Display Zooplankton Stations(checkbox TRUE/FAlSE)
   - Select Regions(SelectInputs multiple = TRUE)
   - Select Fish(checkbox multiple = TRUE)
   - Select Zooplankton(checkbox multiple = TRUE)
   

Interactive search and filtering via DT datatable.

###  Additional Information  
Links to data and code repositories, manuscript details, and citation information.

---

##  Data Sources

- Fish data via [California Department of Fish and WildlifeBay Study](https://wildlife.ca.gov/Conservation/Delta/Bay-Study)
- Zooplankton data via [Interagency Ecological Program (Environmental Monitoring Program)](https://wildlife.ca.gov/Conservation/Delta/Bay-Study)
- Modeled data Fournier et al. (in press)


### Access Datasets and Code
- Data [Dryad Repository](https://datadryad.org/landing/show?id=doi%3A10.5061%2Fdryad.4j0zpc8nd)  
- Code [Zenodo Repository ](https://zenodo.org/records/13800013)

---


## App Citation

'Marino, T.C., Fournier, R.J., Carlson, S.M., & Ruhí, A. (2025)
Bay-Delta Data Explorer: Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches.
https://baydelta.shinyapps.io/Fish-Zooplankton-Risk-Explorer/'

## Bug Reporting
'If you encounter any bugs or have questions about the app, please contact:
 Tyler Marino — tylermarino.research@gmail.com'


## Installation & Usage

### Prerequisites  
#### Softwares
Ensure you have R (≥ 4.0) and RStudio installed.

#### Files
StationSummary.csv
Fournier_et_al_monthly_risk.csv
Fournier_et_al_mean_risk.csv

### Install Required Packages  
Run the following in R
```r
install.packages(c(
  shiny, shinydashboard, shinyWidgets, leaflet, dplyr, 
  ggplot2, tidyverse, readxl, sf, raster, sp, 
  DT, RColorBrewer, patchwork, rsconnect, markdown
))

```

