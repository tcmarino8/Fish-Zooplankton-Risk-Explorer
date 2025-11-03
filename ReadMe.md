#  Fish-Zooplankton Risk Explorer

Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches

---

##  Overview
The Bay-Delta Data Explorer is an interactive R Shiny application designed to visualize and explore ecological risk data from the San Francisco Estuary. It provides insights into seasonal (phenological) patterns, decline risks, and spatial distributions of fish and zooplankton species.  

This app accompanies the manuscript  
 Fournier, R.J., Marino, T.C., Carlson, S.M., & Ruhí, A. “Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches.” Ecological Applications, in press.

---

##  Key Features

###  Map  
Visualize spatial distributions of fish and zooplankton sampling stations across estuarine regions
- Fish stations shown in black  
- Zooplankton stations shown in maroon  
- Interactive region and taxa filters  
- Hover tooltips reveal station metadata
  #### SideBar Selections:
   - Display Fish Stations(checkbox TRUE/FALSE)
   - Display Zooplankton Stations(checkbox TRUE/FAlSE)
   - Select Regions(SelectInputs multiple = TRUE)
   
   

### Phenology of Risk  
Monthly critical decline risk for each fish species by region.  
- Points scale by the percentage of annual catch  
- Saturated colors = high-abundance months (80% annual catch window)  
- Desaturated colors = low-abundance months
    #### SideBar Selections:
   - Select Regions(checkbox multiple = TRUE)
   - Select Fish(checkbox multiple = TRUE)
   - Months(SliderInput multiple = TRUE)

###  1-Year Risk Predictions  
Compare mean risk of fish species during their high-abundance periods with that of their zooplankton prey over a one-year window.  
- Error bars represent “best-case” and “worst-case” model uncertainty bounds
    #### SideBar Selections:
   - Select Regions(checkbox multiple = TRUE)
   - Select Fish(checkbox multiple = TRUE)
   - Select Zooplankton(checkbox multiple = TRUE)
   - Years From Present(Single Value SliderInput multiple = TRUE)

###  10-Year Risk Projections  
Explore projected risk trends over a 10-year period.  
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
Includes
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

- Fish data California Department of Fish and Wildlife (Bay Study)  
- Zooplankton data Interagency Ecological Program (Environmental Monitoring Program)  
- Modeled data Fournier et al. (in press)

### Access Datasets and Code
- Data [Dryad Repository (private for peer review)](httpdatadryad.orgstashsharezfLH561PA-zI0Kf_JAgr9gk9ejY2f3ecASN0zvp_vjM)  
- Code [Zenodo Repository (private for peer review)](httpszenodo.org)

---


## App Citation

'Marino, T.C., Fournier, R.J., Carlson, S.M., & Ruhí, A. (2025)
Bay-Delta Data Explorer: Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches.
https://12022001delta.shinyapps.io/DeclineRisk/'

## Bug Reporting
'If you encounter any bugs or have questions about the app, please contact:
📧 Tyler Marino — tylermarino.research@gmail.com'


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
  DT, RColorBrewer, patchwork, rsconnect
))

```

