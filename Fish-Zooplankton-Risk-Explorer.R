## ----knitOptions-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

knitr::opts_chunk$set(echo = TRUE)


## ----Upload Packages-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(patchwork)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readxl)
library(sf)
library(tidyverse)
library(raster)
library(sp)
library(DT)
library(RColorBrewer)
library(rsconnect)
library(shinyWidgets)





  #### Gathering Overall sataion data ###
      # Read in Station Summary Data
if (file.exists("StationSummary.csv")) {
  StationSummaryData <- read.csv("StationSummary.csv")
  cat("Successfully read StationSummary.csv\n")
} else {
  cat("File StationSummary.csv does not exist.\n")
}

      # reading risk data file
if (file.exists("Fournier_et_al_monthly_risk.csv")) {
  RiskData <- read.csv("Fournier_et_al_monthly_risk.csv")
  cat("Successfully read Fournier_et_al_monthly_risk.csv\n")
} else {
  cat("File Fournier_et_al_monthly_risk.csv does not exist.\n")
}

      # Read in 1 Year Prediction Data
if (file.exists("Fournier_et_al_mean_risk.csv")) {
  FishZoopComparison_data <- read.csv("Fournier_et_al_mean_risk.csv")
  cat("Successfully read Fournier_et_al_mean_risk.csv")
} else {
  cat("File Fournier_et_al_mean_risk.csv does not exist.\n")
}

Predict10yrData <- FishZoopComparison_data

#### For indexing ####

        #Create list of unique station regions
            #Used in checkboxInput for Selecting Regions
station_regions <- unique(StationSummaryData$Region)

        #Remove Center Points
StationSummaryData <- StationSummaryData %>% filter(Group != 'Mean')


        #Reference names scientific = common
SpeciesNamesSciNorm <- c("Engraulis mordax" = 'Northern anchovy', "Atherinopsis californiensis" = 'Jack silverside', "Clupea pallasii" = 'Pacific herring', "Alosa sapidissima" = 'American shad', "Spirinchus thaleichthys" = 'Longfin smelt', "Morone saxatilis" = 'Striped Bass', "Dorosoma petenense" = 'Threadfin shad')


        #Get all Taxa Options
            #Used for checkboxInput for Selecting Taxa
TaxaOptions <- unique(RiskData$Taxa)



#### Preparing data for Phenology of Risk plot #### 
            #renmaing to simpler month identifiers.
phenology_of_risk_data <- RiskData %>% mutate(MonthName = factor(MonthName, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
            #region clarification
phenology_of_risk_data$Region<-dplyr::recode(phenology_of_risk_data$Region, "Sanpablo" = "San Pablo Bay", "Suisun" = "Suisun Bay", "Delta" = "Delta", "Confluence" = "Confluence", "Central" = "Central Bay", "South" = "South Bay")
            #Refactoring regions
phenology_of_risk_data$Region<-factor(phenology_of_risk_data$Region, levels = c("Delta", "Confluence" , "Suisun Bay", "San Pablo Bay", "Central Bay", "South Bay"))
            #Filter for valuable regions
phenology_of_risk_data<-phenology_of_risk_data %>% filter(Region %in% c("Delta", "Confluence", "Suisun Bay", "San Pablo Bay"))

#Data Manipulation For 10yr plot
# Predict10yrData <- Predict10yrData  %>% filter(Region %in% c("San Pablo Bay", "Suisun Bay", "Confluence", "Delta"))
# Predict10yrData$Region <- factor(Predict10yrData$Region, levels = c("Delta", "Confluence" , "Suisun Bay", "San Pablo Bay"))

#Necessary filler information for plots
pd <- position_dodge(1)
alphas<- c("Key" = 1, "Off" = .3)
SHAPES <- c("San Pablo Bay" = 17, "Suisun Bay" = 16, "Delta" = 18, "Confluence" = 15)
LINES <- c("Fish" = 1, "Zoop" = 2)
LINES2 <- c("Fish" = 1, "Zoop" = 3)
COLORS  <- c("Delta" = "#1B9E77", 
             "Confluence" = "#D95F02",
             "Suisun Bay" = "#7570B3",
             "San Pablo Bay" = "#E7298A")

legend_spec_POR <- list(
  list(label = "Delta", color = "#1B9E77"),
  list(label = "Confluence", color = "#D95F02"),
  list(label = "Suisun Bay", color = "#7570B3"),
  list(label = "San Pablo Bay", color = "#E7298A")
  
)

# basemap_dic <- c("GreyScale" = "CartoDB", "BrightLight" = "Esri.WorldStreetMap", "Contrast" = "OpenStreetMap.BZH")


#Information on each plot

POR_blurb <- "Monthly risk that an age-0 fish species would experience a 90% catch decline for that month in each region (hereafter, critical decline risk). Points are scaled to represent monthly percentage of annual catch (based on long-term averages). Months that collectively contain 80% of the mean annual catch are in saturated tones, while months outside of that window are desaturated. Gaps indicate that a species often had zero abundance for that month and region thus risk for that month was not assessed."
SingleYearPredicitons_blurb <- "Mean critical decline risk of fish during their high-abundance windows, paired with critical decline risk for their potential suite of zooplankton prey within that same window. Points represent probabilities calculated from maximum likelihood parameter estimates. Bars represent uncertainty between the “best” and “worst” case scenario. The lower bound represents the “best-case scenarios”, calculated with the most positive population trend and the lowest process error variance possible (based on estimated uncertainty for these parameters). Conversely, upper risk bounds represent “worst-case scenarios” calculated with the most negative population trend and highest process error variance."
TenYearPredictions_blurb <- "Critical decline risk projected out 10 years from present for fish (solid line) in their high-abundance window, and mean risk of their zooplankton prey assemblage (dashed line) during that same window. Bands represent the range between best case and worst case scenarios. Lower risk bounds represent “best case scenarios”, calculated with the most positive population trend and the lowest process error variance (based on estimated uncertainty for these parameters). Conversely, upper risk bounds represent “worst case scenario”, calculated with the most negative population trend and highest process error variance. Asterisks denote significant differences in mean critical decline risk between fishes and zooplankton."
StationData_Blurb <- "Metadata summarizing the datasets included in our modeling exercise. The table summarizes information available for each station (each shown as a unique row). The search bar on the top right allows for filtering species (Taxa) or stations (Station). The fields are: Group (Fish or Zooplankton), Taxa (species surveyed up at each station), Region (South Bay, Central Bay, San Pablo Bay, Suisun Bay, Confluence, Delta), Start.Collection.Year (first year each taxa was collected, in order of taxa appearance), End.Collection.Year (most recent year each taxa was collected, in order of taxa appearance), Total.Years.of.Collection (Number of years collection occurred, in order of taxa appearance), Mean.CPUE (Mean Catch Per Unit Effort), Occurrences (number of non-zero records)."
Map_blurb <- "Locations of the stations used in our models. Collection locations for fish data are represented in black circles, and data were generated by the California Department of Fish and Wildlife Bay Study. Collection locations for zooplankton are represented in maroon circles, and data were generated by the Interagency Ecological Program’s Environmental Monitoring Program. Hovering over data points reveals the station number and assigned region. Selecting and deselecting regions and fish taxa the points on the map provides further geographical insights for station data."
Data_resource_blurb <- "Data, metadata, and R code necessary to reproduce model results, analyses, and figures are accessible as ‘private for peer review’ on <a href='http://datadryad.org/stash/share/zfLH561PA-zI0Kf_JAgr9gk9ejY2f3ecASN0zvp_vjM' target='_blank'> Dryad (data) </a> and Zenodo (code). Raw abundance time series data can be accessed via their collecting agency, the California Department of Fish and Wildlife."
Manuscript_blurb <- "Fournier, R.J., Marino, T.C., Carlson, S.M., & A. Ruhí. Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches. Ecological Applications, in press"
Cite_blurb <- "Marino, T.C., Fournier, R.J., Carlson, S.M. & A. Ruhi. Data explorer: Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches. https://12022001delta.shinyapps.io/DeclineRisk/"
Bug_reporting_blurb <- "If you encounter any bug or have questions about this app, please contact Tyler Marino: tylermarino.research@gmail.com"

months_shortened <- c("January" = "Jan", "February" = "Feb", "March" = "Mar", "April" = "Apr", "May" = "May", "June" = "Jun", 
            "July" = "Jul", "August" = "Aug", "September" = "Sep", "October" = "Oct", "November" = "Nov", "December" = "Dec")
months <- c("January", "February", "March", "April", "May", "June", 
            "July", "August", "September", "October", "November", "December")


?sliderInput




## ----ShinyApp--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(div(h3("Fish-Zooplankton-Risk-Explorer"), h1("Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches"))),
  
  hr(),
  
  fluidRow(            
    tags$head(
      tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.querySelectorAll('a[href^=\"#\"]').forEach(function(link) {
          link.addEventListener('click', function(e) {
            e.preventDefault();
            var tabName = this.getAttribute('href').substring(1);
            var tab = document.querySelector('[data-value=\"' + tabName + '\"]');
            if (tab) tab.click();
          });
        });
      });
    "))
    ),
    div( style = "width: 30%; float: left; box-sizing: border-box;",                                   #Left hand side Tab Bar
      sidebarPanel(
        conditionalPanel(
          condition = " input.Tabs =='Map' || input.Tabs == 'StationData' ",
          checkboxInput(                                                                                 #Creating Check Box input for Fish Stations
          "fish_stations",
          "Display Fish Stations",
          TRUE
          ),
          checkboxInput(                                                                                 #Creating Check Box Input for Zooplankton Stations
            "zooplankton_stations",
            "Display Zooplankton Stations",
            TRUE
          ),
          hr(),                                                                                          #Horizontal Line
        ),
        conditionalPanel(                                                                                #Creating Inputs for Species
          condition = " input.Tabs == 'POR' 
          || input.Tabs == '1yrplots'
          || input.Tabs == '10yrplots' 
          || input.Tabs == 'StationData'
          || input.Tabs == 'Map'",
          selectInput(                                                                            #Creating Check box input for regions
            'region',
            'Select Regions',
            choices = station_regions,
            selected = station_regions,
            multiple = TRUE
          )),
        conditionalPanel(                                                                                #Creating Inputs for Species
          condition = " input.Tabs == 'POR' 
          || input.Tabs == '1yrplots'
          || input.Tabs == '10yrplots' 
          || input.Tabs == 'StationData'",
          
          selectInput(
            'fish',
            'Select Fish',
            choices = TaxaOptions[1:7],
            selected = TaxaOptions[1:3], 
            multiple = TRUE
          )
        ),
        conditionalPanel(                                                                                #Creating Inputs for Species
          condition = "input.Tabs == '1yrplots'
          || input.Tabs == 'StationData' ",
        selectInput(
          'zoops',
          'Select Zooplankton',
          choices = TaxaOptions[8:17],
          selected = TaxaOptions[8:12], 
          multiple = TRUE
        )
        ),
      

        conditionalPanel(
          condition = " input.Tabs =='1yrplots' ",
          sliderInput(                                                                                  #Slider input for years from Present
            "SliderYFP1", label="Year From Present",
            min=1, max=10, value=1, step= 1,                                                       #, animate=TRUE     To see evolution over time.
          ),
          hr(),                                                                                         #Horizontal Line
        ),
        conditionalPanel(
          condition = " input.Tabs =='10yrplots' ",
          sliderInput(                                                                                  #Slider input for years from Present
            "SliderYFP", label="Years From Present",
            min=1, max=10, value= 10, step= 1                                                      #, animate=TRUE     To see evolution over time.
          ),
          hr(),                                                                                         #Horizontal Line
        ),
        conditionalPanel(
          condition = " input.Tabs =='POR' ",
          sliderTextInput(                                                                                  #Slider input for years from Present
            inputId = "SliderMonths", label="Months",
            choices = month.name,
            selected = month.name[c(1, 12)],
            grid = TRUE
          ),
        )
      ),
      
    tags$head(
      tags$style(HTML("
      .custom-text {
        font-size: 20px;
        font-family: arial, sans-serif;
        color: black;
        font-weight: semi-bold
      }
      .custom-text-bold {
        font-size: 20px;
        font-family: arial, sans-serif;
        color: black;
        font-weight: bold
      }
    "))
    )
    ),
    div( style = "width: 70%; float: left; box-sizing: border-box;",
      mainPanel(
        tabsetPanel( id = 'Tabs',
                     tabPanel("Guidlines", includeMarkdown("DeclineRisk.md")),
                     tabPanel('Map', 
                              fluidRow(leafletOutput('DeltaMap'), hr(), div(textOutput('Map_blurb'), class = 'custom-text'))),
                     tabPanel('Phenology of Risk', value = 'POR',
                              fluidRow(div(textOutput('POR_blurb'), class = 'custom-text'), hr(), plotOutput("PORplot", width = "1000px", height = "90vh"))),
                     tabPanel('1 Year Risk Predictions', value = '1yrplots',
                              fluidRow(div(textOutput('SYP_blurb'), class = 'custom-text'), hr(), plotOutput("SingleYearPlot", width = "100%", height = "90vh"))),
                     tabPanel('10 Year Risk Predictions', value = '10yrplots',
                              fluidRow(div(textOutput('TYP_blurb'), class = 'custom-text'), hr(), plotOutput("TenYearPlot", width = "100%", height = "90vh"))),
                     tabPanel('Station Data', value = "StationData",
                              fluidRow(div(textOutput('StationData_blurb'), class = 'custom-text'), hr(),
                                         DTOutput("Station_data", height = "500px"),
                                         leafletOutput('StationMap', height = "500px")
                                       )),
                                       # \DTOutput("Station_data"), leafletOutput('StationMap'))),
                     tabPanel('Additional Information',
                              fluidRow(div(uiOutput('LinktoDataBlurb', class = 'custom-text')), 
                                       hr(),
                                       div("Analysis and model outputs are associated with the following manuscript:", class = 'custom-text-bold'), div(uiOutput('ManuscriptBlurb', class = 'custom-text')),
                                       hr(),
                                       div('Cite App:', class = 'custom-text-bold'), div(textOutput('Cite_blurb'), class = 'custom-text'), 
                                       hr(), 
                                       div(textOutput('BugReportingBlurb'), class = 'custom-text'))
                       
                     )
          
        )
      )
    )
    
  )
  
  
  
)



server <- function(input, output, session) {
  
  
  #### REACTIVE/STATIC FUNCTIONS #### 
  
get_legend_species <- reactive ({
  req(input$fish)
  req(input$zoops)
  req(input$region)
  
  fish_species <- input$fish %||% character(0)
  zoop_species <- input$zoops %||% character(0)
  Selected_Species <- c(fish_species, zoop_species)
  taxaSelected <- c(fish_species, zoop_species)
  to_check <- phenology_of_risk_data %>%
    filter(Region %in% input$region, Taxa %in% taxaSelected)

  # Group by Taxa and count the number of observations for each
  taxa_counts <- to_check %>%
    group_by(Taxa) %>%
    count() %>%
    arrange(desc(n))  # Sort in descending order of count
  species_legend <- taxa_counts$Taxa[1] 

  #Further Check of Regions
  species_legend_regions <- to_check %>%
    filter(Taxa == species_legend) %>%
    pull(Region) %>%
    unique()

  for (taxa in c(input$fish, input$zoops)) {
    regions_to_add <- c()
    if (taxa != species_legend) {
      check_regions <- to_check %>%
        filter(Taxa == taxa) %>%
        pull(Region) %>%
        unique()
      for (region in unique(to_check$Region)) {
        if (!(region %in% species_legend_regions) && !(region %in% regions_to_add)) {
          regions_to_add <- c(region, regions_to_add)
        }
      }
    }
  }
  return (c(species_legend, regions_to_add))
})

    ### Function that takes in length of plots and decides how tall the plot display should be. Ensures no stretch, but allows for closer inspection. ###
get_plot_height <- function(POR = FALSE){
  if (POR){
    num_plots <- length(make_phenologyOR_plots())
    if (num_plots <= 3) {
      return(600)
    }
    else {
      return(1200)
    }
  }
  else {
    num_plots <- length(make_1year_risk_plot())
    if (num_plots <= 4) {
      return(600)
    }
    else {
      return(1200)
    }
  }
}


### Reactive function to filter the station metadata table using inputs given by user. ###
    #This table populates the map.

FetchStationSummaryData <- reactive({
  req(input$region)
  filtered_data <- StationSummaryData
  filtered_data <- filtered_data |> dplyr::filter(Region %in% input$region)                                               # Filter by region
  # Filter based on group input (fish or zooplankton)
  if (input$fish_stations && input$zooplankton_stations) {                                                                                  # Both fish and zooplankton are selected, no further filtering needed
    return(filtered_data)
  } else if (input$fish_stations) {                                                                                                # Only fish selected
    filtered_data <- filtered_data |> dplyr::filter(Group == 'Fish')
  } else if (input$zooplankton_stations) {                                                                                         # Only Zooplankton Selected
    filtered_data <- filtered_data |> dplyr::filter(Group == 'Zooplankton')
  } else {                                                                                                                # Neither fish nor zooplankton selected, return an empty data frame
    return(filtered_data[0, ])
  }
  return(filtered_data)
})

FetchDataSummaryTable <- reactive({
  req(input$fish, input$zoops, input$region)
  
  fish_species <- input$fish %||% character(0)
  zoop_species <- input$zoop %||% character(0)
  species <- c(input$fish, input$zoops)
  species_pattern <- paste(species, collapse = "|")
  
  filtered_type_data <- FetchStationSummaryData()
  filtered_species <- filtered_type_data %>% filter(
    Region %in% input$region,
    str_detect(Taxa, species_pattern)
    
  )
})





  #### PLOTS #####

#### Phenology of Risk ###
      #Input: TaxaInput (list), RegionInput (list)  
      #Ouptput: Phenology of Risk Plots in a grid
  
make_phenologyOR_plots <- reactive ({
  req(input$region)
  req(input$fish)
  req(input$SliderMonths)
  plots_phenology_of_risk <- list()
  
  fish_species <- input$fish %||% character(0)
  taxaSelected <- fish_species
  
  if (length(taxaSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))                           #Return Blank Plot if no taxa selected
  }

  i <- 0
  taxa_data <- phenology_of_risk_data
  for (taxa in taxaSelected) {
    if (taxa == get_legend_species()[1]) {
      # print(get_legend_species())
      if (length(get_legend_species()) > 1) {
        for (region in get_legend_species()[2]) {
          new_row <- list(
            "X" = 1, "Month_name"=input$SliderMonths[1],
            "Region" = region, "Taxa" = taxa, "Probability" = 0,	"U" = 0,	"Q" = 0,	"Timesteps" = 1,	"Best" = 0,
            "Worst" = 0,	"Uup" = 0,	"Udown" = 0, "Month" =	1,	"Percenttot" = 0,	"Window" = "Off",	"MonthName" = months_shortened[input$SliderMonths[1]])
          taxa_data <- rbind(taxa_data, new_row)
        } 
      }

    }
    taxa_region_spec_data <- taxa_data %>% filter(Taxa == taxa, Region %in% input$region)
      if (length(taxa_region_spec_data$Region) < 1) {
        next
      }
    
    
    ### Filter for month ###
    taxa_region_spec_data <- taxa_region_spec_data %>% filter(Month_name %in% months[which(months == input$SliderMonths[1]) : which(months == input$SliderMonths[2])])

    
    plot <- taxa_region_spec_data %>% ggplot()+
      geom_line(aes(x=MonthName, y=Probability, color=Region, group=interaction(Taxa, Region), alpha=Window), linewidth=1.5)+
      geom_point(aes(x=MonthName, y=Probability, color=Region, group=interaction(Taxa, Region), alpha=Window, size=Percenttot))+
      scale_alpha_manual(values = alphas) +
      guides(scale = TRUE, alpha = FALSE) +
      labs(subtitle = taxa, title = SpeciesNamesSciNorm[taxa])+
      theme_classic() +ylab("Critical Decline Risk (%)") +xlab("Month") +ylim(0,100)+
      scale_x_discrete(guide = guide_axis(angle = 50))+
      theme(text = element_text(size = 20),
            title = element_text(size = 20))+
      scale_color_manual(values = c(COLORS), name = ~Region)+
      scale_size_continuous(name = "Percent of Annual Catch")
    
    if ((taxa != get_legend_species()[1])) {
      plot <- plot + guides(color = "none", shape = "none", size = "none", linetype = "none") 
    }
    if (i >= 1) {
      plot <- plot + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    }
    i <- i + 1
  
    plots_phenology_of_risk[[taxa]] <- plot
  }
  
  
  patchwork <- wrap_plots(plots_phenology_of_risk, ncol = 3, guides = 'collect')+
  plot_annotation(title = "Phenology of Risk Plots", theme = theme(plot.title = element_text(size = 35)))&
  theme(legend.position = "right")
  return (patchwork)
})
  
  
#### Single Year Risk Prediction plots ###
      #Input: taxaInputs, regionInputs
      #Output: patchwork of plots.


make_1year_risk_plot <- reactive ({
  req(input$fish)
  req(input$zoops)
  req(input$region)
  plots <- list()

  fish_species <- input$fish %||% character(0)
  zoop_species <- input$zoops %||% character(0)
  taxaSelected <- c(fish_species, zoop_species)

  if (length(taxaSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot If NO TAXA SELECTED
  }

  regionSelected <- input$region
  if (length(regionSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot If NO REGION SELECTED
  }
  i <- 0
  taxa_chosen_data <- FishZoopComparison_data %>%
    filter(Taxa %in% taxaSelected)
  for (taxa in taxaSelected) {                                                                            #Begin iteration of taxa to make all plots for taxa
    taxa_spec_data <- taxa_chosen_data %>%
      filter(Predator == SpeciesNamesSciNorm[taxa],
             Region %in% regionSelected)

    if (length(taxa_spec_data$Region) == 0) {
      next
    }
                                                                                                          #Relevel/Refactor Plots
    taxa_spec_data$Region<-factor(taxa_spec_data$Region, levels = regionSelected)
    taxa_spec_data$Taxa<-as.factor(taxa_spec_data$Taxa)
    taxa_spec_data$Taxa<-relevel(taxa_spec_data$Taxa, taxa)

    Oneyrplotdata <- taxa_spec_data
                                    ### NEED TO CHANGE THE NAMES SO THE AXIS SHOW FULL NAME
    Oneyrplotdata <- Oneyrplotdata %>% mutate(Taxa = recode(Taxa, "Spirinchus thaleichthys" = "S. Thaleichthys", "Engraulis mordax" = "E. Mordax", "Atherinopsis californiensis" = "A. Californiensis",  "Clupea pallasii" = "C. Pallasii", "Alosa sapidissima" = "A. sapidissima",  "Morone saxatilis" = "M. saxatilis", "Dorosoma petenense" = "D. petenense")
)

    j <- 0

    for (region in unique(taxa_spec_data$Region)) {                                                   #Iterate over regions to make individual species region pair plots.
      taxa_region_spec_data <- Oneyrplotdata %>% filter(Region == region)
      if (length(taxa_region_spec_data$Region) < 1) {
        next
      }


      ###Filter by year in the future
      taxa_region_spec_data <- taxa_region_spec_data %>% filter(Timesteps == input$SliderYFP1)


      plot <- taxa_region_spec_data%>% ggplot(aes(x=Taxa, y=Probability))+
        geom_point(aes(color=Region), size=3, position = pd)+
        geom_linerange(aes(ymin=Best, ymax=Worst, group=Region, color=Region, linetype=Group), linewidth=1, position = pd)+
        ylab("Key Window Probability")+
        theme_classic()+
        scale_x_discrete(guide = guide_axis(angle = 47))+
        ylim(0,100)+
        labs(title = paste0(SpeciesNamesSciNorm[taxa], ', ', region), subtitle = taxa)+
        ylab("Critical Decline Risk (%)")+
        xlab("Fish and Potential Zooplankton Prey")+
        scale_color_manual(values = COLORS)+
        scale_linetype_manual(values = LINES2,  'Group') +
        theme(text = element_text(size = 15), title = element_text(size = 14))+
        geom_vline(xintercept = 1.5, linetype="dotted", color = "black", linewidth=1.5)

      if (i >= 0 ) {
        plot <- plot + theme(axis.title.x=element_blank(), axis.title.y=element_blank())                  #Ensuring that only the first plot will have axis labels.
      }

      # if (!(taxa %in% get_legend_species()) && j >= 1) {
      #   plot <- plot + guides(color = "none", shape = "none", size = "none", linetype = "none")                  #Making the legend to include all the necessary information without repeating. Refer to get_legend_species function line ~315
      # }
      j <- j + 1
      i <- i + 1

      plots[[paste0(SpeciesNamesSciNorm[taxa], ', ', region)]] <- plot
    }
  }
  # patchwork <- wrap_plots(plots, ncol = 4, guides = 'collect')+
  # plot_annotation(title = "1 Year Predicted Risk Plots", theme = theme(plot.title = element_text(size = 35)))&
  # theme(legend.position = 'right')
  # return (patchwork)
  # 
  patchwork <- wrap_plots(plots, ncol = 4, guides = "collect") +
    plot_annotation(
      title = "1 Year Predicted Risk Plots",
      theme = theme(plot.title = element_text(size = 35))
    ) &
    theme(legend.position = "right")
  
  return(patchwork)
  
})


  

### 10 Year Prediction Risk Plots ###
        #Input: Taxas Selected(list), Regions Selected (list)
        #Output: All plots in a grid to show the Region Taxa Pairing graphs for
        #This plot displays a 16 plot grid which can be filtered by species, region, or years from present

make_10year_prediction_plot <- reactive( {
  
  req(input$fish)
  req(input$zoops)
  req(input$region)
  req(input$SliderYFP)
  
  fish_species <- input$fish %||% character(0)
  zoop_species <- input$zoops %||% character(0)
  Selected_Species <- c(fish_species, zoop_species)
  taxaSelected <- c(fish_species, zoop_species)
  
  if (length(taxaSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot if No INPUT SPECIES
  }
  
  regionSelected <- input$region
  if (length(regionSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot if No INPUT REGION
  }
  
  plots <- list()
  i <- 0
  for (taxa in taxaSelected) {                                                                                                  #TAKING THE MEAN OF ALL ZOOP TO MAKE A COMPARABLE LINE PLOT
    taxa_spec_data <-Predict10yrData %>% filter(Predator== SpeciesNamesSciNorm[taxa]) %>% filter(Region %in% regionSelected)
    Predict10yrData$Region <- factor(Predict10yrData$Region, levels = c("Delta", "Confluence" , "Suisun Bay", "San Pablo Bay"))
  
    taxa_spec_data<-taxa_spec_data %>%
      group_by(Region, Timesteps, Group) %>% unique() %>%
      mutate(Meanprob=mean(Probability))%>% 
      mutate(Meanbest=mean(Best))%>%
      mutate(Meanworst=mean(Worst))
    
    for (region in unique(taxa_spec_data$Region)) {
      taxa_region_spec_data <- taxa_spec_data %>% filter(Region == region)
      if (length(taxa_region_spec_data$Region) < 1) {
        next
      }
                                                                                             
      
      
      
      
                                                 #### Add slider input for time from present. Bi-slider so you can focus in on important ranges.
      
      taxa_region_spec_data <- taxa_region_spec_data %>% filter(Timesteps %in% seq(1, input$SliderYFP))
      if (length(unique(taxa_region_spec_data$Timesteps)) == 1) {
        
        taxa_region_spec_data$Taxa<-as.factor(taxa_region_spec_data$Taxa)
        taxa_region_spec_data$Taxa<-relevel(taxa_region_spec_data$Taxa, taxa)
        
                                                ### NEED TO CHANGE THE NAMES SO THE AXIS SHOW FULL ITEM
        taxa_region_spec_data <- taxa_region_spec_data %>% mutate(Taxa = recode(Taxa, "Spirinchus thaleichthys" = "S. Thaleichthys", "Engraulis mordax" = "E. Mordax", "Atherinopsis californiensis" = "A. Californiensis",  "Clupea pallasii" = "C. Pallasii", "Alosa sapidissima" = "A. sapidissima",  "Morone saxatilis" = "M. saxatilis", "Dorosoma petenense" = "D. petenense"))                                                 

        plot <- taxa_region_spec_data%>% ggplot(aes(x=Taxa, y=Probability))+
          geom_point(aes(color=Region), size=3, position = pd)+
          geom_linerange(aes(ymin=Best, ymax=Worst, group=Region, color=Region, linetype=Group), linewidth=1, position = pd)+
          ylab("Key Window Probability")+
          theme_classic()+
          scale_x_discrete(guide = guide_axis(angle = 47))+
          ylim(0,100)+
          labs(title = paste0(SpeciesNamesSciNorm[taxa], ', ', region), subtitle = taxa)+
          ylab("Critical Decline Risk (%)")+
          xlab("Fish and Potential Zooplankton Prey")+
          scale_color_manual(values = COLORS)+
          scale_linetype_manual(values = LINES2,  'Group') +
          theme(text = element_text(size = 15), title = element_text(size = 14))+
          geom_vline(xintercept = 1.5, linetype="dotted", color = "black", linewidth=1.5)

        
        
        
      } else{
        plot <- taxa_region_spec_data %>% ggplot()+
          geom_line(aes(x=Timesteps, y=Meanprob, color=Region, linetype=Group), linewidth=2)+
          geom_ribbon(aes(x=Timesteps, ymin=Meanbest, ymax=Meanworst, fill=Region, group=(interaction(Group, Region))), alpha=0.05, size=2)+
          theme_classic() +ylim(0,100) + scale_x_continuous(breaks=seq(1,10,1))+
          theme(text = element_text(size = 20), title = element_text(size = 14))+
          scale_color_manual(values = COLORS)+
          scale_fill_manual(values = COLORS)+
          ylab("Critical Decline Risk (%)") +
          xlab("Years From Present")+
          scale_linetype_manual(values = LINES2, name = 'Group') + 
          labs(title = paste0(SpeciesNamesSciNorm[taxa], ', ', region), subtitle = taxa)
      }

      
         
      if (i >= 1) {
        plot <- plot + theme(axis.title.x = element_blank(), axis.title.y = element_blank())                #Ensuring that only the first plot will have axis labels.
      }
    
    
      plots[[paste0(SpeciesNamesSciNorm[taxa], ', ', region)]] <- plot
    }
  }
  patchwork <- wrap_plots(plots, ncol = 4, guides = "collect") +
    plot_annotation(
      title = "10 Year Predicted Risk Plots",
      theme = theme(plot.title = element_text(size = 35))
    ) &
    theme(legend.position = "right")
  
  return (patchwork)
})

  
  
#### BEFORE ANY ACTIVITY ON THE APP, DISPLAY THE BELOW #### 
  # # Initialize reactive values for zoom and center
  map_state <- reactiveValues(
    zoom = 8,  # Default zoom level
    center = list(lat = 37.48549685534591163, lng = -122.1670034591194991)  # Default center
  )
  

  
####  UPON ACTIVITY ON THE APP #### 
  
  observeEvent(
    c(input$region, input$fish_stations, input$zooplankton_stations, input$Tabs, input$fish,
      input$zoops), {                               #Upon clicking the sidebar, MANIPULATE UI
    
      
      # Update zoom and center from input
      if (!is.null(input$DeltaMap_zoom)) {
        map_state$zoom <- input$DeltaMap_zoom  # Update zoom
      }
      if (!is.null(input$DeltaMap_center)) {
        map_state$center <- input$DeltaMap_center  # Update center as list
      }
      
      
      ### Dynamic Rendition of the Map and Its static blurb###
      output$DeltaMap <- renderLeaflet({
        # Fetch data
        summary_data <- FetchStationSummaryData()
        
        # Create base map with tiles
        map <- leaflet::leaflet() %>%
          addProviderTiles("Esri.WorldStreetMap") %>%
          setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom)
        
        # Check if data is not empty

        if (nrow(summary_data) > 0) {
          # Add circle markers if there is data
          map <- map %>%
            addCircleMarkers(data = summary_data,
              lng = ~Longitude,
              lat = ~Latitude,
              popup = ~paste(Group, " Station ", Station, "<br>", 
                             "Region: ", Region, "<br>",
                             "Observed: ", Taxa), 
              labelOptions = labelOptions(textsize = "18px"),
              color = ~ifelse(Group == "Fish", "black", "maroon"),
              radius = 7,
              fillOpacity = 1,
              stroke = FALSE
            ) %>%
            addLegend(
              position = "bottomright", 
              colors = c("black", "maroon"), 
              labels = c("Fish Stations", "Zooplankton Stations"), 
              opacity = 1 
            )
        }

        return(map)
      })
      
      
    
      

      
  
      
      #### Phenology of Risk plot and blurb ###
      output$PORplot <- renderPlot({
        make_phenologyOR_plots()
      }, width = 1200, height = get_plot_height(POR = TRUE))      #1200
      
      output$POR_blurb <- renderText(POR_blurb)
      
      
      #### Single Year Critical Decline Probability plot and blurb ###
      output$SingleYearPlot <- renderPlot({
        make_1year_risk_plot()
      }, width = 1200, height = get_plot_height())  
      
      output$SYP_blurb <- renderText(SingleYearPredicitons_blurb)
      
      
      #### Ten Year Prediction of Critical Decline Probability plot and blurb ###
      output$TenYearPlot <- renderPlot({
        make_10year_prediction_plot()
      }, width = 1200, height = get_plot_height())
      
      output$TYP_blurb <- renderText(TenYearPredictions_blurb)  
      
      
      #### Station Meta Data and blurb ###
      output$Station_data <- renderDT({
          
        datatable(FetchDataSummaryTable(), 
          selection = 'single', 
          options = list(
          scrollY = "400px", 
          scrollCollapse = TRUE, 
          paging = FALSE,
          scrollX = TRUE  
        ))
      })                             #Dynamic Table for Metadata
      output$StationData_blurb <- renderText(StationData_Blurb)
      
      # Update zoom and center from input
      if (!is.null(input$StationMap_zoom)) {
        map_state$zoom <- input$StationMap_zoom  # Update zoom
      }
      if (!is.null(input$StationMap_center)) {
        map_state$center <- input$StationMap_center  # Update center as list
      }
      
      # --- MAP RENDER ---
      output$StationMap <- renderLeaflet({
        df <- FetchDataSummaryTable()
        leaflet(df) %>%
          addProviderTiles("Esri.WorldStreetMap") %>%
          addCircleMarkers(
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~Station,  # Key connection
            popup = ~paste0(
              "<b>", Group, " Station ", Station, "</b><br>",
              "Region: ", Region, "<br>",
              "Observed: ", Taxa
            ),
            label = ~paste(Station),
            labelOptions = labelOptions(textsize = "18px"),
            color = ~ifelse(Group == "Fish", "black", "maroon"),
            radius = 7,
            fillOpacity = 1,
            stroke = FALSE
          ) %>%
          addLegend(
            position = "bottomright",
            colors = c("black", "maroon"),
            labels = c("Fish Stations", "Zooplankton Stations"),
            opacity = 1
          )
      })
      
      # --- 1️⃣ TABLE → MAP ---
      observeEvent(input$Station_data_rows_selected, {
        req(input$Station_data_rows_selected)
        
        df <- FetchDataSummaryTable()
        sel_idx <- input$Station_data_rows_selected
        if (length(sel_idx) == 0) return()
        
        # Get the Station name for the selected row
        selected_station <- df$Station[sel_idx]
        selected_row <- df[df$Station == selected_station, ]
        
        leafletProxy("StationMap") %>%
          clearPopups() %>%
          addPopups(
            lng = selected_row$Longitude,
            lat = selected_row$Latitude,
            popup = paste0(
              "<b>", selected_row$Group, " Station ", selected_row$Station, "</b><br>",
              "Region: ", selected_row$Region, "<br>",
              "Observed: ", selected_row$Taxa
            )
          ) %>%
          setView(lng = selected_row$Longitude, lat = selected_row$Latitude, zoom = 10)
      })
      
      # --- 2️⃣ MAP → TABLE ---
      observeEvent(input$StationMap_marker_click, {
        click <- input$StationMap_marker_click
        req(click$id)
        
        df <- FetchDataSummaryTable()
        
        # Find the row that matches the marker ID
        idx <- which(df$Station == click$id)
        if (length(idx) != 1) return()
        
        proxy <- dataTableProxy("Station_data")
        selectRows(proxy, idx)
        
        # Optional: move to the page that contains that row
        n_per_page <- 10  # change if you display a different number per page
        page_num <- ceiling(idx / n_per_page)
        selectPage(proxy, page_num)
      })
      
      
      
      
      
      
      
      #### Additional Information blurbs + links ###
      output$Map_blurb <- renderText(Map_blurb)
      output$ManuscriptBlurb <- renderUI({
        HTML(Manuscript_blurb)
      })
      output$LinktoDataBlurb <- renderUI({
        HTML(Data_resource_blurb)
      })
      output$Cite_blurb <- renderText(Cite_blurb)
      output$BugReportingBlurb <- renderText(Bug_reporting_blurb)
      
      
    }
  ) 
  
}
  

#Call the shiny app
shinyApp(ui, server)



