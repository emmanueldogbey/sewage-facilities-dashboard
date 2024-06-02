library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sf)
library(here)
source("helper.R")


ui = 
  dashboardPage(skin="black",
                
  dashboardHeader(title="Sewage Facilities in Ghana", titleWidth=275),
  
  dashboardSidebar(width=275, sidebarMenu(
    menuItem(# use pickerInput because it offers the option to select all
      pickerInput("regions", "Select Region", multiple=TRUE, options=list(`actions-box`=TRUE), choices=choices, selected=choices) # choices defined in helper script
    )
  )),

  dashboardBody(
    fluidRow(
      infoBoxOutput("totalFacilities"),
      infoBoxOutput("functionalFacilities"),
      fluidRow(box(leafletOutput("facilitiesMap", height="420px"), width=12)),
      fluidRow(box(title="Summary Table", DT::dataTableOutput("summaryTable"), width=12))
      
)))

server = function(input, output){
  admBoundaries = 
    st_read(here("data/ghanaAdminBoundaries.geojson")) |> 
      dplyr::filter(admin_level == 4)
    
  sewageFacilities = 
    read_csv(here("data/wastewaterFacilities.csv")) |> 
    janitor::clean_names() |> 
    st_as_sf(coords=c("y", "x"), crs=4326) |> 
    st_join(admBoundaries, join=st_intersects)
  
  
# observe function wrapper so map responds dynamically to user selections
  observe({
    selectedRegions = input$regions
    
    sewageFacilitiesFiltered = dplyr::filter(sewageFacilities, name %in% selectedRegions)
    
    nFacilities = nrow(sewageFacilitiesFiltered)
    
    nfunctionalFacilities = nrow(dplyr::filter(sewageFacilitiesFiltered, condition_of_facility == "Functional"))
    
    infoBoxtotalFacilities = renderInfoBox({
      infoBox("Total Facilities", nFacilities, icon=icon("list"), color="orange", fill=TRUE)
    })
    
    infoBoxfunctionalFacilities = renderInfoBox({
      infoBox("Functional Facilities", nfunctionalFacilities, icon=icon("check-double"), color="green", fill=TRUE)
    })
    
    # if clause to ensure the map responds to user input with the regions filter.  'is.null' is to check whether a selection has been made. If no region has been selected, then only region boundaries are plotted. Similarly, 'nrow' check is linked to the filter by region action (defined above and available on the dashboard sidebar). If selected region has no data i.e nrow=0, plot only region boundaries. Also ensure the infobox responds appropriately to user selection
    
    if (is.null(selectedRegions) | nrow(sewageFacilitiesFiltered) == 0){
      output$totalFacilities = infoBoxtotalFacilities
      output$functionalFacilities = infoBoxfunctionalFacilities
      mapData = NULL
    } 
    else {
      output$totalFacilities = infoBoxtotalFacilities
      output$functionalFacilities = infoBoxfunctionalFacilities
      mapData = sewageFacilitiesFiltered   
    }
    
    output$facilitiesMap = renderLeaflet({
      plot_sewage_map(admBoundaries, mapData)
    }) #plot_sewage_map function defined in helper script
    
    output$summaryTable = DT::renderDataTable(DT::datatable({
      sewageFacilitiesFiltered |> 
      st_drop_geometry() |> 
      dplyr::select(location:condition_of_facility, name) |> 
      rename(Location=location, `Facility Type`=type_of_facility, `Facility Condition`=condition_of_facility, Region=name)
    }))
  })
}

shinyApp(ui=ui, server=server)


  # p("Ministry of Sanitation and Water Resources", align="center"),
  # p(img(src="MSWRLogo.png", height=70, width=70), align="center"),
  # p("Environmental Health and Sanitation Directorate", align="center"),
  # p("Liquid Waste Management Unit", align="center"),