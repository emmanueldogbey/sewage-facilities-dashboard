choices = c("Ahafo" = "Ahafo Region",
            "Ashanti" = "Ashanti Region",
            "Bono East" = "Bono East Region", 
            "Bono" = "Bono Region",
            "Central" = "Central Region",
            "Eastern" = "Eastern Region",
            "Greater Accra" = "Greater Accra Region",
            "North East" = "North East Region",
            "Northern" = "Northern Region",
            "Oti" = "Oti Region",
            "Savannah" = "Savannah Region",
            "Upper East" = "Upper East Region",
            "Upper West" = "Upper West Region",
            "Volta" = "Volta Region",
            "Western North" = "Western North Region",
            "Western" = "Western Region"
)

# helper function for map plot
plot_sewage_map = function(boundaries, facilities = NULL){
  map <- leaflet(boundaries) |> 
    addTiles() |> 
    addPolygons(color="#000", weight=1, fill=NA)
  
  if (!is.null(facilities)) {
    map <- map |> 
      addMarkers(data = facilities, label=~as.character(location),
                 labelOptions = labelOptions(noHide = T)) 
  }
  
  return(map)
}





              