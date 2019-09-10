############## DOWNLOAD BUTTONS ##############
# download button for drivetime location
output$download_locations <- downloadHandler(
  # set file name
  filename = function() {
    paste(input$location, ".xlsx", sep = "")
  },
  # get content
  content = function(fname) {
    # get the data
    dataset <- get_location_data()
    # check if only relevant features should be downloaded
    if (input$switch_data) {
      # check if each column consists of less than 5% data
      limit <- 0.05 * nrow(dataset)
      few_values <- sapply(
        dataset,
        function(x, limit) {
          sum(!is.na(x)) < limit
        },
        limit
      )
      # remove all columns with less than 5% or >955 NA values
      dataset <- dataset[, !few_values]
    }
    # remove geometry
    dataset$geometry <- NULL
    # save the file
    openxlsx::write.xlsx(x = dataset, file = fname, row.names = FALSE)
  }
)
# same as above
output$download_drivetime <- downloadHandler(
  filename = function() {
    paste("drivetime.xlsx", sep = "")
  },
  content = function(fname) {
    dataset <- get_isochrone_data()
    dataset <- dataset$points
    if (input$switch_data2) {
      limit <- 0.05 * nrow(dataset)
      few_values <- sapply(
        dataset,
        function(x, limit) {
          sum(!is.na(x)) < limit
        },
        limit
      )
      dataset <- dataset[, !few_values]
    }
    # remove geometry
    dataset$geometry <- NULL
    # save the file
    openxlsx::write.xlsx(x = dataset, file = fname, row.names = FALSE)
  }
)
############## OSM TEXT ##############
url_osm <- a("OSM Wiki.",
  href = "https://wiki.openstreetmap.org/wiki/Map_features"
)
osm <- "Data downloaded from the OpenStreetMap API.
For more informations about the API, have a look at the "


############## PICKER INPUTS  ##############
# update the input for keys
updatePickerInput(
  session,
  "key_locations",
  choices = c(
    "aeroway", "amenity", "building", "craft", "emergency", "historic",
    "landuse", "leisure", "man_made", "office", "place", "public_transport",
    "railway", "shop", "sport", "tourism"
  )
)
# update the input for keys
updatePickerInput(
  session,
  "key_drivetime",
  choices = c(
    "aeroway", "amenity", "building", "craft", "emergency", "historic",
    "landuse", "leisure", "man_made", "office", "place", "public_transport",
    "railway", "shop", "sport", "tourism"
  )
)
# update the input for keys
updatePickerInput(
  session,
  "key_cluster",
  choices = c(
    "aeroway", "amenity", "building", "craft", "emergency", "historic",
    "landuse", "leisure", "man_made", "office", "place", "public_transport",
    "railway", "shop", "sport", "tourism"
  )
)

# update the input for drivetime values
updatePickerInput(
  session, "drivetime",
  choices = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60)
)

updatePickerInput(
  session, "drivetime_cluster",
  choices = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60)
)

updatePickerInput(
  session,
  "palette",
  choices = list(
    "Brewer Sequential" = c(
      "Reds", "Greens", "Blues", "Greys", "Oranges", "Purples",
      "BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd",
      "RdPu", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
    ),
    "Viridis sequential" = c(
      "viridis", "magma", "plasma", "inferno", "cividis"
    ),
    "Custom palettes" = c(
      "pal1", "pal2", "pal3", "pal4"
    )
  ),
  selected = "PuRd"
)
