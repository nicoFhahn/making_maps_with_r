# error message if data gathering fails for locations
error_locations <- eventReactive(input$bttn_data_locations, {
  # get the location data
  data <- get_location_data()
  # check if multiple cities have been entered
  if (is.null(data)) {
    if (!is.null(input$switch_cities)) {
      # get the city names
      cities <- trimws(unlist(
        stringr::str_split(input$location, "(\\,|\\;)")
      ))
    } else {
      cities <- input$location
    }
    return(paste("Unable to download data for", cities))
  }
  if (!is.null(input$switch_cities)) {
    if (input$switch_cities) {
      # get the city names
      cities <- trimws(unlist(
        stringr::str_split(input$location, "(\\,|\\;)")
      ))
      # match entered city names with city names in data
      no_data <- paste(cities[!cities %in% data$city], collapse = ", ")
    }
    else if (nrow(data) == 1) {
      # if no data was found return the location name
      if (class(data)[1] != "sf") {
        if (data[, 3] == "NODATA") {
          no_data <- input$location
        } else {
          # if data was found use an empty vector
          no_data <- c()
        }
      } else {
        no_data <- c()
      }
    } else {
      no_data <- c()
    }
  } else if (nrow(data) == 1) {
    # if no data was found return the location name
    if (class(data)[1] != "sf") {
      if (data[, 3] == "NODATA") {
        no_data <- input$location
      } else {
        # if data was found use an empty vector
        no_data <- c()
      }
    } else {
      no_data <- c()
    }
  } else if (nrow(data) == 0) {
    no_data <- input$location
  } else {
    no_data <- c()
  }
  if (length(no_data) > 0 && no_data[1] != "") {
    # return error message with all places for which no data was found
    paste("Unable to download data for", no_data)
  }
})
# error message if data gathering fails for locations
error_drivetime <- eventReactive(input$bttn_data_drivetime, {
  # get the location data
  data <- get_isochrone_data()
  if (class(data) == "character") {
    data
  } else if (class(data) == "data.frame") {
    if (data[1, 3] == "NODATA") {
      "Unable to download data for the location"
    }
  }
})

error_cluster <- eventReactive(input$cluster_start, {
  # get the location data
  data <- get_cluster_data()
  if (input$radio_type_cluster == "Drivetime") {
    if (class(data) != "data.frame") {
      data <- data$data
    }
  }
  # check if multiple cities have been entered
  if (!is.null(input$switch_cities_cluster)) {
    if (input$switch_cities_cluster) {
      # get the city names
      cities <- trimws(unlist(
        stringr::str_split(input$city_cluster, "(\\,|\\;)")
      ))
      # match entered city names with city names in data
      no_data <- paste(cities[!cities %in% data$city], collapse = ", ")
    } else if (nrow(data) == 1) {
      # if no data was found return the location name
      if (ncol(data) == 3) {
        if (data[, 3] == "NODATA") {
          if (input$city_cluster != "" &&
            input$radio_type_cluster != "Drivetime") {
            no_data <- input$city_cluster
          } else {
            no_data <- "the location"
          }
        }
      } else {
        # if data was found use an empty vector
        no_data <- c()
      }
    } else {
      no_data <- c()
    }
  } else if (nrow(data) == 1) {
    # if no data was found return the location name
    if (ncol(data) == 3) {
      if (data[, 3] == "NODATA") {
        if (input$city_cluster != "" &&
          input$radio_type_cluster != "Drivetime") {
          no_data <- input$city_cluster
        } else {
          no_data <- "the location"
        }
      }
    } else {
      # if data was found use an empty vector
      no_data <- c()
    }
  } else {
    no_data <- ""
  }
  if (length(no_data) > 0 && no_data[1] != "") {
    # return error message with all places for which no data was found
    paste("Unable to download data for", no_data)
  }
})

############## TEXT RENDERING ##############
# error text if something fails
output$text_error <- renderText({
  # get the error message
  text <- error_locations()
  text
})
# error text if something fails
output$text_error_drivetime <- renderText({
  # get the error message
  text <- error_drivetime()
  text
})

output$text_error_cluster <- renderText({
  # get the error message
  text <- error_cluster()
  text
})
