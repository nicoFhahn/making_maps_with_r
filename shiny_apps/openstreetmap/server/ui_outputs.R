############## UI OUTPUTS  ##############
# pretty self-explanatory:
# match the values to the key entered
output$ui_value_locations <- renderUI({
  if (input$key_locations == "amenity") {
    pickerInput(
      inputId = "value", label = "Select values:",
      width = "100%",
      choices = list(
        Sustenance = keys$amenity[1:10],
        Education = keys$amenity[11:21],
        Transportation = keys$amenity[22:42],
        Financial = keys$amenity[43:45],
        Healthcare = keys$amenity[46:55],
        Entertainment = keys$amenity[56:70],
        Others = keys$amenity[71:113]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_locations == "building") {
    pickerInput(
      inputId = "value", label = "Select values:",
      width = "100%",
      choices = list(
        Accommodation = keys$building[1:12],
        Commercial = keys$building[13:19],
        Religious = keys$building[20:26],
        Civic = keys$building[27:39],
        Other = keys$building[40:66]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_locations == "emergency") {
    pickerInput(
      inputId = "value", label = "Select values:",
      width = "100%",
      choices = list(
        "Medical Rescue" = keys$emergency[1:5],
        "Fire Fighters" = keys$emergency[6:13],
        Lifeguards = keys$emergency[14:18],
        Other = keys$emergency[19:23]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_locations == "landuse") {
    pickerInput(
      inputId = "value", label = "Select values:",
      width = "100%",
      choices = list(
        "Common landuse" = keys$landuse[1:5],
        "Other" = keys$landuse[6:29]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_locations == "place") {
    pickerInput(
      inputId = "value", label = "Select values:",
      width = "100%",
      choices = list(
        "Administratively declared places" = keys$place[1:7],
        "Populated settlements, urban" = keys$place[8:14],
        "Populated settlements, urban and rural" =
          keys$place[15:20],
        Other = keys$place[21:28]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_locations == "railway") {
    pickerInput(
      inputId = "value", label = "Select values:",
      width = "100%",
      choices = list(
        "Tracks" = keys$railway[1:12],
        "Stations and stops" = keys$railway[13:17],
        Other = keys$railway[18:27]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_locations == "shop") {
    pickerInput(
      inputId = "value", label = "Select values:",
      width = "100%",
      choices = list(
        "Food, beverages" = keys[[14]][1:24],
        "General stores" = keys[[14]][25:30],
        "Clothing, shoes, accessories" = keys[[14]][31:43],
        "Health and beauty" = keys[[14]][44:60],
        "DIY, household" = keys[[14]][61:78],
        "Furniture" = keys[[14]][79:91],
        "Electronics" = keys[[14]][92:98],
        "Outdoors and sports" = keys[[14]][99:119],
        "Art, music, hobbies" = keys[[14]][120:132],
        "Stationery, gifts, books, newspapers" =
          keys[[14]][133:139],
        "others" = keys[[14]][140:160]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_locations != "") {
    pickerInput(
      inputId = "value", label = "Select values:",
      width = "100%",
      choices = keys[[input$key_locations]],
      options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  }
})
# pretty self-explanatory:
# match the values to the key entered
output$ui_value_drivetime <- renderUI({
  if (input$key_drivetime == "amenity") {
    pickerInput(
      inputId = "value_drivetime", label = "Select values:",
      width = "100%",
      choices = list(
        Sustenance = keys$amenity[1:10],
        Education = keys$amenity[11:21],
        Transportation = keys$amenity[22:42],
        Financial = keys$amenity[43:45],
        Healthcare = keys$amenity[46:55],
        Entertainment = keys$amenity[56:70],
        Others = keys$amenity[71:113]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_drivetime == "building") {
    pickerInput(
      inputId = "value_drivetime", label = "Select values:",
      width = "100%",
      choices = list(
        Accommodation = keys$building[1:12],
        Commercial = keys$building[13:19],
        Religious = keys$building[20:26],
        Civic = keys$building[27:39],
        Other = keys$building[40:66]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_drivetime == "emergency") {
    pickerInput(
      inputId = "value_drivetime", label = "Select values:",
      width = "100%",
      choices = list(
        "Medical Rescue" = keys$emergency[1:5],
        "Fire Fighters" = keys$emergency[6:13],
        Lifeguards = keys$emergency[14:18],
        Other = keys$emergency[19:23]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_drivetime == "landuse") {
    pickerInput(
      inputId = "value_drivetime", label = "Select values:",
      width = "100%",
      choices = list(
        "Common landuse" = keys$landuse[1:5],
        "Other" = keys$landuse[6:29]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_drivetime == "place") {
    pickerInput(
      inputId = "value_drivetime", label = "Select values:",
      width = "100%",
      choices = list(
        "Administratively declared places" = keys$place[1:7],
        "Populated settlements, urban" = keys$place[8:14],
        "Populated settlements, urban and rural" =
          keys$place[15:20],
        Other = keys$place[21:28]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_drivetime == "railway") {
    pickerInput(
      inputId = "value_drivetime", label = "Select values:",
      width = "100%",
      choices = list(
        "Tracks" = keys$railway[1:12],
        "Stations and stops" = keys$railway[13:17],
        Other = keys$railway[18:27]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_drivetime == "shop") {
    pickerInput(
      inputId = "value_drivetime", label = "Select values:",
      width = "100%",
      choices = list(
        "Food, beverages" = keys[[14]][1:24],
        "General stores" = keys[[14]][25:30],
        "Clothing, shoes, accessories" = keys[[14]][31:43],
        "Health and beauty" = keys[[14]][44:60],
        "DIY, household" = keys[[14]][61:78],
        "Furniture" = keys[[14]][79:91],
        "Electronics" = keys[[14]][92:98],
        "Outdoors and sports" = keys[[14]][99:119],
        "Art, music, hobbies" = keys[[14]][120:132],
        "Stationery, gifts, books, newspapers" =
          keys[[14]][133:139],
        "others" = keys[[14]][140:160]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_drivetime != "") {
    pickerInput(
      inputId = "value_drivetime", label = "Select values:",
      width = "100%",
      choices = keys[[input$key_drivetime]],
      options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  }
})

output$ui_value_cluster <- renderUI({
  if (input$key_cluster == "amenity") {
    pickerInput(
      inputId = "value_cluster", label = "Select values:",
      width = "100%",
      choices = list(
        Sustenance = keys$amenity[1:10],
        Education = keys$amenity[11:21],
        Transportation = keys$amenity[22:42],
        Financial = keys$amenity[43:45],
        Healthcare = keys$amenity[46:55],
        Entertainment = keys$amenity[56:70],
        Others = keys$amenity[71:113]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_cluster == "building") {
    pickerInput(
      inputId = "value_cluster", label = "Select values:",
      width = "100%",
      choices = list(
        Accommodation = keys$building[1:12],
        Commercial = keys$building[13:19],
        Religious = keys$building[20:26],
        Civic = keys$building[27:39],
        Other = keys$building[40:66]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_cluster == "emergency") {
    pickerInput(
      inputId = "value_cluster", label = "Select values:",
      width = "100%",
      choices = list(
        "Medical Rescue" = keys$emergency[1:5],
        "Fire Fighters" = keys$emergency[6:13],
        Lifeguards = keys$emergency[14:18],
        Other = keys$emergency[19:23]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_cluster == "landuse") {
    pickerInput(
      inputId = "value_cluster", label = "Select values:",
      width = "100%",
      choices = list(
        "Common landuse" = keys$landuse[1:5],
        "Other" = keys$landuse[6:29]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_cluster == "place") {
    pickerInput(
      inputId = "value_cluster", label = "Select values:",
      width = "100%",
      choices = list(
        "Administratively declared places" = keys$place[1:7],
        "Populated settlements, urban" = keys$place[8:14],
        "Populated settlements, urban and rural" =
          keys$place[15:20],
        Other = keys$place[21:28]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_cluster == "railway") {
    pickerInput(
      inputId = "value_cluster", label = "Select values:",
      width = "100%",
      choices = list(
        "Tracks" = keys$railway[1:12],
        "Stations and stops" = keys$railway[13:17],
        Other = keys$railway[18:27]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_cluster == "shop") {
    pickerInput(
      inputId = "value_cluster", label = "Select values:",
      width = "100%",
      choices = list(
        "Food, beverages" = keys[[14]][1:24],
        "General stores" = keys[[14]][25:30],
        "Clothing, shoes, accessories" = keys[[14]][31:43],
        "Health and beauty" = keys[[14]][44:60],
        "DIY, household" = keys[[14]][61:78],
        "Furniture" = keys[[14]][79:91],
        "Electronics" = keys[[14]][92:98],
        "Outdoors and sports" = keys[[14]][99:119],
        "Art, music, hobbies" = keys[[14]][120:132],
        "Stationery, gifts, books, newspapers" =
          keys[[14]][133:139],
        "others" = keys[[14]][140:160]
      ), options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  } else if (input$key_cluster != "") {
    pickerInput(
      inputId = "value_cluster", label = "Select values:",
      width = "100%",
      choices = keys[[input$key_cluster]],
      options = list(
        `actions-box` = TRUE,
        title = "(Optional): Please select a value:"
      ), multiple = TRUE
    )
  }
})

# Add a switch button if multiple locations have been entered
output$ui_multiple_locations <- renderUI({
  # check if any location has been entered so far
  if (input$location != "") {
    # if yes check for commas or semicolon as they may be used to seperate
    # city names
    if (stringr::str_detect(
      input$location,
      "[A-Za-z](\\,|\\;)([A-Za-z]|[[:space:]][A-Za-z])"
    )) {
      # add the switch
      prettySwitch(
        inputId = "switch_cities",
        label = "Download data for multiple locations?",
        status = "success",
        fill = TRUE,
        value = TRUE
      )
    }
  }
})
# Add a switch button if multiple locations have been entered
output$ui_multiple_locations_cluster <- renderUI({
  # check if any location has been entered so far
  if (input$city_cluster != "") {
    # if yes check for commas or semicolon as they may be used to seperate
    # city names
    if (stringr::str_detect(
      input$city_cluster,
      "[A-Za-z](\\,|\\;)([A-Za-z]|[[:space:]][A-Za-z])"
    )) {
      # add the switch
      prettySwitch(
        inputId = "switch_cities_cluster",
        label = "Do analysis for multiple locations?",
        status = "success",
        fill = TRUE,
        value = TRUE
      )
    }
  }
})
# Add a download button for the location tab
output$ui_download_locations <- renderUI({
  # check if the generate data button has already been pressed
  if (!is.null(clicks_locations$data)) {
    # get the data
    dat <- get_location_data()
    # check if the data are actual points or just a NODATA point
    if (!"NODATA" %in% dat$name) {
      downloadBttn("download_locations", style = "fill")
    }
  }
})
output$ui_dens_cluster_locations <- renderUI({
  # check if the generate data button has already been pressed
  if (!is.null(clicks_locations$data)) {
    # get the data
    dat <- get_location_data()
    # check if the data are actual points or just a NODATA point
    if (!"NODATA" %in% dat$name) {
      prettySwitch(
        inputId = "switch_dens_cluster_locations",
        label = "Start density based clustering?",
        status = "success",
        fill = TRUE
      )
    }
  }
})
# Add an error message
output$ui_error_locations <- renderUI({
  # check if data is available
  if (!is.null(clicks_locations$data)) {
    textOutput("text_error")
  }
})

# Add an error message
output$ui_error_drivetime <- renderUI({
  # check if data is available
  if (!is.null(clicks_drivetime$data)) {
    textOutput("text_error_drivetime")
  }
})

output$ui_error_cluster <- renderUI({
  if (!is.null(clicks_cluster$data)) {
    textOutput("text_error_cluster")
  }
})
# add a message at the end of the box about osm
output$OSM <- renderUI({
  tagList(
    osm,
    url_osm
  )
})
# add a switch to download relevant features only
output$ui_switch_locations <- renderUI({
  # check if data is available
  if (!is.null(clicks_locations$data)) {
    prettySwitch(
      inputId = "switch_data",
      label = "Only download relevant features?",
      status = "success",
      fill = TRUE
    )
  }
})
# add a download button for the drivetime data
output$ui_download_drivetime <- renderUI({
  # check if the generate data button has already been pressed
  if (!is.null(clicks_drivetime$data)) {
    # get the data
    data <- get_isochrone_data()
    # if data is available show the download button
    if (class(data) == "list") {
      downloadBttn("download_drivetime", style = "fill")
    }
  }
})
# add a switch to download relevant features only
output$ui_switch_drivetime <- renderUI({
  # check if data is available
  if (!is.null(clicks_drivetime$data)) {
    prettySwitch(
      inputId = "switch_data2",
      label = "Only download relevant features?",
      status = "success",
      fill = TRUE
    )
  }
})

output$ui_names_locations <- renderUI({
  if (!is.null(clicks_locations$data)) {
    data <- get_location_data()
    if (!is.null(data)) {
      if (nrow(data) >= 1) {
        if (nrow(data) == 1) {
          # if no data was found return the location name
          if (class(data)[1] != "sf") {
            if (data[, 3] == "NODATA") {
              choices <- NULL
            } else {
              # if data was found use an empty vector
              choices <- sort(unique(data$name))
              choices <- enc2native(choices)
            }
          } else {
            choices <- sort(unique(data$name))
            choices <- enc2native(choices)
          }
        } else {
          choices <- sort(unique(data$name))
          choices <- enc2native(choices)
        }
      } else {
        choices <- NULL
      }
    } else {
      choices <- NULL
    }
  } else {
    choices <- NULL
  }
  pickerInput("names_locations", "(Optional) Choose specific locations:",
    choices,
    multiple = TRUE, width = "100%",
    options = list(
      `actions-box` = TRUE
    )
  )
})

output$ui_names_drivetime <- renderUI({
  if (!is.null(clicks_drivetime$data)) {
    data <- get_isochrone_data()
    if (is.character(data)) {
      no_data <- "bla"
    } else {
      data <- data$points
      if (is.null(data)) {
        no_data <- "bla"
      }
      else if (nrow(data) == 1) {
        # if no data was found return the location name
        if (class(data)[1] != "sf") {
          if (data[, 3] == "NODATA") {
            no_data <- "bla"
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
    }
    if (length(no_data) == 0) {
      choices <- sort(unique(data$name))
      choices <- enc2native(choices)
    }
  } else {
    choices <- NULL
  }
  if (!exists("choices")) {
    choices <- NULL
  }
  pickerInput("names_drivetime", "(Optional) Choose specific locations:",
    choices,
    multiple = TRUE, width = "100%",
    options = list(
      `actions-box` = TRUE
    )
  )
})

output$ui_names_getis <- renderUI({
  if (input$cluster_map_type == "Getis-Ord") {
    if (!is.null(clicks_cluster$data)) {
      data <- get_cluster_data()
      if (input$radio_type_cluster == "Drivetime"
      && !"data.frame" %in% class(data)) {
        data <- data$data
      }
      data <- data[!duplicated(data), ]
      if (class(data)[1] == "sf") {
        choices <- sort(unique(data$name))
        choices <- enc2native(choices)
      } else {
        choices <- NULL
      }
    } else {
      choices <- NULL
    }
    pickerInput("names_cluster", "(Optional) Choose specific locations:",
      choices,
      multiple = TRUE, width = "100%",
      options = list(
        `actions-box` = TRUE
      )
    )
  }
})
