############## REACTIVE VALUES ##############
# initialize reactive values for the buttons
clicks_locations <- reactiveValues(data = NULL)
clicks_drivetime <- reactiveValues(data = NULL)
last_city <- reactiveValues(data = NULL)
clicks_cluster <- reactiveValues(data = NULL)
############## EVENT OBSERVER ##############
# observe the location button
observeEvent(input$bttn_data_locations, {
  # set of the location button to clicked
  clicks_locations$data <- "clicked"
  # set the value of the drivetime button to NULL
  clicks_drivetime$data <- NULL
})
observeEvent(input$eps_locations, {
  clicks_distance$data <- "clicked"
  clicks_minpts$data <- NULL
})
observeEvent(input$minpts_locations, {
  clicks_minpts$data <- "clicked"
  clicks_distance$data <- NULL
  last_city$data[input$bttn_data_locations] <- input$location
})
observeEvent(input$cluster_start, {
  clicks_cluster$data <- "clicked"
})
# observe the drivetime button
observeEvent(input$bttn_data_drivetime, {
  # set of the drivetime button to clicked
  clicks_drivetime$data <- "clicked"
  # set the location button to NULL
  clicks_locations$data <- NULL
})

observeEvent(input$radio_type, {
  updateTextInput(session, "location", value = "")
  updateTextInput(session, "latitude",
    value = "",
    placeholder = 48.133326
  )
  updateTextInput(session, "longitude",
    value = "",
    placeholder = 11.522423
  )
  updateTextInput(session, "address_data",
    value = "",
    placeholder = "Street Number, ZIP City"
  )
})

observeEvent(input$radio_type_cluster, {
  updateTextInput(session, "city_cluster", value = "")
  updateTextInput(session, "latitude_cluster",
    value = "",
    placeholder = 48.133326
  )
  updateTextInput(session, "longitude_cluster",
    value = "",
    placeholder = 11.522423
  )
  updateTextInput(session, "address_cluster",
    value = "",
    placeholder = "Street Number, ZIP City"
  )
})
