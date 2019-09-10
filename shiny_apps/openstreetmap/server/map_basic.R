############## MAP ##############
output$mymap <- renderLeaflet({
  # check if the input
  if (input$goal == "Download <br> geospatial data") {
    if (input$radio_type == "Cities/Districts") {
      map <- leaflet() %>%
        addProviderTiles("OpenStreetMap",
          group = "OpenStreetMap"
        ) %>%
        addProviderTiles("Esri.WorldImagery",
          group = "Esri.WorldImagery"
        ) %>%
        addProviderTiles("CartoDB.Positron",
          group = "CartoDB.Positron"
        ) %>%
        addProviderTiles("CartoDB.DarkMatter",
          group = "CartoDB.DarkMatter"
        ) %>%
        addLayersControl(names(tiles), position = c("topleft")) %>%
        addFullscreenControl() %>%
        leaflet.extras::addSearchOSM(
          options = searchOptions(hideMarkerOnCollapse = TRUE, zoom = 11)
        ) %>%
        setView(11.577779, 48.137168, zoom = 11) %>%
        addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
        htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
      # if (!is.null(clicks_locations$data)) {
      #  map <- map()
      # }
    } else if (input$radio_type == "Drivetime/Radius") {
      map <- leaflet() %>%
        addProviderTiles("OpenStreetMap",
          group = "OpenStreetMap"
        ) %>%
        addProviderTiles("Esri.WorldImagery",
          group = "Esri.WorldImagery"
        ) %>%
        addProviderTiles("CartoDB.Positron",
          group = "CartoDB.Positron"
        ) %>%
        addProviderTiles("CartoDB.DarkMatter",
          group = "CartoDB.DarkMatter"
        ) %>%
        addLayersControl(names(tiles), position = c("topleft")) %>%
        addFullscreenControl() %>%
        leaflet.extras::addSearchOSM(
          options = searchOptions(hideMarkerOnCollapse = TRUE, zoom = 11)
        ) %>%
        setView(11.577779, 48.137168, zoom = 11) %>%
        addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
        htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
    }
  } else if (input$goal == "Location <br> analysis") {
    map <- leaflet() %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "CartoDB.DarkMatter") %>%
      addLayersControl(names(tiles), position = c("topleft")) %>%
      addFullscreenControl() %>%
      leaflet.extras::addSearchOSM(
        options = searchOptions(hideMarkerOnCollapse = TRUE, zoom = 11)
      ) %>%
      setView(11.577779, 48.137168, zoom = 11) %>%
      addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
  }
  map
})
