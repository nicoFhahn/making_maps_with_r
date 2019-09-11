get_cluster_data <- eventReactive(input$cluster_start, {
  if (input$radio_type_cluster == "Cities/Districts") {
    if (!is.null(input$switch_cities_cluster)) {
      # get all the location names
      cities <- trimws(unlist(
        stringr::str_split(input$city_cluster, "(\\,|\\;)")
      ))
      if (!input$switch_cities_cluster) {
        # if data for multiple location should not be downloaded use the first
        # location name
        cities <- cities[1]
      }
    } else {
      # use the one name provided
      cities <- input$city_cluster
    }
  } else {
    if (!input$file_upload_cluster) {
      if (input$radio_format == "Address") {
        # get the adress
        adr <- input$address_cluster
        # try to geocode the adress
        point <- try(stplanr::geo_code(adr), silent = TRUE)
        if (class(point) != "try-error") {
          point <- data.frame(
            lat = point[2],
            lon = point[1]
          )
        } else {
          # return a default data frame if geocoding is unsucessful
          return(
            data.frame(
              latitude = 48.137168,
              longitude = 11.577779,
              name = "NODATA"
            )
          )
        }
        # save the coordinates
      } else {
        point <- data.frame(
          lat = as.numeric(
            stringr::str_replace(input$latitude_cluster, "\\,", "\\.")
          ),
          lon = as.numeric(
            stringr::str_replace(input$longitude_cluster, "\\,", "\\.")
          )
        )
      }
      # check that a numeric value has been emtered for radius and
      # and point
      if (any(is.na(point[1, ])) ||
        (input$polygon_type_cluster == "Radius" &&
          is.na(as.numeric(input$radius_drivetime_cluster)))) {
        return(
          data.frame(
            latitude = 48.137168,
            longitude = 11.577779,
            name = "NODATA"
          )
        )
      }
    } else {
      # read the dataset
      dataset <- read.xlsx(input$file_cluster$datapath)
      # check if there are lat and lon cols
      if (all(c("lat", "lon") %in% colnames(dataset))) {
        point <- data.frame(
          lat = as.numeric(dataset$lat),
          lon = as.numeric(dataset$lon)
        )
        # remove NA values
        point <- point[complete.cases(point), ]
        # checking again
        if (nrow(point) == 0 ||
          (input$polygon_type_cluster == "Radius" &&
            is.na(as.numeric(input$radius_drivetime_cluster)))) {
          return(
            data.frame(
              latitude = 48.137168,
              longitude = 11.577779,
              name = "NODATA"
            )
          )
        }
        # check if there is an address column
      } else if ("address" %in% colnames(dataset)) {
        # get the addresses
        addr <- dataset$address
        # remove NA
        addr <- addr[complete.cases(addr)]
        # geocode all addresses
        point <- future_map(addr, function(x) {
          Sys.sleep(0.1)
          coded <- try(stplanr::geo_code(x), silent = TRUE)
          if (class(coded) != "try-error") {
            coded <- data.frame(
              lat = coded[2],
              lon = coded[1]
            )
            coded
          }
        })
        # bind everything together
        point <- do.call(rbind, point)
        if (is.null(point)) {
          return(
            data.frame(
              latitude = 48.137168,
              longitude = 11.577779,
              name = "NODATA"
            )
          )
        }
        # remove NA again
        point <- point[rowSums(is.na(point)) == 0, ]
        # pretty sure that i can do this in a better way but idk when i did
        # this and i am too lazy to change it sorry
        if (nrow(point) == 0 ||
          (input$polygon_type_cluster == "Radius" &&
            is.na(as.numeric(input$radius_drivetime_cluster)))) {
          return(
            data.frame(
              latitude = 48.137168,
              longitude = 11.577779,
              name = "NODATA"
            )
          )
        }
      } else {
        return(
          data.frame(
            latitude = 48.137168,
            longitude = 11.577779,
            name = "NODATA"
          )
        )
      }
    }
    # transform points into sf with crs = 3035
    points <- point %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(3035)
    # calculate the circles around the points
    if (input$polygon_type_cluster == "Radius") {
      isochrone <- st_buffer(
        points,
        dist = as.numeric(input$radius_drivetime_cluster)
      )
      isochrone <- isochrone %>%
        st_transform(4326)
      # use bboxes for osm query later
      if (nrow(isochrone) > 1) {
        cities <- future_map(1:nrow(isochrone), function(x) {
          sf::st_bbox(isochrone[x, ])
        })
      } else {
        cities <- sf::st_bbox(isochrone)
      }
    } else {
      if (nrow(point) == 1) {
        # calculate the drivetime isochrone
        isochrone <- try(osrmIsochrone(
          loc = c(point$lon, point$lat),
          breaks = seq(0, as.numeric(input$drivetime_cluster),
            length.out = 2
          ),
          res = as.numeric(input$accuracy_cluster)
        ), silent = TRUE)
        if (class(isochrone) == "try-error") {
          return(
            data.frame(
              latitude = as.numeric(stringr::str_replace(
                input$latitude_cluster, "\\,", "\\."
              )),
              longitude = as.numeric(stringr::str_replace(
                input$longitude_cluster, "\\,", "\\."
              )),
              name = "NODATA"
            )
          )
        }
        else {
          isochrone <- isochrone %>% st_as_sf()
        }
        cities <- sf::st_bbox(isochrone)
      } else {
        # use multiprocessing because these settings take a long long time
        if (as.numeric(input$accuracy_cluster > 80) || nrow(point) > 10) {
          future::plan(future::multiprocess)
        }
        # calculate the isochrones
        isochrone <- furrr::future_map(1:nrow(point), function(x) {
          try(osrmIsochrone(
            loc = c(point[x, "lon"], point[x, "lat"]),
            breaks = seq(0, as.numeric(input$drivetime_cluster),
              length.out = 2
            ),
            res = as.numeric(input$accuracy_cluster)
          ), silent = TRUE)
        })
        is_error <- lapply(isochrone, function(x) {
          class(x) == "try-error"
        })
        if (any(unlist(is_error))) {
          isochrone <- isochrone[!unlist(is_error)]
        }
        if (length(isochrone) == 0) {
          return(
            data.frame(
              latitude = "Multiple",
              longitude = "locations",
              name = "NODATA"
            )
          )
        } else {
          isochrone <- lapply(isochrone, st_as_sf)
        }
        future::plan(future::sequential)
        # use bounding boxes again
        cities <- future_map(isochrone, sf::st_bbox)
        isochrone <- do.call(rbind, isochrone)
      }
    }
  }
  if (!is.null(input$value_cluster)) {
    # if the user entered a value use this query
    if (length(input$value_cluster) == 1) {
      # if yes check if the value appears in multiple keys
      keys2 <- unlist(
        future_map(keys, function(x, ...) {
          input$value_cluster %in% x
        })
      )
      keys2 <- names(keys2[as.numeric(keys2) > 0])
      coords <- future_map(keys2, download_key_data,
        cities = cities, value = input$value_cluster
      )
    } else {
      # if multiple values have been entered check which ones appear
      # in multiple keys
      keys2 <- future_map(input$value_cluster, function(x, ...) {
        keys2 <- unlist(
          future_map(keys, function(y, ...) {
            x %in% y
          })
        )
        names(keys2[as.numeric(keys2) > 0])
      })

      keys2 <- future_map(seq_len(length(keys2)), function(x, ...) {
        keys2[[x]] <- data.frame(
          "key" = keys2[[x]],
          "value" = input$value_cluster[[x]]
        )
      })
      keys2 <- do.call(rbind, keys2)
      uniq <- as.character(unique(keys2$key))
      coords <- future_map(seq_len(length(uniq)), function(x, ...) {
        value <- keys2[keys2$key == uniq[x], ]$value
        key <- uniq[x]
        download_key_data(cities = cities, key = key, value = value)
      })
    }
    if (length(coords) > 1) {
      no_data <- unlist(future_map(coords, function(x) {
        class(x[1, 3])[1] == "factor" || nrow(x) == 0
      }))
      index <- which(no_data %in% TRUE)
      if (length(index) > 0) {
        coords <- rlist::list.remove(coords, index)
      }
    }
    all_cols <- unique(unlist(future_map(coords, colnames)))
    # if columns are missing for some of the data, add them as NA
    coords <- future_map(
      coords, function(data2, all_cols, ...) {
        if (!is.null(nrow(data2))) {
          col2 <- colnames(data2)
          missing_cols <- all_cols[!all_cols %in% col2]
          if (length(missing_cols) > 0) {
            data2[, missing_cols] <- NA
          }
          return(data2)
        }
      },
      all_cols
    )
    # bind the entire list while ensuring that the order of the cols is
    # correct
    coords <- do.call(
      rbind,
      future_map(
        coords,
        function(x) x[match(all_cols, names(x))]
      )
    )
  } else {
    # else use this query
    coords <- download_key_data(
      cities = cities,
      key = input$key_cluster
    )
  }
  coords <- coords[!duplicated(coords$osm_id), ]
  if (input$radio_type_cluster == "Drivetime") {
    # try to get the intersections
    coords <- try(coords[unlist(st_intersects(isochrone, coords)), ],
      silent = TRUE
    )
    if (class(coords) == "try-error") {
      return(
        data.frame(
          latitude = as.numeric(stringr::str_replace(
            input$latitude_cluster, "\\,", "\\."
          )),
          longitude = as.numeric(stringr::str_replace(
            input$longitude_cluster, "\\,", "\\."
          )),
          name = "NODATA"
        )
      )
    }
    # return a list
    coords <- list("data" = coords, "isochrone" = isochrone, "point" = point)
  }
  # return the data
  coords
})


observeEvent(list(
  input$cluster_start,
  input$cluster_distance,
  input$cluster_minpts,
  input$palette,
  input$cluster_map_type,
  input$getis_distance,
  input$show_roads,
  input$show_plz,
  input$important,
  input$names_cluster
), {
  if (is.null(input$value_cluster)) {
    map <- leafletProxy("mymap") %>%
      clearControls() %>%
      clearMarkers() %>%
      clearImages() %>%
      clearShapes() %>%
      clearPopups() %>%
      clearWebGLHeatmap() %>%
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
      addLayersControl(
        baseGroups = names(tiles), position = c("topleft")
      ) %>%
      addFullscreenControl() %>%
      leaflet.extras::addSearchOSM(
        options = searchOptions(hideMarkerOnCollapse = TRUE, zoom = 11)
      ) %>%
      addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
    return(map)
  }
  data <- get_cluster_data()
  if (input$radio_type_cluster == "Drivetime"
  && !"data.frame" %in% class(data)) {
    point <- data$point
    isochrone <- data$isochrone
    data <- data$data
  }
  data <- data[!duplicated(data), ]
  if (class(data)[1] != "sf") {
    if (data[1, 3] == "NODATA") {
      map <- leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        clearPopups() %>%
        clearWebGLHeatmap() %>%
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
        setView(11.577779, 48.137168,
          zoom = 11
        ) %>%
        addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
        htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
      return(map)
    }
  }
  Encoding(data$name) <- "UTF-8"
  if (!"addr.street" %in% colnames(data)) {
    data$addr.street <- ""
  }
  Encoding(data$addr.street) <- "UTF-8"
  points_df <- data[, c("latitude", "longitude")]
  points_df$geometry <- NULL
  if (!"NODATA" %in% data$name) {
    if (input$radio_type_cluster == "Cities/Districts") {
      splits <- split(data, f = data$city)
      if (length(splits) == 1) {
        # build a polygon around all points
        around <- chull(points_df)
        around <- points_df[c(around, around[1]), ]
        around <- st_sfc(st_polygon(list(
          as.matrix(around[, c("longitude", "latitude")])
        )), crs = 4326)
        around <- st_transform(around, 3065) %>%
          st_buffer(500) %>%
          st_transform(4326)
        around <- st_as_sf(as.data.frame(around))
      } else {
        around <- future_map(splits, function(x, ...) {
          around <- chull(st_coordinates(x))
          around <- st_coordinates(x)[c(around, around [1]), ]
          around <- st_sfc(st_polygon(list(
            as.matrix(around[, c("X", "Y")])
          )), crs = 4326)
          around <- st_transform(around, 3065) %>%
            st_buffer(500) %>%
            st_transform(4326)
          around <- st_as_sf(as.data.frame(around))
        })
        around <- do.call(rbind, around)
      }
      if (input$show_roads == "Yes") {
        opac <- 1
        # if only one city, use points_df
        if (length(splits) == 1) {
          # get the states of the location
          bl <- sf::st_intersection(states, around)$GEN
          streets_bl <- roads[bl]
          a <- lapply(streets_bl, function(x, ...) {
            as.data.frame(st_intersection(around, x))
          })
          streets_c <- st_as_sf(do.call(rbind, a))
        } else {
          bl <- sf::st_intersection(states, around)$GEN
          streets_bl <- roads[bl]
          a <- lapply(streets_bl, function(x, ...) {
            as.data.frame(st_intersection(around, x))
          })
          streets_c <- st_as_sf(do.call(rbind, a))
        }
      } else {
        streets_c <- st_linestring(cbind(
          points_df[1:2, 2],
          points_df[1:2, 1]
        ))
        opac <- 0
      }
    } else {
      around <- isochrone
      if (input$show_roads == "Yes") {
        opac <- 1
        bl <- unique(sf::st_intersection(states, around)$GEN)
        streets_bl <- roads[bl]
        a <- lapply(streets_bl, function(x, ...) {
          as.data.frame(st_intersection(around, x))
        })
        streets_c <- st_as_sf(do.call(rbind, a))
      } else {
        opac <- 0
        streets_c <- st_linestring(cbind(
          points_df[1:2, 2],
          points_df[1:2, 1]
        ))
      }
    }
    if (input$cluster_map_type == "Clustering") {
      set.seed(123)
      # get values for epsilon and min. pts
      # accept comma and point seperation
      eps <- as.numeric(
        stringr::str_replace(input$cluster_distance, "\\,", "\\.")
      )
      minpts <- as.numeric(
        stringr::str_replace(input$cluster_minpts, "\\,", "\\.")
      )
      if (is.na(minpts) || is.na(eps)) {
        map <- leafletProxy("mymap") %>%
          clearControls() %>%
          clearMarkers() %>%
          clearImages() %>%
          clearShapes() %>%
          clearPopups() %>%
          clearWebGLHeatmap() %>%
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
        return(map)
      }
      # dbscan algorithm
      points_df$cluster <- dbscan(points_df, eps = eps, minPts = minpts)$cluster
      # add size of clusters to df
      points_df <- points_df %>%
        dplyr::group_by(cluster) %>%
        dplyr::mutate(count = n())
      points_df$name <- data$name
      points_df$addr.street <- data$addr.street
      points_df <- points_df[points_df$cluster != 0, ]
      # encode the names
      # get the palette
      if (input$palette %in% c(
        "viridis", "cividis", "magma", "inferno",
        "plasma"
      )) {
        pal <- get(input$palette)
        pal <- colorNumeric(pal(10, direction = 1),
          domain = points_df$count
        )
      } else if (input$palette %in% names(custom_palettes)) {
        pal <- custom_palettes[[input$palette]]
        pal <- colorNumeric(pal, domain = points_df$count)
      } else {
        pal <- RColorBrewer::brewer.pal(10, input$palette)
        pal <- pal[3:length(pal)]
        pal <- colorNumeric(pal, domain = points_df$count)
      }
      if (length(unique(points_df$cluster)) == 0) {
        map <- leafletProxy("mymap") %>%
          clearControls() %>%
          clearMarkers() %>%
          clearImages() %>%
          clearShapes() %>%
          clearPopups() %>%
          clearWebGLHeatmap() %>%
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
        return(map)
      }
      # plot depends on the number of different clusters
      # there are a lot of special cases for the plots coming and i won't
      # explain because it mostly only depends on the number of clusters
      # or something like that and i changed the legend or added one or what
      # do i know
      if (length(unique(points_df$cluster)) > 0) {
        polygons <- future_map(
          seq_len(
            length(unique(points_df$cluster))
          ),
          function(x, ...) {
            cluster <- points_df[
              points_df$cluster == x,
              c("longitude", "latitude")
            ]
            if (nrow(cluster) >= 2) {
              if (nrow(cluster) == 2) {
                cluster[3, ] <- cluster[1, ] + 0.000001
              }
              ch <- chull(cluster)
              coords <- cluster[c(ch, ch[1]), ]
              poly <- st_sfc(st_polygon(list(as.matrix(coords))))
              poly <- poly %>% st_buffer(0.002)
              poly <- st_as_sf(as.data.frame(poly))
              st_crs(poly) <- "+proj=longlat +datum=WGS84 +no_defs"
              poly
            } else {
              point <- cluster %>%
                st_as_sf(
                  coords = c(
                    "longitude",
                    "latitude"
                  ),
                  crs = 4326
                ) %>%
                st_transform(3035)
              st_buffer(point, 200) %>% st_transform(4326)
            }
          }
        )
        polygons <- do.call(rbind, polygons)
      }
      # plot the Clustering
      # once again plots depend on the number of clusters oh yeah and i
      # checked if i already plotted something before/some data exists
      # because it crashed sometimes
      if (input$radio_type_cluster != "Drivetime" || !exists("isochrone")) {
        if (input$show_plz == "Yes") {
          intersect <- st_intersects(around, plz)
          intersect <- plz[unlist(intersect), ]
          pal2 <- colorNumeric(
            RColorBrewer::brewer.pal(9, "Blues")[3:9],
            intersect$einwohner
          )
        }
        if (length(unique(points_df$cluster)) > 1) {
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
          if (input$show_plz == "Yes") {
            map <- map %>%
              addPolygons(
                data = intersect, color = ~ pal2(einwohner),
                fillColor = ~ pal2(einwohner),
                label = paste(
                  "ZIP code:", intersect$plz,
                  "<br>", "Residents:",
                  intersect$einwohner
                ) %>%
                  future_map(htmltools::HTML)
              )
          }
          map <- map %>%
            addPolygons(
              data = polygons, color = "#fc6b60",
              opacity = 0.2
            ) %>%
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addCircleMarkers(
              data = points_df, lat = ~latitude,
              lng = ~longitude,
              label = paste(
                "Name:",
                points_df$name, "<br>",
                "Cluster:",
                points_df$cluster, "<br>",
                "Cluster size:",
                points_df$count, "<br>",
                "Street: ",
                points_df$addr.street
              ) %>%
                future_map(htmltools::HTML),
              color = ~ pal(count),
              opacity = 0.65, radius = 5
            )
          if (input$show_plz == "Yes") {
            map <- map %>%
              addLegend(
                data = intersect, pal = pal2,
                values = ~einwohner,
                title = "Number of residents:",
                opacity = 1,
                position = "bottomleft"
              )
          }
          map <- map %>%
            addLegend(
              data = points_df, pal = pal, values = ~count,
              position = "bottomleft", title = "Cluster size:",
              opacity = 0.9
            )
        } else if (length(unique(points_df$cluster)) == 0) {
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addCircleMarkers(
              data = points_df, lat = ~latitude,
              lng = ~longitude,
              label = paste(
                "Name:",
                points_df$name, "<br>",
                "Cluster:",
                points_df$cluster, "<br>",
                "Cluster size:",
                points_df$count, "<br>",
                "Street: ",
                points_df$addr.street
              ) %>%
                future_map(htmltools::HTML),
              color = ~ pal(count),
              opacity = 0.65, radius = 5
            ) %>%
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
        } else {
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
          if (input$show_plz == "Yes") {
            map <- map %>%
              addPolygons(
                data = intersect, color = ~ pal2(einwohner),
                fillColor = ~ pal2(einwohner),
                label = paste(
                  "ZIP code:", intersect$plz,
                  "<br>", "Residents:",
                  intersect$einwohner
                ) %>%
                  future_map(htmltools::HTML)
              )
          }
          map <- map %>%
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addPolygons(
              data = polygons, color = "#fc6b60",
              opacity = 0.2
            ) %>%
            addCircleMarkers(
              data = points_df, lat = ~latitude,
              lng = ~longitude,
              label = paste(
                "Name:",
                points_df$name, "<br>",
                "Cluster:",
                points_df$cluster, "<br>",
                "Cluster size:",
                points_df$count, "<br>",
                "Street: ",
                points_df$addr.street
              ) %>%
                future_map(htmltools::HTML),
              color = ~ pal(count),
              opacity = 0.65, radius = 5
            )
          if (input$show_plz == "Yes") {
            map <- map %>%
              addLegend(
                data = intersect, pal = pal2,
                values = ~einwohner,
                title = "Number of residents:",
                opacity = 1,
                position = "bottomleft"
              )
          }
          map <- map %>%
            addLegend(
              data = points_df, colors = pal(1),
              labels = unique(points_df$count),
              position = "bottomleft", title = "Cluster size:",
              opacity = 0.9
            )
        }
      }
      # more plots yay
      if (input$radio_type_cluster == "Drivetime") {
        lat <- point$lat
        lon <- point$lon
        if (input$show_plz == "Yes") {
          intersect <- st_intersects(around, plz)
          intersect <- plz[unlist(intersect), ]
          pal2 <- colorNumeric(
            RColorBrewer::brewer.pal(9, "Blues")[3:9],
            intersect$einwohner
          )
        }
        if (length(unique(points_df$cluster)) > 1) {
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addPolygons(data = isochrone, color = "#fcfcfc") %>%
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
          if (input$show_plz == "Yes") {
            map <- map %>%
              addPolygons(
                data = intersect, color = ~ pal2(einwohner),
                fillColor = ~ pal2(einwohner), opacity = 0.9,
                label = paste(
                  "ZIP code:", intersect$plz,
                  "<br>", "Residents:",
                  intersect$einwohner
                ) %>%
                  future_map(htmltools::HTML)
              )
          }
          map <- map %>%
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addPolygons(
              data = polygons, color = "#fc6b60",
              opacity = 0.2
            ) %>%
            addAwesomeMarkers(
              lat = lat,
              lng = lon,
              icon = icon.fa, label = "Starting point"
            ) %>%
            addCircleMarkers(
              data = points_df, lat = ~latitude,
              lng = ~longitude,
              label = paste(
                "Name:",
                points_df$name, "<br>",
                "Cluster:",
                points_df$cluster, "<br>",
                "Cluster size:",
                points_df$count, "<br>",
                "Street: ",
                points_df$addr.street
              ) %>%
                future_map(htmltools::HTML),
              color = ~ pal(count),
              opacity = 0.65, radius = 5
            )
          if (input$show_plz == "Yes") {
            map <- map %>%
              addLegend(
                data = intersect, pal = pal2,
                values = ~einwohner,
                title = "Number of residents:",
                opacity = 1,
                position = "bottomleft"
              )
          }
          map <- map %>%
            addLegend(
              data = points_df, pal = pal, values = ~count,
              position = "bottomleft", title = "Cluster size:",
              opacity = 0.9
            )
        } else if (length(unique(points_df$cluster)) == 0) {
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addPolygons(data = isochrone, color = "#fcfcfc") %>%
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addAwesomeMarkers(
              lat = lat,
              lng = lon,
              icon = icon.fa, label = "Starting point"
            ) %>%
            addCircleMarkers(
              data = points_df, lat = ~latitude,
              lng = ~longitude,
              label = paste(
                "Name:",
                points_df$name, "<br>",
                "Cluster:",
                points_df$cluster, "<br>",
                "Cluster size:",
                points_df$count, "<br>",
                "Street: ",
                points_df$addr.street
              ) %>%
                future_map(htmltools::HTML),
              color = ~ pal(count),
              opacity = 0.65, radius = 5
            ) %>%
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
        } else {
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addPolygons(data = isochrone, color = "#fcfcfc") %>%
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
          if (input$show_plz == "Yes") {
            map <- map %>%
              addPolygons(
                data = intersect, color = ~ pal2(einwohner),
                fillColor = ~ pal2(einwohner), opacity = 0.9,
                label = paste(
                  "ZIP code:", intersect$plz,
                  "<br>", "Residents:",
                  intersect$einwohner
                ) %>%
                  future_map(htmltools::HTML)
              )
          }
          map <- map %>%
            addPolygons(
              data = polygons, color = "#fc6b60",
              opacity = 0.2
            ) %>%
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addAwesomeMarkers(
              lat = lat,
              lng = lon,
              icon = icon.fa, label = "Starting point"
            ) %>%
            addCircleMarkers(
              data = points_df, lat = ~latitude,
              lng = ~longitude,
              label = paste(
                "Name:",
                points_df$name, "<br>",
                "Cluster:",
                points_df$cluster, "<br>",
                "Cluster size:",
                points_df$count, "<br>",
                "Street: ",
                points_df$addr.street
              ) %>%
                future_map(htmltools::HTML),
              color = ~ pal(count),
              opacity = 0.65, radius = 5
            )
          if (input$show_plz == "Yes") {
            map <- map %>%
              addLegend(
                data = intersect, pal = pal2,
                values = ~einwohner,
                title = "Number of residents:",
                opacity = 1,
                position = "bottomleft"
              )
          }
          map <- map %>%
            addLegend(
              data = points_df, colors = pal(1),
              labels = unique(points_df$count),
              position = "bottomleft", title = "Cluster size:",
              opacity = 0.9
            )
        }
      }
    } else if (input$cluster_map_type == "Getis-Ord") {
      # turn points into matrix
      df <- as.matrix(points_df)
      # get the distance
      dist <- as.numeric(
        stringr::str_replace(input$getis_distance, "\\,", "\\.")
      )
      # check that distance is a numeric variable > 0
      if (!is.na(dist) && dist > 0) {
        # get the radius for the close by locations
        radius <- dist * 1000
        # get points as spatical object and create buffers around each
        points <- as.data.frame(points_df[, c("latitude", "longitude")]) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
          st_transform(3035)
        buffers <- st_buffer(points, dist = radius)
        buffers <- buffers %>%
          st_transform(4326)
        points <- points %>%
          st_transform(4326)
        coords <- as.data.frame(st_coordinates(points))
        if (nrow(buffers) >= 2000) {
          future::plan(future::multiprocess)
        }
        # count the number of intersections for each point
        intersections <- try(furrr::future_map(
          seq_len(nrow(buffers)), function(x, ...) {
            nrow(suppressMessages(sf::st_intersection(
              buffers[x, ],
              points
            )))
          }
        ), silent = TRUE)
        if (nrow(buffers) >= 2000) {
          future::plan(future::sequential)
        }
        # sometimes i wonder why i started coding
        if (class(intersections)[1] == "try-error") {
          if (input$radio_type_cluster != "Drivetime") {
            data("world.cities")
            points <- world.cities[
              world.cities$name == input$city_cluster,
              c("lat", "long")
            ]
            # if not set the view to munich
            if (nrow(points) == 0) {
              map <- leafletProxy("mymap") %>%
                clearControls() %>%
                clearMarkers() %>%
                clearImages() %>%
                clearShapes() %>%
                clearPopups() %>%
                clearWebGLHeatmap() %>%
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
                  options = searchOptions(
                    hideMarkerOnCollapse = TRUE,
                    zoom = 11
                  )
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
            } else {
              # if the city can be found set the viewpoint to the city
              map <- leafletProxy("mymap") %>%
                clearControls() %>%
                clearMarkers() %>%
                clearImages() %>%
                clearShapes() %>%
                clearPopups() %>%
                clearWebGLHeatmap() %>%
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
                  options = searchOptions(
                    hideMarkerOnCollapse = TRUE,
                    zoom = 11
                  )
                ) %>%
                setView(points$long[1], points$lat[1], zoom = 11) %>%
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
          } else {
            map <- leafletProxy("mymap") %>%
              clearControls() %>%
              clearMarkers() %>%
              clearImages() %>%
              clearShapes() %>%
              clearPopups() %>%
              clearWebGLHeatmap() %>%
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
              setView(data$longitude, data$latitude, zoom = 11) %>%
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
          return(map)
        }
        # bind together
        df <- cbind(df, unlist(intersections) - 1)
        if (!is.null(input$important)) {
          pairs2 <- pairs[input$important]
          if (input$radio_type_cluster == "Cities/Districts") {
            if (!is.null(input$switch_cities_cluster)) {
              # get all the location names
              cities <- trimws(unlist(
                stringr::str_split(input$city_cluster, "(\\,|\\;)")
              ))
              if (!input$switch_cities_cluster) {
                # if data for multiple location should not be
                # downloaded use the first
                # location name
                cities <- cities[1]
              }
            } else {
              # use the one name provided
              cities <- input$city_cluster
            }
          } else {
            cities <- future_map(isochrone$geometry, sf::st_bbox)
          }
          locations <- future_map(pairs2, function(x, ...) {
            download_key_data(
              cities = cities,
              key = x$key,
              value = x$value,
              df = TRUE,
              request_limit = 1
            )
          })
          locations <- future_map(locations, function(x, ...) {
            x[!duplicated(x$osm_id), ]
          })
          if (nrow(buffers) >= 750) {
            future::plan(future::multiprocess)
          }
          # count the number of intersections for each point
          intersections2 <- furrr::future_map(locations, function(x, ...) {
            try(furrr::future_map(seq_len(nrow(buffers)), function(y, ...) {
              nrow(suppressMessages(sf::st_intersection(buffers[y, ], x)))
            }))
          })
          intersections <- unlist(intersections)
          intersections2 <- future_map(intersections2, unlist)
          intersections2[[length(intersections2) + 1]] <- intersections
          intersections <- intersections2
          intervals <- future_map(intersections, function(x) {
            if (length(unique(x)) > 4) {
              classInt::classIntervals(x, n = 5, style = "fisher")
            } else {
              classInt::classIntervals(x,
                n = length(unique(x)),
                style = "fisher"
              )
            }
          })
          i_breaks <- future_map(intervals, function(x) {
            round(x$brks)
          })
          recoded <- furrr::future_map2(
            intersections,
            i_breaks,
            function(x, y) {
              z <- x
              z[x <= y[2]] <- 0
              z[x <= y[3] & x > y[2]] <- 1
              z[x <= y[4] & x > y[3]] <- 2
              z[x <= y[5] & x > y[4]] <- 3
              z[x <= y[6] & x > y[5]] <- 4
              z
            }
          )
          df[, 3] <- Reduce("+", recoded)
          recoded <- future_map(recoded, function(x, ...) {
            x[df[, 3] > 0]
          })
          intersections <- future_map(intersections, function(x, ...) {
            x[df[, 3] > 0]
          })
          if (nrow(buffers) >= 750) {
            future::plan(future::sequential)
          }
        }
        # only use points with at least one neighbor
        df <- df[df[, 3] > 0, ]
        # now for some special cases where no getis-ord can be calculated
        # and standard maps will be returned
        if (nrow(df) == 0) {
          if (input$radio_type_cluster != "Drivetime") {
            data("world.cities")
            points <- world.cities[
              world.cities$name == input$city_cluster,
              c("lat", "long")
            ]
            # if not set the view to munich
            if (nrow(points) == 0) {
              map <- leafletProxy("mymap") %>%
                clearControls() %>%
                clearMarkers() %>%
                clearImages() %>%
                clearShapes() %>%
                clearPopups() %>%
                clearWebGLHeatmap() %>%
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
                  options = searchOptions(
                    hideMarkerOnCollapse = TRUE,
                    zoom = 11
                  )
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
            } else {
              # if the city can be found set the viewpoint to the city
              map <- leafletProxy("mymap") %>%
                clearControls() %>%
                clearMarkers() %>%
                clearImages() %>%
                clearShapes() %>%
                clearPopups() %>%
                clearWebGLHeatmap() %>%
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
                  options = searchOptions(
                    hideMarkerOnCollapse = TRUE,
                    zoom = 11
                  )
                ) %>%
                setView(points$long[1], points$lat[1], zoom = 11) %>%
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
          } else {
            map <- leafletProxy("mymap") %>%
              clearControls() %>%
              clearMarkers() %>%
              clearImages() %>%
              clearShapes() %>%
              clearPopups() %>%
              clearWebGLHeatmap() %>%
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
              setView(data$longitude, data$latitude, zoom = 11) %>%
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
          return(map)
        }
        # set up neighbourhood
        nb <- dnearneigh(df[, 1:2], 0, dist, longlat = TRUE)
        # turn to listw object
        q1 <- nb2listw(nb, style = "W", zero.policy = TRUE)
        # calculate getis ord
        g1 <- localG(df[, 3], q1)
        # another special case where all getis-ords are NA or NaN
        if (all(is.na(g1) | is.infinite(g1))) {
          warning("Getis ord couldn't be calculated")
          if (input$radio_type_cluster != "Drivetime") {
            data("world.cities")
            points <- world.cities[
              world.cities$name == input$city_cluster,
              c("lat", "long")
            ]
            # if not set the view to munich
            if (nrow(points) == 0) {
              map <- leafletProxy("mymap") %>%
                clearControls() %>%
                clearMarkers() %>%
                clearImages() %>%
                clearShapes() %>%
                clearPopups() %>%
                clearWebGLHeatmap() %>%
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
                  options = searchOptions(
                    hideMarkerOnCollapse = TRUE,
                    zoom = 11
                  )
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
            } else {
              # if the city can be found set the viewpoint to the city
              map <- leafletProxy("mymap") %>%
                clearControls() %>%
                clearMarkers() %>%
                clearImages() %>%
                clearShapes() %>%
                clearPopups() %>%
                clearWebGLHeatmap() %>%
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
                  options = searchOptions(
                    hideMarkerOnCollapse = TRUE,
                    zoom = 11
                  )
                ) %>%
                setView(points$long[1], points$lat[1], zoom = 11) %>%
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
          } else {
            map <- leafletProxy("mymap") %>%
              clearControls() %>%
              clearMarkers() %>%
              clearImages() %>%
              clearShapes() %>%
              clearPopups() %>%
              clearWebGLHeatmap() %>%
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
              setView(point$lon, point$lat, zoom = 11) %>%
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
          return(map)
        }
        df2 <- as.data.frame(df)
        # round getis-ord value
        df2$g1 <- round(as.numeric(g1), 3)
        data2 <- data[, c("latitude", "longitude", "name", "addr.street")]
        data2$geometry <- NULL
        data2$latlon <- paste(data2$latitude, data2$longitude, sep = "")
        df2$latlon <- paste(df2$latitude, df2$longitude, sep = "")
        df2 <- join(df2, data2, by = "latlon")
        df2 <- df2[, c(1:4, 8, 9)]
        df2 <- df2[!(is.nan(df2$g1) | is.infinite(df2$g1)), ]
        # get the range of the getis ord values
        r <- range(df2$g1, na.rm = TRUE)
        # get the lowest and highest value
        r <- c(floor(r)[1]:ceiling(r)[2])
        # get the number of negative and positive numbers
        l <- c(length(r[r < 0]), length(r[r > 0]))
        # define some palettees
        blues <- c(
          "#00d2f9", "#09d2f7", "#0ed2f7", "#21dbff", "#23dafc",
          "#2bdeff", "#2bfff1", "#4cfff4", "#60fff6", "#72fff7",
          "#8bf9f3", "#a1fcf7", "#b2fefa", "#c4fffb", "#e0fffc"
        )
        reds <- c(
          "#f7f3b7", "#f7f2a5", "#fff899", "#ede574", "#ede35e",
          "#EDE574", "#fcd414", "#ffa65e", "#ff9c4c", "#ff8b2d", "#f97f1b",
          "#ff8e90", "#f46b6d", "#FF4E50", "#ff3f41", "#ff1e20"
        )
        # use a certain number of hex colors from each palette, depending on
        # the highest and lowest values for getis
        pal1 <- blues[seq(1, 15, length.out = l[1])]
        pal2 <- reds[seq(1, 16, length.out = l[2])]
        # enter white in the middle for 0
        pal <- c(pal1, "#fffcf9", pal2)
        # create palette
        pal3 <- colorNumeric(pal, domain = df2$g1)
        # do morans test
        test <- moran.test(df[, 3], q1, zero.policy = TRUE)
        test_df <- as.data.frame(test$p.value)
        # check if it is usable
        if (is.na(test_df[1, 1])) {
          test_df[1, 1] <- 1
        }
        # sentence for legend
        col_moran <- ifelse(test_df[1, 1] < 0.05,
          "palegreen",
          "lightcoral"
        )
        test_df[1, 1] <- ifelse(test_df[1, 1] < 0.05,
          "Significant at 5% level",
          "Not significant at 5% level"
        )
        colnames(test_df) <- "sig"
        if (!is.null(input$important)) {
          labels1 <- future_map(input$important, function(x, ...)
            paste(x, "locations:", unlist(intersections[x])))
          labels1 <- Reduce("cbind", labels1)
          if (length(input$important) > 1) {
            labels1 <- apply(labels1, 1, paste, collapse = " <br >")
          }
          labels1 <- paste("<br>", labels1, "<br>")
        } else {
          labels1 <- ""
        }
        # encode names
        if (!is.null(input$names_cluster)) {
          df2 <- df2[df2$name %in% input$names_cluster, ]
        }
        if (input$radio_type_cluster != "Drivetime") {
          if (input$show_plz == "Yes") {
            intersect <- st_intersects(around, plz)
            intersect <- plz[unlist(intersect), ]
            pal <- colorNumeric(
              RColorBrewer::brewer.pal(9, "Blues")[3:9],
              intersect$einwohner
            )
          }
          # plot with custom legend depending on morans test result
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
          if (input$show_plz == "Yes") {
            map <- map %>%
              addPolygons(
                data = intersect, color = ~ pal(einwohner),
                fillColor = ~ pal(einwohner),
                label = paste(
                  "ZIP code:", intersect$plz,
                  "<br>", "Residents:",
                  intersect$einwohner
                ) %>%
                  future_map(htmltools::HTML)
              )
          }
          map <- map %>%
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addCircleMarkers(
              data = df2,
              lat = ~latitude,
              lng = ~longitude,
              label = paste(
                "Name:",
                df2$name, "<br>",
                "Getis ord:", df2$g1, "<br>",
                "Other locations:", df2$V3,
                labels1, "<br>",
                "Street: ",
                df2$addr.street
              ) %>%
                future_map(htmltools::HTML),
              color = ~ pal3(g1),
              opacity = 0.9, radius = 5
            ) %>%
            addLegend(
              data = test_df, labels = test_df$sig,
              position = "bottomleft", opacity = 0.9,
              colors = col_moran, title = "Moran's test:"
            )
          if (input$show_plz == "Yes") {
            map <- map %>%
              addLegend(
                data = intersect, pal = pal,
                values = ~einwohner,
                title = "Number of residents:",
                opacity = 1,
                position = "bottomleft"
              )
          }
          map <- map %>%
            addLegend(
              data = df2, pal = pal3, values = ~ g1[!is.na(g1)],
              position = "bottomleft", title = "Getis ord:",
              opacity = 0.9
            )
        } else {
          if (input$show_plz == "Yes") {
            intersect <- st_intersects(around, plz)
            intersect <- plz[unlist(intersect), ]
            pal <- colorNumeric(
              RColorBrewer::brewer.pal(9, "Blues")[3:9],
              intersect$einwohner
            )
          }

          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addPolygons(data = isochrone, color = "#fcfcfc") %>%
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
          if (input$show_plz == "Yes") {
            map <- map %>%
              addPolygons(
                data = intersect, color = ~ pal(einwohner),
                fillColor = ~ pal(einwohner),
                label = paste(
                  "ZIP code:", intersect$plz,
                  "<br>", "Residents:",
                  intersect$einwohner
                ) %>%
                  future_map(htmltools::HTML)
              )
          }
          map <- map %>%
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addAwesomeMarkers(
              lat = point$lat,
              lng = point$lon,
              icon = icon.fa,
              label = "Starting point"
            ) %>%
            addCircleMarkers(
              data = df2,
              lat = ~latitude,
              lng = ~longitude,
              label = paste(
                "Name:",
                df2$name, "<br>",
                "Getis ord:", df2$g1, "<br>",
                "Other locations:", df2$V3,
                labels1, "<br>",
                "Street: ",
                df2$addr.street
              ) %>%
                future_map(htmltools::HTML),
              color = ~ pal3(g1),
              opacity = 0.9, radius = 5
            ) %>%
            addLegend(
              data = test_df, labels = test_df$sig,
              position = "bottomleft", opacity = 0.9,
              colors = col_moran, title = "Moran's test:"
            )
          if (input$show_plz == "Yes") {
            map <- map %>%
              addLegend(
                data = intersect, pal = pal,
                values = ~einwohner,
                title = "Number of residents:",
                opacity = 1,
                position = "bottomleft"
              )
          }
          map <- map %>%
            addLegend(
              data = df2, pal = pal3, values = ~ g1[!is.na(g1)],
              position = "bottomleft", title = "Getis ord:",
              opacity = 0.9
            )
        }
      } else {
        if (input$radio_type_cluster != "Drivetime") {
          data("world.cities")
          points <- world.cities[
            world.cities$name == input$city_cluster,
            c("lat", "long")
          ]
          # if not set the view to munich
          if (nrow(points) == 0) {
            map <- leafletProxy("mymap") %>%
              clearControls() %>%
              clearMarkers() %>%
              clearImages() %>%
              clearShapes() %>%
              clearPopups() %>%
              clearWebGLHeatmap() %>%
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
          } else {
            # if the city can be found set the viewpoint to the city
            map <- leafletProxy("mymap") %>%
              clearControls() %>%
              clearMarkers() %>%
              clearImages() %>%
              clearShapes() %>%
              clearPopups() %>%
              clearWebGLHeatmap() %>%
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
              setView(points$long[1], points$lat[1], zoom = 11) %>%
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
        } else {
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            setView(data$longitude, data$latitude, zoom = 11) %>%
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
      }
    } else {
      opac <- ifelse(opac == 1, 0.7, 0)
      # calculate two dimensional density
      # for the function webglheatmap we need a distance. this distance is
      # always calculated using the distance haversine between all points and
      # taking a quantile depending on the number of points
      if (input$radio_type_cluster == "Cities/Districts") {
        if (length(splits) > 1) {
          dist <- lapply(splits, function(x, ...) {
            dist <- as.vector(distm(st_coordinates(x), fun = distHaversine))
            dist <- dist[!duplicated(dist)]
            if (nrow(x) > 200) {
              quantile(dist, 0.05)
            } else if (nrow(x) > 150) {
              quantile(dist, 0.1)
            } else if (nrow(x) > 100) {
              quantile(dist, 0.15)
            } else if (nrow(x) > 50) {
              quantile(dist, 0.2)
            } else {
              quantile(dist, 0.25)
            }
          })
          dist <- unlist(dist)
          points_df <- lapply(splits, function(x, ...)
            st_coordinates(x) %>%
              as.data.frame() %>%
              st_as_sf(coords = c("X", "Y")) %>%
              st_set_crs(4326))
          map_base <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addLayersControl(names(tiles), position = c("topleft")) %>%
            addFullscreenControl() %>%
            leaflet.extras::addSearchOSM(
              options = searchOptions(hideMarkerOnCollapse = TRUE, zoom = 11)
            ) %>%
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
          for (i in seq_len(length(dist))) {
            map_base <- map_base %>%
              addWebGLHeatmap(
                data = points_df[[i]],
                size = as.numeric(dist[i]), units = "m",
                intensity = 0.1, gradientTexture = "skyline",
                alphaRange = 1, opacity = 0.8
              )
          }
        } else {
          points_df <- points_df %>%
            st_as_sf(coords = c("longitude", "latitude")) %>%
            st_set_crs(4326)
          st_crs(points_df) <- 4326
          dist <- as.vector(distm(st_coordinates(points_df)[, 1:2],
            fun = distHaversine
          ))
          dist <- dist[!duplicated(dist)]
          if (nrow(points_df) > 200) {
            dist <- quantile(dist, 0.05)
          } else if (nrow(points_df) > 150) {
            dist <- quantile(dist, 0.1)
          } else if (nrow(points_df) > 100) {
            dist <- quantile(dist, 0.15)
          } else if (nrow(points_df) > 50) {
            dist <- quantile(dist, 0.2)
          } else {
            dist <- quantile(dist, 0.25)
          }
          map_base <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addWebGLHeatmap(
              data = points_df, size = as.numeric(dist),
              units = "m", intensity = 0.1,
              gradientTexture = "skyline", alphaRange = 1,
              opacity = 0.8
            ) %>%
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
      }
      if (input$radio_type_cluster != "Drivetime" || !exists("isochrone")) {
        map <- map_base
      } else {
        if (!input$file_upload_cluster) {
          lat <- as.numeric(point$lat)
          lon <- as.numeric(point$lon)
          if (input$polygon_type_cluster != "Radius") {
            dist <- as.vector(distm(
              cbind(points_df[, "longitude"], points_df[, "latitude"]),
              fun = distHaversine
            ))
            dist <- dist[!duplicated(dist)]
            if (nrow(points_df) > 200) {
              dist <- quantile(dist, 0.05)
            } else if (nrow(points_df) > 150) {
              dist <- quantile(dist, 0.1)
            } else if (nrow(points_df) > 100) {
              dist <- quantile(dist, 0.15)
            } else if (nrow(points_df) > 50) {
              dist <- quantile(dist, 0.2)
            } else {
              dist <- quantile(dist, 0.25)
            }
          } else {
            dist <- 1 / 4 * as.numeric(input$radius_drivetime_cluster)
          }
          points_df <- points_df %>%
            st_as_sf(coords = c("longitude", "latitude")) %>%
            st_set_crs(4326)
          map <- leafletProxy("mymap") %>%
            clearControls() %>%
            clearMarkers() %>%
            clearImages() %>%
            clearShapes() %>%
            clearPopups() %>%
            clearWebGLHeatmap() %>%
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
            addPolygons(
              data = isochrone, color = "#fc6b60",
              opacity = 0.2
            ) %>%
            addWebGLHeatmap(
              data = points_df, size = as.numeric(dist),
              units = "m", intensity = 0.1,
              gradientTexture = "skyline", alphaRange = 1,
              opacity = 0.7
            ) %>%
            addPolylines(
              data = streets_c, opacity = opac, weight = 1,
              color = "#f53374"
            ) %>%
            addAwesomeMarkers(
              lat = lat,
              lng = lon,
              icon = icon.fa, label = "Starting point"
            ) %>%
            addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
        } else {
          lat <- as.numeric(point$lat)
          lon <- as.numeric(point$lon)
          if (input$polygon_type_cluster == "Radius") {
            dist <- 1 / 4 * as.numeric(input$radius_drivetime_cluster)
            points_df <- points_df %>%
              st_as_sf(coords = c("longitude", "latitude")) %>%
              st_set_crs(4326)
            around$id <- 1:nrow(around)
            splits <- st_intersection(around, points_df)
            splits <- split(splits, splits$id)
            norow <- lapply(splits, nrow)
            splits <- splits[unlist(norow) > 0]
            map_basic <- leafletProxy("mymap") %>%
              clearControls() %>%
              clearMarkers() %>%
              clearImages() %>%
              clearShapes() %>%
              clearPopups() %>%
              clearWebGLHeatmap() %>%
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
              addPolygons(
                data = st_union(isochrone), color = "#fc6b60",
                opacity = 0.2
              ) %>%
              addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
              htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
            for (i in 1:length(splits)) {
              map_basic <- map_basic %>%
                addWebGLHeatmap(
                  data = splits[[i]], size = dist,
                  units = "m", intensity = 0.1,
                  gradientTexture = "skyline", alphaRange = 1,
                  opacity = 0.7
                )
            }
            map <- map_basic %>%
              addPolylines(
                data = streets_c, opacity = opac, weight = 1,
                color = "#f53374"
              ) %>%
              addAwesomeMarkers(
                lat = lat,
                lng = lon,
                icon = icon.fa, label = "Starting point"
              )
          } else {
            points_df <- points_df %>%
              st_as_sf(coords = c("longitude", "latitude")) %>%
              st_set_crs(4326)
            around$id <- 1:nrow(around)
            splits <- st_intersection(around, points_df)
            splits <- split(splits, splits$id)
            dist <- lapply(splits, function(x, ...) {
              dist <- as.vector(distm(st_coordinates(x), fun = distHaversine))
              dist <- dist[!duplicated(dist)]
              if (nrow(x) > 200) {
                quantile(dist, 0.05)
              } else if (nrow(x) > 150) {
                quantile(dist, 0.1)
              } else if (nrow(x) > 100) {
                quantile(dist, 0.15)
              } else if (nrow(x) > 50) {
                quantile(dist, 0.2)
              } else {
                quantile(dist, 0.25)
              }
            })
            dist <- unlist(dist)
            map_base <- leafletProxy("mymap") %>%
              clearControls() %>%
              clearMarkers() %>%
              clearImages() %>%
              clearShapes() %>%
              clearPopups() %>%
              clearWebGLHeatmap() %>%
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
              addPolylines(
                data = streets_c, opacity = opac, weight = 1,
                color = "#f53374"
              ) %>%
              addPolygons(
                data = st_union(isochrone), color = "#fc6b60",
                opacity = 0.2
              ) %>%
              addMiniMap(tiles = tiles[[1]], toggleDisplay = TRUE) %>%
              htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
            for (i in seq_len(length(dist))) {
              map_base <- map_base %>%
                addWebGLHeatmap(
                  data = splits[[i]],
                  size = as.numeric(dist[i]), units = "m",
                  intensity = 0.1, gradientTexture = "skyline",
                  alphaRange = 1, opacity = 0.8
                )
            }
            map <- map_base %>%
              addPolylines(
                data = streets_c, opacity = opac, weight = 1,
                color = "#f53374"
              ) %>%
              addAwesomeMarkers(
                lat = lat,
                lng = lon,
                icon = icon.fa, label = "Starting point"
              )
          }
        }
      }
    }
  } else {
    if (input$radio_type_cluster != "Drivetime") {
      data("world.cities")
      points <- world.cities[
        world.cities$name == input$city_cluster,
        c("lat", "long")
      ]
      # if not set the view to munich
      if (nrow(points) == 0) {
        map <- leafletProxy("mymap") %>%
          clearControls() %>%
          clearMarkers() %>%
          clearImages() %>%
          clearShapes() %>%
          clearPopups() %>%
          clearWebGLHeatmap() %>%
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
      } else {
        # if the city can be found set the viewpoint to the city
        map <- leafletProxy("mymap") %>%
          clearControls() %>%
          clearMarkers() %>%
          clearImages() %>%
          clearShapes() %>%
          clearPopups() %>%
          clearWebGLHeatmap() %>%
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
          setView(points$long[1], points$lat[1], zoom = 11) %>%
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
    } else {
      map <- leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        clearPopups() %>%
        clearWebGLHeatmap() %>%
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
        setView(data$longitude, data$latitude, zoom = 11) %>%
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
  }
  map
})
