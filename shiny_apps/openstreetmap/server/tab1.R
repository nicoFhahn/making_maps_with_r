# get location data
get_location_data <- eventReactive(input$bttn_data_locations, {
  if (input$key_locations != "" && input$location != "") {
    # check if multiple locations have been entered
    if (!is.null(input$switch_cities)) {
      # get all the location names
      cities <- trimws(unlist(
        stringr::str_split(input$location, "(\\,|\\;)")
      ))
      if (!input$switch_cities) {
        # if data for multiple location should not be downloaded use the first
        # location name
        cities <- cities[1]
      }
    } else {
      # use the one name provided
      cities <- input$location
    }
    if (!is.null(input$value)) {
      # if the user entered a value use this query
      # check if only one value has been entered
      if (length(input$value) == 1) {
        # if yes check if the value appears in multiple keys
        keys2 <- unlist(
          future_map(keys, function(x, ...) {
            input$value %in% x
          })
        )
        keys2 <- names(keys2[as.numeric(keys2) > 0])
        coords <- future_map(keys2, download_key_data,
          cities = cities, value = input$value
        )
      } else {
        # if multiple values have been entered check which ones appear
        # in multiple keys
        keys2 <- future_map(input$value, function(x, ...) {
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
            "value" = input$value[[x]]
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
          class(x[1, 3])[1] == "factor"
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
        key = input$key_locations
      )
    }
  }
  # return the data
  coords[!duplicated(coords$osm_id), ]
})

# plot a map from the location data
observeEvent(list(input$bttn_data_locations, input$names_locations), {
  # try to get the data
  if (is.null(input$value)) {
    map <- leafletProxy("mymap") %>%
      clearControls() %>%
      clearMarkers() %>%
      clearImages() %>%
      clearShapes() %>%
      clearPopups() %>%
      clearMarkerClusters() %>%
      clearTiles() %>%
      addProviderTiles("OpenStreetMap",
                       group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Esri.WorldImagery") %>%
      addProviderTiles("CartoDB.Positron",
                       group = "CartoDB.Positron") %>%
      addProviderTiles("CartoDB.DarkMatter",
                       group = "CartoDB.DarkMatter") %>%
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
  data <- try(get_location_data())
  # if it fails give it a lot of time and try again
  if (class(data) == "try-error") {
    Sys.sleep(5)
    data <- try(get_location_data())
    # if it still fails use a NODATA data.frame
    if (class(data) == "try-error") {
      data <- data.frame(
        longitude = 11.577779,
        latitude = 48.137168,
        name = "NODATA"
      )
    }
  }
  # check if data was found
  if (!any(c("NODATA" %in% levels(data$name), "NODATA" %in% data$name))) {
    if (!is.null(input$names_locations)) {
      data <- data[data$name %in% input$names_locations, ]
    }
    # check if data for more than one city was downloaded
    if (length(unique(data$city)) > 1) {
      # find the centroids of each city
      centroid <- ddply(data, .(city), summarize,
        longitude = mean(longitude),
        latitude = mean(latitude)
      )
      # get the most southern and most northern centroid
      distance <- centroid[centroid$latitude == min(centroid$latitude) |
        centroid$latitude == max(centroid$latitude), 2:3]
      # calculate the distance between these centroids
      distance_centroids <- geosphere::distHaversine(
        distance[1, ],
        distance[2, ]
      )
      # set a value for the zoom depending on the distance.
      # the higher the distance, the further zoomed out the map is
      zoom <- ifelse(distance_centroids > 500000, 7,
        ifelse(distance > 350000, 8,
          ifelse(distance > 200000, 9,
            ifelse(distance > 100000, 10, 11)
          )
        )
      )
      # get the centroid of these for the setView part
      centroid <- data.frame(
        latitude = mean(distance$latitude),
        longitude = mean(distance$longitude)
      )
    } else {
      # if only data for one city has been download use the centroid
      # as the setView point
      centroid <- data.frame(
        latitude = mean(data$latitude),
        longitude = mean(data$longitude)
      )
      # and set the zoom to 11
      zoom <- 11
    }

    Encoding(data$name) <- "UTF-8"
    Encoding(data$addr.street) <- "UTF-8"
    data$addr.street[is.na(data$addr.street)] <- ""
    map <- leafletProxy("mymap") %>%
      clearControls() %>%
      clearMarkers() %>%
      clearImages() %>%
      clearShapes() %>%
      clearPopups() %>%
      clearMarkerClusters() %>%
      clearTiles() %>%
      addProviderTiles("OpenStreetMap",
                       group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Esri.WorldImagery") %>%
      addProviderTiles("CartoDB.Positron",
                       group = "CartoDB.Positron") %>%
      addProviderTiles("CartoDB.DarkMatter",
                       group = "CartoDB.DarkMatter") %>%
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
    }") %>%
      # add the markers
      addMarkers(
        lng = data$longitude, lat = data$latitude,
        label = paste(
          "Name: ", data$name, "<br>",
          "Street: ", data$addr.street
        ) %>%
          future_map(htmltools::HTML),
        clusterOptions = markerClusterOptions(),
        clusterId = "pointsCluster"
      ) #%>%
      #addEasyButton(easyButton(
      #  states = list(
      #    easyButtonState(
      #      stateName = "unfrozen-markers",
      #      icon = "ion-toggle",
      #      title = "Freeze Clusters",
      #      onClick = JS("
      #    function(btn, map) {
      #      var clusterManager =
      #        map.layerManager.getLayer('cluster', 'pointsCluster');
      #      clusterManager.freezeAtZoom();
      #      btn.state('frozen-markers');
      #    }")
      #    ),
      #    easyButtonState(
      #      stateName = "frozen-markers",
      #      icon = "ion-toggle-filled",
      #      title = "UnFreeze Clusters",
      #      onClick = JS("
      #    function(btn, map) {
      #      var clusterManager =
      #        map.layerManager.getLayer('cluster', 'pointsCluster');
      #      clusterManager.unfreeze();
      #      btn.state('unfrozen-markers');
      #    }")
      #    )
      #  )
      # ))
  } else {
    # if no data was found check if the city can be found
    data("world.cities")
    points <- world.cities[
      world.cities$name == input$location,
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
        clearMarkerClusters() %>%
        clearTiles() %>%
        addProviderTiles("OpenStreetMap",
                         group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery",
                         group = "Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.Positron",
                         group = "CartoDB.Positron") %>%
        addProviderTiles("CartoDB.DarkMatter",
                         group = "CartoDB.DarkMatter") %>%
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
    } else {
      # if the city can be found set the viewpoint to the city
      map <- leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        clearPopups() %>%
        clearMarkerClusters() %>%
        clearTiles() %>%
        
        addLayersControl(
          baseGroups = names(tiles), position = c("topleft")
        ) %>%
        addFullscreenControl() %>%
        leaflet.extras::addSearchOSM(
          options = searchOptions(hideMarkerOnCollapse = TRUE, zoom = 11)
          ) %>%
        setView(data$longitude, data$latitude, zoom = 11) %>%
        addProviderTiles("OpenStreetMap",
                         group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery",
                         group = "Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.Positron",
                         group = "CartoDB.Positron") %>%
        addProviderTiles("CartoDB.DarkMatter",
                         group = "CartoDB.DarkMatter") %>%
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

get_isochrone_data <- eventReactive(input$bttn_data_drivetime, {
  # check if all the input data is there
  if ((((input$latitude != "" && input$longitude != "") ||
    input$address_data != "") ||
    input$file_upload == TRUE) &&
    (input$drivetime != "" || input$radius != "") &&
    input$key_drivetime != "") {
    # turn latitude and longitude into numeric values
    if (!input$file_upload) {
      if (input$format_data != "Address") {
        lat <- suppressWarnings(as.numeric(stringr::str_replace(
          input$latitude, "\\,", "\\."
        )))
        lon <- suppressWarnings(as.numeric(stringr::str_replace(
          input$longitude, "\\,", "\\."
        )))
        point <- data.frame(
          "lat" = lat,
          "lon" = lon
        )
      } else {
        addr <- input$address_data
        coded <- try(stplanr::geo_code(addr), silent = TRUE)
        if (class(coded) != "try-error") {
          lat <- coded[2]
          lon <- coded[1]
          point <- data.frame(
            lat = lat,
            lon = lon
          )
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
    } else {
      # read excel file
      dataset <- read.xlsx(input$upload_data$datapath)
      # check if there are lat and lon cols and if yes get the values
      if (all(c("lat", "lon") %in% colnames(dataset))) {
        point <- data.frame(
          lat = as.numeric(dataset$lat),
          lon = as.numeric(dataset$lon)
        )
        # remove NA rows
        point <- point[complete.cases(point), ]
        # check if there are points and a numeric values was used for radius
        if (nrow(point) == 0 ||
          (input$radio_analysis == "Radius" &&
            is.na(as.numeric(input$radius)))) {
          return(
            data.frame(
              latitude = 48.137168,
              longitude = 11.577779,
              name = "NODATA"
            )
          )
        }
        # check if address col exists
      } else if ("address" %in% colnames(dataset)) {
        # get the address
        addr <- dataset$address
        # remove na
        addr <- addr[complete.cases(addr)]
        # geocode the adresses
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
        # bind them together
        point <- do.call(rbind, point)
        # use only non-NA rows
        dataset <- dataset[rowSums(is.na(point)) == 0, ]
        point <- point[rowSums(is.na(point)) == 0, ]
        if (nrow(point) == 0 ||
          (input$radio_analysis == "Radius" &&
            is.na(as.numeric(input$radius)))) {
          # check if there are points and the radius is numeric
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
    lat <- point$lat
    lon <- point$lon
    if (length(lat) > 1) {
      future::plan(future::multiprocess)
    }
    # check that they are not NA meaning a numeric value has been entered
    if (!any(is.na(lat)) && !any(is.na(lon))) {
      # check if a drivetime analysis should be done
      if (input$radio_analysis == "Drivetime") {
        # turn accuracy and drivetime into numeric values
        acc <- as.numeric(input$accuracy)
        drivetime <- as.numeric(input$drivetime)
        # save the coordinates in a named vector
        # save the coordinates in a named vector
        coords <- lapply(seq_len(length(lat)), function(x, ...) {
          c("lon" = lon[x], "lat" = lat[x])
        })
        # download isochrone data
        isochrone <- lapply(coords, function(x, ...) {
          osrmIsochrone(
            loc = c(x[1], x[2]),
            breaks = seq(0, drivetime, length.out = 2),
            res = acc
          ) %>%
            st_as_sf()
        })
      } else {
        # if radial analysis was done save the coords in a data.frame
        coords <- lapply(seq_len(length(lat)), function(x, ...) {
          data.frame("lon" = lon[x], "lat" = lat[x])
        })
        input_points <- lapply(coords, function(x, ...) {
          x %>%
            st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
            st_cast("POINT") %>%
            st_transform(3035)
        })
        # define five different radii
        radii <- seq(0, as.numeric(input$radius), length.out = 6)
        # calculate the five radii
        isochrone <- lapply(input_points, function(x, ...) {
          lapply(radii[2:6], function(y, ...) {
            st_buffer(x, dist = y) %>%
              st_transform(4326)
          })
        })
        # bind the data together
        isochrone <- lapply(isochrone, function(x) {
          do.call(rbind, x)
        })
      }
      # if (class(isochrone)[1] != "try-error") {
      if (input$radio_analysis != "Drivetime") {
        # set labels for radial analysis
        labs <- paste(paste(radii[1:5], radii[2:6], sep = " - "), "meters")
        # order the labs
        labs <- ordered(labs, levels = labs)
      }
      isochrone <- lapply(isochrone, function(x, ...) {
        if (input$radio_analysis == "Drivetime") {
          labs <- paste(x$min, "-", x$max, "minutes")
        }
        x$labs <- labs
        x
      })
      # get a bbox around the biggest isochrone/circle
      cities <- lapply(isochrone, function(x, ...) {
        sf::st_bbox(x[nrow(x), ]$geometry)
      })
      # download data for the bbox
      # check if a value was selected
      if (!is.null(input$value_drivetime)) {
        # check if only one value has been entered
        if (length(input$value_drivetime) == 1) {
          # if yes check if the value appears in multiple keys
          keys2 <- unlist(
            lapply(keys, function(x, ...) {
              input$value_drivetime %in% x
            })
          )
          keys2 <- names(keys2[as.numeric(keys2) > 0])
          points <- future_map(keys2, download_key_data,
            cities = cities, value = input$value_drivetime
          )
        } else {
          # if multiple values have been entered check which ones appear
          # in multiple keys
          keys2 <- lapply(input$value_drivetime, function(x, ...) {
            keys2 <- unlist(
              lapply(keys, function(y, ...) {
                x %in% y
              })
            )
            names(keys2[as.numeric(keys2) > 0])
          })

          keys2 <- lapply(seq_len(length(keys2)), function(x, ...) {
            keys2[[x]] <- data.frame(
              "key" = keys2[[x]],
              "value" = input$value_drivetime[[x]]
            )
          })
          keys2 <- do.call(rbind, keys2)
          uniq <- as.character(unique(keys2$key))
          points <- future_map(seq_len(length(uniq)), function(x, ...) {
            value <- keys2[keys2$key == uniq[x], ]$value
            key <- uniq[x]
            download_key_data(cities = cities, key = key, value = value)
          })
        }
        if (length(points) > 1) {
          no_data <- unlist(lapply(points, function(x) {
            class(x[1, 3])[1] == "factor" || nrow(x) == 0
          }))
          index <- which(no_data %in% TRUE)
          if (length(index) > 0) {
            points <- rlist::list.remove(points, index)
          }
        }
        if (length(points) == 0 ||
            length(points) == 1 && nrow(points[[1]]) == 0) {
          return("No data found for the area")
        }
        all_cols <- unique(unlist(lapply(points, colnames)))
        # if columns are missing for some of the data, add them as NA
        points <- lapply(
          points, function(data2, all_cols, ...) {
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
        points <- do.call(
          rbind,
          lapply(
            points,
            function(x) x[match(all_cols, names(x))]
          )
        )
      } else {
        points <- download_key_data(
          cities = cities,
          key = input$key_drivetime
        )
      }
      # check if data was found
      if (!"NODATA" %in% points$name) {
        # check which points are in the polygons
        contained <- lapply(isochrone, function(x, ...) {
          sf::st_contains(x$geometry, points$geometry)
        })
        # add a drivetime/radius column
        new_col <- input$radio_analysis
        points[, new_col] <- NA
        contained_iso <- lapply(contained, function(x, ...) {
          lapply(x, length)
        })
        isochrone <- lapply(
          seq_len(length(contained_iso)),
          function(x, ...) {
            isochrone[[x]][contained_iso[[x]] != 0, ]
          }
        )
        if (length(contained) == 1) {
          if (input$radio_analysis != "Drivetime") {
            points[unlist(contained[[1]][5]), new_col] <-
              as.character(labs[5])
            points[unlist(contained[[1]][4]), new_col] <-
              as.character(labs[4])
            points[unlist(contained[[1]][3]), new_col] <-
              as.character(labs[3])
            points[unlist(contained[[1]][2]), new_col] <-
              as.character(labs[2])
            points[unlist(contained[[1]][1]), new_col] <-
              as.character(labs[1])
            points <- points[!is.na(points[, new_col])[, new_col], ]
            temp <- points[, new_col]
            temp$geometry <- NULL
            temp[, new_col] <- ordered(temp[, new_col], levels = levels(labs))
            points[, new_col] <- temp[, new_col]
          } else {
            points <- points[unlist(contained), ]
          }
        } else {
          # get the contained points
          points <- lapply(seq_len(length(isochrone)), function(i, z, ...) {
            x <- contained[[i]]
            y <- isochrone[[i]]
            matches <- lapply(x, function(x) x)
            matches <- lapply(1:length(matches), function(i) {
              if (i == 1) {
                matches[[i]]
              } else {
                matches[[i]][!matches[[i]] %in% matches[[i - 1]]]
              }
            })
            p <- lapply(1:length(matches), function(a, ...) {
              match <- matches[[a]]
              if (length(match) > 0) {
                z[match, new_col] <- as.character(y$labs[a])
                return(z[match, ])
              } else {
                NULL
              }
            })
            return(p)
          },
          z = points
          )
          # bind the points together
          points <- lapply(points, function(x) {
            do.call(rbind, x)
          })
          points <- do.call(rbind, points)
        }
        # bind the points together
        coords <- do.call(rbind, coords)
        # get the biggest isochrone from each starting point
        biggest <- lapply(isochrone, function(x) x[nrow(x), ])
        biggest <- lapply(seq_len(length(biggest)), function(x) {
          biggest[[x]][1, "lon_start"] <- coords[x, 1]
          biggest[[x]][1, "lat_start"] <- coords[x, 2]
          biggest[[x]]
        })
        biggest2 <- do.call(rbind, biggest)
        points <- sf::st_intersection(biggest2, points)
        points$distance <- geosphere::distHaversine(
          matrix(c(
            points$lon_start,
            points$lat_start
          ),
          ncol = 2
          ),
          st_coordinates(points)
        )
        # bind the points together
        # bind the points together
        isochrone <- do.call(rbind, isochrone)
        # remove duplicates (a building in a 100m radius also shows up
        # in 200 m radius for example)
        # rare case: points are found in the bbarea but are not in the
        # radius.
        if (is.null(points)) {
          return("No data found for the area")
        }
        if (input$format_data == "Address" || input$file_upload) {
          if (input$format_data == "Address" && !input$file_upload) {
            dataset <- as.data.frame(matrix(input$address_data))
            colnames(dataset) <- "address"
          }
          if ("address" %in% colnames(dataset)) {
            coords2 <- cbind(coords, dataset)
            coords2$a <- paste(coords$lon, coords$lat, sep = "")
            coords2 <- coords2[coords2$lat != 0 & coords2$lon != 0, ]
            points2 <- points
            points2$a <- paste(points$lon_start, points$lat_start, sep = "")
            points2$geometry <- NULL
            coords3 <- coords2[, !duplicated(colnames(coords2))]
            points2 <- plyr::join(coords3, points2, by = "a")
            points2 <- points2[!is.na(points2$distance), ]
            points <- cbind(points2$address, points)
            colnames(points)[1] <- "address"
          }
        }
        # return everything important for the plotting later as a list
        future::plan(future::sequential)
        points <- points[!duplicated(points$osm_id), ]
        return(
          list(
            isochrone = isochrone,
            coords = coords,
            points = points
          )
        )
        # else return no data found for the area
      } else {
        return("No data found for the area")
      }
    } else {
      # or enter numeric values only
      return("No data found for the area")
    }
  }
})

# plot a map from the drivetime/isochrone data
observeEvent(list(
  input$bttn_data_drivetime, input$names_drivetime
), {
  # try to get the data
  if (is.null(input$value_drivetime)) {
    map <- leafletProxy("mymap") %>%
      clearControls() %>%
      clearMarkers() %>%
      clearImages() %>%
      clearShapes() %>%
      clearPopups() %>%
      clearMarkerClusters() %>%
      clearTiles() %>%
      addProviderTiles("OpenStreetMap",
                       group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Esri.WorldImagery") %>%
      addProviderTiles("CartoDB.Positron",
                       group = "CartoDB.Positron") %>%
      addProviderTiles("CartoDB.DarkMatter",
                       group = "CartoDB.DarkMatter") %>%
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
  data <- try(get_isochrone_data())
  # if it fails, try again
  if (class(data) == "try-error") {
    Sys.sleep(5)
    data <- try(get_isochrone_data())
    if (class(data) == "try-error") {
      # else return a NODATA frame with the lat and long
      data <- data.frame(
        longitude = as.numeric(
          stringr::str_replace(
            input$longitude, "\\,", "\\."
          )
        ),
        latitude = as.numeric(
          stringr::str_replace(
            input$latitude, "\\,", "\\."
          )
        ),
        name = "NODATA"
      )
    }
  }
  # get the points of the locations
  if (class(data) == "character") {
    if (!is.na(
      as.numeric(stringr::str_replace(input$longitude, "\\,", "\\."))
    ) &&
      !is.na(
        as.numeric(stringr::str_replace(input$latitude, "\\,", "\\."))
      )
    ) {
      map <- leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        clearPopups() %>%
        clearMarkerClusters() %>%
        clearTiles() %>%
        
        addProviderTiles("OpenStreetMap",
                         group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery",
                         group = "Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.Positron",
                         group = "CartoDB.Positron") %>%
        addProviderTiles("CartoDB.DarkMatter",
                         group = "CartoDB.DarkMatter") %>%
        addLayersControl(
          baseGroups = names(tiles), position = c("topleft")
        ) %>%
        addFullscreenControl() %>%
        leaflet.extras::addSearchOSM(
          options = searchOptions(hideMarkerOnCollapse = TRUE, zoom = 11)
          ) %>%
        setView(
          as.numeric(
            stringr::str_replace(input$longitude, "\\,", "\\.")
          ),
          as.numeric(
            stringr::str_replace(input$latitude, "\\,", "\\.")
          ),
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
    } else {
      map <- leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        clearPopups() %>%
        clearMarkerClusters() %>%
        clearTiles() %>%
        
        addProviderTiles("OpenStreetMap",
                         group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery",
                         group = "Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.Positron",
                         group = "CartoDB.Positron") %>%
        addProviderTiles("CartoDB.DarkMatter",
                         group = "CartoDB.DarkMatter") %>%
        addLayersControl(
          baseGroups = names(tiles), position = c("topleft")
        ) %>%
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
  }
  points <- data$points
  if (class(data) != "character" && class(data) != "data.frame") {
    if (!is.null(input$names_drivetime)) {
      points <- points[points$name %in% input$names_drivetime, ]
    }
    # get the color palette
    isochrone <- data$isochrone
    actual_levels <- levels(isochrone$labs)[
      levels(isochrone$labs) %in% isochrone$labs
    ]
    isochrone$labs <- as.character(isochrone$labs)
    isochrone$labs <- ordered(isochrone$labs, levels = actual_levels)
    pal <- colorFactor(
      viridis::viridis(length(unique(data$isochrone$labs)),
        direction = -1
      ),
      isochrone$labs
    )
    # get the isochrone
    isochrone <- data$isochrone
    # get the coordinates
    coords <- data$coords
    # check if data frame is returned / radius analysis
    if (class(coords) == "data.frame") {
      # save the coords in a vector
      coords <- as.matrix(coords)
      # add the title
      title <- "Radius"
      # sort the data.frame so the smallest radius gets colored first
      poly_data <- isochrone[order(isochrone$labs, decreasing = TRUE), ]
    } else {
      # add a title
      title <- "Drivetime"
      # save the data
      poly_data <- isochrone
    }
    # plot the map
    if (length(unique(poly_data$labs)) < 2) {
      Encoding(points$name) <- "UTF-8"
      Encoding(points$addr.street) <- "UTF-8"
      points$addr.street[is.na(points$addr.street)] <- ""
      map <- leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        clearPopups() %>%
        clearMarkerClusters() %>%
        clearTiles() %>%
        
        addProviderTiles("OpenStreetMap",
                         group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery",
                         group = "Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.Positron",
                         group = "CartoDB.Positron") %>%
        addProviderTiles("CartoDB.DarkMatter",
                         group = "CartoDB.DarkMatter") %>%
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
    }") %>%
        addPolygons(
          data = poly_data,
          color = "#E84A5F"
        ) %>%
        addLegend(
          data = isochrone,
          colors = "#E84A5F",
          labels = unique(poly_data$labs),
          title = title,
          opacity = 1,
          position = "bottomleft"
        ) %>%
        addAwesomeMarkers(
          lat = coords[, 2], lng = coords[, 1],
          label = "Starting point", icon = icon.fa
        ) %>%
        addMarkers(
          lat = points$latitude, lng = points$longitude,
          label = paste(
            "Name: ", points$name, "<br>",
            "Distance from location. ",
            round(points$distance, 1), " meters", "<br>",
            "Street: ", points$addr.street
          ) %>%
            future_map(htmltools::HTML),
          clusterOptions = markerClusterOptions(),
          clusterId = "pointsCluster"
        )  #%>%
      #addEasyButton(easyButton(
      #  states = list(
      #    easyButtonState(
      #      stateName = "unfrozen-markers",
      #      icon = "ion-toggle",
      #      title = "Freeze Clusters",
      #      onClick = JS("
      #    function(btn, map) {
      #      var clusterManager =
      #        map.layerManager.getLayer('cluster', 'pointsCluster');
      #      clusterManager.freezeAtZoom();
      #      btn.state('frozen-markers');
      #    }")
      #    ),
      #    easyButtonState(
      #      stateName = "frozen-markers",
      #      icon = "ion-toggle-filled",
      #      title = "UnFreeze Clusters",
      #      onClick = JS("
      #    function(btn, map) {
      #      var clusterManager =
      #        map.layerManager.getLayer('cluster', 'pointsCluster');
      #      clusterManager.unfreeze();
      #      btn.state('unfrozen-markers');
      #    }")
      #    )
      #  )
      # ))
    } else {
      Encoding(points$name) <- "UTF-8"
      Encoding(points$addr.street) <- "UTF-8"
      points$addr.street[is.na(points$addr.street)] <- ""
      map <- leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        clearPopups() %>%
        clearMarkerClusters() %>%
        clearTiles() %>%
        
        addProviderTiles("OpenStreetMap",
                         group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery",
                         group = "Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.Positron",
                         group = "CartoDB.Positron") %>%
        addProviderTiles("CartoDB.DarkMatter",
                         group = "CartoDB.DarkMatter") %>%
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
    }") %>%
        addPolygons(
          data = poly_data,
          color = ~ pal(labs)
        ) %>%
        addLegend(
          data = isochrone,
          pal = pal,
          values = ~labs,
          title = title,
          opacity = 1,
          position = "bottomleft"
        ) %>%
        addAwesomeMarkers(
          lat = coords[, 2], lng = coords[, 1],
          label = "Starting point", icon = icon.fa
        ) %>%
        addMarkers(
          lat = points$latitude, lng = points$longitude,
          label = paste(
            "Name: ", points$name, "<br>",
            "Distance from location. ",
            round(points$distance, 1), " meters", "<br>",
            "Street: ", points$addr.street
          ) %>%
            future_map(htmltools::HTML),
          clusterOptions = markerClusterOptions(),
          clusterId = "pointsCluster"
        )  #%>%
      #addEasyButton(easyButton(
      #  states = list(
      #    easyButtonState(
      #      stateName = "unfrozen-markers",
      #      icon = "ion-toggle",
      #      title = "Freeze Clusters",
      #      onClick = JS("
      #    function(btn, map) {
      #      var clusterManager =
      #        map.layerManager.getLayer('cluster', 'pointsCluster');
      #      clusterManager.freezeAtZoom();
      #      btn.state('frozen-markers');
      #    }")
      #    ),
      #    easyButtonState(
      #      stateName = "frozen-markers",
      #      icon = "ion-toggle-filled",
      #      title = "UnFreeze Clusters",
      #      onClick = JS("
      #    function(btn, map) {
      #      var clusterManager =
      #        map.layerManager.getLayer('cluster', 'pointsCluster');
      #      clusterManager.unfreeze();
      #      btn.state('unfrozen-markers');
      #    }")
      #    )
      #  )
      # ))
    }
  } else {
    # plot nothing
    map <- NULL
  }
  # return the map
  map
})
