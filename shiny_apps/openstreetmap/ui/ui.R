############## UI ##############
ui <- fillPage(
  title = "Geospatial analysis",
  # style settings
  tags$head(
    tags$style(
      type = "text/css",
      "html, body {width:100%;height:100%}",
      HTML(
        '#sidebar {
        background-color: #4b4636;
        font-family: "Georgia";
        color: #ffa451;
        }
        body, label, input, button, select {
        font-family: "Georgia";
        }
        #tabs{
        font-family: "Georgia";
        }
        #text_error {
        color: red;
        font-size:20px
        }
        #text_error_drivetime {
        color: red;
        font-size:20px
        }
        #text_error_cluster {
        color: red;
        font-size:20px
        }
        #OSM {
        font-size:10px
        }
        #testtest {
        color: red;
        font-size:20px
        }
        .modebar{
        display: none !important;
        }'
      ),
      setBackgroundColor(color = "#d5d1c5")
    ),
    # load scripts and styles
    includeCSS("styles.css"),
    includeScript("gomap.js")
  ),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  # the map
  leafletOutput(
    "mymap",
    width = "100%",
    height = "100vh"
  ),
  # absolute panel
  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = TRUE,
    top = 40,
    left = "auto",
    right = 20,
    bottom = "auto",
    width = "25%",
    height = "auto",
    h3(
      "Geospatial analysis",
      style = "font-size:1.25em;"
    ),
    # Radio buttons
    radioGroupButtons(
      "goal",
      "What do you want to do?",
      choices = c(
        "Download <br> geospatial data",
        "Location <br> analysis"
      ),
      selected = "Download <br> geospatial data",
      justified = TRUE
    ),
    # First tab
    conditionalPanel(
      "input.goal == 'Download <br> geospatial data'",
      # radio buttuns
      radioGroupButtons(
        inputId = "radio_type",
        label = "What kind of data?",
        choices = c("Cities/Districts", "Drivetime/Radius"),
        selected = "Cities/Districts",
        justified = TRUE
      ),
      # first first tab
      conditionalPanel(
        "input.radio_type == 'Cities/Districts'",
        textInput(
          "location",
          "Enter city name:",
          width = "100%"
        ),
        # ui
        uiOutput("ui_multiple_locations"),
        # picker
        pickerInput(
          inputId = "key_locations",
          label = "Select key:",
          choices = NULL,
          width = "100%",
          options = list(
            title = "Please select a key:"
          ),
          multiple = FALSE
        ),
        # ui
        uiOutput("ui_value_locations"),
        uiOutput("ui_names_locations"),
        # button
        conditionalPanel(
          "input.location != '' && input.key_locations != ''",
          actionBttn(
            "bttn_data_locations",
            label = "Generate data",
            style = "fill"
          )
        ),
        p(),
        # some more ui stuff
        uiOutput("ui_switch_locations"),
        uiOutput("ui_download_locations"),
        uiOutput("ui_error_locations")
      ),
      # first second tab
      conditionalPanel(
        "input.radio_type == 'Drivetime/Radius'",
        # file upload
        prettySwitch(
          "file_upload",
          label = "Download data for multiple locations?",
          status = "success",
          fill = TRUE
        ),
        # conditional panel
        conditionalPanel(
          "input.file_upload == false",
          radioGroupButtons(
            inputId = "format_data",
            label = "Choose address format:",
            choices = c("Lat/Lon", "Address"),
            selected = "Lat/Lon",
            justified = TRUE
          ),
          # conditional panel for latitude and longitude
          conditionalPanel(
            "input.format_data != 'Address'",
            column(
              width = 6,
              textInput(
                "latitude",
                "Enter latitude:",
                width = "100%",
                placeholder = 48.133326
              )
            ),
            column(
              width = 6,
              textInput(
                "longitude",
                "Enter longitude:",
                width = "100%",
                placeholder = 11.522423
              )
            )
          ),
          # conditional panel for address
          conditionalPanel(
            "input.format_data == 'Address'",
            textInput(
              "address_data",
              label = "Enter address:",
              placeholder = "Street Number, ZIP City",
              width = "100%"
            )
          )
        ),
        # aaaand another conditional panel
        conditionalPanel(
          "input.file_upload != false",
          p("The dataset should have a column named lat and one named lon
            or one named address."),
          fileInput(
            "upload_data",
            "Upload xlsx file:",
            accept = ".xlsx",
            width = "100%"
          )
        ),
        # radio buttons
        radioGroupButtons(
          inputId = "radio_analysis",
          label = "Drivetime or Radius?",
          choices = c("Drivetime", "Radius"),
          selected = "Drivetime",
          justified = TRUE
        ),
        # i dont even know where i am
        conditionalPanel(
          "input.radio_analysis == 'Drivetime'",
          column(
            width = 6,
            pickerInput(
              inputId = "drivetime",
              label = "Minutes:",
              choices = NULL,
              width = "100%",
              options = list(
                title = "Nothing selected"
              ),
              multiple = FALSE
            )
          ),
          column(
            width = 6,
            # picker
            pickerInput(
              inputId = "accuracy",
              label = "Accuracy:",
              choices = seq(10, 100, 10),
              width = "100%",
              selected = 30,
              multiple = FALSE
            )
          )
        ),
        # conditioner
        conditionalPanel(
          "input.radio_analysis == 'Radius'",
          textInput(
            "radius",
            "Enter radius in meters:",
            width = "100%",
            placeholder = 500
          )
        ),
        # picker
        pickerInput(
          "key_drivetime",
          "Select key:",
          choices = NULL,
          width = "100%",
          options = list(
            title = "Please select a key:"
          ),
          multiple = FALSE
        ),
        # ui
        uiOutput("ui_value_drivetime"),
        uiOutput("ui_names_drivetime"),
        uiOutput("button_drivetime"),
        # is this still the first tab? fuck me
        conditionalPanel(
          "(((input.latitude != '' && input.longitude != '') ||
          input.address_data != '') ||
          input.file_upload != false)
          && (input.drivetime != ''|| input.radius != '')
          && input.key_drivetime != ''",
          actionBttn(
            "bttn_data_drivetime",
            label = "Generate data",
            style = "fill"
          )
        ),
        p(),
        # ui outputs
        uiOutput("ui_switch_drivetime"),
        uiOutput("ui_download_drivetime"),
        uiOutput("ui_error_drivetime")
      )
    ),
    # second tab yay
    conditionalPanel(
      "input.goal == 'Location <br> analysis'",
      # radio buttons
      radioGroupButtons(
        inputId = "radio_type_cluster",
        label = "What kind of locations?",
        choices = c("Cities/Districts", "Drivetime"),
        selected = "Cities/Districts",
        justified = TRUE
      ),
      # conditiónal
      conditionalPanel(
        "input.radio_type_cluster == 'Cities/Districts'",
        textInput(
          "city_cluster",
          label = "Enter city name:",
          width = "100%"
        ),
        # ui
        uiOutput("ui_multiple_locations_cluster")
      ),
      # cönditiönäl
      conditionalPanel(
        "input.radio_type_cluster == 'Drivetime'",
        prettySwitch(
          "file_upload_cluster",
          label = "Analyze multiple locations?",
          status = "success",
          fill = TRUE
        ),
        # condi
        conditionalPanel(
          "input.file_upload_cluster == false",
          radioGroupButtons(
            inputId = "radio_format",
            label = "Choose address format:",
            choices = c("Lat/Lon", "Address"),
            selected = "Lat/Lon",
            justified = TRUE
          )
        ),
        # cond
        conditionalPanel(
          "input.file_upload_cluster == false &&
        input.radio_format == 'Lat/Lon'",
          # lat and lon
          column(
            width = 6,
            textInput(
              "latitude_cluster",
              label = "Enter latitude:",
              placeholder = 48.133326
            )
          ),
          column(
            width = 6,
            textInput(
              "longitude_cluster",
              label = "Enter longitude:",
              placeholder = 11.522423
            )
          )
        ),
        # con
        conditionalPanel(
          "input.file_upload_cluster == false &&
        input.radio_format != 'Lat/Lon'",
          textInput(
            "address_cluster",
            label = "Enter address:",
            placeholder = "Street Number, ZIP City",
            width = "100%"
          )
        ),
        # co
        conditionalPanel(
          "input.file_upload_cluster == true",
          p("The dataset should have one column named lat and one named lon or
          one named address."),
          fileInput(
            inputId = "file_cluster",
            label = "Upload xlsx file:",
            accept = ".xlsx",
            width = "100%"
          )
        ),
        # group buttons
        radioGroupButtons(
          "polygon_type_cluster",
          label = "Drivetime or Radius?",
          choices = c("Drivetime", "Radius"),
          selected = "Drivetime",
          justified = TRUE
        ),
        # c
        conditionalPanel(
          "input.polygon_type_cluster == 'Drivetime'",
          column(
            width = 6,
            pickerInput(
              "drivetime_cluster",
              label = "Minutes:",
              choices = NULL,
              options = list(
                title = "Nothing selected"
              ),
              multiple = FALSE,
              width = "100%"
            )
          ),
          column(
            width = 6,
            pickerInput(
              inputId = "accuracy_cluster",
              label = "Accuracy:",
              choices = seq(10, 100, 10),
              width = "100%",
              selected = 30,
              multiple = FALSE
            )
          )
        ),
        # co
        conditionalPanel(
          "input.polygon_type_cluster == 'Radius'",
          textInput(
            "radius_drivetime_cluster",
            label = "Enter radius in meters:",
            width = "100%",
            placeholder = "500"
          )
        )
      ),
      # picker
      pickerInput(
        "key_cluster",
        "Select key:",
        choices = NULL,
        width = "100%",
        options = list(
          title = "Please select a key:"
        ),
        multiple = FALSE
      ),
      # ui
      uiOutput("ui_value_cluster"),
      radioGroupButtons(
        "cluster_map_type",
        label = "Type of analysis:",
        choices = c("Clustering", "Heatmap", "Getis-Ord"),
        selected = "Clustering",
        justified = TRUE
      ),
      # con
      conditionalPanel(
        "input.cluster_map_type == 'Clustering'",
        column(
          width = 6,
          textInput(
            "cluster_distance",
            label = "Select epsilon value:",
            value = 0.02,
            width = "100%"
          )
        ),
        column(
          width = 6,
          textInput(
            "cluster_minpts",
            label = "Min. number of points:",
            value = 5,
            width = "100%"
          )
        ),
        pickerInput(
          "palette",
          label = "Select palette:",
          choices = NULL,
          selected = NULL,
          width = "100%"
        )
      ),
      # cond
      conditionalPanel(
        "input.cluster_map_type == 'Getis-Ord'",
        textInput(
          "getis_distance",
          label = "Choose distance in kilometers:",
          value = 0.5,
          width = "100%"
        ),
        pickerInput(
          "important",
          label = "Select up to 3 important types of locations:",
          choices = c(
            "Education",
            "Entertainment",
            "Healthcare",
            "Housing",
            "Leisure",
            "Office buildings",
            "Public transport",
            "Restaurants, bars and food shops"
          ),
          multiple = TRUE,
          options = list(
            "max-options" = 3
          ),
          width = "100%"
        ),
        uiOutput("ui_names_getis")
      ),
      # condi
      conditionalPanel(
        "((input.radio_type_cluster == 'Cities/Districts'
      && input.city_cluster != '' && input.cluster_map_type == 'Heatmap')
      || (input.radio_type_cluster == 'Cities/Districts'
      && input.city_cluster != '' && input.cluster_map_type == 'Clustering'
      && input.cluster_distance != '' && input.cluster_minpts != ''
      && input.key_cluster != '') ||
      (input.radio_type_cluster == 'Cities/Districts'
      && input.city_cluster != '' && input.cluster_map_type == 'Getis-Ord'
      && input.getis_distance != '' && input.key_cluster != '')) ||
      ((input.radio_type_cluster == 'Drivetime'
      && (((input.latitude_cluster != '' && input.longitude_cluster != '' &&
      input.radio_format != 'Address') ||
      input.address_cluster != '' && input.radio_format == 'Address') ||
      input.file_upload_cluster == true)
      && input.key_cluster != '') &&
      ((input.polygon_type_cluster == 'Drivetime' &&
      input.drivetime_cluster != '') ||
      (input.polygon_type_cluster == 'Radius' &&
      input.radius_drivetime_cluster != '')
      ) && ((input.cluster_map_type == 'Clustering' &&
      input.cluster_distance != '' && input.cluster_minpts != '') ||
      (input.cluster_map_type == 'Getis-Ord' && input.getis_distance != '') ||
      input.cluster_map_type == 'Heatmap'))",
        # button
        conditionalPanel(
          "input.cluster_map_type == 'Clustering' ||
                         input.cluster_map_type == 'Getis-Ord'",
          awesomeRadio("show_plz",
            label = "Show number of residents? (Germany only)",
            choices = c("Yes", "No"),
            selected = "No",
            inline = TRUE,
            width = "67%",
            status = "warning"
          )
        ),
        awesomeRadio("show_roads",
          label = "Show road network? (Germany only)",
          choices = c("Yes", "No"),
          selected = "No",
          inline = TRUE,
          width = "67%",
          status = "warning"
        ),
        actionBttn(
          inputId = "cluster_start",
          label = "Start analysis",
          style = "fill"
        )
      ),
      br(),
      # i hate this
      uiOutput("ui_error_cluster")
    ),
    # doonneee
    htmlOutput("OSM")
  )
)
