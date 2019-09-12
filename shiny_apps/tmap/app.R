#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# load packages
packages <- c(
  "colourpicker", "maps", "raster",
  "RColorBrewer", "shiny", "shinyjs", "shinydashboard", "shinyWidgets",
  "sf", "spData", "stplanr", "stringr", "tmap", "viridis"
)
not_installed <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(not_installed) > 1) {
  install.packages(not_installed, repos = "https://cran.rstudio.com/")
}
if (!"spDataLarge" %in% installed.packages()[, "Package"]) {
  install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
}
packages <- c(packages, "spDataLarge")
lapply(packages, library, character.only = TRUE)
# load shapefiles for bavaria
bavaria <- st_read("../../datasets/bavaria.shp")
# nicer colnames
colnames(bavaria) <- c(
  "Ort", "Art", "BIP je Einwohner", "Durchschnittsalter",
  "Siedlungsdichte", "Arbeitslosenquote",
  "Beschaeftigtenquote", "Haushaltseinkommen",
  "Studierende", "Bevoelkerungsentwicklung", "geometry"
)
# turn values into numeric
bavaria$`BIP je Einwohner` <- as.numeric(as.character(
  bavaria$`BIP je Einwohner`
))
bavaria$Durchschnittsalter <- as.numeric(as.character(
  bavaria$Durchschnittsalter
))
bavaria$Siedlungsdichte <- as.numeric(as.character(
  bavaria$Siedlungsdichte
))
bavaria$Arbeitslosenquote <- as.numeric(as.character(
  bavaria$Arbeitslosenquote
))
bavaria$Beschaeftigtenquote <- as.numeric(as.character(
  bavaria$Beschaeftigtenquote
))
bavaria$Haushaltseinkommen <- as.numeric(as.character(
  bavaria$Haushaltseinkommen
))
bavaria$Studierende <- as.numeric(as.character(
  bavaria$Studierende
))
bavaria$Bevoelkerungsentwicklung <- as.numeric(
  as.character(bavaria$Bevoelkerungsentwicklung)
)
# load europe elevation raster
europe_raster <- raster("../../datasets/elevation1x1_new.tif")
# load europe shapefiles
europe_shape <- read_sf("../../datasets/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
europe_shape <- europe_shape[europe_shape$CONTINENT == "Europe", ]
# cities above populaiton of 1 million
cities <- world.cities[world.cities$pop >= 1000000, ]
cities <- cities %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_cast("POINT")
cities <- st_intersection(cities, st_union(europe_shape))
europe_shape <- st_cast(europe_shape, "MULTILINESTRING")

# Define UI for application
ui <- dashboardPage(
  skin = "black",
  # make header
  header = dashboardHeader(
    title = "Making Maps in R",
    # add buttons for next and previous page
    tags$li(
      class = "dropdown",
      tags$li(
        class = "dropdown",
        actionBttn(
          inputId = "Previous",
          label = "Previous page",
          style = "minimal",
          color = "primary"
        )
      ),
      tags$li(
        class = "dropdown",
        actionBttn(
          inputId = "Next",
          label = "Next page",
          style = "minimal",
          color = "primary"
        )
      )
    )
  ),
  # add a sidebar
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      # add the first menu item and its subitems
      menuItem(
        startExpanded = TRUE,
        "tmap",
        menuSubItem(
          "Basics",
          tabName = "tmap_basics"
        ),
        menuSubItem(
          "Adding layers",
          tabName = "tmap_layers"
        ),
        menuSubItem(
          "Aesthetics",
          tabName = "tmap_aesthetics"
        ),
        menuSubItem(
          "Colour settings",
          tabName = "tmap_colour"
        ),
        menuSubItem(
          "Automatic breaks",
          tabName = "tmap_breaks"
        ),
        menuSubItem(
          "Layout",
          tabName = "tmap_layout"
        ),
        menuSubItem(
          "Facets",
          tabName = "tmap_facets"
        )
      )
    )
  ),
  # add the body
  body = dashboardBody(
    tags$head(tags$style(HTML("
                            #code_europe {
                              font-size: 20px;
                            }
                            "))),
    tabItems(
      tabItem(
        # first tab
        tabName = "tmap_basics",
        fillPage(
          fluidRow(
            column(
              width = 4,
              # add a picker input
              pickerInput(
                inputId = "tmap1",
                choices = NULL,
                width = "100%",
                options = list(
                  title = "Nothing selected"
                ),
                multiple = FALSE
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              # add the plot output
              plotOutput(
                "tmap1plot",
                width = "100%",
                height = "53em"
              )
            )
          )
        )
      ),
      tabItem(
        # second tab
        tabName = "tmap_layers",
        fillPage(
          title = "Adding layers",
          useShinyjs(),
          fluidRow(
            # add a button
            column(
              width = 3,
              actionBttn(
                "addlayer",
                label = "Add layer",
                style = "fill"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 9,
              # add the plot output
              plotOutput("europe_plot", height = "60em")
            ),
            column(
              width = 3,
              verbatimTextOutput("code_europe")
            )
          )
        )
      ),
      tabItem(
        # third tab
        "tmap_aesthetics",
        fillPage(
          title = "Aesthetics",
          fluidRow(
            box(
              width = 12,
              column(
                width = 2,
                # add a colour picker
                colourpicker::colourInput(
                  inputId = "fill_colour",
                  label = "Select fill colour:",
                  value = "#9DE0AD"
                )
              ),
              column(
                width = 2,
                # add a colour picker
                colourpicker::colourInput(
                  inputId = "lines_colour",
                  label = "Select lines colour:",
                  value = "#45ADA8"
                )
              ),
              column(
                width = 2,
                # add a slider
                sliderTextInput(
                  inputId = "alpha",
                  label = "Select fill opacity:",
                  choices = seq(0, 1, 0.05),
                  selected = 1
                )
              ),
              column(
                width = 2,
                # add a slider
                sliderTextInput(
                  inputId = "alpha_lines",
                  label = "Select lines opacity:",
                  choices = seq(0, 1, 0.05),
                  selected = 1
                )
              ),
              column(
                width = 2,
                # add a slider
                sliderTextInput(
                  inputId = "linewidth",
                  label = "Select linewidth:",
                  choices = 1:10
                )
              ),
              column(
                width = 2,
                # add a slider
                sliderTextInput(
                  inputId = "linetype",
                  label = "Select linetype:",
                  choices = 0:6,
                  selected = 1
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              # add the plot output
              plotOutput(
                "aesthetics_plot",
                width = "100%",
                height = "53em"
              )
            )
          )
        )
      ),
      tabItem(
        # third tab
        "tmap_colour",
        fillPage(
          title = "Color settings",
          fluidRow(
            box(
              width = 12,
              column(
                width = 3,
                # add a picker
                pickerInput(
                  "variable",
                  label = "Select variable:",
                  choices = NULL,
                  selected = NULL
                )
              ),
              column(
                width = 3,
                # add a text input
                textInput(
                  "breaks",
                  label = "Enter custom breaks:"
                )
              ),
              column(
                width = 1,
                # add a picker
                pickerInput(
                  "bins",
                  label = "Number of bins",
                  choices = 2:10
                )
              ),
              column(
                width = 2,
                # add radio buttons
                radioGroupButtons(
                  inputId = "custom_palette",
                  label = "Use custom palette?",
                  choices = c("Yes", "No"),
                  selected = "No",
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon("ok",
                      lib = "glyphicon"
                    )
                  )
                )
              ),
              column(
                width = 3,
                # add a conditional panel
                conditionalPanel(
                  "input.custom_palette == 'No'",
                  # add a picker
                  pickerInput(
                    "palette",
                    label = "Select palette:",
                    choices = NULL,
                    selected = NULL,
                    width = "100%"
                  )
                ),
                # add a conditional panel
                conditionalPanel(
                  "input.custom_palette == 'Yes'",
                  # add a text input
                  textInput(
                    "palette_custom",
                    label = "Enter HEX values:",
                    value = "#E5FCC2, #9DE0AD, #45ADA8, #547980, #594F4F"
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              # add the plot output
              plotOutput("bavaria_1", height = "50em")
            ),
            column(
              width = 6,
              # add the plot output
              plotOutput("bavaria_2", height = "50em")
            )
          )
        )
      ),
      tabItem(
        "tmap_breaks",
        fillPage(
          fluidRow(
            box(
              width = 12,
              column(
                width = 3,
                pickerInput(
                  "bavaria_style",
                  "Select style:",
                  choices = NULL,
                  selected = NULL,
                  width = "100%"
                )
              ),
              column(
                width = 3,
                uiOutput("ui_style")
              ),
              column(
                width = 3,
                radioGroupButtons(
                  inputId = "custom_palette_2",
                  label = "Use custom palette?",
                  choices = c("Yes", "No"),
                  selected = "No",
                  justified = TRUE,
                  checkIcon = list(
                    "yes" = icon("ok",
                      lib = "glyphicon"
                    )
                  )
                )
              ),
              column(
                width = 3,
                conditionalPanel(
                  "input.custom_palette_2 == 'No'",
                  pickerInput(
                    "palette_2",
                    label = "Select palette:",
                    choices = NULL,
                    selected = NULL,
                    width = "100%"
                  )
                ),
                conditionalPanel(
                  "input.custom_palette_2 == 'Yes'",
                  textInput(
                    "palette_custom_2",
                    label = "Enter HEX values:",
                    value = "#E5FCC2, #9DE0AD, #45ADA8, #547980, #594F4F"
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              # add the plot output
              plotOutput("bavaria_3", height = "50em")
            ),
            column(
              width = 6,
              # add the plot output
              plotOutput("bavaria_4", height = "50em")
            )
          )
        )
      ),
      tabItem(
        "tmap_layout",
        fillPage(
          fluidRow(
            column(
              width = 3,
              actionBttn(
                "next_layouts",
                label = "Next layouts",
                style = "fill"
              )
            )
          ),
          br(),
          fluidRow(
            plotOutput(
              "plot_layouts",
              height = "60em"
            )
          )
        )
      ),
      tabItem(
        "tmap_facets",
        fillPage(
          fluidRow(
            box(
              width = 12,
              column(
                width = 3,
                pickerInput(
                  inputId = "ncol",
                  label = "Number of cols:",
                  choices = seq(1, 4),
                  selected = 4
                )
              ),
              column(
                width = 3,
                pickerInput(
                  inputId = "nrow",
                  label = "Number of rows:",
                  choices = seq(1, 4),
                  selected = 1
                )
              ),
              column(
                width = 3,
                pickerInput(
                  inputId = "facet_var",
                  label = "Select variable:",
                  choices = c(
                    "Total energy average price",
                    "Total energy production",
                    "Total energy consumption",
                    "Coal total production",
                    "Coal total consumption",
                    "Hydropower total production",
                    "Hydropower total consumption"
                  ),
                  selected = "Total energy average price"
                )
              ),
              column(
                width = 3,
                pickerInput(
                  "palette_3",
                  label = "Select palette:",
                  choices = NULL,
                  selected = NULL,
                  width = "100%"
                )
              )
            )
          ),
          fluidRow(
            plotOutput(
              "plot_facets",
              height = "60em"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  ########## EVENT OBSERVER ##########
  # move to next page
  observeEvent(input$Next, {
    newtab <- switch(input$tabs,
      "tmap_basics" = "tmap_layers",
      "tmap_layers" = "tmap_aesthetics",
      "tmap_aesthetics" = "tmap_colour",
      "tmap_colour" = "tmap_breaks",
      "tmap_breaks" = "tmap_layout",
      "tmap_layout" = "tmap_facets",
      "tmap_facets" = "tmap_animated"
    )
    updateTabItems(session, "tabs", newtab)
  })
  # move to previous page
  observeEvent(input$Previous, {
    newtab <- switch(input$tabs,
      "tmap_animated" = "tmap_facets",
      "tmap_facets" = "tmap_layout",
      "tmap_layout" = "tmap_breaks",
      "tmap_breaks" = "tmap_colour",
      "tmap_colour" = "tmap_aesthetics",
      "tmap_aesthetics" = "tmap_layers",
      "tmap_layers" = "tmap_basics"
    )
    updateTabItems(session, "tabs", newtab)
  })
  ########## PLOT PAGE 1 ##########
  # Plot a simple plot depending on what is chosen in the picker
  # also add the code used for the plot as credits
  output$tmap1plot <- renderPlot({
    if (input$tmap1 != "") {
      if (input$tmap1 == "tm_fill()") {
        tm_shape(us_states) +
          tm_fill(col = "darkgrey") +
          tm_credits(
            paste("tm_shape(us_states) +",
              "  tm_fill()",
              sep = "\n"
            ),
            size = 1.2
          )
      } else if (input$tmap1 == "tm_borders()") {
        tm_shape(us_states) +
          tm_borders() +
          tm_credits(
            paste("tm_shape(us_states) +",
              "  tm_borders()",
              sep = "\n"
            ),
            size = 1.2
          )
      } else if (input$tmap1 == "tm_fill() + tm_borders()") {
        tm_shape(us_states) +
          tm_fill(col = "darkgrey") +
          tm_borders() +
          tm_credits(
            paste("tm_shape(us_states) +",
              "  tm_fill() +",
              "  tm_borders()",
              sep = "\n"
            ),
            size = 1.2
          )
      } else {
        tm_shape(us_states) +
          tm_polygons(col = "darkgrey") +
          tm_credits(
            paste("tm_shape(us_states) +",
              "  tm_polygons()",
              sep = "\n"
            ),
            size = 1.2
          )
      }
    } else {
      NULL
    }
  })
  ########## PLOT PAGE 2 ##########
  # addlayer is shinyjs and tells you how often a button was pressed
  # depending on how often it was pressed, a different amount of plots
  # will be shown
  # normal plot
  output$europe_plot <- renderPlot({
    if (as.numeric(input$addlayer) %% 3 == 0) {
      tm_shape(europe_raster) + tm_raster()
    } else if (as.numeric(input$addlayer) %% 3 == 1) {
      tm_shape(europe_raster) + tm_raster() +
        tm_shape(europe_shape) + tm_lines(alpha = 0.3)
    } else {
      tm_shape(europe_raster) + tm_raster() +
        tm_shape(europe_shape) + tm_lines(alpha = 0.3) +
        tm_shape(cities) + tm_dots(size = 0.3)
    }
  })
  output$code_europe <- renderText({
    if (as.numeric(input$addlayer) %% 3 == 0) {
      "tm_shape(europe_raster) +\n tm_raster()"
    } else if (as.numeric(input$addlayer) %% 3 == 1) {
      paste(
        "tm_shape(europe_raster) +\n tm_raster() +\n",
        "tm_shape(europe_shape) +\n tm_lines(alpha = 0.3)"
      )
    } else {
      paste(
        "tm_shape(europe_raster) +\n tm_raster() +\n",
        "tm_shape(europe_shape) +\n tm_lines(alpha = 0.3) +\n",
        "tm_shape(cities) +\n tm_dots(size = 0.3)"
      )
    }
  })
  ########## PLOT PAGE 3 ##########
  # this will get a bit ugly now
  output$aesthetics_plot <- renderPlot({
    # define the texts that will be shown as code in the plot later
    text_col <- paste('    col = "', input$fill_colour, '",', sep = "")
    text_col_border <- paste('    border.col = "',
      input$lines_colour, '",',
      sep = ""
    )
    text_alpha <- paste("    alpha = ", input$alpha, ",", sep = "")
    text_alpha_border <- paste("    border.alpha = ",
      input$alpha_lines, ",",
      sep = ""
    )
    text_lwd <- paste("    lwd = ", input$linewidth, ",", sep = "")
    text_lty <- paste("    lty = ", input$linetype, sep = "")
    # plot it with user values
    tm_shape(us_states) +
      tm_polygons(
        col = input$fill_colour,
        border.col = input$lines_colour,
        alpha = as.numeric(input$alpha),
        border.alpha = as.numeric(input$alpha_lines),
        lwd = as.numeric(input$linewidth),
        lty = as.numeric(input$linetype)
      ) +
      # add the code
      tm_credits(
        paste("tm_shape(us_states) +",
          "  tm_polygons(",
          text_col,
          text_col_border,
          text_alpha,
          text_alpha_border,
          text_lwd,
          text_lty,
          "  )",
          sep = "\n"
        ),
        size = 1.2
      )
  })
  ########## PLOT PAGE 4 ##########
  output$bavaria_1 <- renderPlot({
    # again some text editing for the credits
    text_var <- paste('    col = "', input$variable, '",', sep = "")
    # split the input by comma or semicolon, remove whitespaces,
    # turn into numeric and sort basically
    breaks <- unique(
      sort(
        as.numeric(
          trimws(
            unlist(
              str_split(input$breaks, "\\,|\\;")
            )
          )
        )
      )
    )
    if (input$custom_palette == "Yes") {
      # split by comma or semicolon again for a custom palette
      palette <- trimws(
        unlist(
          str_split(input$palette_custom, "\\,|\\;")
        )
      )
      # text editing
      text_palette <- paste('    pal = c(\n      "',
        paste(palette, collapse = '",\n      "'),
        '"\n    )',
        sep = ""
      )
    } else {
      palette <- input$palette
      # text editing
      text_palette <- paste('    pal = "', input$palette, '"', sep = "")
    }
    # at least 2 values for breaks needed, if not use default
    if (length(breaks) < 2) {
      tm_shape(bavaria) +
        tm_polygons(
          col = input$variable,
          pal = palette
        ) +
        tm_credits(paste("tm_shape(bavaria) +",
          "  tm_polygons(",
          text_var,
          text_palette,
          "  )",
          sep = "\n"
        ),
        size = 1.2, position = c("right", "top")
        ) +
        tm_layout(
          legend.position = c("left", "bottom"),
          legend.text.size = 1.2,
          legend.title.size = 1.2
        )
    } else {
      # text editing
      text_breaks <- paste("    breaks = c(
      ",
        paste(breaks, collapse = ",
      "), "),",
        sep = ""
      )
      tm_shape(bavaria) +
        tm_polygons(
          col = input$variable,
          breaks = breaks,
          pal = palette
        ) +
        tm_credits(paste("tm_shape(bavaria) +",
          "  tm_polygons(",
          text_var,
          text_breaks,
          text_palette,
          "  )",
          sep = "\n"
        ),
        size = 1.2, position = c("right", "top")
        ) +
        tm_layout(
          legend.position = c("left", "bottom"),
          legend.text.size = 1.2,
          legend.title.size = 1.2
        )
    }
  })
  # basically the same again now with n instead of breaks
  output$bavaria_2 <- renderPlot({
    text_var <- paste('    col = "', input$variable, '",', sep = "")
    n <- as.numeric(input$bins)
    if (input$custom_palette == "Yes") {
      palette <- trimws(
        unlist(
          str_split(input$palette_custom, "\\,|\\;")
        )
      )
      text_palette <- paste('    pal = c(\n      "',
        paste(palette, collapse = '",\n      "'),
        '" \n    )',
        sep = ""
      )
    } else {
      palette <- input$palette
      text_palette <- paste('    pal = "', input$palette, '"', sep = "")
    }
    text_bins <- paste("    n =", n, ",")
    tm_shape(bavaria) +
      tm_polygons(
        col = input$variable,
        n = n,
        pal = palette
      ) +
      tm_credits(paste("tm_shape(bavaria) +",
        "  tm_polygons(",
        text_var,
        text_bins,
        text_palette,
        "  )",
        sep = "\n"
      ),
      size = 1.2, position = c("right", "top")
      ) +
      tm_layout(
        legend.position = c("left", "bottom"),
        legend.text.size = 1.2,
        legend.title.size = 1.2
      )
  })


  ########## PLOT PAGE 5 ##########
  # first the lonely ui output
  output$ui_style <- renderUI({
    if (input$bavaria_style == "cat") {
      choices <- "Art"
    } else {
      choices <- c(
        "BIP je Einwohner", "Durchschnittsalter", "Siedlungsdichte",
        "Arbeitslosenquote", "Beschaeftigtenquote", "Haushaltseinkommen",
        "Studierende", "Bevoelkerungsentwicklung"
      )
    }
    pickerInput(
      "bavaria_var",
      label = "Select variable:",
      choices = choices,
      selected = NULL
    )
  })

  output$bavaria_3 <- renderPlot({
    # again some text editing for the credits
    text_var <- paste('    col = "', input$bavaria_var, '",', sep = "")
    if (input$custom_palette_2 == "Yes") {
      # split by comma or semicolon again for a custom palette
      palette <- trimws(
        unlist(
          str_split(input$palette_custom_2, "\\,|\\;")
        )
      )
      # text editing
      text_palette <- paste('    pal = c(\n      "',
        paste(palette, collapse = '",\n      "'),
        '"\n    )',
        sep = ""
      )
    } else {
      palette <- input$palette_2
      # text editing
      text_palette <- paste('    pal = "', input$palette_2, '"', sep = "")
    }
    tm_shape(bavaria) +
      tm_polygons(
        col = input$bavaria_var,
        pal = palette
      ) +
      tm_credits(paste("tm_shape(bavaria) +",
        "  tm_polygons(",
        text_var,
        text_palette,
        "  )",
        sep = "\n"
      ),
      size = 1.2, position = c("right", "top")
      ) +
      tm_layout(
        legend.position = c("left", "bottom"),
        legend.text.size = 1.2,
        legend.title.size = 1.2
      )
  })
  output$bavaria_4 <- renderPlot({
    # again some text editing for the credits
    text_var <- paste('    col = "', input$bavaria_var, '",', sep = "")
    text_style <- paste('    style = "', input$bavaria_style, '",', sep = "")
    if (input$custom_palette_2 == "Yes") {
      # split by comma or semicolon again for a custom palette
      palette <- trimws(
        unlist(
          str_split(input$palette_custom_2, "\\,|\\;")
        )
      )
      # text editing
      text_palette <- paste('    pal = c(\n      "',
        paste(palette, collapse = '",\n      "'),
        '"\n    )',
        sep = ""
      )
    } else {
      palette <- input$palette_2
      # text editing
      text_palette <- paste('    pal = "', input$palette_2, '"', sep = "")
    }
    tm_shape(bavaria) +
      tm_polygons(
        col = input$bavaria_var,
        style = input$bavaria_style,
        pal = palette
      ) +
      tm_credits(paste("tm_shape(bavaria) +",
        "  tm_polygons(",
        text_var,
        text_style,
        text_palette,
        "  )",
        sep = "\n"
      ),
      size = 1.2, position = c("right", "top")
      ) +
      tm_layout(
        legend.position = c("left", "bottom"),
        legend.text.size = 1.2,
        legend.title.size = 1.2
      )
  })
  ########## PLOT PAGE 6 ##########
  # all plots of vavaria with different themes or stuff
  output$plot_layouts <- renderPlot({
    bavaria_basic <- tm_shape(bavaria) +
      tm_polygons(
        col = "Art",
        pal = c("white", "skyblue")
      ) +
      tm_layout(legend.position = c("left", "bottom")) +
      tm_layout(
        legend.title.size = 1.5,
        legend.text.size = 1.2
      )
    bavaria_zero <- bavaria_basic +
      tm_credits(
        paste(
          "tm_shape(bavaria) +",
          "  tm_polygons(",
          '    col = "Art"',
          '    pal = c("white", "skyblue")',
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )
    bavaria_one <- bavaria_basic +
      tm_compass(type = "8star", position = c("left", "center")) +
      tm_scale_bar(size = 1.2) +
      tm_credits(
        paste(
          "... +",
          "  tm_compass(",
          '    type = "8star",',
          '    position = c("left", "center")',
          "    ) +",
          "  tm_scale_bar()",
          sep = "\n"
        ),
        position = c("right", "top"),
        size = 1.2
      ) +
      tm_layout(
        legend.title.size = 1.5,
        legend.text.size = 1.2
      )
    bavaria_two <- bavaria_basic + tm_layout(title = "Bavaria") +
      tm_logo("../../datasets/bavaria.png", height = 4) +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          '    title = "Bavaria"',
          "    ) +",
          "  tm_logo(",
          '    "bavaria.png"',
          "    height = 4)",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )
    bavaria_three <- bavaria_basic + tm_layout(scale = 2) +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          "    scale = 2",
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"),
        size = 1.2
      )
    bavaria_four <- bavaria_basic + tm_layout(bg.color = "salmon") +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          '    bg.color = "salmon"',
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )
    bavaria_five <- bavaria_basic + tm_layout(frame = FALSE) +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          "    frame = FALSE",
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )
    bavaria_six <- bavaria_basic + tm_layout(frame.lwd = 5) +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          "    frame.lwd = 5",
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )
    bavaria_seven <- bavaria_basic + tm_layout(inner.margins = 0.2) +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          "    inner.margins = 0.2",
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )
    bavaria_eight <- bavaria_basic + tm_layout(legend.show = FALSE) +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          "    legend.show = FALSE",
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )

    bavaria_nine <- bavaria_basic +
      tm_layout(sepia.intensity = 0.4) +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          "    sepia.intensity = 0.4",
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )

    bavaria_ten <- bavaria_basic +
      tm_layout(saturation = 2) +
      tm_credits(
        paste(
          "... +",
          "  tm_layout(",
          "    saturation = 2",
          "    )",
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )

    bavaria_eleven <- bavaria_basic +
      tm_style("bw") +
      tm_layout(legend.position = c("left", "bottom")) +
      tm_layout(
        legend.title.size = 1.5,
        legend.text.size = 1.2
      ) +
      tm_credits(
        paste(
          "... +",
          '  tm_style("bw")',
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )

    bavaria_twelve <- bavaria_basic +
      tm_style("classic") +
      tm_layout(legend.position = c("left", "bottom")) +
      tm_layout(
        legend.title.size = 1.5,
        legend.text.size = 1.2
      ) +
      tm_credits(
        paste(
          "... +",
          '  tm_style("classic")',
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )

    bavaria_thirteen <- bavaria_basic +
      tm_style("cobalt") +
      tm_layout(legend.position = c("left", "bottom")) +
      tm_layout(
        legend.title.size = 1.5,
        legend.text.size = 1.2
      ) +
      tm_credits(
        paste(
          "... +",
          '  tm_style("cobalt")',
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )
    bavaria_fourteen <- tm_shape(bavaria) +
      tm_polygons(col = "Art") +
      tm_style("col_blind") +
      tm_layout(legend.position = c("left", "bottom")) +
      tm_layout(
        legend.title.size = 1.5,
        legend.text.size = 1.2
      ) +
      tm_credits(
        paste(
          "... +",
          '  tm_style("col_blind")',
          sep = "\n"
        ),
        position = c("right", "top"), size = 1.2
      )
    if (as.numeric(input$next_layouts) %% 8 == 0) {
      tmap_arrange(bavaria_zero, ncol = 3)
    } else if (as.numeric(input$next_layouts) %% 8 == 1) {
      tmap_arrange(bavaria_zero, bavaria_one, bavaria_two, ncol = 3)
    } else if (as.numeric(input$next_layouts) %% 8 == 2) {
      tmap_arrange(bavaria_zero, bavaria_three, bavaria_four, ncol = 3)
    } else if (as.numeric(input$next_layouts) %% 8 == 3) {
      tmap_arrange(bavaria_zero, bavaria_five, bavaria_six, ncol = 3)
    } else if (as.numeric(input$next_layouts) %% 8 == 4) {
      tmap_arrange(bavaria_zero, bavaria_seven, bavaria_eight, ncol = 3)
    } else if (as.numeric(input$next_layouts) %% 8 == 5) {
      tmap_arrange(bavaria_zero, bavaria_nine, bavaria_ten, ncol = 3)
    } else if (as.numeric(input$next_layouts) %% 8 == 6) {
      tmap_arrange(bavaria_zero, bavaria_eleven, bavaria_twelve, ncol = 3)
    } else if (as.numeric(input$next_layouts) %% 8 == 7) {
      tmap_arrange(bavaria_zero, bavaria_thirteen, bavaria_fourteen, ncol = 3)
    }
  })
  ########## PLOT PAGE 7 ##########
  output$plot_facets <- renderPlot({
    # load the data
    data("us_states")
    us <- read.csv("../../datasets/us_energy_census.csv")
    choices <- c(
      "Total energy average price",
      "Total energy production",
      "Total energy consumption",
      "Coal total production",
      "Coal total consumption",
      "Hydropower total production",
      "Hydropower total consumption"
    )
    var_names <- c(
      "TotalPrice",
      "TotalP",
      "TotalE",
      "CoalP",
      "CoalC",
      "HydroP",
      "HydroC"
    )
    # edit the data a bit (could prolly do it with dplyr but too lazy)
    var <- var_names[match(input$facet_var, choices)]
    vars <- paste(var, c("2010", "2011", "2012", "2013"), sep = "")
    us$State <- as.character(us$State)
    us <- us[us$State %in% us_states$NAME, ]
    us <- us[, c("State", vars)]
    colnames(us) <- NULL
    us2 <- rbind(
      as.matrix(us[, c(1, 2)]),
      as.matrix(us[, c(1, 3)]),
      as.matrix(us[, c(1, 4)]),
      as.matrix(us[, c(1, 5)])
    )
    # https://www.kaggle.com/lislejoem/us_energy_census_gdp_2010-14
    us <- data.frame(us2)
    colnames(us) <- c("State", var)
    us[, var] <- as.numeric(as.character(us[, var]))
    rownames(us) <- seq_len(nrow(us))
    us$Year <- c(
      rep("2010", 49),
      rep("2011", 49),
      rep("2012", 49),
      rep("2013", 49)
    )
    us_states$State <- us_states$NAME
    us <- merge(us, us_states[, 7:8], by = "State", all.y = TRUE)
    us <- sf::st_as_sf(us)
    pal <- input$palette_3
    if (pal %in% c("viridis", "civids", "magma", "plasma", "inferno")) {
      pal <- get(pal)
      pal <- pal(10, direction = -1)
    }
    # plot with facets
    tm_shape(us) + tm_polygons(
      col = var, style = "cont",
      pal = pal
    ) +
      tm_facets(
        by = "Year",
        ncol = as.numeric(input$ncol),
        nrow = as.numeric(input$nrow)
      ) +
      tm_layout(
        legend.outside.size = 0.1,
        legend.title.size = 1.2,
        legend.text.size = 1.2,
        panel.label.size = 4
      )
  })

  ########## PICKER INPUTS ##########
  # set the values for all the pickers
  updatePickerInput(
    session,
    "variable",
    choices = c(
      "BIP je Einwohner", "Durchschnittsalter", "Siedlungsdichte",
      "Arbeitslosenquote", "Beschaeftigtenquote", "Haushaltseinkommen",
      "Studierende", "Bevoelkerungsentwicklung"
    )
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
      "Brewer Categorical" = c(
        "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
        "Set2", "Set3"
      ),
      "Brewer Diverging" = c(
        "BrBG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
        "Spectral"
      ),
      "Viridis sequential" = c(
        "viridis", "magma", "plasma", "inferno", "cividis"
      )
    ),
    selected = "Reds"
  )

  updatePickerInput(
    session,
    "palette_2",
    choices = list(
      "Brewer Sequential" = c(
        "Reds", "Greens", "Blues", "Greys", "Oranges", "Purples",
        "BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd",
        "RdPu", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
      ),
      "Brewer Categorical" = c(
        "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
        "Set2", "Set3"
      ),
      "Brewer Diverging" = c(
        "BrBG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
        "Spectral"
      ),
      "Viridis sequential" = c(
        "viridis", "magma", "plasma", "inferno", "cividis"
      )
    ),
    selected = "Reds"
  )

  updatePickerInput(
    session,
    "palette_3",
    choices = list(
      "Brewer Sequential" = c(
        "Reds", "Greens", "Blues", "Greys", "Oranges", "Purples",
        "BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd",
        "RdPu", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
      ),
      "Brewer Categorical" = c(
        "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
        "Set2", "Set3"
      ),
      "Brewer Diverging" = c(
        "BrBG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
        "Spectral"
      ),
      "Viridis sequential" = c(
        "viridis", "magma", "plasma", "inferno", "cividis"
      )
    ),
    selected = "viridis"
  )

  updatePickerInput(session, "tmap1", choices = c(
    "tm_fill()",
    "tm_borders()",
    "tm_fill() + tm_borders()",
    "tm_polygons()"
  ))
  updatePickerInput(session, "bavaria_style", choices = c(
    "pretty",
    "equal",
    "quantile",
    "jenks",
    "cont",
    "cat"
  ))
}

# Run the application
shinyApp(ui = ui, server = server)
