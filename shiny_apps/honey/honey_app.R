packages_required <- c(
  "shiny", "sf", "leaflet", "RColorBrewer", "stringr"
)
not_installed <- packages_required[!packages_required %in%
                                     installed.packages()[, "Package"]]
not_installed <- not_installed[not_installed != "leafletplugins"]
if (length(not_installed) > 0) {
  lapply(not_installed, install.packages, dependencies = TRUE)
}
# load them
lapply(packages_required, library, character.only = TRUE)
honey <- read_sf("../../datasets/honey.shp")
colnames(honey)[3:8] <- c("Number_of_colonies", "Yield_per_colony",
                          "Total_production", "Stocks","Price_per_lb",
                          "Value_of_production"
                          )
ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      selectInput(
        inputId = "year",
        label = "Select year:",
        choices = 1998:2012,
        multiple = FALSE
      )
    ),
    column(
      width = 4,
      selectInput(
        inputId = "variable",
        label = "Select variable:",
        choices = colnames(honey)[3:8],
        multiple = FALSE
      )
    ),
    column(
      width = 4,
      selectInput(
        inputId = "palette",
        label = "Select palette:",
        choices = c("YlOrRd", "YlGnBu", "BuPu", "Oranges", "Greys"),
        multiple = FALSE
      )
    )
  ),
  fluidRow(
    leafletOutput(
      "map",
      height = "80vh"
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    honey_data <- honey[honey$year == as.numeric(input$year), input$variable]
    pal <- colorNumeric(brewer.pal(9, input$palette),
                        st_drop_geometry(honey_data))
    title <- paste(str_replace_all(input$variable, "\\_", " "), ":", sep = "")
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = honey_data, color = ~pal(get(input$variable))) %>%
      addLegend(data = honey_data, pal = pal, values = ~get(input$variable),
                title = title, opacity = 1, position = "bottomleft")
  })
}

shinyApp(ui = ui, server = server)