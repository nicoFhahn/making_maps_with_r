# a bunch of required packages
# maptools
options(scipen = 10)
packages_required <- c(
  "devtools", "dbscan", "dplyr", "furrr", "future", "geosphere",
  "leaflet", "leaflet.extras", "leafletplugins", "maps",
  "openxlsx", "osmdata", "osrm", "plyr", "raster", "readr",
  "RColorBrewer", "rlist", "rgdal", "sf", "shiny", "shinyWidgets", "sp",
  "spdep", "stplanr", "stringr", "viridis"
)
# check which ones are not installed
not_installed <- packages_required[!packages_required %in%
  installed.packages()[, "Package"]]
# install them
if (!"devtools" %in% installed.packages()[, "Package"]) {
  install.packages("devtools", dependencies = TRUE)
}
if ("leafletplugins" %in% not_installed) {
  devtools::install_github("asmith26/leafletplugins")
}
not_installed <- not_installed[not_installed != "leafletplugins"]
if (length(not_installed) > 0) {
  lapply(not_installed, install.packages, dependencies = TRUE)
}
# load them
lapply(packages_required, library, character.only = TRUE)
future::plan(multiprocess)
# define an icon
icon.fa <- makeAwesomeIcon(
  icon = "flag", markerColor = "red",
  library = "fa",
  iconColor = "black"
)
# define some palettes
pal1 <- c("#A8E6CE", "#DCEDC2", "#FFD3B5", "#FFAAA6", "#FF8C94")
pal2 <- c("#A7226E", "#EC2049", "#F26B38", "#F7DB4F", "#2F9599")
pal3 <- c("#E1F5C4", "#EDE574", "#F9D423", "#FC913A", "#FF4E50")
pal4 <- c("#FE4365", "#FC9D9A", "#F9CDAD", "#C8C8A9", "#83AF9B")
tiles <- list(
  OpenStreetMap = "OpenStreetMap",
  Esri.WorldImagery = "Esri.WorldImagery",
  CartoDB.Positron = "CartoDB.Positron",
  CartoDB.DarkMatter = "CartoDB.DarkMatter"
)
plz <- read_sf("data_sources/plz.shp")
# read in census data so you dont have to do it all the time later
input_tidy <- readr::read_csv2("data_sources/census_tidy.csv")
input_ras <- rasterFromXYZ(input_tidy, crs = st_crs(3035)$proj4string)
# load the road network of germany
future::plan(multiprocess)
# roads <- future_map(
#  paste("road_network/",
#    list.files("road_network", pattern = ".shp"),
#    sep = ""
#  ),
#  read_sf
# )
# name the roads
# names(roads) <- c(
#  "Sachsen-Anhalt", "Baden-Württemberg", "Bayern", "Berlin",
#  "Brandenburg", "Bremen", "Hamburg", "Hessen",
#  "Mecklenburg-Vorpommern", "Niedersachsen",
#  "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland",
#  "Sachsen", "Schleswig-Holstein", "Thüringen"
#  #  "Bayern", "Berlin"
# )
states <- read_sf("data_sources/vg2500_bld.shp")
st_crs(states) <- 4326
Encoding(states$GEN) <- "latin"
future::plan(sequential)
ui <- source(file.path("ui", "ui.R"), local = TRUE)$value
############## SERVER ##############
server <- function(input, output, session) {
  source(file.path("server", "tab1.R"), local = TRUE)$value
  source(file.path("server", "tab2.R"), local = TRUE)$value
  source(file.path("server", "error_messages.R"), local = TRUE)$value
  source(file.path("server", "reactive_observer.R"), local = TRUE)$value
  source(file.path("server", "map_basic.R"), local = TRUE)$value
  source(file.path("server", "picker_download.R"), local = TRUE)$value
  source(file.path("server", "ui_outputs.R"), local = TRUE)$value
  source(file.path("server", "lists.R"), local = TRUE)$value
  source(file.path("server", "functions.R"), local = TRUE)$value
}

shinyApp(ui, server)
