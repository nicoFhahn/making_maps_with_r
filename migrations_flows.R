library(readxl)
library(stplanr)
library(mapdeck)
ger <- read_xlsx("Germany.xlsx", sheet = "Germany by Citizenship",
                 skip = 20, col_names = TRUE)
ger <- ger[, c(1:3, 38)]
ger <- ger[!ger$OdName %in% c("Unknown", "Total"), ]
ger <- ger[ger$Coverage == "Foreigners", ]
lat1 <- unlist(lapply(codes, function(x) x[2]))
lat2 <- geo_code("Fladungen")[2]
lon1 <- unlist(lapply(codes, function(x) x[1]))
lon2 <- geo_code("Fladungen")[1]
ger$lat_origin <- NA
ger$lng_origin <- NA
ger$lat_goal <- NA
ger$lng_goal <- NA
ger[ger$Type == "Immigrants", ]$lat_goal <- lat2
ger[ger$Type == "Immigrants", ]$lng_goal <- lon2
ger[ger$Type != "Immigrants", ]$lat_origin <- lat2
ger[ger$Type != "Immigrants", ]$lng_origin <- lon2
nrow(ger[ger$Type != "Immigrants", ])
ger[1:189, ]$lat_goal <- lat1[1:189]
ger[1:189, ]$lng_goal <- lon1[1:189]
ger[190:379, ]$lat_origin <- lat1[190:379]
ger[190:379, ]$lng_origin <- lon1[190:379]
set_token("pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA")
ger$count1 <- NA
ger$count2 <- NA
ger[ger$Type == "Immigrants", ]$count1 <- ger[ger$Type == "Immigrants", ]$OdName
ger[ger$Type == "Immigrants", ]$count2 <- "Germany"
ger[ger$Type != "Immigrants", ]$count1 <- "Germany"
ger[ger$Type != "Immigrants", ]$count2 <- ger[ger$Type != "Immigrants", ]$OdName
ger <- as.data.frame(ger)
ger[ger$Type == "Immigrants", ]$lat_goal <- ger[ger$Type == "Immigrants", ]$lat_goal + rnorm(190, mean = 0.5)
ger[ger$Type == "Immigrants", ]$lng_goal <- ger[ger$Type == "Immigrants", ]$lng_goal + rnorm(190, mean = 0.05)
immigrants <- ger[ger$Type == "Immigrants", ]
emmigrants <- ger[ger$Type != "Immigrants", ]
immigrants$Immigrants <- immigrants$`2008`
emmigrants$Emmigrants <- emmigrants$`2008`
immigrants <- immigrants[immigrants$OdName %in% emmigrants$OdName, ]
emmigrants <- emmigrants[emmigrants$OdName %in% immigrants$OdName, ]
all(emmigrants$OdName == immigrants$OdName)
ger <- immigrants
ger$Immigrants <- immigrants$Immigrants
ger$Emmigrants <- emmigrants$Emmigrants
im_iv <- classInt::classIntervals(ger$Immigrants, n = 10, style = "fisher")$brks[2:11]
em_iv <- classInt::classIntervals(ger$Emmigrants, n = 10, style = "fisher")$brks[2:11]
ger$Immigrants_fac <- NA
ger$Emmigrants_fac <- NA
ger$Immigrants_fac <- ifelse(ger$Immigrants <= im_iv[1], im_iv[1],
                             ifelse(ger$Immigrants <= im_iv[2], im_iv[2],
                                    ifelse(ger$Immigrants <= im_iv[3], im_iv[3],
                                           ifelse(ger$Immigrants <= im_iv[4], im_iv[4],
                                                  ifelse(ger$Immigrants <= im_iv[5], im_iv[5],
                                                         ifelse(ger$Immigrants <= im_iv[6], im_iv[6],
                                                                ifelse(ger$Immigrants <= im_iv[7], im_iv[7],
                                                                       ifelse(ger$Immigrants <= im_iv[8], im_iv[8],
                                                                              ifelse(ger$Immigrants <= im_iv[9], im_iv[9],im_iv[10])))))))))
ger$Emmigrants_fac <- ifelse(ger$Emmigrants <= em_iv[1], em_iv[1],
                             ifelse(ger$Emmigrants <= em_iv[2], em_iv[2],
                                    ifelse(ger$Emmigrants <= em_iv[3], em_iv[3],
                                           ifelse(ger$Emmigrants <= em_iv[4], em_iv[4],
                                                  ifelse(ger$Emmigrants <= em_iv[5], em_iv[5],
                                                         ifelse(ger$Emmigrants <= em_iv[6], em_iv[6],
                                                                ifelse(ger$Emmigrants <= em_iv[7], em_iv[7],
                                                                       ifelse(ger$Emmigrants <= em_iv[8], em_iv[8],
                                                                              ifelse(ger$Emmigrants <= em_iv[9], em_iv[9],em_iv[10])))))))))
ger$Emmigrants_fac <- ordered(ger$Emmigrants_fac)
ger$Immigrants_fac <- ordered(ger$Immigrants_fac)  
mapdeck(style = mapdeck_style('dark')) %>%
  add_arc(
    data = ger[ger$Type == "Immigrants", ],
    origin = c("lng_origin", "lat_origin"),
    destination = c("lng_goal", "lat_goal"),
    stroke_from = "Emmigrants_fac",
    stroke_to = "Immigrants_fac",
    layer_id = 'arclayer',
    tooltip = "info",
    palette = "oranges",
    legend = TRUE
  )
codes <- lapply(seq_len(nrow(ger)), function(x, ...) {
  geo_code(ger[x, 3])
})