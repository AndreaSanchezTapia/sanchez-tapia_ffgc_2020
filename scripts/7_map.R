library(rgdal)
library(scales)
library(tmaps)
library(sf)
library(spData)
#palette change in july 2020
library(RColorBrewer)
pal <- brewer.pal(5, "PiYG")

#reads official shapefiles
PDA     <- read_sf("./data/1_map/shapefiles/rbpda/", "LimitedaRB")
br101   <- read_sf("./data/1_map/shapefiles/rbpda/", "BR_101")
saojoao <- read_sf("./data/1_map/shapefiles/rbpda/", "riosaojoao")
SOSma   <- read_sf("./data/1_map/shapefiles/brasil/SOSMA/rema_fd4d801731725513a4d77aa9bb35534b5305.shp")

#assigns the correct CRS (UTM in Brazil SAD69)

PDA     <- st_set_crs(PDA, 29193)
br101   <- st_set_crs(br101, 29193)
saojoao   <- st_set_crs(saojoao, 29193)

# reprojects to WGS84
PDA.wsg84     <- st_transform(PDA, 4326)
SOS.wgs84     <- st_transform(SOSma, 4326)
br101.wgs84   <- st_transform(br101, 4326)
saojoao.wgs84 <- st_transform(saojoao, 4326)

# reads the shapefiles of the fires - digitalized by AST from paper arquives from the PABR
#1993 fire
fuego1993 <- read_sf("./data/1_map/queimadas/incendio1993.shp")
fuego1993 <- st_set_crs(fuego1993, 29193)## isto é UTM em SAD69 brasileiro
fuego1993.wsg84 <- st_transform(fuego1993, 4326)

#2002 fire
fuego2002 <- read_sf("./data/1_map/queimadas/incendio2002.shp")
st_crs(fuego2002)
fuego2002.wsg84 <- st_transform(fuego2002, 4326)

#2007 fire
fuego2007 <- read_sf("./data/1_map/queimadas/Incendio_2007.shp")
st_crs(fuego2007)
fuego2007.wsg84 <- st_transform(fuego2007, 4326)

#2010 fire
fuego2010 <- read_sf("./data/1_map/queimadas/incendio2010p.shp")
st_crs(fuego2010)
fuego2010.wsg84 <- st_transform(fuego2010, 4326)

# reads plot shapefiles
#2002, 2010
parcelas <- read_sf("./data/1_map/shapefiles/parcelasandre.shp")
st_crs(parcelas)
parcelas.wsg84 <- st_transform(parcelas, 4326)

# 1990, secondary forest, mature forest
parcelas_j <- read_sf("./data/1_map/shapefiles/parcelasjeronimo.shp")
st_crs(parcelas_j)
parcelas_j_wsg84 <- st_transform(parcelas_j, 4326)
parcelas.wsg84 <- parcelas.wsg84 %>% mutate(col = rep(c(pal[1], pal[2]), each = 3))
parcelas_j_wsg84 <- parcelas_j_wsg84 %>% slice(1:9) %>% mutate(col = rep(c(pal[3:5]), each = 3))

#creates the map and saves it to png file
library(tmap)
pabr <- tm_shape(PDA.wsg84) +
  tm_borders(lwd = 1.5) +
tm_shape(SOS.wgs84) +
  tm_fill(col = "lightgreen", alpha = 0.3) +
  tm_borders(col = "darkgreen", alpha = 0.1) +
tm_shape(fuego1993.wsg84) +
  tm_fill(col = "grey", alpha = 0.4) +
  tm_borders(col = "grey90") +
tm_shape(fuego2002.wsg84) +
  tm_fill(col = "grey", alpha = 0.4) +
  tm_borders(col = "grey90") +
tm_shape(fuego2007.wsg84) +
  tm_lines(col = "grey", alpha = 0.4) +
  #tm_borders(col = "grey90") +
tm_shape(fuego2010.wsg84) +
  tm_fill(col = "grey", alpha = 0.4) +
  tm_borders(col = "grey90") +
tm_shape(saojoao.wgs84) +
  tm_borders(col = "dodgerblue", lwd = 1.5) +
tm_shape(br101.wgs84) +
    tm_lines(col = "red", lwd = 2) +
tm_shape(PDA.wsg84) +
    tm_borders(col = "black", lwd = 1.5) +
tm_shape(parcelas.wsg84) +
  tm_symbols(col = "col", size = 1, border.col = "black") +
tm_shape(parcelas_j_wsg84) +
  tm_symbols(col = "col", size = 1, border.col = "black") +
  tm_add_legend(labels = c("Fires 1993 - 2010", "BAF fragments"),
                col = c("grey","lightgreen"),
                alpha = c(0.4),
                type = "fill",
                size = 3
                ) +
  tm_add_legend(labels = c("PABR limits","Roads"),
                col = c("black","red"),
                type = "line",
                lwd = 1.5,
                ) +
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 1, 2), text.size = 1, position = c("left", "bottom")) +
  tm_layout(frame = F,
            legend.outside = F,
            legend.position  = c("right", "top"),
            legend.height = 1.5,
            legend.width = 1,
            legend.just = 1,
            ) +
  tm_add_legend(type = "symbol",
                labels = c("Mature Forest",
                           "Sec. Forest",
                           "Low disturbance",
                           "Medium disturbance",
                           "High disturbance"
                           ),
                col = pal) +
  NULL
pabr
st_bbox(point)
pabr_region <- st_bbox(PDA.wsg84) %>%
  st_as_sfc()

#plot points
#legend
legend(x = -42.26, y = -22.5,
       fill = c(alpha("lightgreen", 0.7), alpha("grey", 0.8)),
       legend = c("Forest remnants", "Fires 1993-2010"),
       box.lwd = "n",
       cex = 1,
       border = "grey80")
legend(x = -42.26, y = -22.51,
       pch = 21,
       legend = c("High-disturbance",
                  "Medium disturbance",
                  "Low disturbance",
                  "Secondary forests",
                  "Mature forests"),
       box.lwd = "n",
       pt.bg = pal,
       col = "black",
       pt.cex = c(1.5, 1.5, 1.5, 1.5, 1.5), cex = 1)
#mtext("C", 3, las = 1, line = 0.2, cex = 1.5, adj = 0)
dev.off()



library(tmap)
library(dplyr)
# map

library(sf)
dir <- "./data/1_map/baf/Andrea"
pts <- "./data/1_map/baf/Andrea 2"

shp <- list.files(dir, full.names = T, pattern = ".shp$")
pts <- list.files(pts, full.names = T, pattern = ".shp$")
shp
bioma <- read_sf(shp[1])
geo_dist <- read_sf(shp[2])
sam <- read_sf(shp[4])
point <- read_sf(pts)


bioma$col <- ifelse(bioma$LEGEND == "ATLANTIC FOREST", "darkgreen", "grey90")
point <- point %>%
  mutate(col = case_when(
  Mono =="Occurrence" ~ "grey",
  Mono =="No" ~ "black",
  Mono =="Yes" ~ "red"))
library(grDevices)
bioma_map <- tm_shape(bioma, bbox = st_bbox(point)) +
  tm_fill("col", alpha = 0.5) +
  tm_shape(point) +
  tm_symbols(col = "col", size = 0.3, alpha = 0.7) +
  tm_add_legend(type = "fill",
                col = "darkgreen",
                alpha = 0.5,
                labels = c("BAF")) +
  tm_add_legend(type = "symbol",
                labels = c("Occurrences", "Monodominance", "Non-monodominance"),
                col = c("grey", "red", "black")) +
  tm_layout(title = expression(paste(italic("M. polymorphum"), " in the Brazilian Atlantic Forest")),
            title.fontface = 2,
            title.size = 1,
            title.bg.color = "white") +
  NULL



tmap_save(tm = pabr, filename = "./figs/1_map_pabr.png")
tmap_save(tm = bioma_map, filename = "./figs/7_map_bioma.png")
tmap_arrange(bioma_map, pabr)

library(grid)
bioma_map
pabr
print(bioma_map, vp = viewport(0.1, 0.8, width = 0.25, height = 0.25))
