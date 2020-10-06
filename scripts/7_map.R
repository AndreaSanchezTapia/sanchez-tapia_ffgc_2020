# load packages
library(rgdal)
library(scales)
library(tmap)
library(sf)
library(spData)
library(dplyr)
library(grid)
library(RColorBrewer)
library(grDevices)
library(brazilmaps)

# brazil shapefile
br <- brazilmaps::get_brmap("Brazil")
estados_shp <- brazilmaps::get_brmap(geo = "State")
#palette change in july 2020
pal <- brewer.pal(5, "PiYG")

# makes the maps of the whole species distribution----

dir <- "./data/1_map/baf"
shp <- list.files(dir, full.names = T, pattern = ".shp$")

bioma <- read_sf(shp[1])
geo_dist <- read_sf(shp[2])
point <- read_sf(shp[3])
sam <- read_sf(shp[4])

# prepares colour column and annotation for the inset
bioma <- bioma %>% mutate(col = case_when(
  LEGEND == "ATLANTIC FOREST" ~  "darkseagreen4",
  LEGEND == "CERRADO" ~  "grey85",
  !LEGEND %in% c("ATLANTIC FOREST", "CERRADO") ~ "grey85"))
bioma <- bioma %>%
  mutate(lab = if_else(LEGEND == "CERRADO", "BAF", "")) #This is only for label positioning

# sets color for the whole set of points and
point <- point %>%
  mutate(col = case_when(
  Mono =="Occurrence" ~ "white",
  Mono =="No" ~ "black",
  Mono =="Yes" ~ "orange"))

# ...for the points that were actually visited----
point_det <- point %>%
  filter(Mono != "Occurrence") %>%
  mutate(col = case_when(
    Mono =="No" ~ "black",
    Mono =="Yes" ~ "orange"))

inset_region <- st_bbox(point_det) %>%
  st_as_sfc()

bioma_map <- tm_shape(bioma, bbox = st_bbox(point)) +
  tm_fill("col", alpha = 1) +
  tm_shape(point) +
  tm_symbols(col = "col", size = 0.15, alpha = 0.8) +
  tm_scale_bar() +
  #tm_shape(bboxrjes1) +
  #tm_borders(lwd = 1.5) +
  tm_add_legend(type = "fill",
                col = "darkseagreen4",
                alpha = 1,
                labels = c("BAF")) +
  tm_add_legend(type = "symbol",
                labels = c("Known occurrences",
                           "Confirmed monodominance (≥60%)",
                           "Confirmed non-monodominance (<60%)"),
                col = c("white", "orange", "black")) +
 tm_layout(legend.width = 2,
            legend.bg.color = "white",
            legend.position = c("left", "top")) +
            #legend.outside = TRUE,
#            legend.outside.position = "right",
 #           legend.outside.size = 1.1,
  #          legend.stack = "horizontal"
  tm_shape(br) +
  tm_borders() +
  #tm_compass() +
#  tm_layout(title = expression(paste(italic("M. polymorphum"),
 #                                    " in the Brazilian Atlantic Forest")),
  #          title.fontface = 2,
   #         title.bg.alpha = 1,
    #        title.size = 1.5) +
  NULL
bioma_map

tmap_save(tm = bioma_map, filename = "./figs/7_map_bioma.png")

