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
  LEGEND == "CERRADO" ~  "grey75",
  !LEGEND %in% c("ATLANTIC FOREST", "CERRADO") ~ "grey75"))
bioma <- bioma %>%
  mutate(lab = if_else(LEGEND == "CERRADO", "BAF", "")) #This is only for label positioning

# sets color for the whole set of points and
point <- point %>%
  mutate(col = case_when(
  Mono =="Occurrence" ~ "white",
  Mono =="No" ~ "black",
  Mono =="Yes" ~ "red"))

# ...for the points that were actually visited----
point_det <- point %>%
  filter(Mono != "Occurrence") %>%
  mutate(col = case_when(
    Mono =="No" ~ "black",
    Mono =="Yes" ~ "red"))

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
                           "Confirmed monodominance (>60%)",
                           "Confirmed non-monodominance (<60%)"),
                col = c("white", "red", "black")) +
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

tmap_save(tm = bioma_map, filename = "./figs/7_map_bioma12.png")

#detailed inset


bioma_det <- tm_shape(bioma, bbox = st_bbox(bboxrjes1)) +
  tm_fill("col", alpha = 1) +
  tm_shape(point_det) +
  tm_symbols(col = "col", size = 0.3, alpha = 0.7) +
#  tm_add_legend(type = "fill",
 #               col = "darkseagreen4",
  #              alpha = 1,
   #             labels = c("BAF")) +
#  tm_add_legend(type = "symbol",
 #               labels = c("Confirmed monodominance (>60%)",
  #                         "Confirmed non-monodominance (<60%)"),
   #             col = c("white", "red", "black"),
    #            size = 0.3) +
#  tm_layout(title = expression(paste(italic("M. polymorphum"),
 #                                    " in the Brazilian Atlantic Forest")),
  #          title.fontface = 2,
   #         title.bg.alpha = 1,
    #        title.size = 1.5) +
  NULL

bioma_det
png(filename = "./figs/7_map_bioma10.png", res = 300,
    width = 300 / 72 * 500, height = 300 / 72 * 500)
bioma_map
print(bioma_det, vp = viewport(0.7, 0.25, width = 0.3, height = 0.3))
dev.off()




# in which biomes is the species present?
st_join(point, bioma, st_intersects) %>% count(CD_LEGENDA)
# in which states is the species present?
st_crs(point)
estados_shp <- estados_shp %>%  st_transform(st_crs(point))
estados_intersect <- st_join(point, estados_shp, st_intersects) %>% count(nome)
#27 caatinga, 82 cerrado, 395 Baf, 26 pampa

# reads all the sosma shapefiles
dirs <- list.files("./data/sosma", full.names = TRUE, recursive = TRUE, pattern = ".zip")

#unzips

#purrr::map_df(dirs, ~unzip(zipfile = ., exdir = "./data/sosma"))

sosma_all <- list.files("./data/sosma", pattern = ".shp$", full.names = TRUE)
estados <- stringr::str_sub(basename(sosma_all), 1, 2)
sosma_all <- purrr::map(sosma_all, ~read_sf(.))
names(sosma_all) <- estados

# only forest
mata <- purrr::map(sosma_all, ~filter(., legenda == "Mata"))
#only bounding boxes
bboxes <- purrr::map(mata, ~st_bbox(.))
bboxes <- purrr::map(bboxes, ~st_as_sfc(.))
bbox_all <- st_union(bboxes[[1]], bboxes[[2]]) %>%
  st_union(bboxes[[3]]) %>%
  st_union(bboxes[[4]]) %>%
  st_union(bboxes[[5]]) %>%
  st_union(bboxes[[6]]) %>%
  st_union(bboxes[[7]]) %>%
  st_union(bboxes[[8]]) %>%
  st_union(bboxes[[9]])

bbox_rjes <- st_union(bboxes$RJ, bboxes$ES)
bboxrjes1 <- st_bbox(bbox_rjes) %>% st_as_sfc()

###interseções só com mata----
intersections <- purrr::map(.x = mata,
                            ~st_join(x = point,
                                     y = .x,
                                     join = st_intersects))
points_in_mata <- purrr::map(intersections, ~count(., legenda, UF))
estados_intersect <- estados_intersect %>% mutate(UF = as.factor(case_when(
  nome == "BAHIA" ~ "BA",
  nome == "ESPÍRITO SANTO" ~ "ES",
  nome == "RIO DE JANEIRO" ~ "RJ",
  nome == "SÃO PAULO" ~ "SP",
  nome == "MINAS GERAIS" ~ "MG",
  nome == "PARANÁ" ~ "PR",
  nome == "RIO GRANDE DO SUL" ~ "RS",
  nome == "DISTRITO FEDERAL" ~ "DF",
  nome == "GOIÁS" ~ "GO",
  nome == "MATO GROSSO DO SUL" ~ "MS",
  nome == "SANTA CATARINA" ~ "SC"
)))
estados_pontos <- estados_intersect %>% data.frame() %>% mutate(legenda = "pontos totais")

# only 98 points in forest features
dominancia_in_mata <- purrr::map(intersections, ~count(., legenda, Mono, UF))

bind_rows(points_in_mata) %>% filter(!is.na(UF))
bind_rows(dominancia_in_mata) %>% filter(!is.na(UF)) %>% count(Mono)
# 47 no, 48 occs (unvisited) 3 yes

# in which features are these points
inter_tudo <- purrr::map(.x = sosma_all,
                            ~st_join(x = point,
                                     y = .x,
                                     join = st_intersects))
inter_tudo
points_in_baf <- purrr::map(inter_tudo, ~count(., legenda, UF))
points_in_baf2 <- purrr::map(inter_tudo, ~count(., legenda))
dominancia_in_baf <- purrr::map(inter_tudo, ~count(., legenda, Mono, UF))

bind_rows(points_in_baf) %>% filter(!is.na(UF)) %>%
  count(legenda)
# 98 in forest, 41 in urbanized areas, 13 in non forest areas: 152 points

bind_rows(dominancia_in_baf) %>% filter(!is.na(UF)) %>%
  #count(legenda, Mono)
  tally()

#395 Baf  only 152 fall into some part of the shape
inter_tudo_one <- bind_rows(inter_tudo) %>% filter(!is.na(UF))
estados_pontos
inter_tudo_one %>% count(Mono, UF, legenda) %>% bind_rows(estados_pontos) %>% View()

