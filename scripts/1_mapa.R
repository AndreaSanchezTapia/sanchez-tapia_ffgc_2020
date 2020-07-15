library(rgdal)
library(scales)
library(maps)
#palette change in july 2020
library(RColorBrewer)
pal <- brewer.pal(5, "PiYG")

#reads official shapefiles
PDA     <- readOGR("./data/1_map/shapefiles/rbpda/", "LimitedaRB", verbose = F)
br101   <- readOGR("./data/1_map/shapefiles/rbpda/", "BR_101", verbose = F)
saojoao <- readOGR("./data/1_map/shapefiles/rbpda/", "riosaojoao", verbose = F)
SOSma   <- readOGR(dsn = "./data/1_map/shapefiles/brasil/SOSMA/",
                   layer = "rema_fd4d801731725513a4d77aa9bb35534b5305",
                   verbose = F)
#assigns the correct CRS (UTM in Brazil SAD69)
proj4string(PDA)     <- CRS("+init=epsg:29193")
proj4string(br101)   <- CRS("+init=epsg:29193")
proj4string(saojoao) <- CRS("+init=epsg:29193")

# reprojects to WGS84
PDA.wsg84     <- spTransform(PDA, CRS("+proj=longlat +datum=WGS84"))
SOS.wgs84     <- spTransform(SOSma, CRS("+proj=longlat +datum=WGS84"))
br101.wgs84   <- spTransform(br101, CRS("+proj=longlat +datum=WGS84"))
saojoao.wgs84 <- spTransform(saojoao, CRS("+proj=longlat +datum=WGS84"))

# reads the shapefiles of the fires - digitalized by AST from paper arquives from the PABR
#1993 fire
fuego1993 <- readOGR(dsn = "./data/1_map/queimadas", layer = "incendio1993" , verbose = F)
proj4string(fuego1993) <- CRS("+init=epsg:29193")## isto é UTM em SAD69 brasileiro
fuego1993.wsg84 <- spTransform(fuego1993, CRS("+proj=longlat +datum=WGS84"))

#2002 fire
fuego2002 <- readOGR(dsn = "./data/1_map/queimadas", layer = "incendio2002", verbose = F)
fuego2002.wsg84 <- spTransform(fuego2002, CRS("+proj=longlat +datum=WGS84"))

#2007 fire
fuego2007 <- readOGR(dsn = "./data/1_map/queimadas", layer = "Incendio_2007", verbose = F)
fuego2007.wsg84 <- spTransform(fuego2007, CRS("+proj=longlat +datum=WGS84"))

#2010 fire
fuego2010 <- readOGR(dsn = "./data/1_map/queimadas", layer = "incendio2010p", verbose = F)
fuego2010.wsg84 <- spTransform(fuego2010, CRS("+proj=longlat +datum=WGS84"))

# reads plot shapefiles
#2002, 2010
parcelas <- readOGR(dsn = "./data/1_map/shapefiles/", layer = "parcelasandre", verbose = F)
parcelas.wsg84 <- spTransform(parcelas, CRS("+proj=longlat +datum=WGS84"))
# 1990, secondary forest, mature forest
parcelas_j <- readOGR(dsn = "./data/1_map/shapefiles/", layer = "parcelasjeronimo", verbose = F)
parcelas_j_wsg84 <- spTransform(parcelas_j, CRS("+proj=longlat +datum=WGS84"))

#creates the map and saves it to png file
png("./figs/1_map.png", res = 300, height = 500*300/72, width = 500*300/72)
par(mfrow = c(1,1), mar = c( 3, 3, 1.5, 1))
plot(PDA.wsg84, add = F, border = "black")
plot(SOS.wgs84, add = T, col = alpha("lightgreen", 0.3), border = alpha("darkgreen", 0.1))
plot(fuego1993.wsg84, add = T, border = "grey90", col = alpha("grey", 0.4))
plot(fuego2002.wsg84, add = T, border = "grey90", col = alpha("grey", 0.4))
plot(fuego2007.wsg84, add = T, border = "grey90", col = alpha("grey", 0.4))
plot(fuego2010.wsg84, add = T, border = "grey90", col = alpha("grey", 0.4))
plot(saojoao.wgs84,   add = T, border = "dodgerblue", lwd = 2)
plot(br101.wgs84,     add = T, col = "red", lwd = 2)
plot(PDA.wsg84,       add = T, border = "black", lwd = 1.5)
map.scale(ratio = F, cex = 1.4)
map.axes(cex.axis = 1.4)
#plot points
points(parcelas.wsg84, bg = rep(c(pal[1], pal[2]), each = 3), pch = 21, cex = rep(c(1.5, 1.5), each = 3), col = "black")
points(parcelas_j_wsg84[1:9, ], bg = rep(c(pal[3:5]), each = 3), pch = 21, cex = rep(c(1.5, 1.5, 1.5), each = 3), col = "black")
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


