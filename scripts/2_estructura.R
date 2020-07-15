library(scales)
library(dplyr)
#estructura
source('./fct/anovas.paper.R')
source('./fct/aov.perm.R')



#Lee tablas de datos original
com.ord <-  read.table("./data/comord.txt", header = T, row.names = 1)
traits.ord <-
  read.table("./data/traitsord.txt",
             header = T,
             row.names = 1)


###Formatea estructura


estructura <- read.table("./data/plots/estructura.txt", header = T, sep = "\t")
estructura <- estructura %>%
  mutate(Bloco = as.character(Bloco)) %>%
  mutate(Bloco = ifelse(Bloco == "", NA, Bloco)) %>%
  tidyr::fill(., Bloco)
names(estructura)[4] <- "Plant density"
names(estructura)[5] <- "Mean height"
names(estructura)[6] <- "Basal area"
domina <- com.ord$Mopoly / rowSums(com.ord)
estructura$dominancia <- domina


estructura <- estructura %>%
  mutate(fire = case_when(grepl("1990", Bloco) ~ "Low",
                          grepl("2002", Bloco) ~ "Medium",
                          grepl("2010", Bloco) ~ "High",
                          grepl("Fm", Bloco) ~ "Mature",
                          grepl("Fs", Bloco) ~ "Sec.")) %>%
  mutate(fire = as.factor(fire))
levels(estructura$fire)

estructura <- estructura %>%
  mutate(fire = forcats::fct_relevel(fire, "High", "Medium", "Low", "Sec.", "Mature"))
levels(estructura$fire)

# dominance
par(mar = c(9, 7, 1, 1))
dom <- anovas.paper(x = domina,
                    y = estructura$fire,
                    ylab = expression(paste("Relative ", italic("M. polymorphum "), "density")),
                    bars = F)
mtext(text = c("a", "a", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.1, font = 2)

#el resto de la estructura (repite dominancia)
library(vegan)
library(FD)
anovas.table <- data.frame(row.names = 1:10)
Simpson <- diversity(com.ord, index = "simpson")
estructura$Simpson <- Simpson
rar <- rarefy(com.ord, 1, se = F)## no sé por qué está dando error
estructura$rar <- rar
png(paste("./figs/2_structure.png"),
    width = 1200 * 300 / 72,
    height = 900 * 300 / 72,
    res = 300
)
par(mar = c(7, 7, 2, 1), mfrow = c(3, 3))
{
anovas.table$dom <- anovas.paper(x = estructura$dominancia,
                                 y = estructura$fire,
                                 ylab = expression(paste(italic("M. polymorphum "), "rel. density")),
                                 bars = F,
                                 names = rep(NA, 5))
mtext(text = c("a", "a", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.1, font = 2)
#mtext("A.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()


anovas.table$simp <- anovas.paper(estructura$Simpson,
                                  y = estructura$fire,
                                  ylab = "Simpson's diversity",
                                  names = rep(NA, 5))
mtext(text = c("a", "a", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("B.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()



anovas.table$density <- anovas.paper(estructura$`Plant density`,
                                     y = estructura$fire,
                                     names = rep(NA, 5),
                                     ylab = "Plant density (ind/ha)")
mtext(text = c("a", "b", "c", "d", "c"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("C.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()


anovas.table$height <- anovas.paper(estructura$`Mean height`,
                                    y = estructura$fire,
                                    names = rep(NA, 5),
                                    ylab = "Mean canopy height (m)")
mtext(text = c("a", "a", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("D.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)

#dev.off()




anovas.table$basalarea  <-  anovas.paper(estructura$`Basal area`,
                                         y = estructura$fire,
                                         names = rep(NA, 5),
                                         ylab = expression(paste("Basal area (m"^2,")")))
mtext(text = c("a", "a", "b", "b", "c"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("E.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()



anovas.table$LAI <- anovas.paper(estructura$LAI,
                                 y = estructura$fire,
                                 names = rep(NA, 5),
                                 ylab = "Leaf Area Index (LAI)")
mtext(text = c("a", "b", "c", "d", "e"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("F.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()



anovas.table$Grass <- anovas.paper(estructura$Grass_cover,
                                   y = estructura$fire,
                                   ylab = "Grass cover (%)")
mtext(text = c("a", "b", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("G.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()


anovas.table$ram <- anovas.paper(estructura$ramification,
                                 y = estructura$fire,
                                 #group.names = "",
                                 ylab = "Mean plant ramification")
mtext(text = c("a", "a", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("H.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()

areas3 <- read.csv("./data/areas_all.csv", header = T)
areas.ord <- areas3[order(areas3$cod, names(com.ord)), ]
rownames(areas.ord) <- areas.ord$cod
areasCWM <- dbFD(areas.ord[, c("N", "polygon", "polygon90","pixelsum")], a = com.ord, corr = "cailliez", messages = F)
areasCWM.bin <- dbFD(areas.ord[, c("N", "polygon", "polygon90", "pixelsum")], a = decostand(com.ord, "pa"), corr = "cailliez", messages = F)
areasCWM.bin$CWM
estructura$areasCWM_bin <- areasCWM.bin$CWM$pixelsum
estructura$areasCWM <- areasCWM$CWM$pixelsum


anovas.table$geog_area <- anovas.paper(estructura$areasCWM,
                          y = estructura$fire,
                          ylab = expression(paste("W. mean distr. area (",km^2,")")))
mtext(text = c("a", "a", "b", "c", "ab"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.1, font = 2)
mtext("I.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
}
dev.off()

head(estructura)
write.csv(estructura, "./results/estructura.csv")


#4_functional
## Variation in functional traits along the gradient
# CWM -----
library(FD)
DBFD <- dbFD(traits.ord, a = com.ord, corr = "cailliez", messages = F)

png(filename = paste("./figs/6_functional_traits.png"), width = 800 * 300 / 72, height = 600 * 300 / 72, res = 300)
par(mar = c(7, 7, 2, 1), mfrow = c(2,2))
{
estructura$SLA <- DBFD$CWM$SLA
anovas.table$SLA <- anovas.paper(estructura$SLA,
                                 y = estructura$fire,
                                 names = rep(NA, 5),
                                 ylab = "SLA")
mtext(text = c("a", "a", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("A.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)

estructura$Wdensity <- DBFD$CWM$Wdensity
anovas.table$Wdensity <- anovas.paper(estructura$Wdensity,
                                      y = estructura$fire,
                                      names = rep(NA, 5),
                                      ylab = expression(paste("Wood density (g/cm"
                                                   ^"3",")")))
mtext(text = c("a", "ab", "a", "b", "c"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("B.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)


estructura$Bthickness <- DBFD$CWM$Bthickness

anovas.table$Bthickness <- anovas.paper(estructura$Bthickness/10,
                                        y = estructura$fire,
#                                        group.names = "",
                                        ylab = "Bark thickness (mm)")
mtext(text = c("a", "a", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("C.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)


estructura$Smass <- DBFD$CWM$Smass
anovas.table$Smass <- anovas.paper(estructura$Smass/1000,
                                   y = estructura$fire,
                                   ylab = "Seed mass (g)")
mtext(text = c("ab", "a", "ab", "b", "c"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("D.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
}
dev.off()


anovas.table
write.csv(anovas.table, "./results/anovas_table.csv")
write.csv(estructura, "./results/estructura.csv")



library(dplyr)
anovas.table <- read.csv("./results/anovas_table.csv", row.names = 1)
row.names(anovas.table) <- c("High - Low",
                             "High - Medium",
                             "High - Mature",
                             "High - Secondary",
                             "Medium - Low",
                             "Low - Mature",
                             "Low - Secondary" ,
                             "Medium - Mature",
                             "Medium - Secondary",
                             "Secondary - Mature")
anovas.table <- anovas.table[c(2, 1, 4, 3, 5, 9, 8, 7, 6, 10),]
TABLE <- anovas.table %>%
  mutate(`Dominance` =
           paste0("Diff = ", round(dom.Diff, 2),
                  " (p= ", round(dom.BH.p,3), ") ", dom.BH.sig),
    `Simpson's diversity` =
      paste0("Diff = ", round(simp.Diff, 2),
             " (p= ", round(simp.BH.p,3), ") ", simp.BH.sig),
    `Tree density (ind/ha)` =
      paste0("Diff = ", round(density.Diff, 2),
             " (p= ", round(density.BH.p,3), ") ", density.BH.sig),
    `Mean canopy height (m)` =
      paste0("Diff = ", round(height.Diff, 2),
             " (p= ", round(height.BH.p,3), ") ", height.BH.sig),
    `Basal area (m²/ha)` =
      paste0("Diff = ", round(basalarea.Diff, 2),
             " (p= ", round(basalarea.BH.p,3), ") ", basalarea.BH.sig),
                 `LAI` =
      paste0("Diff = ", round(LAI.Diff, 2),
             " (p= ", round(LAI.BH.p,3), ") ", LAI.BH.sig),
                 `Grass cover (%)` =
      paste0("Diff = ", round(Grass.Diff, 2),
             " (p= ", round(Grass.BH.p,3), ") ", Grass.BH.sig),
                 `Mean plant ramification (number of trunks)` =
      paste0("Diff = ", round(ram.Diff, 2),
             " (p= ", round(ram.BH.p,3), ") ", ram.BH.sig),
                 `Mean geographic area (km²)` =
      paste0("Diff = ", round(geog_area.Diff, 2),
             " (p= ", round(geog_area.BH.p,3), ") ", geog_area.BH.sig),
                                            ) %>%
  select(-contains("."))

#rownames
row.names(TABLE) <- c(
  "High-Medium",
  "High-Low",
  "High-Secondary",
  "High-Mature",
  "Medium-Low",
  "Medium-Secondary",
  "Medium-Mature",
  "Low-Secondary",
  "Low-Mature",
  "Secondary-Mature"
)
TABLE <- TABLE %>% tibble::rownames_to_column("Comparison")
write.csv(TABLE, "./results/Table3_anovas_results.csv")
