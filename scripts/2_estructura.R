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
Simpson <- diversity(com.ord, index = "simpson")
estructura$Simpson <- Simpson

estructura <- estructura %>%
  mutate(fire = forcats::fct_relevel(fire, "High", "Medium", "Low", "Sec.", "Mature"))


# dominance
par(mar = c(9, 7, 1, 1))
dom <- anovas.paper(x = domina,
                    y = estructura$fire,
                    ylab = expression(paste("Relative ", italic("M. polymorphum "), "density")),
                    bars = FALSE)
mtext(text = c("a", "a", "b", "c", "d"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.1, font = 2)

#estructura por bloco n= 3

#el resto de la estructura (repite dominancia)
library(vegan)
library(FD)
anovas.table <- data.frame(row.names = 1:10)

rar <- rarefy(com.ord, 1, se = F)
estructura$rar <- rar
png(paste("./figs/2_structure_blocos.png"),
    width = 1200 * 300 / 72,
    height = 900 * 300 / 72,
    res = 300
)

estructura2 <- estructura %>% group_by(Bloco) %>% summarise_all(.funs = mean)
estructura2$fire <- as.factor(rep(c("Low", "Medium", "High", "Mature", "Sec."), each = 3))
estructura2 <- estructura2 %>%
  mutate(fire = forcats::fct_relevel(fire, "High", "Medium", "Low", "Sec.", "Mature"))

par(mar = c(7, 7, 2, 1), mfrow = c(3, 3))

anovas.table$dom <- anovas.paper(x = estructura2$dominancia,
                                 y = estructura2$fire,
                                 ylab = expression(paste(italic("M. polymorphum "), "rel. density")),
                                 bars = F,
                                 names = rep(NA, 5))
mtext(text = c("a", "a", "a", "ab", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.1, font = 2)
#mtext("A.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()


anovas.table$simp <- anovas.paper(estructura2$Simpson,
                                  y = estructura2$fire,
                                  ylab = "Simpson's diversity",
                                  names = rep(NA, 5))
mtext(text = c("a", "a", "ab", "bc", "c"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("B.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()



anovas.table$density <- anovas.paper(estructura2$`Plant density`,
                                     y = estructura2$fire,
                                     names = rep(NA, 5),
                                     ylab = "Plant density (ind/ha)")
mtext(text = c("a", "a", "ab", "b", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("C.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()


anovas.table$height <- anovas.paper(estructura2$`Mean height`,
                                    y = estructura2$fire,
                                    names = rep(NA, 5),
                                    ylab = "Mean canopy height (m)")
mtext(text = c("a", "a", "a", "a", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("D.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)

#dev.off()




anovas.table$basalarea  <-  anovas.paper(estructura2$`Basal area`,
                                         y = estructura2$fire,
                                         names = rep(NA, 5),
                                         ylab = expression(paste("Basal area (m"^2,")")))
mtext(text = c("a", "a", "a", "a", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("E.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()



anovas.table$LAI <- anovas.paper(estructura2$LAI,
                                 y = estructura2$fire,
                                 names = rep(NA, 5),
                                 ylab = "Leaf Area Index (LAI)")
mtext(text = c("a", "a", "b", "b", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("F.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()



anovas.table$Grass <- anovas.paper(estructura2$Grass_cover,
                                   y = estructura2$fire,
                                   ylab = "Grass cover (%)")
mtext(text = c("a", "b", "b", "b", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("G.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)
#dev.off()


anovas.table$ram <- anovas.paper(estructura2$ramification,
                                 y = estructura2$fire,
                                 #group.names = "",
                                 ylab = "Mean plant ramification")
mtext(text = c("a", "a", "b", "b", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
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
estructura2$areasCWM_bin <- tapply(estructura$areasCWM_bin, INDEX = estructura$Bloco, FUN = mean)
estructura2$areasCWM <- tapply(estructura$areasCWM, INDEX = estructura$Bloco, FUN = mean)

anovas.table$geog_area <- anovas.paper(estructura2$areasCWM,
                          y = estructura2$fire,
                          ylab = expression(paste("W. mean distr. area (",km^2,")")))
mtext(text = c("ab", "ab", "ac", "cd", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.1, font = 2)
mtext("I.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)

dev.off()

head(estructura)


#4_functional
## Variation in functional traits along the gradient
# CWM -----
library(FD)
DBFD <- dbFD(traits.ord, a = com.ord, corr = "cailliez", messages = F)

png(filename = paste("./figs/6_functional_traits_bloco.png"), width = 800 * 300 / 72, height = 600 * 300 / 72, res = 300)
par(mar = c(7, 7, 2, 1), mfrow = c(2,2))

estructura$SLA <- DBFD$CWM$SLA
estructura2$SLA <- tapply(estructura$SLA, estructura$Bloco, mean)
anovas.table$SLA <- anovas.paper(estructura2$SLA,
                                 y = estructura2$fire,
                                 names = rep(NA, 5),
                                 ylab = "SLA")
mtext(text = c("a", "a", "a", "ab", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("A.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)

estructura$Wdensity <- DBFD$CWM$Wdensity
estructura2$Wdensity <- tapply(estructura$Wdensity, estructura$Bloco, mean)
anovas.table$Wdensity <- anovas.paper(estructura2$Wdensity,
                                      y = estructura2$fire,
                                      names = rep(NA, 5),
                                      ylab = expression(paste("Wood density (g/cm"^"3",")")))
mtext(text = c("a", "a", "a", "ab", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("B.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)


estructura$Bthickness <- DBFD$CWM$Bthickness
estructura2$Bthickness <- tapply(estructura$Bthickness, estructura$Bloco, mean)
anovas.table$Bthickness <- anovas.paper(estructura2$Bthickness/10,
                                        y = estructura2$fire,
#                                        group.names = "",
                                        ylab = "Bark thickness (mm)")
mtext(text = c("a", "a", "a", "ab", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("C.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)


estructura$Smass <- DBFD$CWM$Smass
estructura2$Smass <- tapply(estructura$Smass, estructura$Bloco, mean)
anovas.table$Smass <- anovas.paper(estructura2$Smass/1000,
                                   y = estructura2$fire,
                                   ylab = "Seed mass (g)")
mtext(text = c("a", "a", "a", "a", "b"), side = 3, line = 0, at = c(1, 2, 3, 4, 5), cex = 1.2, font = 2)
mtext("D.", 3, las = 1, line = 0.2, cex = 1.5, at = 0.1)

dev.off()

head(estructura)
anovas.table
write.csv(anovas.table, "./results/anovas_table_bloco.csv")
write.csv(estructura, "./results/estructura.csv")
write.csv(estructura2, "./results/estructura2.csv")


# Formats anova table
library(dplyr)
#anovas.table <- readr::read_csv("./results/anovas_table.csv")
anovas.table <- readr::read_csv("./results/anovas_table_bloco.csv")
anovas.table <- anovas.table[,-1]
names(anovas.table)
#reorder rows
TABLE <- anovas.table %>%
  mutate(Comparison = dom.comparison) %>%
  mutate(`Dominance` =
           paste0("Diff = ", round(dom.Diff, 2),
                  " (p= ", round(dom.p,3), ") ", dom.sig.),
    `Simpson's diversity` =
      paste0("Diff = ", round(simp.Diff, 2),
             " (p= ", round(simp.p,3), ") ", simp.sig.),
    `Tree density (ind/ha)` =
      paste0("Diff = ", round(density.Diff, 2),
             " (p= ", round(density.p,3), ") ", density.sig.),
    `Mean canopy height (m)` =
      paste0("Diff = ", round(height.Diff, 2),
             " (p= ", round(height.p,3), ") ", height.sig.),
    `Basal area (m²/ha)` =
      paste0("Diff = ", round(basalarea.Diff, 2),
             " (p= ", round(basalarea.p,3), ") ", basalarea.sig.),
    `LAI` =
      paste0("Diff = ", round(LAI.Diff, 2),
             " (p= ", round(LAI.p,3), ") ", LAI.sig.),
    `Grass cover (%)` =
      paste0("Diff = ", round(Grass.Diff, 2),
             " (p= ", round(Grass.p,3), ") ", Grass.sig.),
                 `Mean plant ramification (number of trunks)` =
      paste0("Diff = ", round(ram.Diff, 2),
             " (p= ", round(ram.p,3), ") ", ram.sig.),
    `Mean geographic area (km²)` =
      paste0("Diff = ", round(geog_area.Diff, 2),
             " (p= ", round(geog_area.p,3), ") ", geog_area.sig.),
    `SLA ` =
      paste0("Diff = ", round(SLA.Diff, 2),
             " (p= ", round(SLA.p,3), ") ", SLA.sig.),
    `Wood density` =
      paste0("Diff = ", round(Wdensity.Diff, 2),
             " (p= ", round(Wdensity.p,3), ") ", Wdensity.sig.),
    `Bark thickness` =
      paste0("Diff = ", round(Bthickness.Diff, 2),
             " (p= ", round(Bthickness.p,3), ") ", Bthickness.sig.),
    `Seed mass` =
      paste0("Diff = ", round(Smass.Diff, 2),
             " (p= ", round(Smass.p,3), ") ", Smass.sig.)
                                            ) %>%
  select(-contains("."))

#rownames
names(TABLE)


write.csv(TABLE, "./results/Table3_anovas_results_bloco.csv")

