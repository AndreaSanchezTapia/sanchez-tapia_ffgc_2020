library(dplyr)
library(wesanderson)
library(vegan)
source("./fct/anovas.paper.R")
pal <- wesanderson::wes_palette("Darjeeling1")[c(1, 4, 3, 5, 2)]
com.ord <-  read.table("./data/comord.txt", header = T, row.names = 1)
traits.ord <-
  read.table("./data/traitsord.txt",
             header = T,
             row.names = 1)

#3_taxonomic
png("./figs/5_taxonomic_turnover_area.png", height = 500 * 300 / 72, width = 900 * 300/72, res = 300)
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
pcoa.tax <- cmdscale(vegdist(decostand(com.ord, "norm"), "eucl"), eig = T, k = 2)
plot.pcoa(pcoa.tax,group = estructura$fire, choices = c(1, 2), cex = 1.2)
abline(h = 0, lty = 2, v = 0)
mtext("A. Floristic composition", 3, las = 1, line = 0.2, cex = 1.5, adj = 0)

# CWM -----
library(FD)
DBFD <- dbFD(traits.ord, a = com.ord, corr = "cailliez", messages = F)
DBFD.bloco <- dbFD(traits.ord, a = com.bloco, corr = "cailliez", messages = F)
names(DBFD$CWM)
CWM <- DBFD$CWM[, c("SLA", "Wdensity", "Bthickness", "Smass",
                    "Height")]
CWM$Area <- estructura$areasCWM
GoCWM <- gowdis(CWM)
pcoaGow <- cmdscale(GoCWM, eig = T)
plot.pcoa(pcoaGow, group = estructura$fire)
abline(h = 0, lty = 2, v = 0)
mtext("B. Functional composition", 3, las = 1, line = 0, cex = 1.5, adj = 0)
legend("bottomright", legend = c("High", "Medium", "Low", "Secondary", "Mature"),
       col = pal,
       pch = 19, bty = "n", cex = 1.2)
dev.off()


par(mfrow = c(1,1))
all <- cbind(est, CWM)
pcoaGow <- cmdscale(GoCWM, eig = T)
pca.all <- rda(scale(all))
png("./figs/4_pca_all.png", width = 400 * 300 / 72, height = 380 * 300 / 72, res = 300)
par(mfrow = c(1,1), mar = c(5, 4, 1, 1))

plot.pca(pca.all, group = estructura$fire)
arrows(0, 0, scores(pca.all)$species[, 1], scores(pca.all)$species[, 2], length = 0.1, col = alpha("grey", 0.6))
text(pca.all, display = "species", cex = 0.7)
legend("topright",
       legend = c("High", "Medium", "Low", "Secondary", "Mature"),
       col = pal,
       pch = 19, bty = "n")
dev.off()
