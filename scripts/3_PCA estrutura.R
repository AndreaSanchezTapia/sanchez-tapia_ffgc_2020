library(dplyr)
library(wesanderson)
source("./fct/anovas.paper.R")
pal <- wesanderson::wes_palette("Darjeeling1")[c(1, 4, 3, 5, 2)]
#PCA estructura
estructura <- read.csv("./results/estructura.csv", row.names = 1)
levels(estructura$fire)
estructura <- estructura %>% mutate(fire = forcats::fct_relevel(fire, "High", "Medium", "Low", "Sec.", "Mature"))
levels(estructura$fire)
names(estructura)
est <- estructura %>% rename(`Geographic area`  = areasCWM,
                             `Plant density` = Plant.density,
                             `Mean height` = Mean.height,
                             `Basal area` = Basal.area,
                             `Grass cover` = Grass_cover) %>%
  select(`Plant density`,
         `Mean height`,
         `Grass cover`,
         LAI,
         `Basal area`,
#         `Geographic area`,
         ramification) %>% rename(`Ram.` = ramification, 
                                  `Grass %` = `Grass cover`)
pca.est <- rda(scale(est))

png("./figs/4_pca_estrutura_no_area.png", width = 400 * 300 / 72, height = 380 * 300 / 72, res = 300)
par(mfrow = c(1,1), mar = c(5, 4, 1, 1))
plot.pca(pca.est, group = estructura$fire)
arrows(0, 0, scores(pca.est)$species[, 1], scores(pca.est)$species[, 2], length = 0.1, col = alpha("grey", 0.6))
text(pca.est, display = "species", cex = 0.7)
legend("topright",
       legend = c("High", "Medium", "Low", "Secondary", "Mature"),
       col = pal,
       pch = 19, bty = "n")
dev.off()


