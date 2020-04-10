library(vegan)
#Table1

aa <- colSums(com.ord[1:18, ])
bb <- colSums(com.ord[19:36, ])
cc <- colSums(com.ord[37:54, ])
dd <- colSums(com.ord[55:72, ])
ee <- colSums(com.ord[73:90, ])
gg <- data.frame(High = cc, Medium = bb, Low = aa, Secondary = ee, Mature = dd)
gg <- decostand(gg, "pa")
gg <- gg %>% tibble::rownames_to_column("Code")
head(gg)

nomes <- read.table("./data/nomes.txt") %>% 
  rename(genus = cod, epithet = nome) %>% 
  tibble::rownames_to_column("Code") %>% 
  mutate(Species = paste(genus, epithet))
head(nomes)
gg <- left_join(gg,nomes) %>% select(Species, 2:6, Code)
write.csv(gg, "./data/Table1_PA_table.csv")
