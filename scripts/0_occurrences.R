# reads community matrix
com.ord <-  read.table("./data/comord.txt", header = T, row.names = 1)
# list of names and taxonomy
nomes <- read.table("./data/nomes.txt", header = T, sep = "\t")
# not all names collected are in the plots
nomes.filt <- nomes[nomes$cod %in% names(com.ord), ]
# creates genera
genera <- strsplit(as.character(nomes.filt$nome), split = " ")

#creates a data.frame with genera and specific epithet
nomes.filt$gen <- t(data.frame(strsplit(as.character(nomes.filt$nome), " "))[1, ])[, "1"]
nomes.filt$epiteto <- t(data.frame(strsplit(as.character(nomes.filt$nome), " "))[2, ])[, "2"]
head(nomes.filt)

# occurrence download
library(rgbif)
library(dplyr)
nombres <- droplevels(nomes.filt$nome)

#This will take a while and might change with time----
#for each name
for (i in seq_along(nombres)) {
   print(nombres[i])
  #suggest a key
   key <- name_suggest(q = nombres[i], rank = 'species')$key
   print(key)
   if (!is.null(key)) {
     occs <- list()
     for (k in 1:length(key)) {
       occs[[k]] <- occ_search(
         taxonKey = key[k],
         limit = 100000,
         hasCoordinate = TRUE,
         basisOfRecord = "PRESERVED_SPECIMEN",
         hasGeospatialIssue = F,
         return = 'data',
         fields = "minimal"
       )
     }

     print(lapply(occs,dim))
     if (any(!is.null(lapply(occs,dim)))) {
       dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
       occs.f <- subset(occs, dim.null == T)
       occs.f <- bind_rows(occs.f)
       occs.f <- occs.f[!duplicated(occs.f[,c(1,3,4)]),]
       print(dim(occs.f))
       write.csv(
         x = data.frame(occs.f),
         file = paste0("./results/geo/data/", nombres[i], ".csv")
       )
     }
   } else {

     cat(paste("No key found for", nombres[i], "\n"))
   }
   cat(paste(nombres[i], "DONE", "\n"))
 }

#some manual searches due to name errors
which(nombres == "Schinus terebinthifolius")
levels(nombres)[102] <- "Schinus terebinthifolia"
which(nombres == "Xylosma glaberrima")
levels(nombres)[121] <- "Xylosma glaberrimum"
nombres[121]
which(nombres == "Maytenus samydaeformis")
levels(nombres)

##name corrections
nomes.filt$nome <- nombres
nomes.filt$epiteto <- t(data.frame(strsplit(as.character(nomes.filt$nome), " "))[2, ])[, "2"]
View(nomes.filt)#good

## makes the maps to calculate AOO
library(maps)
library(adehabitatHR)
library(data.table)
data(worldMapEnv)

## data cleaning
nomes.filt$nome <- droplevels(nomes.filt$nome)

# downloads template worldclim rasters
library(raster)
tmean <-  getData('worldclim', var = 'tmean', res = 2.5)

#1 removes NAs (points outside the rasters)----
for (i in seq_along(nombres)) {
  (sp <- nombres[i])
  if (file.exists(paste0("./results/geo/data/", sp, ".csv"))) {
    a <- read.csv(paste0("./results/geo/data/", sp, ".csv"),
                  header = TRUE,
                  row.names = 1)
    coord <- a[,c("decimalLongitude", "decimalLatitude")]
    b <- raster::extract(tmean, coord)
    c <- cbind(a, b)
    d <- c[complete.cases(b),]
    write.csv(
      x = d,
      file = paste0("./results/geo/clean/", nombres[i], ".csv")
    )
  }
}

# creates a data.frame with number of records, convex hull areas
areas2 <- nomes.filt
areas2$N <- 0
areas2$polygon <- 0
areas2$polygon90 <- 0
head(areas2)
for (i in seq_along(nombres)) {
  (sp <- areas2$nome[i])
  if (file.exists(paste0("./results/geo/clean/", sp, ".csv"))) {
    a <- read.csv(paste0("./results/geo/clean/", sp, ".csv"),
                  header = TRUE,
                  row.names = 1)
    # maps
    map('world')
    #equador
    abline(h = 0, lty = 2)
    axis(1, las = 1)
    axis(2, las = 1)
    box()
    #occurrence points
    points(a$decimalLongitude, a$decimalLatitude, cex = 1,
           pch = 21, col = "red")
    mtext(text = nomes.filt$nome[i],  side = 3, font = 3, cex = 1)
    # already complete cases

    #coords
    coord <- cbind(a$decimalLongitude, a$decimalLatitude)
    areas2$N[i] <- nrow(coord)

    if (nrow(coord) < 5) {
      cat(paste("not enough records for",sp))
    } else {
      mcp.sp <- mcp(SpatialPoints(coord), percent = 100)
      mcp.sp95 <- mcp(SpatialPoints(coord), percent = 95)
      mcp.sp90 <- mcp(SpatialPoints(coord), percent = 90)

      plot(mcp.sp90, add = T, border = "red", lwd = 2)

      areas2$polygon[i] <- mcp.sp$area
      areas2$polygon90[i] <- mcp.sp90$area

    }
    print(areas2[i,])
  } else {
    cat(paste("no occurrences were found for " , sp))
  }
}

write.table(areas2, "./data/areas2.txt")

#######so far only polygons but we will use AOO instead######----
#count pixels-----

write.csv(nomes.filt,"./data/nomes_clean.csv")
nomes.filt <- read.csv("./data/nomes_clean.csv", row.names = 1)
areas2hd <- read.table("./data/areas2.txt")
tibble::glimpse(areas2hd)
nombres <- areas2hd$nome
setdiff(areas2hd$nome, nombres)
# tmean is already in disk
tmean <-  getData('worldclim', var = 'tmean', res = 2.5)

# reads each clean occ table

pixel.sum <- list()
for (i in seq_along(areas2hd$nome)) {
  (sp <- areas2hd$nome[i])
  if (file.exists(paste0("./results/geo/clean/", sp, ".csv"))) {
    a <- read.csv(paste0("./results/geo/clean/", sp, ".csv"),
                  header = TRUE,
                  row.names = 1)
      coord <- cbind(a$decimalLongitude, a$decimalLatitude)

      #rasterizes the presence points and counts pixels
    presences <- raster::rasterize(coord, tmean[[1]], field = 1)
    pa <- getValues(presences)
    pixel.sum[[i]] <- data.frame(nome = sp, pixelsum = sum(pa, na.rm = T))
  } else {
    pixel.sum[[i]] <- data.frame(nome = sp, pixelsum = 0)
  }
}
pixel.sum <- data.table::rbindlist(pixel.sum)

#joins all the area-related variables and saves a dataframe that will be used later to calculate geographical area CWM---
areas_tudo <- dplyr::left_join(areas2hd, pixel.sum, by = "nome")
write.csv(areas_tudo, file = "./data/areas_all.csv")
