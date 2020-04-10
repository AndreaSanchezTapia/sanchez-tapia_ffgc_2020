pal <- wesanderson::wes_palette("Darjeeling1")[c(1, 4, 3, 5, 2)]
anovas.paper <- function(x,
                         y,
                         at = c(1, 2, 3, 4, 5),
                         bars = FALSE,
                         ylab,
                         bplot = TRUE,
                         mean = FALSE, ...) {
  
    if (bars == TRUE) {
      def.par <- par(no.readonly = TRUE)
      layout(matrix(c(2, 1, 1, 1, 1, 1), 6, 1, byrow = TRUE))
      par(mar = c(5, 4, 0, 1))
    }
    sig <- function(p) {
      s <- character(length = dim(p)[1])
      for (i in 1:dim(p)[1]) {
        if (p[i] < 0.1) {
          s[i] = "•"
          if (p[i] < 0.05) {
            s[i] = "*"
            if (p[i] < 0.01) {
              s[i] = "**"
              if (p[i] < 0.001) {
                s[i] = "***"
              }
            }
          }
        }
      }
      return(s)
    }
    
    
    stripchart(x ~ y,
               #bty = "n",
               vertical = TRUE,
               pch = 19,
               cex = 1,
               at = at,
               las = 2,
               axes = F,
               mgp = c(4.5, 1, 1),
               method = "jitter",
               ylab = ylab,
               cex.lab = 2,
               cex.axis = 2,
               #bg = "white",
               jitter = 0.05,
               col = alpha(pal, 0.7),
               xlim = c(0.5, 5.5))

    medias <- tapply(x, INDEX = y, FUN = mean)
    if (mean == TRUE) {
      segments(
        x0 = at - 0.1,
        y0 = medias,
        x1 = at + 0.1,
        y1 = medias,
        col = "gray",
        lwd = 3
      )
    }
    if (bplot == TRUE) {
      boxplot(
        x ~ y,
        #bty = "n",
        at = at,
        #mgp = c(4, 1, 1),
        add = T,
        col = alpha(pal, 0.5),
        #border = alpha(c("red", "yellow", "darkgreen", "orange", "green"),1),
        las = 2,
        cex.lab = 2,
        cex.axis = 2,
        pch = ".",
        ...
      )
    }
    
    #library(lmPerm)
    #anov <- aovp(x~as.factor(y),perm="Exact",settings=FALSE)
    #tuk <- TukeyHSD(anov,ordered=F)[1][[1]]
    A <- aov.perm(x, y, B = 1000, balanced = TRUE)
    holm <- p.adjust(A$Partial.p.value[, 2], "holm")
    holm.sig <- sig(as.matrix(holm))
    BH.p <- p.adjust(A$Partial.p.value[, 2], method = "BH")
    BH.sig <- sig(as.matrix(BH.p))
    ANOVA <-
      cbind(A$Partial.p.value, holm, holm.sig, BH.p, BH.sig)###sumando la corrección de bonferroni
    #print(rownames(tuk)[sign(tuk[,"upr"])==sign(tuk[,"lwr"])])#hay diff
    #comparacion <- sign(tuk[,"upr"])==sign(tuk[,"lwr"])
    comp.ord <- ANOVA[c(3, 1, 4, 2, 6, 10, 8, 7, 5, 9), ]
    if (bars == TRUE) {
      par(mar = c(0, 4, 0, 1))
      plot(
        x,
        y,
        type = "n",
        xlab = "",
        ylab = "",
        axes = F,
        xlim = c(1, 5)
      )
    }
    table <-
      cbind(
        comp.ord[, 'BH.p'] < 0.05,
        c(1.4, 1.4, 1.4, 1.4, 2.2, 2.2, 2.2, 3, 3, 3.8),
        c(2.2, 3, 3.8, 4.6, 3, 3.8, 4.6, 3.8, 4.6, 4.6),
        seq(1.5, 4.8, length.out = 10)
      )
    colnames(table) <- c("true", "x1", "x2", "y")
    table <- data.frame(table)
    if (bars == TRUE) {
      for (i in 1:nrow(table)) {
        if (table$true[i] == 0)
          arrows(
            x0 = table$x1[i],
            y0 = table$y[i],
            x1 = table$x2[i],
            y1 = table$y[i],
            col = "red",
            length = 0.02,
            angle = 90,
            code = 3
          )
        par(def.par)
        
      }
    }
    return(comp.ord)
  }

plot.pcoa <- function(pcoa,
                      group = fire,
                      choices = c(1, 2),
                      cex = 1) {
  library(scales)
  library(vegan)
  plot(
    pcoa$points[, choices[1]],
    pcoa$points[, choices[2]],
    xlab = paste0("PC", choices[1], " ", round(pcoa$eig[choices[1]] / sum(pcoa$eig) *
                                                 100, 2), "%"),
    ylab = paste0("PC", choices[2], " ", round(pcoa$eig[choices[2]] / sum(pcoa$eig) *
                                                 100, 2), "%"),
    col = alpha(
      pal[unclass(group)], 0.8),
    pch = 19,
    cex = cex
  )
  #coord <- locator(n=1)
  #legend(coord$x,coord$y,legend=unique(group)[c(3,2,1,5,4)],pch=21,pt.bg=alpha(c("red","orange","yellow","green","darkgreen"),0.9))
}

plot.pca <- function(ord,
                     group = fire,
                     choices = c(1, 2)) {
  library(scales)
  library(vegan)
  AXIS1 <- round(eigenvals(ord)[1] / sum(eigenvals(ord)) * 100, 2)
  AXIS2 <- round(eigenvals(ord)[2] / sum(eigenvals(ord)) * 100, 2)
  plot(
    ord,
    type = "n",
    xlab = paste0("PC", choices[1], " ", AXIS1, "%"),
    ylab = paste0("PC", choices[2], " ", AXIS2, "%"),
    scaling = 2
  )
  points(
    ord,
    display = "sites",
    col = alpha(pal[unclass(group)], 0.8),
    pch = 19,
    cex = 1,
    scaling = 2
  )
  #points(ord,display="species",pch=3,cex=0.6)
  #text(rda(decostand(com.ord,"hellinger")),display="species",pch=3,cex=0.6,font=2)
  #coord <- locator(n=1)
  #legend(coord$x,coord$y,legend=unique(group)[c(3,2,1,5,4)],pch=21,pt.bg=alpha(c("red","orange","yellow","green","darkgreen"),0.9))
}
