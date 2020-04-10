library(minpack.lm)
library(forcats)
library(mgcv)
#4 lai
estructura <- read.csv("./results/estructura.csv", row.names = 1)
levels(estructura$fire)
estructura <- estructura %>%
  mutate(fire = fct_relevel(fire, "High", "Medium", "Low", "Sec.", "Mature"))
names(estructura )


x <- estructura$LAI
y <- estructura$Grass_cover
data <- cbind(x, y)
data <- data.frame(data)

equis <- seq(0, max(x, na.rm = T), 0.01)
par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))
max(x, na.rm = T)

#loess
plot(y ~ x, pch = 16, col = pal[unclass(estructura$fire)], main = "loess")
model <- loess(y ~ x)
yv <- predict(model, equis)
lines(equis, yv, col = "red", lwd = 2)

#
# #gam
plot(y ~ x, pch = 16, col = pal[unclass(estructura$fire)], main = "gam")
gam <- gam(y ~ s(x))
yv <- predict(gam, list(x = equis))
lines(equis, yv, col = "red", lwd = 2)
text(4, 10, paste("AIC =" ,round(AIC(gam),2), "\n R2 = ", round(modelr::rsquare(gam,data),2)))#

plot(y ~ x, pch = 16, col = pal[unclass(estructura$fire)], main = "poli3")
poli3 <- lm(y~x+I(x^3))
yv <- predict(poli3, list(x = equis))
lines(equis, yv, col = "green", lwd = 3)
text(4, 15, paste("AIC =" ,round(AIC(poli3),2), "\n R2 = ", round(modelr::rsquare(poli3,data),2)))

plot(y ~ x, pch = 16, col = pal[unclass(estructura$fire)], main = "poli2")
poli2 <- lm(y~x+I(x^2))
yv <- predict(poli2, list(x = equis))
lines(equis, yv, col = "blue", lwd = 2)
text(4, 20, paste("AIC =" ,round(AIC(poli2),2), "\n R2 = ", round(modelr::rsquare(poli2,data),2)))

# #lineal
plot(y ~ x, pch = 16, col = pal[unclass(estructura$fire)], main = "lineal")
lineal <- lm(y~x)
yv <- predict(lineal, list(x = equis))
lines(equis, yv, col = "red", lwd = 2)
text(4, 10, paste("AIC =" ,round(AIC(lineal),2), "\n R2 = ", round(modelr::rsquare(lineal,data),2)))
#
# #log
plot(y ~ x, pch = 16, col = pal[unclass(estructura$fire)], main = "log")
logg <- lm(y~log(x+1))
yv <- predict(logg, list(x = equis))
lines(equis, yv, col = "red", lwd = 2)
text(4, 10, paste("AIC =" ,round(AIC(logg),2), "\n R2 = ", round(modelr::rsquare(logg,data),2)))


#
# #exp
plot(y ~ x, pch = 16, col = pal[unclass(estructura$fire)], main = "exp")
expp <- lm(y~exp(x) )
yv <- predict(expp, list(x = equis))
lines(equis, yv, col = "red", lwd = 2)
text(4, 10, paste("AIC =" ,round(AIC(expp),2), "\n R2 = ", round(modelr::rsquare(expp,data),2)))


#
#logistic
plot(y ~ x, pch = 16, col = pal[unclass(estructura$fire)], main = "logistic")
logistic <- nlsLM(y ~ Asym/(1 + exp((xmid - log(x))/scal)), #lower = 6, upper = 100,
                 start = list(Asym = 50, xmid = 2, scal = 2), trace = T)
loggg <- nlsLM(y ~ -SSlogis(x, Asym, xmid, scal))
yv <- predict(loggg, list(x = equis))
lines(equis, yv, col = "black", lwd = 3)
text(4, 10, paste("AIC =" ,round(AIC(loggg),2), "\n R2 = ",
                  round(modelr::rsquare(loggg,data),2)))


aic <- AIC(gam, lineal, poli2, poli3 , expp, logg, loggg) %>%
  tibble::rownames_to_column("model")
V <- list(gam, lineal, poli2, poli3 , expp, logg, loggg)
names(V) <- c("gam", "lineal", "poli2", "poli3", "expp", "logg", "loggg")
r2 <- purrr::map_df(V, ~modelr::rsquare(., data = data)) %>% t() %>%
  data.frame() %>% rename(R2 = ".") %>% tibble::rownames_to_column("model") %>% arrange(desc(R2))
rms <- purrr::map_df(V, ~modelr::rmse(., data = data)) %>% t() %>% data.frame() %>% rename(rmse = ".") %>% tibble::rownames_to_column("model")

#log_results <- left_join(aic, r2) %>% left_join(rms) %>% arrange(desc(R2))
results <- left_join(aic, r2) %>% left_join(rms) %>% arrange(desc(R2))
log_results
results
summary(loggg$m)
abline(h = logistic)
names(estructura)
mod1 <- glm(estructura$Grass_cover~., data = estructura[,-c(1,2,3,9, 10, 11, 12, 13, 14, 15, 16, 17,18,19,22)])
mod2 <- glm(estructura$Grass_cover~1, data = estructura[,-c(1,2,3,9, 10, 11, 12, 13, 14, 15, 16, 17,18,19,22)])
mod <- step(mod2, scope = formula(mod1))



#LAI
png(filename = paste("./figs/3lai_grass.png"),
    width = 400 * 300 / 72, height = 380 * 300 / 72, res = 300)
par(mar = c(5, 5, 2, 1), mfrow = c(1, 1))
plot(x, y, pch = 19,
     col = alpha(pal[unclass(estructura$fire)]), xlab = "Leaf Area Index", ylab = "Grass cover (%)", cex = 1.2)
legend("topright", legend = c("High", "Medium", "Low", "Secondary", "Mature"),
       col = pal, pch = 19, bty = "n", cex = 1.2)

#logistic
#logistic <- nlsLM(y ~ Asym/(1 + exp((xmid - log(x))/scal)),
#                start = list(Asym = 0, xmid = 1, scal = 2))
#loggg <- nlsLM(y ~ -SSlogis(x, Asym, xmid, scal))
#yv <- predict(loggg, list(x = equis))
lines(equis, yv, col = "black", lwd = 3)
text(4, 10, paste("AIC =" ,round(AIC(loggg),2), "\n R2 = ", round(modelr::rsquare(loggg,data),2)))
dev.off()
summary(loggg)



##glms
mod1 <- glm(estructura$Grass_cover~., data = estructura[,-c(1,2,3,9, 10, 11, 12, 13, 14, 15, 16, 17,18,19,22)])
mod2 <- glm(estructura$Grass_cover~1, data = estructura[,-c(1,2,3,9, 10, 11, 12, 13, 14, 15, 16, 17,18,19,22)])
mod <- step(mod2, scope = formula(mod1))

mod1 <- glm(estructura$LAI~., data = estructura[,-c(1,2,3,9, 10, 11, 12, 13, 14, 15, 16, 17,18,19,22)])
mod2 <- glm(estructura$LAI~1, data = estructura[,-c(1,2,3,9, 10, 11, 12, 13, 14, 15, 16, 17,18,19,22)])
mod <- step(mod2, scope = formula(mod1))

mod1 <- glm(estructura$dominancia~., data = estructura[,-c(1,2,3,9, 10, 11, 12, 13, 14, 15, 16, 17,18,19,22)])
mod2 <- glm(estructura$dominancia~1, data = estructura[,-c(1,2,3,9, 10, 11, 12, 13, 14, 15, 16, 17,18,19,22)])
mod <- step(mod2, scope = formula(mod1))

