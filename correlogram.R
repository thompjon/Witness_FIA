
require(ncf)
library(ncf)
summary(ols1 <- lm(Shannon.dif ~ peak_ag, data=df)  )

## lets look at the rang of distances between town centers
geodist <- dist(data.frame(df$LONGITUDE, df$LATITUDE))
model <- ols1 # or binom1 or pois1
correlog1.1 <- correlog(df$LONGITUDE, df$LATITUDE, residuals(model),
                        na.rm=T, increment=15, resamp=100, latlon = TRUE)
par(fig=c(.6,1,.0,.4), new = T)
sym <- c(1, 16) # open and filled circles
plot(correlog1.1$correlation[1:30], type="b",
     pch= ifelse(correlog1.1$p[1:30]<0.05,1,16), #correct for multiple testing
     #pch=sym[(p.adjust(correlog1.1$p[1:20], "holm")<0.099)+1], #correct for multiple testing
     cex=1.15, lwd=1.5, xlab="distance (km)", ylab="Moran's I", cex.lab=1.15, cex.axis=1.15, xaxt = "n", ylim = c(-.1,.4), main = "residuals from OLS w Peak_ag")
at <-   seq(from = 0 , to = 30, by = 5)
axis(1, at = at, labels = at *15, cex.axis = 1.15)

abline(h=0)
