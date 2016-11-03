setwd("Y:\\Witnesstree_fia\\Maps_and_figures\\JTMapsR")
library(sp)
library(maptools)
library(RColorBrewer)
library(classInt)

GLO_DIV <- read.csv("D:\\Dropbox\\WT_FIA_Vellend\\GLO_Div.csv")
div.only <- subset(GLO_DIV , select = c("UniqueID",  "Shannon.dif", "Simpson.dif" ))

states <- readShapePoly("NE_states")
towns <- readShapePoly("basic_town_data")
full.shape <- readShapePoly("Y:\\Witnesstree_fia\\PROTOCOL/Spatial/all_study_data2")
study_data <- merge(full.shape, div.only, by.x = "UniqueID", by.y = "UniqueID", all.x = F)

#study_data <- full.shape[full.shape$Min_Thrshd == "0.9" | full.shape$Min_Thrshd == "0.95",]
df <- study_data@data




plotvar <- study_data@data$Shannon.dif
nclr <- 9
plotclr <- brewer.pal(nclr,"YlGnBu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

windows(8.5,8.5)

plot(states, col = "light grey")
plot(study_data, col = colcode, add = T, border = F)

legend(1189757, 2968200, legend= round(class$br, 2)[-length(class$br)],
       fill=attr(colcode, "palette"), cex=1.4, bty="n", title = "Shannon Dif")


require(ncf)
library(ncf)
summary(ols1 <- lm(Shannon.dif ~ 1, data=df)  )

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
     cex=1.15, lwd=1.5, xlab="distance (km)", ylab="Moran's I", cex.lab=1.15, cex.axis=1.15, xaxt = "n", ylim = c(-.1,.4))
at <-   seq(from = 0 , to = 30, by = 5)
axis(1, at = at, labels = at *15, cex.axis = 1.15)

abline(h=0)

savePlot("D:\\Dropbox\\WT_FIA_Vellend\\shannon_map", type="jpeg",  device = dev.cur(), restoreConsole = TRUE )


