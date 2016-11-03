#GLO <- read.csv("c:/Users/User/Documents/MV_temp/DivChange/WT_FIA_HarvardForest_MV.csv", header = TRUE)
 GLO <- read.csv("D:\\Dropbox\\WT_FIA_Vellend\\WT_FIA_HarvardForest_MV.csv")

head(GLO)

# check to see if "other" taxa are common (answer: no)
hist(GLO$OTHER_WT)
hist(GLO$OTHER_FIA)

# just the veg data for each time period (wt and fia)
GLO.wt <- GLO[,36:60]
GLO.fia <- GLO[,61:85]

# check to make sure abundances sum to 1 (they do, at least very close)
wt.sum <- rowSums(GLO.wt)
hist(wt.sum)
fia.sum <- rowSums(GLO.fia)
hist(fia.sum)

# create empty matrices to hold Shannon and Simpson diversity indices
Shannon.wt <- matrix(nrow=nrow(GLO), ncol=1)
Shannon.fia <- matrix(nrow=nrow(GLO), ncol=1)
Simpson.wt <- matrix(nrow=nrow(GLO), ncol=1)
Simpson.fia <- matrix(nrow=nrow(GLO), ncol=1)

# calculate diversity indices
for (i in 1:nrow(GLO)) {
	wt.dat <- GLO.wt[i,GLO.wt[i,]>0]
	fia.dat <- GLO.fia[i,GLO.fia[i,]>0]

	Shannon.wt[i] <- -sum(wt.dat*log(wt.dat))
	Shannon.fia[i] <- -sum(fia.dat*log(fia.dat))

	Simpson.wt[i] <- 1 - sum(wt.dat^2)
	Simpson.fia[i] <- 1 - sum(fia.dat^2)
}

# re-express Simpson index so not bounded at 1
Simpson.wt <- Simpson.wt / (1 - Simpson.wt)
Simpson.fia <- Simpson.fia / (1 - Simpson.fia)

# calculate the difference across time
Shannon.dif <- Shannon.fia - Shannon.wt
Simpson.dif <- Simpson.fia - Simpson.wt

hist(Shannon.dif)
hist(Simpson.dif)

GLO.div <- data.frame(GLO, Shannon.dif, Simpson.dif)
write.csv(GLO.div, file = "D:\\Dropbox\\WT_FIA_Vellend\\GLO_Div.csv", row.names = F)

# plot relationship of difference with peak_ag
plot(GLO$peak_ag, Shannon.dif, xlab="peak_ag", ylab="ShannonDivFIA - ShannonDivWT")
reg1 <- lm(Shannon.dif~peak_ag)
lines(c(min(xvar),max(xvar)), c(min(predict(reg1)),max(predict(reg1))), col="red", lty=2, lwd=2)


# plot relationship of dif with whatever variable defined as xvar
xvar <- GLO$peak_ag
reg1 <- lm(Shannon.dif~xvar)
plot(xvar, Shannon.dif)
reg1

# add regression line if effect of xvar positive
lines(c(min(xvar),max(xvar)), c(min(predict(reg1)),max(predict(reg1))), col="red", lty=2, lwd=2)

# add regression line if effect of xvar negative
lines(c(max(xvar),min(xvar)), c(min(predict(reg1)),max(predict(reg1))), col="red", lty=2, lwd=2)

