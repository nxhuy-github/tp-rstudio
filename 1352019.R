eucalyptus <- read.table(file="eucalyptus.txt", sep=" ", header=TRUE, dec=".", row.names=1)
eucalyptus
row.names(eucalyptus)
reg<-lm(ht~circ,data=eucalyptus)
resume<-summary(reg)
resume
plot(ht~circ,data=eucalyptus)
circ=seq(min(eucalyptus[,"circ"]),max(eucalyptus[,"circ"]),length=100)
grille<-data.frame(circ)
ICdte<-predict(reg,new=grille,interval="confidence",level=0.95)
matlines(grille$circ,cbind(ICdte),lty=c(1,2,2),col=1)
res<-rstudent(reg)
plot(res,pch=15,ylab="Résidus",ylim=c(-3,3))
abline(h=c(-2,0,2),lty=c(2,1,2))
seuil<-qt(0.975,df=reg$df.res)
beta0min<-coef(resume)[1,1]-seuil*coef(resume)[1,2]
beta0max<-coef(resume)[1,1]+seuil*coef(resume)[1,2]
beta1min<-coef(resume)[2,1]-seuil*coef(resume)[2,2]
beta1max<-coef(resume)[2,1]+seuil*coef(resume)[2,2]
beta0min
beta0max
beta1min
beta1max
newdata = data.frame(circ=c(50, 100, 200, 500))
predict(reg, newdata, interval="predict") 

consommation <- read.table(file="icecream-R.dat", header=TRUE)
consommation
plot(Income~Period,data=consommation)
regmult<-lm(Consumption~Price+Income+Temp,data=consommation)
resume<-summary(regmult)
resume
t<-qt(1-(0.05/(2*4)),df=regmult$df.res)
IC<-rbind(coef(resume)[,1]-t*coef(resume)[,2],coef(resume)[,1]+t*coef(resume)[,2])
IC

par(mfrow = c(2,3))

library(ellipse)
for (i in 1:2){
for (j in (i+1):3){
plot(ellipse(regmult, c(i+1, j+1), level=0.95, type = "1", xlab=paste("beta", i, sep=""), ylab=paste("beta", j, sep="")))
points(coef(resume)[i],coef(resume)[j],pch=3)
IC<-rbind(coef(resume)[,1]-coef(resume)[,2]*qt(0.975,regmult$df.res),coef(resume)[,1]+qt(0.975,regmult$df.res))
lines(c(IC[1,i],IC[1,i],IC[2,i],IC[2,i],IC[1,i]),c(IC[1,j],IC[2,j],IC[2,j],IC[1,j],IC[1,j]),lty=2)
}}

library(FactoMineR)
hotels <- read.csv(file="ESIEADMTD5_EX1.CSV", sep = ",", row.names=1)
hotels
summary(hotels)
res.pca <- PCA(hotels, quanti.sup=7:8, quali.sup=1, graph=FALSE)
val.propres <- res.pca$eig[,1]
val.propres
res.pca$eig
plot(1:5, val.propres, type="b", ylab="Valeurs propres", xlab="Composante", main="Scree plot")
plot(res.pca,habillage=1,title="Individus - 1er plan")
plot(res.pca,habillage=1, axes=2:3, title="Individus - 2e plan")
plot(res.pca,choix="var", title="Cercles des corrélations - 1:2")
plot(res.pca,choix="var", axes=3:4, title="Cercles des corrélations - 3:4")