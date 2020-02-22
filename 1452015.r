#Exercice 1
#1
x <- read.table("eucalyptus.txt", header = T)

#2
reg <- lm(circ~ht, data=x)
resume <- summary(reg)

#3
plot(circ~ht, data=x)
ht=seq(min(x[,"ht"]), max(x[,"ht"]), length=100)
grille <- data.frame(ht)
ICdte<-predict(reg,new=grille, interval="confidence", level=0.95)
matlines(grille$ht,cbind(ICdte),lty=c(1,2,2),col=1)

#4
res <- rstudent(reg)
plot(res,pch=15,ylab="Résidus",ylim=c(-3,3))
abline(h=c(-2,0,2),lty=c(2,1,2))

seuil <- qt(0.975,df=reg$df.res)
beta0min <- coef(resume)[1,1]-seuil*coef(resume)[1,2]
beta0max <- coef(resume)[1,1]+seuil*coef(resume)[1,2]
beta1min <- coef(resume)[2,1]-seuil*coef(resume)[2,2]
beta1max <- coef(resume)[2,1]+seuil*coef(resume)[2,2]

#5
for(i in c(50,100,200,500)){
  nouveaudonnee <- data.frame(circ = i)
  predict(reg, nouveaudonnee, interval="predict")
}

#Exercice 2
#1
a <- read.table("icecream-R.dat", header = T)
regmlt <- lm(Consumption ~ Period + Price + Income + Temp, data=a)
sal <- lm(Income ~  Period, data=a)
resume <- summary(regmlt)
 
#2
regmlt <- lm(Consumption ~ Price + Income + Temp, data=a)
resume <- summary(regmlt)

#3
t <- qt(1-(0.05/(2*4)), df=regmlt$df.residual)
IC <- rbind(coef(resume)[,1]-t*coef(resume)[,2],coef(resume)[,1]+t*coef(resume)[,2])

#4
install.packages('ellipse')
library(ellipse)
for(i in 1:2){
  for(j in (i+1):3){
    plot(ellipse(regmlt,c(i+1,j+1),level=0.95,type="1",xlab=paste("beta",i,sep=""),ylab=paste("beta",j,sep="")))
    points(coef(resume)[i],coef(resume)[j],pch=3)
    IC <- rbind(coef(resume)[,1]-coef(resume)[,2]*qt(0.975,regmlt$df.residual),coef(resume)[,1]+qt(0.975,regmlt$df.residual))
    lines(c(IC[1,i],IC[1,i],IC[2,i],IC[2,i],IC[1,i]),c(IC[1,j],IC[2,j],IC[2,j],IC[1,j],IC[1,j]),lty=2)
  }
}


#Exercice 3
#1
install.packages('FactoMineR')
hotels <- read.csv(file="ESIEADMTD5_EX1.CSV", sep=",", row.names = 1)
summary(hotels)
resultat.pca <- PCA(hotels, quantile.sup=7:8, quali.sup=1)
val.propres <- resultat.pca$eig[,1]
plot(1:5, val.propres, type="b", ylab="Valeurs propres", xlab="Composante", main="Scree plot")

#2
plot(resultat.pca, choix="ind",title="1er plan")
plot(resultat.pca, habillage=1, axes=2:3, title="2e plan")

#3
plot(resultat.pca, choix="var", title="1:2")
plot(resultat.pca, choix="var", axes=3:4, title="3:4")
