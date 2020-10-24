######## STATISTIQUE EN GRANDE DIMENSION ##############       
 
############# PROJET 1#################################"

####################EXERCICE 1############################
# a-)
 #Loi normale
Y=rnorm(50,-1.5,2)

 #Loi uniforme 
Z=runif(50)

#Concatenation de Y et Z 
X=c(Y,Z)

#Histogramme de X
hist(X,prob=TRUE, xlab = "X",ylab = "Densité de X", col = "red",main="Histogramme de X")

#Construction d'estimateur par noyau
#Choix d'une grille de points x et Definition de la densite f de X
m=1000
x=seq(-10,10,length.out = m)
f1=dnorm(x,-1.5,sqrt(2))
f2=dunif(x,0,1)
f=1/2*f1+1/2*f2

#Representation graphique de la densité f en jaune.
plot(x,f,type = 'l',lwd=2, col='yellow',main = "courbe de la densité f")


#Definition de la fonction renvoyant le noyau gaussien
noyau_gauss=function(u){
  g=1/sqrt(2*pi)*exp(-u^2/2)
  return(g)
}

#Definition de l'estimateur utilisant le noyau gaussien
G=function(X,x,h){
  n=length(X)
  G=0
  for(i in 1:n){
    ui=(X[i]-x)/h
    G=G+noyau_gauss(ui)
  }
  return(G/(n*h))
}
######################################


##Densité estimé par le noyau gaussien
j=rep(0,m)
c=0
h=0.2
for (y in x) {
  c=c+1
  j[c]=G(X,y,h)
}

#representation graphique en bleue du densité estimé par le noyau gaussien

lines(x,j,type="l",lwd=2,col='blue',ylab="j")


#Definition de la fonction renvoyant le noyau rectangulaire
noyau_rect=function(u){
  R=1/2*(u<=1&u>=-1)
  return(R)
}

#Definition de l'estimateur utilisant le noyau rectangulaire
R=function(X,x,h){
  n=length(X)
  R=0
  for(i in 1:n){
    ui=(X[i]-x)/h
    R=R+noyau_rect(ui)
  }
  return(R/(n*h))
}



##Densité estimé par le noyau rectagulaire 
L=rep(0,m)
i=0
h=0.2
for (y in x) {
  i=i+1
  L[i]=R(X,y,h)
}

#representation graphique en noir

lines(x,L,type="l",ylab='L',lwd=2,col='black')



h = seq(0.1,1.6,length.out=50)
J = rep(0,50)
dx=x[2]-x[1]
for (c in 1:50) {
  H = density(X,bw=h[c],kernel="rectangular",n=m,from=-10, to= 10)$y
  n=length(X)
  Hi=rep(0,n)
  for (i in 1:n) {
    Xi = X[(1:n) != i] # donnees privees de la i-eme
    Hi[i] = density(Xi,bw=h[c],kernel="rectangular",n=1,from=X[i], to= X[i])$y
  }
  J[c] = dx*sum(H^2)-2/n*sum(Hi)
}

plot(h,J)

#Q10
# Minimisation de l'erreur
hopt = h[which.min(J)]

hatf_hopt = density(X,bw=hopt,kernel="gaussian",n=1000,from=-10, to= 10)

# Figure
lines(x,f,type="l",lwd=2,lty="dotdash")
#
library(dplyr)
library(MASS)

setwd("C:/Users/33758/Downloads")
getwd()
valeur_ratp = read.csv(file = "ratp.csv", header = TRUE, sep = ";")
attach(valeur_ratp)
names(valeur_ratp)
str(valeur_ratp)
A=PM10
W=na.omit(A)
D=W[1:50]
hist(D, col = 'green',main = 'Histogramme de notre nouveau vecteur D')
m=1000
d=seq(10,20,length.out = m)

noyau_gauss=function(u){
  g=1/sqrt(2*pi)*exp(-u^2/2)
  return(g)
}

#Definition de l'estimateur utilisant le noyau gaussien
G=function(D,d,h){
  n=length(D)
  G=0
  for(i in 1:n){
    ui=(D[i]-d)/h
    G=G+noyau_gauss(ui)
  }
  return(G/(n*h))
}
######################################


##Densité estimé par le noyau gaussien
j=rep(0,m)
c=0
h=0.2
for (y in d) {
  c=c+1
  j[c]=G(D,y,h)
}

#representation graphique en bleue
plot(j,col='red', lwd = 2,main = "densité 
     estimé par le noyau gaussien")
lines(j,lwd=2,col='blue',ylab="j", main="representation graphique en bleue du densité estimé par le noyau gaussien")

noyau_rect=function(u){
  R=1/2*(u<=1&u>=-1)
  return(R)
}

#Definition de l'estimateur utilisant le noyau rectangulaire
R=function(D,d,h){
  n=length(D)
  R=0
  for(i in 1:n){
    ui=(D[i]-d)/h
    R=R+noyau_rect(ui)
  }
  return(R/(n*h))
}

##Densité estimé par le noyau rectagulaire 
L=rep(0,m)
i=0
h=0.2
for (y in d) {
  i=i+1
  L[i]=R(D,y,h)
}

plot(L,col='green', lwd = 2,main = "densité 
     estimé par le noyau rectangulaire")
lines(L,lwd=2,col='yellow',ylab="L", xlab='X', main="representation graphique en bleue du densité estimé par le noyau gaussien")
