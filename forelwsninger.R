#forelesning 21.01 sannsynligheter
# diselaggregat eks
#uordnet med tilbakelegging


suksesser <- 3
trekk <- 5
p <- 0.6
dbinom(suksesser,trekk,p)
# dhyper(k,N,s,n) uordnet uten tilbakelegging
####################################33
# Total sannsynlighet
#terninger eksempel
# finne p(5|DK)
pdk <- c(1/4,1/8,1/8,1/8,3/8)
pbdk <- c(0,1/6,1/10,1/12,1/20)
pBk <-pdk*pbdk
(sum(pBk))
#########################3333
# 28.01 time 2
alt<- seq(0,30,1)
alt
#pnorm(5,8,0.73)
#xval <- seq(0,12,0.01)
#yval <- dnorm(xval,8,0.73)
#plot(xval,yval,type = "l", col="orange")
# del(a)
Xval <- seq(0,10)
Xval
Yval <- dbinom(Xval,8,0.73)
plot(Xval,Yval,type ="h", col="orange")
sann.X <- dbinom(5,8,0.73)
sann.X

# del (b)
Yval2 <- dnbinom(Xval,8,0.73)
plot(Xval,Yval2,type ="h", col="orange")
sann.x2 <- dnbinom(7,8,0.73)
sann.x2

# del (c)
pois <- dpois(0:10, 1,7)
plot(Xval,pois,type ="h", col="orange")
pois.x<- dpois(6,1,7)
pois.x

# del (d)
a <- 2
b <- 8
c <- 3
range <- seq(a,b,0.01)
y <- dunif(range,a,b)
y
plot(range, y, type="l", xlim =c(2,8))

x.sann <- dunif(c,a,b)
x.sann

# legge til areal til venstre til X
x <- c
cord.a <- c(a,seq(a,x,0.01),x)
cord.b <- c(0,dunif(seq(a,x,0.01),a,b),0)
polygon(cord.a, cord.b,col="blue")

# del2
# a
library(magrittr)
library(shadow)
x <- seq(0,8)
n <- 8
p <- 0.73
Bfordeling <- pbinom(x,8,0.73)
Bfordeling
plot(x,Bfordeling,type= "s",main = "Cumulative distribution function for Bin(8,0.73)")
utregning <- pbinom(2:5, n,p)
utregning
polygon(utregning,col="red")
# summen av sannsynligheter
(pbinom(2:5, n,p) %>% sum)
polygon(c(2:5),utregning, col="red")

###
k <- 4
p <- 0.55
xV <- seq(0,8)
Nbfordeling <- pnbinom(x,k,0.55)
Nbfordeling
plot(xV,Nbfordeling,type= "s",main = "Negativ binomisk fordeling")
sann.3 <- pnbinom(3:7,k,p)
sann.3

####
lambda <- 1.7
Pfordeling <- ppois(4:6,lambda)
Pfordeling

####
a <- 2
b <- 3
c <- 5
d <- 8
Xvl <- seq(a,d)
CDF <- punif(Xvl, min = b,max = c-1)
CDF
plot(Xvl,CDF,type = "l",main = "Diskret uniform fordeling  for atb???X < c")

#############33
library(extraDistr)
library(rmutil)
XsVal <- seq(0,8,0.1)
be <- dbnbinom(XsVal,5,4,4)
gammaPdf <- dbetapr(XsVal,8,4,3)
FischerPdf <- df(XsVal,7,3)
Fcdf <- pf(2,7,3)
Fcdf

plot(XsVal,be,type= "l" ,col="blue")
gammaCdf <- pbetapr(3,8,4,3)
gammaCdf



cdfNegativBeta <- pbnbinom(7,4,5,4)
cdfNegativBeta


variance <- alpha*beta/((alpha+beta)^2*(alpha+beta+1))
variance
cdfBeta <- pbbinom(5,8,8,3)
cdfBeta
plot(XsVal,cdfBeta,type="l", col= "red")
cdf2 <- pbeta(5,3,8)
cdf2
lines(cdf2,col="blue",type = "h")

###
xVals <- seq(0,1,0.01)
yValBeta <- pbeta(xVals,14,19)
yValNorm<- pnorm(xVals,0.4242,0.0847)
plot(xVals,yValBeta,type = "l", col="blue")
lines(xVals,yValNorm,type = "l",col="orange")

Bet <- pbeta(0.4,14,19)
NormB <- pnorm(0.4,0.4242,0.0847)
Bet
NormB
