
h1 <- function(x){
  x^2
}
h2 <- function(x){
  abs(x)
}

h3 <- function(x){
  cos(x)+x^2+exp(x)
}

h<-h1

u <- 0
s <- 3
m1 <- 10^6

mean(rep(mean(h(rnorm(m1,u,s))),1000))

x <- array(NA,6)

for(i in 1:6){
  m2 <- 10^i
  int_range <- (1:m2) / (m2 + 1)
  q <- qnorm(int_range)
  x[i] <- mean(h(q))
}
x

gh <- fastGHQuad::gaussHermiteData(16)

nw <- createNIGrid(dim=1, type="GHN", level=6)


# x2 <- gh$x[which(gh$w>10^-10)]
# w2 <- gh$w[which(gh$w>10^-10)]
#
# sum(h(sqrt(2)*gh$x*s+u)*gh$w)/sum(gh$w)

w2<- nw$weights
x2<- nw$nodes[,1]
sum(h(x2*s+u)*w2)/sum(w2)
mean(rep(mean(h(rnorm(m1,u,s))),1000))
#length(w2)




#################################################################
library(Rfast)
library(mvtnorm)

g1 <- function(x, y){
  x^2 + y^2
}

g2 <- function(x, y){
  x*y
}



g <- g2

u <- c(.5,0.1)
s <- diag(2)*3
m <- 10^6

rands <- rmvnorm(m, u, s)

m2 <- 10^4

int_range <- (1:m2) / (m2 + 1)
q <- qnorm(int_range)

r1 <- q[sample(m2)]*sqrt(s[1,1])+u[1]
r2 <- q[sample(m2)]*sqrt(s[2,2])+u[2]

r3 <- q*sqrt(s[1,1])+u[1]
r4 <- q*sqrt(s[2,2])+u[2]
#
#
# gh <- fastGHQuad::gaussHermiteData(16)
#
# x2 <- gh$x[which(gh$w>10^-10)]
# w2 <- gh$w[which(gh$w>10^-10)]
#length(w2)
library(mvQuad)


nw <- createNIGrid(dim=1, type="GHN", level=6, ndConstruction ="sparse")


#nrow(nw$weights)

#
# nodes <- nw$nodes[which(nw$weights > 10^-6),]
# weights <- nw$weights[which(nw$weights > 10^-6)]

# length(weights)
# length(nw$weights)

#rescale(nw, m=u, C=s)


mean(g(rands[,1], rands[,2]))
mean(g(r1, r2))
mean(g(r3, r4))

# sum(g(sqrt(2)*x2*sqrt(s[1,1])+u[1],
#       sqrt(2)*x2*sqrt(s[2,2])+u[2])*w2)/sum(w2)

#rescale(nw, m=u, C=s)

nodes <- nw$nodes
weights <- nw$weights

sum(g(nodes[,1]*sqrt(s[1,1])+u[1],
      nodes[,1]*sqrt(s[2,2])+u[2])*weights)/sum(weights)

# length(weights)
# length(nw$weights)






