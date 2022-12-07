# step 1 Parameters
d <- 0.7
b <- 3
m <- 4
Nt <- 42

# step 2 Mice function
mice <- function(Nt,d,b,m){
  Nt1 <- (1+b)*(1-d)*Nt + m
  return(Nt1)
}

#step 3 test function
Nt1 <- mice(Nt,d,b,m)

#step 4 new variable 
N <- rep(NA,100)

#step 5 create for loop
N[1] <- Nt
for(i in 2:100){
  N[i] <- mice(Nt,d,b,m)
  Nt <- N[i]
}

# step 6 Plot N over time 
plot(N,xlab="time",ylab="N",pch=19,col="black")


