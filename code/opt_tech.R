#clean the environment 
rm(list = ls())
gc()
cat("\014")

#set working directory
setwd("C:/Users/irana/OneDrive - IESEG/Documents/Courses/Semester_2/opt_tech/Individual_Project")


#load library for data
library(emdbook)

#load data
rfp <- ReedfrogPred

#look at the data definition
?ReedfrogPred

#explore data
head(rfp)


##### Subset the data to a single class and density
rfp_sub <- subset(rfp, (rfp$pred=='pred')&(rfp$size=="small")&(rfp$density==10))
#rfp_sub

#create the binomially distributed data and set the initial p value
#We took 50% so we start at the middle
killed <- rfp_sub$density-rfp_sub$surv
N=rfp_sub$density
p=0.5

# expression of data likelihood(log scale)
sum(dbinom(killed, size=N, prob=p, log=TRUE))    

# specify vector of "successes" (being eaten.)
num_killed <- rfp_sub$density-rfp_sub$surv     
num_killed

#Given our observed k (number killed), and N = 10 for each trial, 
#lets look at the likelihood that p = 0.5 for each of our trials

# evaluate data likelihood with p=0.5
dbinom(num_killed,size=10,prob=0.5)  

# joint data likelihood
prod(dbinom(num_killed,size=10,prob=0.5))    


# prepare for visualizing the likelihood across parameter space
p <- seq(0.01, 1, length=100)     

Like <- numeric(length=100)


# plot out the likelihood
for(i in 1:100){
  Like[i] <- prod(dbinom(num_killed,size=10,prob=p[i]))
}
plot(Like~p,lty="solid",type="l", xlab="Predation Probability", ylab="Likelihood")
dev.off()


# plot out the log-likelihood
p <- seq(0.01, 1, by=0.01)

LogLike <- numeric(length=100)
for(i in 1:100){
  LogLike[i] <- sum(dbinom(num_killed, size=10, 
                          prob=p[i],log=TRUE))
}

plot(LogLike~p,lty="solid",type="l", xlab="Predation Probability", ylab="Log Likelihood")
dev.off()

# MLE for probability of preditory
p[which(LogLike==max(LogLike))]     


#replot with log and max likelyhood as abline
plot(LogLike~p,lty="solid",type="l", xlab="Predation Probability", ylab="Log Likelihood")
abline(v=0.25,lwd=3)
dev.off()

########################likelihood function uning optim########################################

# p: probability of predatary per trial (param to estimate)
# k: number killed per trial   (data)
# N: number of tadpoles per trial (data)

binomNLL1 <- function(p, k, N) {
  -sum(dbinom(k, size=N, prob=p, log=TRUE))
}

# using the optim() function to find the MLE,
# and to estimate the parameter value that maximizes the likelihood function 
opt_results <- optim(fn=binomNLL1, par = c(p=0.5), 
              N = 10, k = num_killed,
              method = "BFGS")   


#get the output
par <- opt_results$par
value <- opt_results$value
value_like <- exp(-opt_results$value)
counts <- opt_results$counts
convergence <- opt_results$convergence
message <- opt_results$message

par
value
value_like
counts
convergence
message


#Plot the outcomes:
hist(num_killed,xlim=c(0,10),freq=F)
curve(dbinom(x,prob=opt_results$par,size=10),add=T,from=0,to=10,n=11)
dev.off()

#clean the environment before closing the R session
#rm(list = ls())
#gc()
#cat("\014")
#dev.off()

