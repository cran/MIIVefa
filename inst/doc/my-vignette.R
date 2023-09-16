## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MIIVefa)
library(mnormt)

## ----demo, eval=FALSE---------------------------------------------------------
#  miivefa(
#    data = yourdata,
#    sigLevel = 0.05,
#    scalingCrit = 'sargan+factorloading_R2',
#    correlatedErrors = NULL
#  )

## ----sim1a--------------------------------------------------------------------
seed <- 1235
#generate latent factor values
eta <- rmnorm(n=500, 
              mean = c(0,0,0,0), 
              varcov = matrix(c(1,.5, .5, .5,
                                .5,1, .5, .5,
                                .5, .5,1, .5,
                                .5, .5, .5,1), nrow = 4))
#generate errors
seed <- 1235
e <- rmnorm(n=500,
            varcov =  diag(.25, nrow = 20))
lambda <- cbind(c(1, .8, .75, .7, .65, rep(0,9), .4, rep(0,5)),
                c(rep(0,5), 1, .8, .75, .7, .65, rep(0,9), .3),
                c(rep(0,10), 1, .8, .75, .7, .65, rep(0,5)),
                c(rep(0,15),  1, .8, .75, .7, .65))

rep <- 500
#obtain observed variable values
sim1 <- eta %*% t(lambda) + e
#create column names
colnames(sim1) <- paste0("x", 1:ncol(sim1))
#make it a data frame
sim1 <- as.data.frame(sim1)

## ----sim1b--------------------------------------------------------------------
miivefa(sim1, .01)

## ----sim2a--------------------------------------------------------------------
seed <- 1234 #for replication purpose
#generate latent factor values
eta <- rmnorm(n=500, 
              mean = c(0,0), 
              varcov = matrix(c(1,.5,
                                .5,1), nrow = 2))
#generate errors
seed <- 1234 
CE <- 0.15
e <- rmnorm(n=500,
            mean = rep(0,8),
            varcov =  matrix(rbind(c(.25, 0, 0, 0, CE, 0, 0, 0),
                                   c(0, .25, 0, 0, 0, CE, 0, 0),
                                   c(0, 0, .25, 0, 0, 0, CE, 0),
                                   c(0, 0, 0, .25, 0, 0, 0, CE),
                                   c(CE, 0, 0, 0, .25, 0, 0, 0),
                                   c(0, CE, 0, 0, 0, .25, 0, 0),
                                   c(0, 0, CE, 0, 0, 0, .25, 0),
                                   c(0, 0, 0, CE, 0, 0, 0, .25)), nrow=8, ncol=8))
#factor loading matrix
lambda <- matrix(c(1,0,
                   .8,0,
                   .7,0,
                   .6,0,
                   0,1,
                   0,.8,
                   0,.7,
                   .4,.6), nrow = 8, byrow = T)
#obtain observed variable values
sim2 <- eta %*% t(lambda) + e
#create column names
colnames(sim2) <- paste0("x", 1:ncol(sim2))
#make it a data frame
sim2 <- as.data.frame(sim2)

## ----sim2b, warning=F---------------------------------------------------------
miivefa(sim2, .01) 

## ----sim2c--------------------------------------------------------------------
miivefa(sim2, .01,
    correlatedErrors = 'x1~~x5
        x2~~x6
        x3~~x7
        x4~~x8') 

## ----sim3a--------------------------------------------------------------------
seed <- 1237 #for replication purpose
#generate latent factor values
eta <- rmnorm(n=500, 
              mean = c(0,0), 
              varcov = matrix(c(1,.5,
                                .5,1), nrow = 2))
#generate residuals
seed <- 1237
e <- rmnorm(n=500,
            varcov =  diag(.25, nrow = 10))
#factor loading matrix
lambda <- matrix(c(1,0,
                   .8,0,
                   .7,0,
                   .6,0,
                   0,1,
                   0,.8,
                   0,.7,
                   .4,.6,
                   0,0,
                   0,0), nrow = 10, byrow = T)
#obtain observed variable values
sim3 <- eta %*% t(lambda) + e
#create column names
colnames(sim3) <- paste0("x", 1:ncol(sim3))
#make it a data frame
sim3 <- as.data.frame(sim3)

## ----sim3b, warning=F---------------------------------------------------------
miivefa(sim3, .01)

## ----emp1a--------------------------------------------------------------------
library(lavaan)
holzingerdata <- lavaan::HolzingerSwineford1939[,7:15]
head(holzingerdata)

## ----emp1b--------------------------------------------------------------------
miivefa(holzingerdata, sigLevel = .01, 'sargan+factorloading_R2')

## ----emp2a--------------------------------------------------------------------
library(MPsychoR)
data("Bergh")
miivefa(Bergh[,1:10], .01, 'sargan+factorloading_R2') 

## ----emp3a, warning=F---------------------------------------------------------
data("SDOwave")
sdowavedata <- SDOwave[,
c(paste0(c('I1.','I2.','I3.','I4.'), 1996), 
paste0(c('I1.','I2.','I3.','I4.'), 2000))]
miivefa(sdowavedata, .01)

## ----emp3b, warning=F---------------------------------------------------------
miivefa(sdowavedata, .01, 
        correlatedErrors =
        'I1.1996~~I1.2000
        I2.1996~~I2.2000
        I3.1996~~I3.2000
        I4.1996~~I4.2000')

