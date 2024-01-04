## HEADER ####
## code to sim the error in different folders

## This master script creates a series of simulated mean distance error files
# for bounding box placement.

## NB1 the simulated mean and sd are hard coded in the source() files
## NB2 the gold standard is a regular live run thought to be of high
## 'gold standard' quality

# box error function
box.error <- function(x0, y0, dist.m, dist.sd){
  a <- runif(1, 0, 360)
  d <- rnorm(1,dist.m, dist.sd)
  d <- d+abs(min(d))
  x1=x0 + (d * cos(a))
  y1=y0 + (d * sin(a))
  return(cbind(abs(x1), abs(y1)))
}


# vars are: class, x, y, w, h
wid <- 0.046875
heit <- 0.08333333

golddir  <-  '_gold/'
comphidir <- '_comp-hi/'
complodir <- '_comp-lo/'
livehidir <- '_live-hi/'
livelodir <- '_live-lo/'

traindir <- 'labels/train/'
valdir <- 'labels/val/'

trainf <- gsub('.txt' , '' , 
               x = list.files(paste(golddir,traindir, sep='')))
valf <- gsub('.txt' , '' , 
             x = list.files(paste(golddir, valdir, sep='')))

#make comp hi bb labels
source('sim-dist-scripts/create_sim_comp-hi.R')

#make comp lo bb labels
source('sim-dist-scripts/create_sim_comp-lo.R')

#make live hi bb labels
source('sim-dist-scripts/create_sim_live-hi.R')

#make live lo bb labels
source('sim-dist-scripts/create_sim_live-lo.R')



## trash code ####


# Joe dist and sd
#  

# x1 = x0 + (d * cos(a))
# y1 = y0 + (d * sin(a))

# simulated point for Joe's mean and sd
# x <- rnorm(100,dist.m, dist.sd)
# hist(x+abs(min(x)),
#      xlim = c(-.1,1))
# 
# ## tight error
# dist.m <- 0.005254117
# dist.sd <- 0.003381175
# 
# x0 <- .25
# y0 <- 0.5
# 
# plot(x = NULL, xlim = c(0,1),
#      ylim = c(0,1))
# points(x=x0, y=y0,
#        cex = 2, pch=16, col = 'blue')
# 
# for(i in 1:100){
#   a <- runif(1, 0, 360)
#   d <- rnorm(1,dist.m, dist.sd)
#   d <- d+abs(min(d))
#   points(x=x0 + (d * cos(a)), 
#          y=y0 + (d * sin(a)),
#          cex = .7, pch=16, col = 'red')
#   
# }


# ## loose  error
# # mean sd
# # 3 computer effie 0.011145966 0.005688261
# 
# dist.m <- 0.011145966
# dist.sd <- 0.005688261
# 
# x0 <- .75
# y0 <- .5
# 
# points(x=x0, y=y0,
#        cex = 2, pch=16, col = 'blue')
# 
# for(i in 1:100){
#   a <- runif(1, 0, 360)
#   d <- rnorm(1,dist.m, dist.sd)
#   d <- d+abs(min(d))
#   points(x=x0 + (d * cos(a)), 
#          y=y0 + (d * sin(a)),
#          cex = .7, pch=16, col = 'lightgreen')
#   
# }


# box.error <- function(x0, y0, dist.m, dist.sd){
#   a <- runif(1, 0, 360)
#   d <- rnorm(1,dist.m, dist.sd)
#   d <- d+abs(min(d))
#   x1=x0 + (d * cos(a))
#   y1=y0 + (d * sin(a))
#   return(data.frame(x1, y1))
# }

# # test with data frame
# dist.m <- 0.011145966
# dist.sd <- 0.005688261
# 
# fakedat <- data.frame(gold.x = c(.1,.2,.3,.4), gold.y = c(.5,.4,.7,.3))
# 
# box.error(x0 = fakedat$gold.x, y0 = fakedat$gold.y, dist.m = dist.m, dist.sd = dist.sd)
# 
# 


