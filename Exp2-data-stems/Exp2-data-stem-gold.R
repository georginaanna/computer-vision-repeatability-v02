## HEADER ####
## code to sim the stem count error in different folders

## This master script creates a series of simulated stem count error files
# for the number of bounding boxes.

## NB1 it is assumed the 'gold standard' finds all stems
## NB2 live 1st - 2nd; live 1st-3rd
## NB3 live 1st - comp 1st; live 1st - comp 10th

## Read in mean and create differences to simulate
stems <- read.csv('../Exp0-repeatability/stem-stats.csv')
stemlive <- stems[which(stems$method == 'live'),]
stemlive <- stemlive[order(stemlive$mean, decreasing = T), ]
stemcomp <- stems[which(stems$method == 'computer'),]
stemcomp <- stemcomp[order(stemcomp$mean, decreasing = T), ]

comp_worst_mean <- stemlive$mean[1]-stemcomp$mean[10]
comp_best_mean <- stemlive$mean[1]-stemcomp$mean[1]

live_worst_mean <- stemlive$mean[1]-stemlive$mean[3]
live_best_mean <- stemlive$mean[1]-stemlive$mean[2]

comp_sd <- mean(stemcomp$sd)
live_sd <- mean(stemlive$sd)
  
golddir  <-  '_gold/'
compbestdir <- '_comp-best/'
compworstdir <- '_comp-worst/'
livebestdir <- '_live-best/'
liveworstdir <- '_live-worst/'

traindir <- 'labels/train/'
valdir <- 'labels/val/'

trainf <- gsub('.txt' , '' , 
               x = list.files(paste(golddir,traindir, sep='')))
valf <- gsub('.txt' , '' , 
             x = list.files(paste(golddir, valdir, sep='')))

#make comp best stems
source('sim-stems-scripts/create_stem_comp-best.R')

#make comp worst stems
source('sim-stems-scripts/create_stem_comp-worst.R')

#make live best stems
source('sim-stems-scripts/create_stem_live-worst.R')

#make live worst stems
source('sim-stems-scripts/create_stem_live-best.R')



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


