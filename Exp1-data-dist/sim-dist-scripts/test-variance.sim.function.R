## HEADER ####
## code to sim the error in different folders

## trash code ####

# gold standard data

# box error function
box.error <- function(x0, y0, dist.m, dist.sd){
  a <- runif(1, 0, 360)
  d <- rnorm(1,dist.m, dist.sd)
  d <- d+abs(min(d))
  x1=x0 + (d * cos(a))
  y1=y0 + (d * sin(a))
  return(cbind(x1, y1))
}


# vars are: class, x, y, w, h
# comp hi mean sd: 0.011145966 0.005688261 
dist.m <- 0.011145966
dist.sd <- 0.005688261
wid <- 0.046875
heit <- 0.08333333

golddir  <-  '_gold/'
comphidir <- '_comp-hi/'
traindir <- 'labels/train/'
valdir <- 'labels/val/'

trainf <- gsub('.txt' , '' , x = list.files(paste(golddir,traindir, sep='')))
valf <- gsub('.txt' , '' , x = list.files(paste(golddir, valdir, sep='')))


for(i in 1:length(trainf)){
  temp <- read.csv(file = paste(golddir.train,trainf[i],'.txt', sep = ''),
                   header = F, sep = ' ')
  
  temp2 <- matrix(nrow=nrow(temp), ncol=8)
  for(j in 1:nrow(temp2)){
    temp2[j, 1:2] <- box.error(x0 = temp[j, 2],
                               y0 = temp[j, 3],
                               dist.m = dist.m,
                               dist.sd = dist.sd)
    temp2[j, 3:4] <- c(wid, heit)
    
    temp2[j,5] <- temp2[j,1] + temp2[j,3]/2
    temp2[j,6] <- temp2[j,1] - temp2[j,3]/2
    temp2[j,7] <- temp2[j,2] + temp2[j,4]/2
    temp2[j,8] <- temp2[j,2] - temp2[j,4]/2

  # vars are:  x, y, w, h, xtop, xbot, ytop, ybot
  #            1, 2, 3, 4, 5,    6,    7,    8

    ifelse(test = temp2[j,5] > 1, 
           temp2[j,3] <- temp2[j,3]-(temp2[j,5]-1)*2, 
           temp2[j,3] <- temp2[j,3])
    
    ifelse(test = temp2[j,6] < 0,  
           temp2[j,3] <- temp2[j,3]-2*temp2[j,1], 
           temp2[j,3] <- temp2[j,3])
    
    ifelse(test = temp2[j,7] > 1, 
           temp2[j,4] <- temp2[j,4]-(temp2[j,7]-1)*2,  
           temp2[j,4] <- temp2[j,4])
    
    ifelse(test = temp2[j,8] < 0, 
           temp2[j,4] <- temp2[j,4]-2*temp2[j,2],
           temp2[j,4] <- temp2[j,4])
  }
  paste(comphidir, traindir, trainf[i], '.txt', sep='')
}




# Joe dist and sd
#  

# x1 = x0 + (d * cos(a))
# y1 = y0 + (d * sin(a))

# simulated point for Joe's mean and sd
x <- rnorm(100,dist.m, dist.sd)
hist(x+abs(min(x)),
     xlim = c(-.1,1))

## tight error ####
dist.m <- 0.005254117
dist.sd <- 0.003381175

x0 <- .25
y0 <- 0.5

plot(x = NULL, xlim = c(0,1),
     ylim = c(0,1))
points(x=x0, y=y0,
       cex = 2, pch=16, col = 'blue')

for(i in 1:100){
  a <- runif(1, 0, 360)
  d <- rnorm(1,dist.m, dist.sd)
  d <- d+abs(min(d))
  points(x=x0 + (d * cos(a)), 
         y=y0 + (d * sin(a)),
         cex = .7, pch=16, col = 'red')
  
}


## loose  error ####
# mean sd
# 3 computer effie 0.011145966 0.005688261

dist.m <- 0.011145966
dist.sd <- 0.005688261

x0 <- .75
y0 <- .5

points(x=x0, y=y0,
       cex = 2, pch=16, col = 'blue')

for(i in 1:100){
  a <- runif(1, 0, 360)
  d <- rnorm(1,dist.m, dist.sd)
  d <- d+abs(min(d))
  points(x=x0 + (d * cos(a)), 
         y=y0 + (d * sin(a)),
         cex = .7, pch=16, col = 'lightgreen')
  
}





box.error <- function(x0, y0, dist.m, dist.sd){
  a <- runif(1, 0, 360)
  d <- rnorm(1,dist.m, dist.sd)
  d <- d+abs(min(d))
  x1=x0 + (d * cos(a))
  y1=y0 + (d * sin(a))
  return(data.frame(x1, y1))
}

# test with data frame
dist.m <- 0.011145966
dist.sd <- 0.005688261

fakedat <- data.frame(gold.x = c(.1,.2,.3,.4), gold.y = c(.5,.4,.7,.3))

box.error(x0 = fakedat$gold.x, y0 = fakedat$gold.y, dist.m = dist.m, dist.sd = dist.sd)




