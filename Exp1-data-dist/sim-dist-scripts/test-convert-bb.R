dat <- data.frame(x=c(.25, .95, .5),
                  y=c(.05,.5, .5),
                  h=c(.2,.2, .2),
                  w=c(.2,.2, .2))

plot(x=NULL, xlim=c(0,1), ylim = c(1,0))
points(x=dat$x,y=dat$y)

dat$xtop <- dat$x + dat$w/2
dat$xbot <- dat$x - dat$w/2
dat$ytop <- dat$y + dat$h/2
dat$ybot <- dat$y - dat$h/2

for(i in 1:nrow(dat)){
  
  ifelse(test = dat$xtop[i] > 1, 
         dat$w[i] <- dat$w[i]-(dat$xtop[i]-1)*2, 
         dat$w[i] <- dat$w[i])
  
  ifelse(test = dat$xbot[i] < 0,  
         dat$w[i] <- dat$w[i]-2*dat$x[i], 
         dat$w[i] <- dat$w[i])
  
  ifelse(test = dat$ytop[i] > 1, 
         dat$h[i] <- dat$h[i]-(dat$ytop[i]-1)*2,  
         dat$h[i] <- dat$h[i])
  
  ifelse(test = dat$ybot[i] < 0, 
         dat$h[i] <- dat$h[i]-2*dat$y[i],
         dat$h[i] <- dat$h[i])
}

dat$xtop <- dat$x + dat$w/2
dat$xbot <- dat$x - dat$w/2
dat$ytop <- dat$y + dat$h/2
dat$ybot <- dat$y - dat$h/2
