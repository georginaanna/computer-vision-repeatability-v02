# comp-hi
# 3  computer   effie 0.011145966 0.005688261
dist.m <- 0.011145966
dist.sd <- 0.005688261

# COMP-HI TRAIN ####
for(i in 1:length(trainf)){
  temp <- read.csv(file = paste(golddir, traindir, trainf[i],'.txt', sep = ''),
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
  temp2 <- cbind(rep(0, nrow(temp)), temp2)
  
  write.table(x = temp2[, 1:5], 
              paste(comphidir, traindir, trainf[i], '.txt', sep=''),
              sep = ' ',
              row.names = F,
              col.names = F)
  
  rm(temp, temp2)  
}

# COMP-HI VAL####
for(i in 1:length(valf)){
  temp <- read.csv(file = paste(golddir, valdir, valf[i], '.txt', sep = ''),
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
  temp2 <- cbind(rep(0, nrow(temp)), temp2)
  
  write.table(x = temp2[, 1:5], 
              paste(comphidir, valdir, valf[i], '.txt', sep=''),
              sep = ' ',
              row.names = F,
              col.names = F)
  
  rm(temp, temp2)  
}