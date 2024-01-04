# live-best


# keep at least one bounding box
minbox <- 1


# LIVE-BEST TRAIN ####
for(i in 1:length(trainf)){
  temp <- read.csv(file = paste(golddir, traindir, trainf[i],'.txt', sep = ''),
                   header = F, sep = ' ')
  
  nrowtemp <- min(max(minbox,  
                  nrow(temp) - round(rnorm(n = 1, mean = live_best_mean, 
                                           sd = live_sd), 0)), nrow(temp))
  
  temp2 <- sample(x = 1:nrow(temp), size = nrowtemp)

  write.table(x = temp[temp2, ], 
              paste(livebestdir, 
                    traindir, trainf[i], '.txt', sep=''),
              sep = ' ',
              row.names = F,
              col.names = F)
  
  rm(temp, temp2, nrowtemp)  
}

# LIVE-BEST VAL####
for(i in 1:length(valf)){
  temp <- read.csv(file = paste(golddir, valdir, valf[i],'.txt', sep = ''),
                   header = F, sep = ' ')
  
  nrowtemp <- min(max(minbox,  
                      nrow(temp) - round(rnorm(n = 1, mean = live_best_mean, 
                                               sd = live_sd), 0)), nrow(temp))
  
  temp2 <- sample(x = 1:nrow(temp), size = nrowtemp)
  
  write.table(x = temp[temp2, ], 
              paste(livebestdir, 
                    valdir, valf[i], '.txt', sep=''),
              sep = ' ',
              row.names = F,
              col.names = F)
  
  rm(temp, temp2, nrowtemp)  
}