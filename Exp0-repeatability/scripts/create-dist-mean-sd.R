# create mean and sd of dis by subject and method

dist1 <- aggregate(c_dist$distance, list(c_dist$subject, c_dist$method), mean)
dist2 <- aggregate(c_dist$distance, list(c_dist$subject, c_dist$method), sd)

dist0 <- data.frame(subject = dist1$Group.1,
                    method = dist1$Group.2,
                    mean = dist1$x,
                    sd = dist2$x)
write.csv(x = dist0, 
          file = 'dist-stats.csv',
          row.names=F)
