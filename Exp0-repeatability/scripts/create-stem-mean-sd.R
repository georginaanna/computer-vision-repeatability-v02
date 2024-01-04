# create mean and sd of dis by subject and method

stem1 <- aggregate(stems$num_stems, list(stems$subject, stems$method), mean)
stem2 <- aggregate(stems$num_stems, list(stems$subject, stems$method), sd)

stem0 <- data.frame(subject = stem1$Group.1,
                    method = stem1$Group.2,
                    mean = stem1$x,
                    sd = stem2$x)
write.csv(x = stem0, 
          file = 'stem-stats.csv',
          row.names=F)
