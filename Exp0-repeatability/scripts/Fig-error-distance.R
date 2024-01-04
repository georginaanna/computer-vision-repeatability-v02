x1 <- aggregate(x = c_dist$distance,
          by = list(c_dist$subject, c_dist$method),
          FUN = mean)
x2 <- aggregate(x = c_dist$distance,
          by = list(c_dist$subject, c_dist$method),
          FUN = sd)

dist.summ <- data.frame(subject = c(LETTERS[c(1:10, 1,3,10)]),
                        method = x1$Group.2,
                        mean = x1$x,
                        sd = x2$x)


dist.summ$se <- dist.summ$sd/sqrt(30)
alpha <- 0.05
degrees.freedom <- 30 - 1
t.score <- qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
# print(t.score)

margin.error <- t.score * dist.summ$se
dist.summ$lower.bound <- dist.summ$mean - margin.error
dist.summ$upper.bound <- dist.summ$mean + margin.error
dist.summ

sortcomp <- sort(dist.summ$mean[1:10], index.return = T)$ix
sortlive <- sort(dist.summ$mean[11:13], index.return = T)$ix + 10

dist.summ <- dist.summ[c(sortcomp, sortlive),]

# Plot window ####
plot.new()
plot(x = NULL, 
     #xaxt = 'n', 
     xlab = '',
     yaxt = 'n', 
     ylab = '',
     ylim = c(1,13),
     xlim = c(0.2, 1.4))
axis(side = 2, at = 1:13, labels = dist.summ$subject, las = 2)
mtext(text = 'Subject', side = 2, line = 2.5, cex = 1.3)
mtext(text = '% Mean box placement error (95% CI)', side = 1, line = 2.5, cex = 1.3)

points(x = 100*dist.summ$mean, y = 1:13, cex = 1.5,
       pch = c(rep(15,10), rep(15,3)),
       col = c(rep('chartreuse4',10), rep('brown4',3)))
arrows(x0 = 100*dist.summ$lower.bound,
       x1 = 100*dist.summ$upper.bound,
       y0 = 1:13,
       y1 = 1:13,
       length = 0,
       col = c(rep('chartreuse4',10), rep('brown4',3)))
arrows(x0 = 100*mean(dist.summ$mean[1:10]),
       x1 = 100*mean(dist.summ$mean[1:10]),
       y0 = 1,
       y1 = 13,
       length = 0,
       col = 'chartreuse4',
       lty=2, lwd = 2)
arrows(x0 = 100*mean(dist.summ$mean[11:13]),
       x1 = 100*mean(dist.summ$mean[11:13]),
       y0 = 1,
       y1 = 13,
       length = 0,
       col = 'brown4',
       lty=2, lwd = 2)
legend(x = .2, y = 13,
       title = 'Method',
       legend = c('live', 'computer'),
       bty = 'n',
       pch = c(15,15),
       col = c('brown4', 'chartreuse4'), cex = 1.5)

# Print to file ####
png(paste("figures/Fig-error-distance", ".png", sep=""), 
    width=1200, height=1200, res=150)

plot.new()
plot(x = NULL, 
     #xaxt = 'n', 
     xlab = '',
     yaxt = 'n', 
     ylab = '',
     ylim = c(1,13),
     xlim = c(0.2, 1.4))
axis(side = 2, at = 1:13, labels = dist.summ$subject, las = 2)
mtext(text = 'Subject', side = 2, line = 2.5, cex = 1.3)
mtext(text = '% Mean box placement error (95% CI)', side = 1, line = 2.5, cex = 1.3)

points(x = 100*dist.summ$mean, y = 1:13, cex = 1.5,
       pch = c(rep(15,10), rep(15,3)),
       col = c(rep('chartreuse4',10), rep('brown4',3)))
arrows(x0 = 100*dist.summ$lower.bound,
       x1 = 100*dist.summ$upper.bound,
       y0 = 1:13,
       y1 = 1:13,
       length = 0,
       col = c(rep('chartreuse4',10), rep('brown4',3)))
arrows(x0 = 100*mean(dist.summ$mean[1:10]),
       x1 = 100*mean(dist.summ$mean[1:10]),
       y0 = 1,
       y1 = 13,
       length = 0,
       col = 'chartreuse4',
       lty=2, lwd = 2)
arrows(x0 = 100*mean(dist.summ$mean[11:13]),
       x1 = 100*mean(dist.summ$mean[11:13]),
       y0 = 1,
       y1 = 13,
       length = 0,
       col = 'brown4',
       lty=2, lwd = 2)
legend(x = .2, y = 13,
       title = 'Method',
       legend = c('live', 'computer'),
       bty = 'n',
       pch = c(15,15),
       col = c('brown4', 'chartreuse4'), cex = 1.5)
dev.off()

# clean up ####
rm(x1, x2, dist.summ, alpha, degrees.freedom, t.score,
   margin.error, sortcomp, sortlive)
