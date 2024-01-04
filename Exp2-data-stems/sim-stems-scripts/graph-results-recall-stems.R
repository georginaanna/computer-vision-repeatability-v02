# Recall
datrecall <- data.frame(
  g1 = gold1[,'metrics.recall'],
  g2 = gold2[,'metrics.recall'],
  g3 = gold3[,'metrics.recall'],
  ch1 = compworst1[,'metrics.recall'],
  ch2 = compworst2[,'metrics.recall'],
  ch3 = compworst3[,'metrics.recall'],
  cl1 = compbest1[,'metrics.recall'],
  cl2 = compbest2[,'metrics.recall'],
  cl3 = compbest3[,'metrics.recall'],
  lh1 = liveworst1[,'metrics.recall'],
  lh2 = liveworst2[,'metrics.recall'],
  lh3 = liveworst3[,'metrics.recall'],
  ll1 = livebest1[,'metrics.recall'],
  ll2 = livebest2[,'metrics.recall'],
  ll3 = livebest3[,'metrics.recall']
)


## Graph Recall ####
# Make plot canvas

png(paste('graphs/', figname, ".png", sep=""), 
    width=1200, height=1200, res=150)


par(mar=c(5,4,2,2)+.1)
plot.new()
plot(1, type="n", xlab="", ylab="", 
     xaxt = 'n', yaxt = 'n',
     xlim=c(min(epochs),max(epochs)), ylim=c(0,1),
     cex.lab = plotcex)

axis(side = 1, col = 'black') 
mtext(side = 1, line = 2.5, cex = plotcex,
      text = 'Training time (epochs)')
axis(side = 2, col = 'black') 
mtext(side = 2, line = 2.5, cex = plotcex,
      text = 'Recall')

for(i in 1:ncol(datrecall)){
  points(x = epochs, y = datrecall[,i], pch = 16, cex = .8, 
         col = adjustcolor(colslong[i], alpha.f = .5))
  lines(lowess(x = epochs, y = datrecall[,i], f=.66), 
        col = colslong[i], lty = 3, lwd = 2)
}

for(i in 1:5){
  lines(lowess(x = epochs, 
               y = apply(X=datrecall[ ,(i*3-2):(i*3)], MAR = 1, FUN = mean), 
               f=.66),
        col = cols[i], lty = 1, lwd = 2)
}

legend(x = 'topleft', bty = 'n',
       legend = c('Baseline', 'Comp-worst', 'Comp-best', 
                  'Live-worst', 'Live-best'),
       lty = 1,
       col = cols,
       pch = 16,
       cex = plotcex)

dev.off()
rm(datrecall)