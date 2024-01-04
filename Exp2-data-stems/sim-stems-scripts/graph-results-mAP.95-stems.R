# mAP .95
datmap95 <- data.frame(
  g1 = gold1[,'metrics.mAP_0.5.0.95'],
  g2 = gold2[,'metrics.mAP_0.5.0.95'],
  g3 = gold3[,'metrics.mAP_0.5.0.95'],
  ch1 = compworst1[,'metrics.mAP_0.5.0.95'],
  ch2 = compworst2[,'metrics.mAP_0.5.0.95'],
  ch3 = compworst3[,'metrics.mAP_0.5.0.95'],
  cl1 = compbest1[,'metrics.mAP_0.5.0.95'],
  cl2 = compbest2[,'metrics.mAP_0.5.0.95'],
  cl3 = compbest3[,'metrics.mAP_0.5.0.95'],
  lh1 = liveworst1[,'metrics.mAP_0.5.0.95'],
  lh2 = liveworst2[,'metrics.mAP_0.5.0.95'],
  lh3 = liveworst3[,'metrics.mAP_0.5.0.95'],
  ll1 = livebest1[,'metrics.mAP_0.5.0.95'],
  ll2 = livebest2[,'metrics.mAP_0.5.0.95'],
  ll3 = livebest3[,'metrics.mAP_0.5.0.95']
)

## Graph mean avg precision 50 ####
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
      text = 'Precision (mAP 95%)')

for(i in 1:ncol(datmap95)){
  points(x = epochs, y = datmap95[,i], pch = 16, cex = .8, 
         col = adjustcolor(colslong[i], alpha.f = .5))
  lines(lowess(x = epochs, y = datmap95[,i], f=.66), 
        col = colslong[i], lty = 3, lwd = 2)
}

for(i in 1:5){
  lines(lowess(x = epochs, 
               y = apply(X=datmap95[ ,(i*3-2):(i*3)], MAR = 1, FUN = mean), 
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
rm(datmap95)