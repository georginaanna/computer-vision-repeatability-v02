# Training loss
# Train obj + box loss
dattrainloss <- data.frame(
  g1 = gold1[,'train.box_loss'] + gold1[,'train.obj_loss'],
  g2 = gold2[,'train.box_loss'] + gold1[,'train.obj_loss'],
  g3 = gold3[,'train.box_loss'] + gold1[,'train.obj_loss'],
  ch1 = compworst1[,'train.box_loss'] + gold1[,'train.obj_loss'],
  ch2 = compworst2[,'train.box_loss'] + gold1[,'train.obj_loss'],
  ch3 = compworst3[,'train.box_loss'] + gold1[,'train.obj_loss'],
  cl1 = compbest1[,'train.box_loss'] + gold1[,'train.obj_loss'],
  cl2 = compbest2[,'train.box_loss'] + gold1[,'train.obj_loss'],
  cl3 = compbest3[,'train.box_loss'] + gold1[,'train.obj_loss'],
  lh1 = liveworst1[,'train.box_loss'] + gold1[,'train.obj_loss'],
  lh2 = liveworst2[,'train.box_loss'] + gold1[,'train.obj_loss'],
  lh3 = liveworst3[,'train.box_loss'] + gold1[,'train.obj_loss'],
  ll1 = livebest1[,'train.box_loss'] + gold1[,'train.obj_loss'],
  ll2 = livebest2[,'train.box_loss'] + gold1[,'train.obj_loss'],
  ll3 = livebest3[,'train.box_loss'] + gold1[,'train.obj_loss']
)


## Graph Training loss ####
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
      text = 'Training loss')

for(i in 1:ncol(dattrainloss)){
  points(x = epochs, y = dattrainloss[,i], pch = 16, cex = .8, 
         col = adjustcolor(colslong[i], alpha.f = .5))
  lines(lowess(x = epochs, y = dattrainloss[,i], f=.66), 
        col = colslong[i], lty = 3, lwd = 2)
}

for(i in 1:5){
  lines(lowess(x = epochs, 
               y = apply(X=dattrainloss[ ,(i*3-2):(i*3)], MAR = 1, FUN = mean), 
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
rm(dattrainloss)