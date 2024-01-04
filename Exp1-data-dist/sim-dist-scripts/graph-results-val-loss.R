# Validation loss
# val box + obj loss
datvalloss <- data.frame(
  g1 = gold1[,'val.box_loss'] + gold1[,'val.obj_loss'],
  g2 = gold2[,'val.box_loss'] + gold1[,'val.obj_loss'],
  g3 = gold3[,'val.box_loss'] + gold1[,'val.obj_loss'],
  ch1 = comphi1[,'val.box_loss'] + gold1[,'val.obj_loss'],
  ch2 = comphi2[,'val.box_loss'] + gold1[,'val.obj_loss'],
  ch3 = comphi3[,'val.box_loss'] + gold1[,'val.obj_loss'],
  cl1 = complo1[,'val.box_loss'] + gold1[,'val.obj_loss'],
  cl2 = complo2[,'val.box_loss'] + gold1[,'val.obj_loss'],
  cl3 = complo3[,'val.box_loss'] + gold1[,'val.obj_loss'],
  lh1 = livehi1[,'val.box_loss'] + gold1[,'val.obj_loss'],
  lh2 = livehi2[,'val.box_loss'] + gold1[,'val.obj_loss'],
  lh3 = livehi3[,'val.box_loss'] + gold1[,'val.obj_loss'],
  ll1 = livelo1[,'val.box_loss'] + gold1[,'val.obj_loss'],
  ll2 = livelo2[,'val.box_loss'] + gold1[,'val.obj_loss'],
  ll3 = livelo3[,'val.box_loss'] + gold1[,'val.obj_loss']
)

## Graph Validation loss ####
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
      text = 'Validation loss')

for(i in 1:ncol(datvalloss)){
  points(x = epochs, y = datvalloss[,i], pch = 15, cex = .8, 
         col = adjustcolor(colslong[i], alpha.f = .5))
  lines(lowess(x = epochs, y = datvalloss[,i], f=.66), 
        col = colslong[i], lty = 3, lwd = 2)
}

for(i in 1:5){
  lines(lowess(x = epochs, 
               y = apply(X=datvalloss[ ,(i*3-2):(i*3)], MAR = 1, FUN = mean), 
               f=.66),
        col = cols[i], lty = 1, lwd = 2)
}

legend(x = 'topleft', bty = 'n',
       legend = c('Baseline', 'Comp-high', 'Comp-low', 
                  'Live-high', 'Live-low'),
       lty = 1,
       col = cols,
       cex = plotcex)

dev.off()
rm(datvalloss)