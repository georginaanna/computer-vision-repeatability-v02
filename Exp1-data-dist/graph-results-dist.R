## metrics:
# https://stackoverflow.com/questions/54977311/what-is-loss-cls-and-loss-bbox-and-why-are-they-always-zero-in-training
#
# loss_bbox: a loss that measures how "tight" the predicted bounding 
# boxes are to the ground truth object (usually a regression loss, L1,
# smoothL1 etc.).
# 
# loss_cls: a loss that measures the correctness of the classification 
# of each predicted bounding box: each box may contain an object class, 
# or a "background". This loss is usually called cross entropy loss.

# https://github.com/wandb/client/issues/2425#:~:text=train%2Fbox_loss%2C%20train%2Fcls_loss%2C%20train%2Fobj_loss%20are,values%20as%20described%20by%20%40ivangrov

# As of the losses: cls_loss is "class loss", "box _loss" is box loss, 
# and "objectness loss" is object loss. I think it just measures the loss 
# relative to those 3 tasks: correctly classifying a class, fitting a 
# bounding box, and determining if there is any object at all in those 
# coordinates (which is kind of a useful thing for object detectors).

# https://jonathan-hui.medium.com/map-mean-average-precision-for-object-detection-45c121a31173
# Precision & recall
# Precision measures how accurate is your predictions. i.e. the 
# percentage of your predictions are correct.
# Recall measures how good you find all the positives. For example, 
# we can find 80% of the possible positive cases in our top K predictions.


gold1 <- read.csv('data-detection-dist/gold1.csv')
gold2 <- read.csv('data-detection-dist/gold2.csv')
gold3 <- read.csv('data-detection-dist/gold3.csv')
comphi1 <- read.csv('data-detection-dist/comp-hi1.csv')
comphi2 <- read.csv('data-detection-dist/comp-hi2.csv')
comphi3 <- read.csv('data-detection-dist/comp-hi3.csv')
complo1 <- read.csv('data-detection-dist/comp-lo1.csv')
complo2 <- read.csv('data-detection-dist/comp-lo2.csv')
complo3 <- read.csv('data-detection-dist/comp-lo3.csv')
livehi1 <- read.csv('data-detection-dist/live-hi1.csv')
livehi2 <- read.csv('data-detection-dist/live-hi2.csv')
livehi3 <- read.csv('data-detection-dist/live-hi3.csv')
livelo1 <- read.csv('data-detection-dist/live-lo1.csv')
livelo2 <- read.csv('data-detection-dist/live-lo2.csv')
livelo3 <- read.csv('data-detection-dist/live-lo3.csv')

epochs <- 1:50

cols <- c('blue', 'chartreuse4', 'chartreuse3', 'brown4', 'brown1')
colslong <- rep(cols, each=3)

plotcex <- 2

## Graph mean avg precision 50 ####
figname <- 'Fig1-mAP.50'
source('sim-dist-scripts/graph-results-mAP.5.R')

## Graph mean avg precision 95 ####
# Make plot canvas
figname <- 'Fig2-mAP.95'
source('sim-dist-scripts/graph-results-mAP.95.R')

## Graph Recall ####
# Make plot canvas
figname <- 'Fig3-recall'
source('sim-dist-scripts/graph-results-recall.R')

## Graph Training loss ####
# Make plot canvas
figname <- 'Fig4-training-loss'
source('sim-dist-scripts/graph-results-train-loss.R')

## Graph Validation loss ####
# Make plot canvas
figname <- 'Fig5-val-loss'
source('sim-dist-scripts/graph-results-val-loss.R')

