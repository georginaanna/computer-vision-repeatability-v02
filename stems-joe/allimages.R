## HEADER ####
## what: script to manipulate bounding box data for potato stem pics
##        originally prepared by Joe Mhango and Matt Butler. The
##        images are 640 (W) x 480 (H) .png n = 163.  A file of 
##        bounding box data is called train.csv.
##        
##        The purpose of this script is for data formatting for Yolov5.
## who: Ed Harris
## when: Last edited 2022-01-22


## CONTENTS ####
## 00 Setup
## 01 Output data for Yolov5


## 00 Setup ####
setwd(r'(D:\Dropbox\git\data-stems-joe\stems-joe)')

bb <- read.csv('allimages-bb/train.csv')

## 01 Output data for Yolov5 ####

### Point 1 Found bb replication in raw data ####
pics <- list.files('images/')
which(bb$image_id == pics[1]) # compare bb 1:5 and 307:311...

# BEWARE the bbs are input twice!
bb[which(bb$image_id == pics[1]),] # alrighty then

which(bb$image_id == pics[163])
bb[which(bb$image_id == pics[163]),] # delete last half of bb file

bb <- bb[1:306,]

### Point 2 Convert columns to yolov5 format ####
# https://github.com/ultralytics/yolov5/wiki/Train-Custom-Data

# export your labels to YOLO format, with one *.txt file per 
# image (if no objects in image, no *.txt file is required). 
# The *.txt file specifications are:

# One row per object
# Each row is class x_center y_center width height format.
# Box coordinates must be in normalized xywh format (from 0 - 1). 
# If your boxes are in pixels, divide x_center and width by 
# image width, and y_center and height by image height.
# Class numbers are zero-indexed (start from 0).

# converting
# class is one category (all stems)
bb_yolo <- data.frame(class = rep(0, 306),
                      x_center = bb$x/640,
                      y_center = bb$y/480,
                      width = bb$w/640,
                      height = bb$h/480)

# drop .png on char string for writing new file
pics2 <- gsub(pattern = '.png', 
              replace = '',
              pics)

for(i in 1:163){
  temp <- which(bb$image_id == pics[i])
  write.table(x = bb_yolo[temp,], sep = ' ',
            file = paste('labels/',pics2[i], '.txt', sep = ''),
            row.names = F, col.names = F)
}

# for(i in 1:163){
#   temp <- which(bb$image_id == pics[i])
#   write.table(x = bb_yolo[temp,], sep = ',',
#               file = paste('allimages-bb-yolo/',pics2[i], '.csv', sep = ''),
#               row.names = F, col.names = F)
# }

