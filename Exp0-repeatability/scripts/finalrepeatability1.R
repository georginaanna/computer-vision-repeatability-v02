## HEADER ####
## What: Final Repeatability 
## Who: Ed & George 
## When: 2021-07-17

## CONTENTS ####
## 00 Setup ####
## 01 Computer Labeling Data Object ####
## 02 Live Labeling Data Object ####
## 03 Data wrangling for computer labelling ####
## 04 Data wrangling for live labelling ####
## 05 Tidying up the dataframes ####

## 06 Computer n_bbox and n_bbox repeatability ####
## 06.1 Mixed Model and ICC for n_bbox repeatability between computer labellers ####
## 06.2 Graphic 1: Grey version boxplot of the between labeller variation per plot (computer) ####
## 06.3 Graphic 2: Interaction plot for the mean box numbers between computer labellers ####
## 06.4 Graphic 3: Boxplot for the mean number of boxes drawn per computer labellers ####
## 06.5 Graphic 4: Boxplot for the Number of boxes drawn per labeller for each plot ####
## 06.6  Mixed Model and ICC for n_bbox repeatability between the three sites ####
## 06.7  Graphic 5: Boxplot for n_bbox repeatability between the three sites ####
## 06.8  Graphic 6: Barplot for ICC for each within labeller variation at computer ####

## 07 Live n_bbox and n_bbox repeatability ####
## 07.1 Mixed Model and ICC for n_bbox repeatability between live labellers ####
## 07.2 Graphic 7: Box plot of n_bbox numbers between live labellers ####
## 07.3 Graphic 8: Interaction plot of the mean number of boxes per plot per labeller ####

## 08 n_bbox Comapring methodologies ####
## 08.1 Mixed Model and ICC for n_bbox repeatability between the computer method and the live method ####
## 08.2 Graphic 9: Average n_bbox number between the computer method and the live method ####
## 08.3 Graphic 10: Variation between each methods by labeller and plot ####
## 08.4 Graphic 11: Number of boxes drawn per labeller for each plot (live) ####
## 08.5 Graphic 12: Barplot for ICC for each within labeller variation (live) ####

## 09 centre and centre repeatability

##**************************************

## 00 Setup ####

# This should work if the text data files are in the working directory
## Combining all the text files into one meta data file for the analysis
setwd("C:/Users/georg/OneDrive - Harper Adams University/Documents/Masters Project/Data/mastersdata")
files <- list.files(patter = "*.txt", recursive = TRUE) ## only using the txt file not the jpg

## splitting the files between methods 
filescomputer <- files[1:300] ##

fileslive <- files[301:390]

## loading required libraries
library(lme4)
library(lmerTest)

##**************************************

## 01 Computer Labeling Data Object ####

data <- lapply(filescomputer, function(x) {
  out <- data.table::fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})

computerdata <- data.table::rbindlist(data)

## Var names
names(computerdata) <- c('class', 'bb_x_cent', 'bb_y_cent', 'bb_width', 'bb_height', 'text_file_location')

##**************************************

## 02 Live Labeling Data Object ####

data01 <- lapply(fileslive, function(x) {
  out <- data.table::fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})

livedata <- data.table::rbindlist(data01)

## Var names
names(livedata) <- c('class', 'bb_x_cent', 'bb_y_cent', 'bb_width', 'bb_height', 'text_file_location')

##**************************************

## 03 Data wrangling for computer labelling ####

## How many rows per file
computernrows <- numeric()
for(i in 1:length(filescomputer)){
  computernrows[i] <- nrow(read.delim(file = filescomputer[i], header = F, sep = " "))
}

## how many files
computernfiles<- length(filescomputer)

## Create a picture variable
picture <- numeric()

for( i in 1:computernfiles){
  picture <- c(picture, rep(i, times = computernrows[i]))
}

computerdata01 <- data.frame(computerdata, picture)

## separating the image_location to get the file name on its own
computerfilelocation <- data.frame(do.call("rbind", strsplit(as.character(computerdata01$text_file_location), "/", fixed = TRUE))) 

## var names
names(computerfilelocation) <- c('method_01', 'name', 'yolo', 'date', "txt")

## separate to get individual labeller ID
labellerid <- data.frame(do.call("rbind", strsplit(as.character(computerfilelocation$name), "-", fixed = TRUE))) 

## merge data frames using cbind function 
computerdata02 <-cbind(computerdata01, computerfilelocation, labellerid)

## delete out a few columns 
computerdata03 <- subset(computerdata02, select = -c (text_file_location, method_01, name, yolo, txt, X2, X3))

## add a site variable (bhive and harper) 
library(dplyr)

computerdata04 <- computerdata03 %>% 
  mutate(site = case_when(X1 == 'BUTLER' | X1 == 'HARRIS' | X1 == 'WAGER' | X1 == 'MHANGO' ~ '1',
                          X1 == 'POTTER' | X1 == 'LEE' | X1 == 'EFFIE' | X1 == 'NEIL' ~ '2',
                          X1 == 'INDIA1' | X1 == 'INDIA2'~ '3'))

## rename the images 
imagenames <- c("2021-07-07 09.38..44 n0_rgb.jpg", "2021-07-07 09.38..44 n9_rgb.jpg", "2021-07-07 09.38..44 n18_rgb.jpg" , 
                "2021-07-07 09.38..44 n27_rgb.jpg",
                "2021-07-07 09.38..44 n36_rgb.jpg", "2021-07-07 09.38..44 n45_rgb.jpg", "2021-07-07 09.38..44 n54_rgb.jpg", 
                "2021-07-07 09.38..44 n63_rgb.jpg", "2021-07-07 09.38..44 n72_rgb.jpg", "2021-07-07 09.38..44 n81_rgb.jpg")


## add the image names to the first 10 nrows then repeat
image_id <- character()

for(i in 1:length(computernrows)){
  if(i %in% seq(1, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[1], times = computernrows[i]))
  if(i %in% seq(2, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[2], times = computernrows[i]))
  if(i %in% seq(3, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[3], times = computernrows[i]))
  if(i %in% seq(4, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[4], times = computernrows[i]))
  if(i %in% seq(5, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[5], times = computernrows[i]))
  if(i %in% seq(6, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[6], times = computernrows[i]))
  if(i %in% seq(7, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[7], times = computernrows[i]))
  if(i %in% seq(8, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[8], times = computernrows[i]))
  if(i %in% seq(9, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[9], times = computernrows[i]))
  if(i %in% seq(10, length(computernrows), by=10))
    image_id <- c(image_id, rep(imagenames[10], times = computernrows[i]))
}

computerdata05 <- data.frame(computerdata04, image_id)

## add plot number
plotnumbers <- c("1", "2", "3" , "4",
                 "5", "6", "7", 
                 "8", "9", "10")

## add the image names to the first 10 nrows 
# how many plot
nplot <- livenfiles/9 ## 10 plots 

plot <- character()

for(i in 1:length(computernrows)){
  if(i %in% seq(1, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[1], times = computernrows[i]))
  if(i %in% seq(2, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[2], times = computernrows[i]))
  if(i %in% seq(3, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[3], times = computernrows[i]))
  if(i %in% seq(4, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[4], times = computernrows[i]))
  if(i %in% seq(5, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[5], times = computernrows[i]))
  if(i %in% seq(6, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[6], times = computernrows[i]))
  if(i %in% seq(7, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[7], times = computernrows[i]))
  if(i %in% seq(8, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[8], times = computernrows[i]))
  if(i %in% seq(9, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[9], times = computernrows[i]))
  if(i %in% seq(10, length(computernrows), by=10))
    plot <- c(plot, rep(plotnumbers[10], times = computernrows[i]))
}

computerdata06 <- data.frame(computerdata05, plot)

## add 1st 2nd 3rd rep to each pic 

reps <- c("first", "secound", "third") # in order and creating a new object
picrep <- character()

for(i in 1:length(computernrows)){
  if(i %in% seq(1, length(computernrows), by=3))
    picrep <- c(picrep, rep(reps[1], times = computernrows[i]))
  if(i %in% seq(2, length(computernrows), by=3))
    picrep <- c(picrep, rep(reps[2], times = computernrows[i]))
  if(i %in% seq(3, length(computernrows), by=3))
    picrep <- c(picrep, rep(reps[3], times = computernrows[i]))
}

computerdata07 <- data.frame(computerdata06, picrep)

## add a new variable called "method"

method <- c("computer")

computerdata08 <- data.frame(computerdata07, method)

##**************************************

## 04 Data wrangling for live labelling ####

## How many rows per file
livenrows <- numeric()
for(i in 1:length(fileslive)){
  livenrows[i] <- nrow(read.delim(file = fileslive[i], header = F, sep = " "))
}

## how many files
livenfiles<- length(fileslive)

# how many plot
nplot <- livenfiles/9 ## 10 plots 


# Create observer and plot variables

# seq(1, length(files), by=9) # pic 1, observer 1, plots 1 to x
# seq(2, length(files), by=9) # pic 2, observer 1, plots 1 to x
# seq(3, length(files), by=9) # pic 3, observer 1, plots 1 to x
# seq(4, length(files), by=9) # pic 1, observer 2, plots 1 to x
# seq(5, length(files), by=9) # pic 2, observer 2, plots 1 to x
# seq(6, length(files), by=9) # pic 3, observer 2, plots 1 to x

# observer var
recorders <- c("BUTLER", "HARRIS", "WAGER") # in order and creating a new object

# observer var
observer <- character()

for(i in 1:length(livenrows)){
  if(i %in% seq(1, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[1], times = livenrows[i]))
  if(i %in% seq(2, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[1], times = livenrows[i]))
  if(i %in% seq(3, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[1], times = livenrows[i]))
  if(i %in% seq(4, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[2], times = livenrows[i]))
  if(i %in% seq(5, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[2], times = livenrows[i]))
  if(i %in% seq(6, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[2], times = livenrows[i]))
  if(i %in% seq(7, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[3], times = livenrows[i]))
  if(i %in% seq(8, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[3], times = livenrows[i]))
  if(i %in% seq(9, length(livenrows), by=9))
    observer <- c(observer, rep(recorders[3], times = livenrows[i]))
}

livedata01 <- data.frame(livedata, observer)

# Create plot variable
plot <- numeric()

for(i in 1:nplot){
  plot <- c(plot, rep(i, times = sum(livenrows[(i*9-8):(i*9)])))
}

livedata02 <- data.frame(livedata01, plot)

# Create picture variable
picture <- numeric()

for( i in 1:livenfiles){
  picture <- c(picture, rep(i, times = livenrows[i]))
}

livedata03 <- data.frame(livedata02, picture)

## Create picrep variable
## for the the first nrow in every three nrows assign character 'first'
## for the second nrow in every three nrows assign character 'second'
## for the third nrow in every three nrows assign the character 'third' 

# seq(1, length(nrows), by=3) # pic 1, picrep 1, plots 1 to x
# seq(2, length(nrows), by=3) # pic 2, picrep 2, plots 1 to x
# seq(3, length(nrows), by=3) # pic 3, picrep 3, plots 1 to x

reps <- c("first", "secound", "third") # in order and creating a new object
picrep <- character()

for(i in 1:length(livenrows)){
  if(i %in% seq(1, length(livenrows), by=3))
    picrep <- c(picrep, rep(reps[1], times = livenrows[i]))
  if(i %in% seq(2, length(livenrows), by=3))
    picrep <- c(picrep, rep(reps[2], times = livenrows[i]))
  if(i %in% seq(3, length(livenrows), by=3))
    picrep <- c(picrep, rep(reps[3], times = livenrows[i]))
}

livedata04 <- data.frame(livedata03, picrep)

## split the text_file_location column 
livedatafilelocation <- data.frame(do.call("rbind", strsplit(as.character(livedata04$text_file_location), "/", fixed = TRUE))) 

## delete out a few columns 
livedatafilelocation01 <- subset(livedatafilelocation, select = -c (X1, X2))

## replace .txt in image id to.jpg
image_ID <- gsub('.txt', '.jpg', livedatafilelocation01$X4)

## add image_ID to the data frame
livedatafilelocation02 <- data.frame(livedatafilelocation01, image_ID)

## now remove X4
livedatafilelocation03 <- subset(livedatafilelocation02, select = -c (X4))

## Var names
names(livedatafilelocation03) <- c('date', 'image_ID')

## merge data frames using cbind function 
livedata05 <-cbind(livedata04, livedatafilelocation03)

## add a new variable called "method"
method <- c("live")

livedata06 <- data.frame(livedata05, method)

## remove text_file_location column 
livedata07 <- subset(livedata06, select = -c (text_file_location))

## add site column
site <- c("1")

livedata07 <- data.frame(livedata07, site)

##**************************************

##  05 Tidying up the dataframes ####

## tidy up computer labelling
## change X1 to labeller in computerdata08
## change observer to labeller in livedata07

names(computerdata08)[names(computerdata08) == "X1"] <- "labeller"
names(livedata07)[names(livedata07) == "observer"] <- "labeller"
names(livedata07)[names(livedata07) == "image_ID"] <- "image_id"

## reorder dataframes so columns are the same
colnames(computerdata08)
computerdata09 <- computerdata08[, c(10,11,6,12,8,9,7,13,1,2,3,4,5)]

colnames(livedata07)
livedata08 <- livedata07[, c(11,7,8,9,6,13,10,12,1,2,3,4,5)]

## un-identifying labeller

## un-identifying labeller
livedata08 <- livedata08 %>% 
  mutate(labeller = case_when(labeller == 'BUTLER' ~ 'A',
                              labeller == 'HARRIS' ~ 'B',
                              labeller == 'WAGER' ~ 'C'))


computerdata10 <- computerdata09 %>% 
  mutate(labeller = case_when(labeller == 'BUTLER' ~ 'A',
                              labeller == 'HARRIS' ~ 'B',
                              labeller == 'WAGER' ~ 'C',
                              labeller == 'MHANGO' ~ 'D',
                              labeller == 'POTTER' ~ 'E',
                              labeller == 'LEE' ~ 'F',
                              labeller == 'EFFIE' ~ 'G',
                              labeller == 'NEIL' ~ 'H',
                              labeller == 'INDIA1' ~ 'I',
                              labeller == 'INDIA2' ~ 'J'))



## creating one total dataframe
alldata <- rbind(livedata08, computerdata10)

summary(alldata)

alldata$labeller <- as.factor(alldata$labeller)
alldata$plot <- as.factor(alldata$plot)
alldata$method <- as.factor(alldata$method)
alldata$picrep <- as.factor(alldata$picrep)
alldata$site <- as.factor(alldata$site)

##**************************************

## 06 Computer n_bbox and n_bbox repeatability ####

## Creating a number of boxes data frame
computern_bbox <- aggregate(class ~ labeller + plot + picture + site, data = computerdata10, length)
names(computern_bbox)[5] <- "nbox"
computern_bbox$labeller <- as.factor(computern_bbox$labeller)
computern_bbox$plot <- as.factor(computern_bbox$plot)
computern_bbox$picture <- as.factor(computern_bbox$picture) 
computern_bbox$site <- as.factor(computern_bbox$site) 

## 06.1 Mixed Model and ICC for n_bbox repeatability between computer labellers ####
lm0_nbbox <- lmer(nbox ~ plot + (1|labeller), data = computern_bbox)

anova(lm0_nbbox)
summary(lm0_nbbox)

# intra class correlation - (inter - rater reliability)
6.772/(6.772+2.256) # 75% - ICC between observers 

## 06.2 Graphic 1: Grey version boxplot of the between labeller variation per plot (computer) ####
library(RColorBrewer)
names(computern_bbox)[names(computern_bbox) == "plot"] <- "Plot" ## renaming plot
boxplot <- boxplot(nbox~ Plot*labeller, data = computern_bbox, 
        las=2, xlab= "Plot",ylab = "Number of Boxes",
        main = 'High variation between computer labellers (75% ICC)', 
        VADeaths,col=brewer.pal(9,"Greys")) ## reproducibility in box number

## 06.3 Graphic 2: Interaction plot for the mean box numbers between computer labellers ####
ip1 <- interaction.plot(x.factor = computern_bbox$Plot, #x-axis variable
                 trace.factor = computern_bbox$labeller, #variable for lines
                 response = computern_bbox$nbox, #y-axis variable
                 ylab = "Mean Number of Boxes",
                 xlab = "Plot",
                 main = "High variation between computer labellers (75% ICC)",
                 col = c("pink", "dark blue", "red", "green" , "orange", "light blue",
                         "purple", "yellow", "dark grey", "black"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Labeller")

## 06.4 Graphic 3: Boxplot for the mean number of boxes drawn per computer labellers ####
## ggplot 
library(ggplot2)
windowsFonts("Arial" = windowsFont("Arial")) 
library(RColorBrewer) ## color palettes 

## Average number of boxes drawn per labeller 
names(computern_bbox)[names(computern_bbox) == "plot"] <- "Plot"
boxplot02 <- ggplot(computern_bbox, aes(labeller, nbox), y = nbox, fill = Plot) + ## creating a plot using the data frame with the correct crops and locations
  geom_boxplot(alpha=0.7) +
  scale_y_continuous (name = "No of boxes") +
  scale_x_discrete (name = "Labeller") +
  ggtitle("Average number of boxes drawn for each computer labeller over each plot") +
  theme_bw() +theme(plot.title = element_text(size = 12, face = "bold"),
                    text = element_text(size = 12),
                    axis.title = element_text(face="bold"),
                    axis.text.x=element_text(size = 11),
                    legend.position = "bottom" +  scale_fill_brewer(palette = "Accent"))
boxplot02

## 06.5 Graphic 4: Boxplot for the Number of boxes drawn per labeller for each plot ####
names(computern_bbox)[names(computern_bbox) == "plot"] <- "Plot"
boxplot03 <- ggplot(computern_bbox, aes(reorder(labeller, nbox), y = nbox, fill = Plot)) + ## creating a plot using the data frame with the correct crops and locations
  geom_boxplot(alpha=0.7) +
  scale_y_continuous (name = "No of boxes") +
  scale_x_discrete (name = "Labeller") +
  ggtitle("Number of boxes drawn per labeller for each plot") +
  theme_bw() +theme(plot.title = element_text(size = 12, face = "bold"),
                    text = element_text(size = 12),
                    axis.title = element_text(face="bold"),
                    axis.text.x=element_text(size = 11),
                    legend.position = "bottom" + scale_fill_brewer(palette = "Accent"))
boxplot03

## 06.6  Mixed Model and ICC for n_bbox repeatability between the three sites ####
#n_bbox repeatability between sites
lm0_nbbox01 <- lmer(nbox ~ site + (1|labeller), data = computern_bbox)

anova(lm0_nbbox01)
summary(lm0_nbbox01)

8.112/(8.112+4.261) # 65% ICC between sites 

## 06.7  Graphic 5: Boxplot for n_bbox repeatability between the three sites ####

## Average number of boxes drawn per site for computer labelling
names(computern_bbox)[names(computern_bbox) == "plot"] <- "Plot"
boxplot01 <- ggplot(computern_bbox, aes(site, nbox), y = nbox, fill = site) + ## creating a plot using the data frame with the correct crops and locations
  geom_boxplot(alpha=0.7) +
  scale_y_continuous (name = "No of boxes") +
  scale_x_discrete (name = "Site") +
  ggtitle("Average number of boxes drawn per image between the three sites") +
  theme_bw() +theme(plot.title = element_text(size = 12, face = "bold"),
                    text = element_text(size = 12),
                    axis.title = element_text(face="bold"),
                    axis.text.x=element_text(size = 11),
                    legend.position = "bottom" + scale_fill_brewer(palette = "Accent"))
boxplot01

## 06.8  Graphic 6: Barplot for ICC for each within labeller variation at computer ####
## n_bbox repeatability within computer labellers ####

dataa <- computern_bbox[which(computern_bbox$labeller == "A"),]
datab <- computern_bbox[which(computern_bbox$labeller == "B"),]
datac <- computern_bbox[which(computern_bbox$labeller == "C"),]
datad <- computern_bbox[which(computern_bbox$labeller == "D"),]
datae <- computern_bbox[which(computern_bbox$labeller == "E"),]
dataf <- computern_bbox[which(computern_bbox$labeller == "F"),]
datag <- computern_bbox[which(computern_bbox$labeller == "G"),]
datah <- computern_bbox[which(computern_bbox$labeller == "H"),]
datai <- computern_bbox[which(computern_bbox$labeller == "I"),]
dataj <- computern_bbox[which(computern_bbox$labeller == "J"),]


## Variation in Labeller A 
lm_a <- lmer(nbox ~ Plot+ (1|Plot), data = dataa)
summary(lm_a)

16.174/(16.174+1.167) ##93%

## Variation in Labeller B
lm_b <- lmer(nbox ~ 1+ (1|Plot), data = datab)
summary(lm_b)

3.163/(3.163+0.9) ##77% 

## Variation in Labeller C 
lm_c <- lmer(nbox ~ 1+ (1|Plot), data = datac)
summary(lm_c)

4.065/(4.065+0.4) ##91%

## Variation in Labeller D
lm_d <- lmer(nbox ~ 1+ (1|Plot), data = datad)
summary(lm_d)

2.1531/(2.1531+0.667) ##76%

## Variation in Labeller E
lm_e <- lmer(nbox ~ 1+ (1|Plot), data = datae)
summary(lm_e)

1.532/(1.532+2.4) ##38%

## Variation in Labeller F
lm_f <- lmer(nbox ~ 1+ (1|Plot), data = dataf)
summary(lm_f)

3.7568/(3.7568+0.4333) ##89%

## Variation in Labeller G
lm_g <- lmer(nbox ~ 1+ (1|Plot), data = datag)
summary(lm_g)

3.891/(3.891+1.2) ##76%

## Variation in Labeller H
lm_h <- lmer(nbox ~ 1+ (1|Plot), data = datah)
summary(lm_h)

1.983/(1.983+2.233) ##47%

## Variation in Labeller I
lm_i <- lmer(nbox ~ 1+ (1|Plot), data = datai)
summary(lm_i)

1.595/(1.595+1.367) ##53%

## Variation in Labeller J
lm_j <- lmer(nbox ~ 1+ (1|Plot), data = dataj)
summary(lm_j)

4.893/(4.893+3.4) ##59%

## Creating a graph for computer within labeller variation
computerwithin <- c(A=93, C=91, F=89, B=77, D=76, G=76, J=59, I=53,  H=47, E=38)
computerwithin
barplot(computerwithin,
        col="dodgerblue3",
        xlab = "Labeller",
        ylab = "ICC Within Variation Score (%)",
        main = "ICC variation within Individual Computer Labellers",
        ylim = c(0, 100))



## 07 Live n_bbox and n_bbox repeatability ####

## creating a number of boxes data frame
names(liven_bbox)[names(liven_bbox) == "plot"] <- "Plot"
liven_bbox <- aggregate(class ~ labeller + plot + picture + site, data = livedata08, length)
names(liven_bbox)[5] <- "nbox"
liven_bbox$labeller <- as.factor(liven_bbox$labeller)
liven_bbox$plot <- as.factor(liven_bbox$plot)
liven_bbox$picture <- as.factor(liven_bbox$picture) 
liven_bbox$site <- as.factor(liven_bbox$site) 


## 07.1 Mixed Model and ICC for n_bbox repeatability between live labellers ####
betweenlive <- lmer(nbox ~ plot + (1|labeller), data = liven_bbox)

anova(betweenlive)
summary(betweenlive)

# intra class correlation - (inter - rater reliability)
0/(0+3.517) # 0% - ICC between observers 

## 07.2 Graphic 7: Box plot of n_bbox numbers between live labellers ####

library(RColorBrewer)
boxplot(nbox~ plot*labeller, data = liven_bbox, 
        las=2, xlab= "Plot",ylab = "Number of Boxes",
        main = 'Variation between live labellers (0% ICC)', 
        VADeaths,col=brewer.pal(9,"Greys")) ## reproducibility in box number

## 07.3 Graphic 8: Interaction plot of the mean number of boxes per plot per labeller ####
interaction.plot(x.factor = liven_bbox$plot, #x-axis variable
                        trace.factor = liven_bbox$labeller, #variable for lines
                        response = liven_bbox$nbox, #y-axis variable
                        ylab = "Mean Number of Boxes",
                        xlab = "Plot",
                        main = "Mean Number of boxes between live labellers plots",
                        col = c("pink", "dark blue", "green" , "orange"),
                        lty = 1, #line type
                        lwd = 2, #line width
                        trace.label = "Labeller")

## 08 n_bbox Comapring methodologies ####
## Create a data frame of only data from labellers A, B and c
comparisondata <- alldata[which(alldata$labeller == "A" | alldata$labeller == "B" | alldata$labeller == "C" ),]
comparisonn_bbox <- aggregate(class ~ labeller + plot + picture + method, data = comparisondata, length)
names(comparisonn_bbox)[5] <- "nbox"
comparisonn_bbox$labeller <- as.factor(comparisonn_bbox$labeller)
comparisonn_bbox$plot <- as.factor(comparisonn_bbox$plot)
comparisonn_bbox$picture <- as.factor(comparisonn_bbox$picture) 
comparisonn_bbox$method <- as.factor(comparisonn_bbox$method) 

## 08.1 Mixed Model and ICC for n_bbox repeatability between the computer method and the live method ####
compare <- lmer(nbox ~ plot+ method + (1|labeller), data = comparisonn_bbox)

anova(compare)
summary(compare)

1.925/(1.925+4.562) # 29% - ICC between methods (quite poor)

## 08.2 Graphic 9: Average n_bbox number between the computer method and the live method ####
names(comparisonn_bbox)[names(comparisonn_bbox) == "plot"] <- "Plot"
boxplot08 <- ggplot(comparisonn_bbox, aes(method, nbox), y = nbox, fill = method) + ## creating a plot using the data frame with the correct crops and locations
  geom_boxplot(alpha=0.7) +
  scale_y_continuous (name = "No of boxes") +
  scale_x_discrete (name = "Method") +
  ggtitle("Average number of boxes drawn per image between the two methods") +
  theme_bw() +theme(plot.title = element_text(size = 12, face = "bold"),
                    text = element_text(size = 12),
                    axis.title = element_text(face="bold"),
                    axis.text.x=element_text(size = 11),
                    legend.position = "bottom" + scale_fill_brewer(palette = "Accent"))
boxplot08

## 2.5 average box difference between methods 

## 08.3 Graphic 10: Variation between each methods by labeller and plot ####
library(ggplot2)
windowsFonts("Arial" = windowsFont("Arial")) 
library(RColorBrewer) ## color palettes 

## one box per plot comparing the variation between methods 
comparison1 <- ggplot(comparisonn_bbox, aes(x=labeller, y=nbox, fill=method)) + 
  geom_boxplot() +
  facet_wrap(~plot, scale = "free")
comparison1

## 08.4 Graphic:11 Number of boxes drawn per labeller for each plot (live) ####
names(liven_bbox)[names(liven_bbox) == "plot"] <- "Plot"
liveboxplot03 <- ggplot(liven_bbox, aes(reorder(labeller, nbox), y = nbox, fill = Plot)) + ## creating a plot using the data frame with the correct crops and locations
  geom_boxplot(alpha=0.7) +
  scale_y_continuous (name = "No of boxes") +
  scale_x_discrete (name = "Labeller") +
  ggtitle("Number of boxes drawn per labeller for each plot") +
  theme_bw() +theme(plot.title = element_text(size = 12, face = "bold"),
                    text = element_text(size = 12),
                    axis.title = element_text(face="bold"),
                    axis.text.x=element_text(size = 11),
                    legend.position = "bottom" + scale_color_grey()) ## scale_fill_brewer(palette = "Greys"))
liveboxplot03

## 08.5 Graphic 12: Barplot for ICC for each within labeller variation (live) ####

dataaa <- liven_bbox[which(liven_bbox$labeller == "A"),]
databb <- liven_bbox[which(liven_bbox$labeller == "B"),]
datacc <- liven_bbox[which(liven_bbox$labeller == "C"),]

## Variation in Labeller A (live)
lm_aa <- lmer(nbox ~ Plot+ (1|Plot), data = dataaa)
summary(lm_aa)

10.68/(10.68+2.2) ##82%

## Variation in Labeller B (live)
lm_bb <- lmer(nbox ~ 1+ (1|Plot), data = databb)
summary(lm_bb)

3.228/(3.228+2.467) ##56%

## Variation in Labeller c (live)
lm_cc <- lmer(nbox ~ 1+ (1|Plot), data = datacc)
summary(lm_cc)

1.288/(1.288+2.6) ##33%

##**************************************
## 09 centre and centre repeatability

# centre 
# data[1:12, 1:3] # George's 1st pic
# data[13:26, 1:3] # George's 2nd pic

## George psudeo code 
## Get all Matt pic 1
## Get all Matt pic 2 
## Get all Matt pic 3


newdataframe <- data[c(data$plot == 1 & data$observer == "matt" & data$picrep == 'first'), 2:3] ## get the 2nd and third column

test1 <- which(data$plot == 1 & data$observer == "Matt" & data$picrep == 'first')
test2 <- which(data$plot == 1 & data$observer == "Matt" & data$picrep == 'second')


test3 <- setdiff(test2, test1)

diff <- matrix(nrow=length(test1),
               ncol=length(test2))

for(i in 1:length(test1)){ ## for every one picture 
  for(j in 1:length(test2)){
    diff[i,j] <- sqrt((data$bb_x_cent[test2[j]] - data$bb_x_cent[test1[i]])^2 +
                        (data$bb_y_cent[test2[j]] - data$bb_y_cent[test1[i]])^2)
  }
}

# test the code with different indices
threshold <- .02
index <- 11
which(diff[index, ] > 0 & diff[index, ] < threshold)
hist(diff)

##*****************************


