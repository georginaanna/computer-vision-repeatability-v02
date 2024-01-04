## HEADER ####
## What: Compute centre + stem count repeatability
## Who: Ed 
## Last edited: 2022-02-28 (Ed)

## CONTENTS ####
## 00 Setup 
## 01 Box count
## 02 Centre distances

## 00 Setup ####

# Working dir: open project file 'repeatability.proj'

packages <- c('lme4','lmerTest','ggbiplot','dplyr', 'tictoc',
              'rptR', 'irr') # one way to do repeatability
lapply(packages, require, character.only = T); rm(packages)

# Make data (takes a few mins to run)
tic(); source('scripts/centre-data.R'); toc() # make data objects 

# create mean dist summary stats
source('scripts/create-dist-mean-sd.R')

# create mean stem summary stats
source('scripts/create-stem-mean-sd.R')



## 01 Error graphs ####

# center distance
source('scripts/Fig-error-distance.R')

# stem count
source('scripts/Fig-error-st-count.R')

## 02 stats ####

### center distance ####
interaction.plot(x.factor = c_dist$plot,
                 trace.factor = c_dist$subject,
                 response = c_dist$distance)


anova(
  lm(distance ~  method * subject,
           data = c_dist)
)

d0 <- lmer(distance ~  method + (1 | subject),
     data = c_dist)
anova(d0)
summary(d0)
# avg repeatability between both methods
(rept.d0 <- 2.975e-06/(2.975e-06 + 1.996e-05)) 


# just LIVE
d1 <- lmer(distance ~  1 + (1 | subject),
           data = c_dist[c_dist$method == 'live', ])
summary(d1)
# avg repeatability between both methods LOW
(rept.d1 <- 1.905e-08/(1.905e-08 + 1.825e-05)) 

# just COMPUTER
d2 <- lmer(distance ~  1 + (1 | subject),
           data = c_dist[c_dist$method == 'computer', ])
summary(d2)
# avg repeatability between both methods LOW
(rept.d2 <- 2.997e-06/(2.997e-06 + 2.053e-05)) 

### stem count ####
interaction.plot(x.factor = stems$plot,
                 trace.factor = stems$subject,
                 response = stems$num_stems)


anova(
  lm(num_stems ~  method * subject,
     data = stems)
)

s0 <- lmer(num_stems ~  method + (1 | subject),
           data = stems)
anova(s0)
summary(s0)
# avg repeatability between both methods
(rept.s0 <- 5.774/(5.774 + 4.331)) 

# just LIVE
s1 <- lmer(num_stems ~  1 + (1 | subject),
           data = stems[stems$method == 'live', ])
summary(s1)
# avg repeatability for LIVE 
(rept.s1 <- 1.452/(1.452 +  3.533)) 

# just COMPUTER
s2 <- lmer(num_stems ~  1 + (1 | subject),
           data = stems[stems$method == 'computer', ])
summary(s2)
# avg repeatability for COMPUTER
(rept.s2 <- 6.705/(6.705 + 4.261)) 




### Fig: do the methods differ overall? ####
boxplot(num_stems ~ method, data = stems,
        ylab = 'Stem count', xlab = 'Method', cex = 0)
stripchart(num_stems ~ method, 
           data = stems, 
           method = 'jitter',
           col = 'red', pch = 16, cex = .2,
           vertical = T, add=T)

### Fig: do the methods differ overall for individuals? ####
stems_HAU <- stems[stems$group == 'HAU' & stems$subject != 'mhango',]

par(xpd = T)
par(mar = c(5,4,4,2))

interaction.plot(x.factor = stems_HAU$method,
                 trace.factor = stems_HAU$subject,
                 response = stems_HAU$num_stems,
                 ylab = 'Mean stem count', 
                 xlab = 'Method', legend = F,
                 col = 'red', lwd = 2, lty = c(3,1,2))

legend('top', inset = -.3, 0, bty = 'n',
       legend = c('Ha', 'Wa', 'Bu'), 
       title = "Subject", horiz = T,
       col = 'red', lwd = 2, lty = c(1,2,3))

par(mar = c(5,4,4,2)+.1)


### Fig: do the groups differ for computer-only? ####
stems_comp <- stems[stems$method == 'computer', ]
boxplot(num_stems ~ group, data = stems_comp,
        ylab = 'Stem count', xlab = 'Group', cex = 0)
stripchart(num_stems ~ group, 
           data = stems_comp, 
           method = 'jitter',
           col = 'red', pch = 16, cex = .2,
           vertical = T, add=T)

### Fig: do observers differ? ####
groups_comp <- c('HAU', 'BHive', 'HAU', 'BHive', 'HAU', 
            'BHive', 'India', 'India', 'BHive', 'HAU')
col_comp <- c('gray20', 'gray50', 'gray20', 'gray50', 'gray20', 
                'gray50', 'gray80', 'gray80', 'gray50', 'gray20')

par(xpd = T)
par(mar = c(7,4,4,2))
boxplot(num_stems ~ subject, data = stems_comp, las = 2,
        ylab = 'Stem count', xlab = '', cex = 0,
        col = col_comp)
stripchart(num_stems ~ subject, data = stems_comp, 
           method = 'jitter',
           col = 'red', pch = 16, cex = .2,
           vertical = T, add=T)
mtext(line = 4.5, text = "Subject", side = 1)
mtext(line = 2, text = "Observers vary", side = 3)


stems_live <- stems[stems$method == 'live', ]
means_live <- aggregate(x = stems_live$num_stems, 
          by = list(stems_live$subject),
          FUN = mean)
points(x = c(1,3,10), y = means_live$x,
       pch = 16, col = 'blue', cex=1)
# par(xpd = F)
# abline(h = mean(means_live$x), lty = 2, col = 'blue')

legend(x = .8, y = 25 , bty = 'n',
       legend = c('HAU', 'BHive', 'India', 'Live'), 
       title = "", horiz = T,
       col = c('gray20','gray50', 'gray80', 'blue'),
       pch = c(15,15,15,16))

par(mar = c(5,4,4,2))


### Stats for box count ####
# possibly of interest for repeatability
# https://github.com/mastoffel/rptR
# https://cran.r-project.org/web/packages/rptR/index.html
# https://cran.r-project.org/web/packages/rptR/vignettes/rptR.html

m0_stems <- rpt(num_stems ~ method + (1 | plot) + (1 | subject), 
          data = stems,
          datatype = "Gaussian",
          grname = c("plot", "subject"), 
          npermut = 0,
          nboot = 1000)
print(m0_stems)
summary(m0_stems)


plot(m0_stems, type = "boot", 
     grname = "plot", 
     cex.main = 0.8)
plot(m0_stems, type = "boot", 
     grname = "subject", 
     cex.main = 0.8)

## 02 Centres distance ####

# centres distance within observers

### Fig: do the methods differ overall? ####
boxplot(distance ~ method, 
        data = c_dist,
        ylab = 'Centre difference', xlab = 'Method', cex = 0)
stripchart(distance ~ method, 
           data = c_dist, 
           method = 'jitter',
           col = 'red', pch = 16, cex = .2,
           vertical = T, add=T)

### Fig: do the methods differ overall for individuals? ####
c_dist_HAU <- c_dist[c_dist$group == 'HAU' & c_dist$subject != 'mhango', ]

par(xpd = T)
par(mar = c(5,4,4,2))

# interaction.plot(x.factor = c_dist_HAU$method,
#                  trace.factor = c_dist_HAU$subject,
#                  response = c_dist_HAU$distance)
interaction.plot(x.factor = c_dist_HAU$method,
                 trace.factor = c_dist_HAU$subject,
                 response = c_dist_HAU$distance,
                 ylab = 'Centre difference', 
                 xlab = 'Method', legend = F,
                 col = 'red', lwd = 2, lty = c(3,1,2))

legend('top', inset = -.3, 0, bty = 'n',
       legend = c('Ha', 'Wa', 'Bu'), 
       title = "Subject", horiz = T,
       col = 'red', lwd = 2, lty = c(1,2,3))

par(mar = c(5,4,4,2)+.1)


### Fig: do the groups differ for computer-only? ####
c_dist_comp <- c_dist[c_dist$method == 'computer', ]
boxplot(distance ~ group, data = c_dist_comp,
        ylab = 'Centre difference', xlab = 'Group', cex = 0)
stripchart(distance ~ group, 
           data = c_dist_comp, 
           method = 'jitter',
           col = 'red', pch = 16, cex = .2,
           vertical = T, add=T)

### Stats for box count ####
# possibly of interest for repeatability
# https://github.com/mastoffel/rptR
# https://cran.r-project.org/web/packages/rptR/index.html
# https://cran.r-project.org/web/packages/rptR/vignettes/rptR.html

m0_centres <- rpt(distance ~  (1 | plot) + (1 | subject), 
                data = c_dist,
                datatype = "Gaussian",
                grname = c("plot", "subject"), 
                npermut = 0,
                nboot = 1000)
print(m0_centres)
summary(m0_centres)


plot(m0_centres, type = "boot", 
     grname = "plot", 
     cex.main = 0.8)
plot(m0_centres, type = "boot", 
     grname = "subject", 
     cex.main = 0.8)


