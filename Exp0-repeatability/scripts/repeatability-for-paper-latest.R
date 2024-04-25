## HEADER ####
## What: Repeatability for paper
## Who: Ed and George
## Last edited: 2024-04-21 (George)

## CONTENTS ####
## 00 Setup 
## 01 Box count
## 02 Centre distances

## 01 Box count
m0_stems <- rpt(num_stems ~ method + (1 | plot) + (1 | subject), 
                  data = stems,
                  datatype = "Gaussian",
                  grname = c("plot", "subject"), 
                  npermut = 0,
                  nboot = 1000)
print(m0_stems)
summary(m0_stems)

# subject repeatability for live
m1_stems <- rpt(num_stems ~  (1 | plot) + (1 | subject), 
                  data = stems[stems$method == 'live', ],
                  datatype = "Gaussian",
                  grname = c("plot", "subject"), 
                  npermut = 0,
                  nboot = 1000)
print(m1_stems)
summary(m1_stems)

# subject repeatability for comp
m2_stems <- rpt(num_stems ~  (1 | plot) + (1 | subject), 
                data = stems[stems$method == 'computer', ],
                datatype = "Gaussian",
                grname = c("plot", "subject"), 
                npermut = 0,
                nboot = 1000)
print(m2_stems)
summary(m2_stems)



## 02 Centre distances
# repeatability between methods
m0_centres <- rpt(distance ~  method + (1 | plot) + (1 | subject), 
                  data = c_dist,
                  datatype = "Gaussian",
                  grname = c("plot", "subject"), 
                  npermut = 0,
                  nboot = 1000)
print(m0_centres)
summary(m0_centres)

# subject repeatability for live
m1_centres <- rpt(distance ~  (1 | plot) + (1 | subject), 
                  data = c_dist[c_dist$method == 'live', ],
                  datatype = "Gaussian",
                  grname = c("plot", "subject"), 
                  npermut = 0,
                  nboot = 1000)
print(m1_centres)
summary(m1_centres)

# subject repeatability for comp
m2_centres <- rpt(distance ~  (1 | plot) + (1 | subject), 
                  data = c_dist[c_dist$method == 'computer', ],
                  datatype = "Gaussian",
                  grname = c("plot", "subject"), 
                  npermut = 0,
                  nboot = 1000)
print(m2_centres)
summary(m2_centres)
