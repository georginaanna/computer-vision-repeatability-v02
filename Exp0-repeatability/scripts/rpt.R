# repeatability for DISTANCE ####
m0.methods <- rpt(distance ~ method + (1 | subject), 
                  data = c_dist,
                  datatype = "Gaussian",
                  grname = c("subject", 'Fixed', 'Residual'), 
                  npermut = 0,
                  nboot = 1000,
                  adjusted = F)
print(m0.methods)
summary(m0.methods)
plot(m0.methods, xlim = c(0,1))

m0.live <- rpt(distance ~  1 + (1 | subject), 
                  data = c_dist[which(c_dist$method == 'live'),],
                  datatype = "Gaussian",
                  grname = c("subject",  'Residual'), 
                  npermut = 0,
                  nboot = 1000)
print(m0.live)
summary(m0.live)
plot(m0.live, xlim = c(0,1))


m0.computer <- rpt(distance ~ 1 + (1 | subject), 
                data = c_dist[which(c_dist$method == 'computer'),],
                datatype = "Gaussian",
                grname = c("subject",  'Residual'), 
                npermut = 0,
                nboot = 1000)
print(m0.computer)
summary(m0.computer)
plot(m0.computer, xlim = c(0,1))


# repeatability for STEMS ####
s0.methods <- rpt(num_stems ~ method + (1 | subject), 
                  data = stems,
                  datatype = "Gaussian",
                  grname = c("subject", 'Fixed', 'Residual'), 
                  npermut = 0,
                  nboot = 1000,
                  adjusted = F)
print(s0.methods)
summary(s0.methods)
plot(s0.methods, xlim = c(0,1))

s0.live <- rpt(num_stems ~  1 + (1 | subject), 
               data = stems[which(stems$method == 'live'),],
               datatype = "Gaussian",
               grname = c("subject",  'Residual'), 
               npermut = 0,
               nboot = 1000)
print(s0.live)
summary(s0.live)
plot(s0.live, xlim = c(0,1))


s0.computer <- rpt(num_stems ~ 1 + (1 | subject), 
                   data = stems[which(stems$method == 'computer'),],
                   datatype = "Gaussian",
                   grname = c("subject",  'Residual'), 
                   npermut = 0,
                   nboot = 1000)
print(s0.computer)
summary(s0.computer)
plot(s0.computer, xlim = c(0,1))
