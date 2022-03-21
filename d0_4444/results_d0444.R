library(goSorensen)
nSim <- 100000
dRange <- c(0.15, 0.65)
alpha <- 0.05
d0 <- 0.4444

###########################################################################
############################# n1 = 150 #####################################
n1 <- 150

######## Sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l01 <- matrix(NA, length(p11), 9)
colnames(n1l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                   "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                   "coverage")

for(i in 1:length(p11)){
  n1l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                               p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l01 <- as.data.frame(n1l01[,1:3])
dn1l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l02 <- matrix(NA, length(p11), 9)
colnames(n1l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                    "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                    "coverage")

for(i in 1:length(p11)){
  n1l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l02 <- as.data.frame(n1l02[,1:3])
dn1l02

###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l03 <- matrix(NA, length(p11), 9)
colnames(n1l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                    "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                    "coverage")

for(i in 1:length(p11)){
  n1l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l03 <- as.data.frame(n1l03[,1:3])
dn1l03


###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l04 <- matrix(NA, length(p11), 9)
colnames(n1l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                    "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                    "coverage")

for(i in 1:length(p11)){
  n1l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l04 <- as.data.frame(n1l04[,1:3])
dn1l04

###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l05 <- matrix(NA, length(p11), 9)
colnames(n1l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                    "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                    "coverage")

for(i in 1:length(p11)){
  n1l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l05 <- as.data.frame(n1l05[,1:3])
dn1l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l06 <- matrix(NA, length(p11), 9)
colnames(n1l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                    "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                    "coverage")

for(i in 1:length(p11)){
  n1l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l06 <- as.data.frame(n1l06[,1:3])
dn1l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l07 <- matrix(NA, length(p11), 9)
colnames(n1l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                    "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                    "coverage")

for(i in 1:length(p11)){
  n1l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                 p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l07 <- as.data.frame(n1l07[,1:3])
dn1l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l08 <- matrix(NA, length(p11), 9)
colnames(n1l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                    "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                    "coverage")

for(i in 1:length(p11)){
  n1l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                 p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l08 <- as.data.frame(n1l08[,1:3])
dn1l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l09 <- matrix(NA, length(p11), 9)
colnames(n1l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                    "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                    "coverage")

for(i in 1:length(p11)){
  n1l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                 p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l09 <- as.data.frame(n1l09[,1:3])
dn1l09

###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l10 <- matrix(NA, length(p11), 9)
colnames(n1l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n1l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l10 <- as.data.frame(n1l10[,1:3])
dn1l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l11 <- matrix(NA, length(p11), 9)
colnames(n1l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n1l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l11 <- as.data.frame(n1l11[,1:3])
dn1l11


###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l12 <- matrix(NA, length(p11), 9)
colnames(n1l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n1l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l12 <- as.data.frame(n1l12[,1:3])
dn1l12


###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l13 <- matrix(NA, length(p11), 9)
colnames(n1l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n1l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l13 <- as.data.frame(n1l13[,1:3])
dn1l13

###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l14 <- matrix(NA, length(p11), 9)
colnames(n1l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n1l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l14 <- as.data.frame(n1l14[,1:3])
dn1l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n1l15 <- matrix(NA, length(p11), 9)
colnames(n1l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n1l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n1, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn1l15 <- as.data.frame(n1l15[,1:3])
dn1l15

###########################################################################
############################# n2 = 500 #####################################
n2 <- 500

######## sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l01 <- matrix(NA, length(p11), 9)
colnames(n2l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l01 <- as.data.frame(n2l01[,1:3])
dn2l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l02 <- matrix(NA, length(p11), 9)
colnames(n2l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l02 <- as.data.frame(n2l02[,1:3])
dn2l02

###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l03 <- matrix(NA, length(p11), 9)
colnames(n2l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l03 <- as.data.frame(n2l03[,1:3])
dn2l03

###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l04 <- matrix(NA, length(p11), 9)
colnames(n2l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l04 <- as.data.frame(n2l04[,1:3])
dn2l04

###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l05 <- matrix(NA, length(p11), 9)
colnames(n2l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l05 <- as.data.frame(n2l05[,1:3])
dn2l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l06 <- matrix(NA, length(p11), 9)
colnames(n2l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l06 <- as.data.frame(n2l06[,1:3])
dn2l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l07 <- matrix(NA, length(p11), 9)
colnames(n2l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l07 <- as.data.frame(n2l07[,1:3])
dn2l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l08 <- matrix(NA, length(p11), 9)
colnames(n2l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l08 <- as.data.frame(n2l08[,1:3])
dn2l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l09 <- matrix(NA, length(p11), 9)
colnames(n2l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l09 <- as.data.frame(n2l09[,1:3])
dn2l09

###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l10 <- matrix(NA, length(p11), 9)
colnames(n2l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l10 <- as.data.frame(n2l10[,1:3])
dn2l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l11 <- matrix(NA, length(p11), 9)
colnames(n2l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l11 <- as.data.frame(n2l11[,1:3])
dn2l11

###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l12 <- matrix(NA, length(p11), 9)
colnames(n2l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l12 <- as.data.frame(n2l12[,1:3])
dn2l12


###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l13 <- matrix(NA, length(p11), 9)
colnames(n2l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l13 <- as.data.frame(n2l13[,1:3])
dn2l13

###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l14 <- matrix(NA, length(p11), 9)
colnames(n2l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l14 <- as.data.frame(n2l14[,1:3])
dn2l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n2l15 <- matrix(NA, length(p11), 9)
colnames(n2l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n2l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n2, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn2l15 <- as.data.frame(n2l15[,1:3])
dn2l15

###########################################################################
############################# n3 = 1000 #####################################
n3 <- 1000

######## sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l01 <- matrix(NA, length(p11), 9)
colnames(n3l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l01 <- as.data.frame(n3l01[,1:3])
dn3l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l02 <- matrix(NA, length(p11), 9)
colnames(n3l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l02 <- as.data.frame(n3l02[,1:3])
dn3l02


###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l03 <- matrix(NA, length(p11), 9)
colnames(n3l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l03 <- as.data.frame(n3l03[,1:3])
dn3l03

###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l04 <- matrix(NA, length(p11), 9)
colnames(n3l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l04 <- as.data.frame(n3l04[,1:3])
dn3l04


###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l05 <- matrix(NA, length(p11), 9)
colnames(n3l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l05 <- as.data.frame(n3l05[,1:3])
dn3l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l06 <- matrix(NA, length(p11), 9)
colnames(n3l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l06 <- as.data.frame(n3l06[,1:3])
dn3l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l07 <- matrix(NA, length(p11), 9)
colnames(n3l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l07 <- as.data.frame(n3l07[,1:3])
dn3l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l08 <- matrix(NA, length(p11), 9)
colnames(n3l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l08 <- as.data.frame(n3l08[,1:3])
dn3l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l09 <- matrix(NA, length(p11), 9)
colnames(n3l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l09 <- as.data.frame(n3l09[,1:3])
dn3l09


###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l10 <- matrix(NA, length(p11), 9)
colnames(n3l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l10 <- as.data.frame(n3l10[,1:3])
dn3l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l11 <- matrix(NA, length(p11), 9)
colnames(n3l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l11 <- as.data.frame(n3l11[,1:3])
dn3l11


###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l12 <- matrix(NA, length(p11), 9)
colnames(n3l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l12 <- as.data.frame(n3l12[,1:3])
dn3l12


###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l13 <- matrix(NA, length(p11), 9)
colnames(n3l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l13 <- as.data.frame(n3l13[,1:3])
dn3l13

###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l14 <- matrix(NA, length(p11), 9)
colnames(n3l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l14 <- as.data.frame(n3l14[,1:3])
dn3l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n3l15 <- matrix(NA, length(p11), 9)
colnames(n3l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n3l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n3, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn3l15 <- as.data.frame(n3l15[,1:3])
dn3l15

###########################################################################
############################# n4 = 2000 #####################################
n4 <- 2000

######## sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l01 <- matrix(NA, length(p11), 9)
colnames(n4l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l01 <- as.data.frame(n4l01[,1:3])
dn4l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l02 <- matrix(NA, length(p11), 9)
colnames(n4l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l02 <- as.data.frame(n4l02[,1:3])
dn4l02

###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l03 <- matrix(NA, length(p11), 9)
colnames(n4l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l03 <- as.data.frame(n4l03[,1:3])
dn4l03

###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l04 <- matrix(NA, length(p11), 9)
colnames(n4l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l04 <- as.data.frame(n4l04[,1:3])
dn4l04

###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l05 <- matrix(NA, length(p11), 9)
colnames(n4l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l05 <- as.data.frame(n4l05[,1:3])
dn4l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l06 <- matrix(NA, length(p11), 9)
colnames(n4l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l06 <- as.data.frame(n4l06[,1:3])
dn4l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l07 <- matrix(NA, length(p11), 9)
colnames(n4l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l07 <- as.data.frame(n4l07[,1:3])
dn4l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l08 <- matrix(NA, length(p11), 9)
colnames(n4l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l08 <- as.data.frame(n4l08[,1:3])
dn4l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l09 <- matrix(NA, length(p11), 9)
colnames(n4l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l09 <- as.data.frame(n4l09[,1:3])
dn4l09

###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l10 <- matrix(NA, length(p11), 9)
colnames(n4l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l10 <- as.data.frame(n4l10[,1:3])
dn4l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l11 <- matrix(NA, length(p11), 9)
colnames(n4l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l11 <- as.data.frame(n4l11[,1:3])
dn4l11


###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l12 <- matrix(NA, length(p11), 9)
colnames(n4l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l12 <- as.data.frame(n4l12[,1:3])
dn4l12


###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l13 <- matrix(NA, length(p11), 9)
colnames(n4l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l13 <- as.data.frame(n4l13[,1:3])
dn4l13


###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l14 <- matrix(NA, length(p11), 9)
colnames(n4l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l14 <- as.data.frame(n4l14[,1:3])
dn4l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n4l15 <- matrix(NA, length(p11), 9)
colnames(n4l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n4l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n4, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn4l15 <- as.data.frame(n4l15[,1:3])
dn4l15

###########################################################################
############################# n5 = 2500 #####################################
n5 <- 2500

######## sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l01 <- matrix(NA, length(p11), 9)
colnames(n5l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l01 <- as.data.frame(n5l01[,1:3])
dn5l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l02 <- matrix(NA, length(p11), 9)
colnames(n5l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l02 <- as.data.frame(n5l02[,1:3])
dn5l02

###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l03 <- matrix(NA, length(p11), 9)
colnames(n5l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l03 <- as.data.frame(n5l03[,1:3])
dn5l03

###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l04 <- matrix(NA, length(p11), 9)
colnames(n5l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l04 <- as.data.frame(n5l04[,1:3])
dn5l04

###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l05 <- matrix(NA, length(p11), 9)
colnames(n5l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l05 <- as.data.frame(n5l05[,1:3])
dn5l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l06 <- matrix(NA, length(p11), 9)
colnames(n5l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l06 <- as.data.frame(n5l06[,1:3])
dn5l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l07 <- matrix(NA, length(p11), 9)
colnames(n5l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l07 <- as.data.frame(n5l07[,1:3])
dn5l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l08 <- matrix(NA, length(p11), 9)
colnames(n5l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l08 <- as.data.frame(n5l08[,1:3])
dn5l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l09 <- matrix(NA, length(p11), 9)
colnames(n5l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l09 <- as.data.frame(n5l09[,1:3])
dn5l09


###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l10 <- matrix(NA, length(p11), 9)
colnames(n5l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l10 <- as.data.frame(n5l10[,1:3])
dn5l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l11 <- matrix(NA, length(p11), 9)
colnames(n5l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l11 <- as.data.frame(n5l11[,1:3])
dn5l11

###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l12 <- matrix(NA, length(p11), 9)
colnames(n5l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l12 <- as.data.frame(n5l12[,1:3])
dn5l12


###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l13 <- matrix(NA, length(p11), 9)
colnames(n5l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l13 <- as.data.frame(n5l13[,1:3])
dn5l13

###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l14 <- matrix(NA, length(p11), 9)
colnames(n5l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l14 <- as.data.frame(n5l14[,1:3])
dn5l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n5l15 <- matrix(NA, length(p11), 9)
colnames(n5l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n5l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n5, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn5l15 <- as.data.frame(n5l15[,1:3])
dn5l15

###########################################################################
############################# n6 = 3000 #####################################
n6 <- 3000

######## sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l01 <- matrix(NA, length(p11), 9)
colnames(n6l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l01 <- as.data.frame(n6l01[,1:3])
dn6l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l02 <- matrix(NA, length(p11), 9)
colnames(n6l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l02 <- as.data.frame(n6l02[,1:3])
dn6l02


###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l03 <- matrix(NA, length(p11), 9)
colnames(n6l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l03 <- as.data.frame(n6l03[,1:3])
dn6l03

###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l04 <- matrix(NA, length(p11), 9)
colnames(n6l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l04 <- as.data.frame(n6l04[,1:3])
dn6l04


###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l05 <- matrix(NA, length(p11), 9)
colnames(n6l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l05 <- as.data.frame(n6l05[,1:3])
dn6l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l06 <- matrix(NA, length(p11), 9)
colnames(n6l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l06 <- as.data.frame(n6l06[,1:3])
dn6l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l07 <- matrix(NA, length(p11), 9)
colnames(n6l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l07 <- as.data.frame(n6l07[,1:3])
dn6l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l08 <- matrix(NA, length(p11), 9)
colnames(n6l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l08 <- as.data.frame(n6l08[,1:3])
dn6l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l09 <- matrix(NA, length(p11), 9)
colnames(n6l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l09 <- as.data.frame(n6l09[,1:3])
dn6l09

###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l10 <- matrix(NA, length(p11), 9)
colnames(n6l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l10 <- as.data.frame(n6l10[,1:3])
dn6l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l11 <- matrix(NA, length(p11), 9)
colnames(n6l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l11 <- as.data.frame(n6l11[,1:3])
dn6l11

###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l12 <- matrix(NA, length(p11), 9)
colnames(n6l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l12 <- as.data.frame(n6l12[,1:3])
dn6l12


###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l13 <- matrix(NA, length(p11), 9)
colnames(n6l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l13 <- as.data.frame(n6l13[,1:3])
dn6l13

###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l14 <- matrix(NA, length(p11), 9)
colnames(n6l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l14 <- as.data.frame(n6l14[,1:3])
dn6l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n6l15 <- matrix(NA, length(p11), 9)
colnames(n6l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n6l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n6, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn6l15 <- as.data.frame(n6l15[,1:3])
dn6l15

###########################################################################
############################# n7 = 5000 #####################################
n7 <- 5000

######## sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l01 <- matrix(NA, length(p11), 9)
colnames(n7l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l01 <- as.data.frame(n7l01[,1:3])
dn7l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l02 <- matrix(NA, length(p11), 9)
colnames(n7l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l02 <- as.data.frame(n7l02[,1:3])
dn7l02

###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l03 <- matrix(NA, length(p11), 9)
colnames(n7l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l03 <- as.data.frame(n7l03[,1:3])
dn7l03

###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l04 <- matrix(NA, length(p11), 9)
colnames(n7l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l04 <- as.data.frame(n7l04[,1:3])
dn7l04

###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l05 <- matrix(NA, length(p11), 9)
colnames(n7l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l05 <- as.data.frame(n7l05[,1:3])
dn7l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l06 <- matrix(NA, length(p11), 9)
colnames(n7l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l06 <- as.data.frame(n7l06[,1:3])
dn7l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l07 <- matrix(NA, length(p11), 9)
colnames(n7l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l07 <- as.data.frame(n7l07[,1:3])
dn7l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l08 <- matrix(NA, length(p11), 9)
colnames(n7l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l08 <- as.data.frame(n7l08[,1:3])
dn7l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l09 <- matrix(NA, length(p11), 9)
colnames(n7l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l09 <- as.data.frame(n7l09[,1:3])
dn7l09

###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l10 <- matrix(NA, length(p11), 9)
colnames(n7l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l10 <- as.data.frame(n7l10[,1:3])
dn7l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l11 <- matrix(NA, length(p11), 9)
colnames(n7l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l11 <- as.data.frame(n7l11[,1:3])
dn7l11

###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l12 <- matrix(NA, length(p11), 9)
colnames(n7l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l12 <- as.data.frame(n7l12[,1:3])
dn7l12


###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l13 <- matrix(NA, length(p11), 9)
colnames(n7l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l13 <- as.data.frame(n7l13[,1:3])
dn7l13

###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l14 <- matrix(NA, length(p11), 9)
colnames(n7l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l14 <- as.data.frame(n7l14[,1:3])
dn7l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n7l15 <- matrix(NA, length(p11), 9)
colnames(n7l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n7l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n7, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn7l15 <- as.data.frame(n7l15[,1:3])
dn7l15

###########################################################################
############################# n8 = 10000 #####################################
n8 <- 10000

######## sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l01 <- matrix(NA, length(p11), 9)
colnames(n8l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l01 <- as.data.frame(n8l01[,1:3])
dn8l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l02 <- matrix(NA, length(p11), 9)
colnames(n8l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l02 <- as.data.frame(n8l02[,1:3])
dn8l02

###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l03 <- matrix(NA, length(p11), 9)
colnames(n8l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l03 <- as.data.frame(n8l03[,1:3])
dn8l03

###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l04 <- matrix(NA, length(p11), 9)
colnames(n8l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l04 <- as.data.frame(n8l04[,1:3])
dn8l04

###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l05 <- matrix(NA, length(p11), 9)
colnames(n8l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l05 <- as.data.frame(n8l05[,1:3])
dn8l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l06 <- matrix(NA, length(p11), 9)
colnames(n8l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l06 <- as.data.frame(n8l06[,1:3])
dn8l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l07 <- matrix(NA, length(p11), 9)
colnames(n8l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l07 <- as.data.frame(n8l07[,1:3])
dn8l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l08 <- matrix(NA, length(p11), 9)
colnames(n8l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l08 <- as.data.frame(n8l08[,1:3])
dn8l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l09 <- matrix(NA, length(p11), 9)
colnames(n8l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l09 <- as.data.frame(n8l09[,1:3])
dn8l09


###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l10 <- matrix(NA, length(p11), 9)
colnames(n8l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l10 <- as.data.frame(n8l10[,1:3])
dn8l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l11 <- matrix(NA, length(p11), 9)
colnames(n8l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l11 <- as.data.frame(n8l11[,1:3])
dn8l11

###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l12 <- matrix(NA, length(p11), 9)
colnames(n8l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l12 <- as.data.frame(n8l12[,1:3])
dn8l12


###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l13 <- matrix(NA, length(p11), 9)
colnames(n8l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l13 <- as.data.frame(n8l13[,1:3])
dn8l13


###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l14 <- matrix(NA, length(p11), 9)
colnames(n8l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l14 <- as.data.frame(n8l14[,1:3])
dn8l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n8l15 <- matrix(NA, length(p11), 9)
colnames(n8l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n8l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n8, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn8l15 <- as.data.frame(n8l15[,1:3])
dn8l15

###########################################################################
############################# n9 = 20000 #####################################
n9 <- 20000

######## sim 1:
p10 = 0.0001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l01 <- matrix(NA, length(p11), 9)
colnames(n9l01) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l01[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l01 <- as.data.frame(n9l01[,1:3])
dn9l01

###### sim 2:
p10 = 0.001
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l02 <- matrix(NA, length(p11), 9)
colnames(n9l02) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l02[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l02 <- as.data.frame(n9l02[,1:3])
dn9l02

###### sim 3:
p10 = 0.001
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l03 <- matrix(NA, length(p11), 9)
colnames(n9l03) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l03[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l03 <- as.data.frame(n9l03[,1:3])
dn9l03

###### sim 4:
p10 = 0.005
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l04 <- matrix(NA, length(p11), 9)
colnames(n9l04) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l04[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l04 <- as.data.frame(n9l04[,1:3])
dn9l04




###### sim 5: 
p10 = 0.005
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l05 <- matrix(NA, length(p11), 9)
colnames(n9l05) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l05[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l05 <- as.data.frame(n9l05[,1:3])
dn9l05

###### sim 6:
p10 = 0.005
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l06 <- matrix(NA, length(p11), 9)
colnames(n9l06) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l06[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l06 <- as.data.frame(n9l06[,1:3])
dn9l06

###### sim 7:
p10 = 0.05
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l07 <- matrix(NA, length(p11), 9)
colnames(n9l07) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l07[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l07 <- as.data.frame(n9l07[,1:3])
dn9l07

###### sim 8:
p10 = 0.05
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l08 <- matrix(NA, length(p11), 9)
colnames(n9l08) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l08[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l08 <- as.data.frame(n9l08[,1:3])
dn9l08

###### sim 9:
p10 = 0.05
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l09 <- matrix(NA, length(p11), 9)
colnames(n9l09) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l09[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l09 <- as.data.frame(n9l09[,1:3])
dn9l09

###### sim 10:
p10 = 0.05
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l10 <- matrix(NA, length(p11), 9)
colnames(n9l10) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l10[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}

dn9l10 <- as.data.frame(n9l10[,1:3])
dn9l10

###### sim 11:
p10 = 0.1
p01 = 0.0001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l11 <- matrix(NA, length(p11), 9)
colnames(n9l11) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l11[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l11 <- as.data.frame(n9l11[,1:3])
dn9l11


###### sim 12:
p10 = 0.1
p01 = 0.001

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l12 <- matrix(NA, length(p11), 9)
colnames(n9l12) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l12[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l12 <- as.data.frame(n9l12[,1:3])
dn9l12

###### sim 13:
p10 = 0.1
p01 = 0.005

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l13 <- matrix(NA, length(p11), 9)
colnames(n9l13) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l13[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l13 <- as.data.frame(n9l13[,1:3])
dn9l13


###### sim 14:
p10 = 0.1
p01 = 0.05

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l14 <- matrix(NA, length(p11), 9)
colnames(n9l14) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")

for(i in 1:length(p11)){
  n9l14[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l14 <- as.data.frame(n9l14[,1:3])
dn9l14

###### sim 15:
p10 = 0.1
p01 = 0.1

p11 <- getP11ToSim(dRange, p10, p01, d0)
d <- attr(p11, "dSorensen")

n9l15 <- matrix(NA, length(p11), 9)
colnames(n9l15) <- c("p11", "d", "Pr_rej", "E(sor_samp)", "bias(sor_samp)", 
                     "(true)se.sor_samp", "E(se.sor_samp)", "bias(se.sor_samp)", 
                     "coverage")


for(i in 1:length(p11)){
  n9l15[i, ] <- c(p11[i], npr_rej(nSim = nSim, n = n9, p11 = p11[i], p1. = p11[i] + p10, 
                                  p.1 = p11[i] + p01, d0 = d0, alpha = alpha, seed = 300187))
}
dn9l15 <- as.data.frame(n9l15[,1:3])
dn9l15

########################## END OF SIMULATIONS ######################

########################## SAVING RESULTS ##########################

nam_pij <- c("(0.0001, 0.0001)", "(0.001, 0.0001)", "(0.001, 0.001)", "(0.005, 0.0001)",
             "(0.005, 0.001)", "(0.005, 0.005)", "(0.05, 0.0001)", "(0.05, 0.001)",
             "(0.05, 0.005)", "(0.05, 0.05)", "(0.1, 0.0001)", "(0.1, 0.001)", 
             "(0.1, 0.005)", "(0.1, 0.05)", "(0.1, 0.1)")


p10_p01 <- as.factor(rep(rep(nam_pij, rep(21, 15)), 9))
n <- as.factor(rep(c("n1 = 150", "n2 = 500", "n3 = 1000", "n4 = 2000", 
                     "n5 = 2500", "n6 = 3000", "n7 = 5000", "n8 = 10000",
                     "n9 = 20000"), rep(21*15, 9)))
data <- rbind(dn1l01, dn1l02, dn1l03, dn1l04, dn1l05, dn1l06, dn1l07, dn1l08, dn1l09, dn1l10, dn1l11, dn1l12, dn1l13, dn1l14, dn1l15,
              dn2l01, dn2l02, dn2l03, dn2l04, dn2l05, dn2l06, dn2l07, dn2l08, dn2l09, dn2l10, dn2l11, dn2l12, dn2l13, dn2l14, dn2l15,
              dn3l01, dn3l02, dn3l03, dn3l04, dn3l05, dn3l06, dn3l07, dn3l08, dn3l09, dn3l10, dn3l11, dn3l12, dn3l13, dn3l14, dn3l15,
              dn4l01, dn4l02, dn4l03, dn4l04, dn4l05, dn4l06, dn4l07, dn4l08, dn4l09, dn4l10, dn4l11, dn4l12, dn4l13, dn4l14, dn4l15,
              dn5l01, dn5l02, dn5l03, dn5l04, dn5l05, dn5l06, dn5l07, dn5l08, dn5l09, dn5l10, dn5l11, dn5l12, dn5l13, dn5l14, dn5l15,
              dn6l01, dn6l02, dn6l03, dn6l04, dn6l05, dn6l06, dn6l07, dn6l08, dn6l09, dn6l10, dn6l11, dn6l12, dn6l13, dn6l14, dn6l15,
              dn7l01, dn7l02, dn7l03, dn7l04, dn7l05, dn7l06, dn7l07, dn7l08, dn7l09, dn7l10, dn7l11, dn7l12, dn7l13, dn7l14, dn7l15,
              dn8l01, dn8l02, dn8l03, dn8l04, dn8l05, dn8l06, dn8l07, dn8l08, dn8l09, dn8l10, dn8l11, dn8l12, dn8l13, dn8l14, dn8l15,
              dn9l01, dn9l02, dn9l03, dn9l04, dn9l05, dn9l06, dn9l07, dn9l08, dn9l09, dn9l10, dn9l11, dn9l12, dn9l13, dn9l14, dn9l15)
data_d0444 <- data.frame(data, p10_p01, n)
data_d0444

save(n1l01, n1l02, n1l03, n1l04, n1l05, n1l06, n1l07, n1l08, n1l09, n1l10, n1l11, n1l12, n1l13, n1l14, n1l15,
     n2l01, n2l02, n2l03, n2l04, n2l05, n2l06, n2l07, n2l08, n2l09, n2l10, n2l11, n2l12, n2l13, n2l14, n2l15,
     n3l01, n3l02, n3l03, n3l04, n3l05, n3l06, n3l07, n3l08, n3l09, n3l10, n3l11, n3l12, n3l13, n3l14, n3l15,
     n4l01, n4l02, n4l03, n4l04, n4l05, n4l06, n4l07, n4l08, n4l09, n4l10, n4l11, n4l12, n4l13, n4l14, n4l15,
     n5l01, n5l02, n5l03, n5l04, n5l05, n5l06, n5l07, n5l08, n5l09, n5l10, n5l11, n5l12, n5l13, n5l14, n5l15,
     n6l01, n6l02, n6l03, n6l04, n6l05, n6l06, n6l07, n6l08, n6l09, n6l10, n6l11, n6l12, n6l13, n6l14, n6l15,
     n7l01, n7l02, n7l03, n7l04, n7l05, n7l06, n7l07, n7l08, n7l09, n7l10, n7l11, n7l12, n7l13, n7l14, n7l15,
     n8l01, n8l02, n8l03, n8l04, n8l05, n8l06, n8l07, n8l08, n8l09, n8l10, n8l11, n8l12, n8l13, n8l14, n8l15,
     n9l01, n9l02, n9l03, n9l04, n9l05, n9l06, n9l07, n9l08, n9l09, n9l10, n9l11, n9l12, n9l13, n9l14, n9l15, 
     file = "AllResultsd0444.RData")


save(data_d0444, file = "data_d0444.Rdata")