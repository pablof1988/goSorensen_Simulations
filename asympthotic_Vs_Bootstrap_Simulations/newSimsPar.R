library(goSorensen)
library(parallel)
source("simfuncsPar.R")

d0.toSim <- c(1/(1 + 2*1.25), 1/(1 + 2*(10/9)), 1/(1 + 1.25), 1/(1 + 10/9))
# 0.2857, 0.3103, 0.4444, 0.4737
n.toSim <- c(500, 1000, 2000, 2500, 3000, 5000, 10000, 20000)
p01p10.toSim <- matrix(c(0.005, 0.005, 
                         0.005, 0.01, 0.01, 0.01, 
                         0.005, 0.05, 0.01, 0.05, 0.05, 0.05, 
                         0.005, 0.1,  0.01, 0.1,  0.05, 0.1, 0.1, 0.1, 
                         0.005, 0.2,  0.01, 0.2,  0.05, 0.2, 0.1, 0.2, 0.2, 0.2), 
                       ncol = 2, byrow = TRUE)

# Extremely time consuming simulations. Better to divide them in smaller pieces, e.g.:

# dRange.d0.2857 <- c(0.15, 0.45)
dRange.d0.3103 <- c(0.15, 0.45)
# dRange.d0.4444 <- c(0.30, 0.60)
# dRange.d0.4737 <- c(0.30, 0.60)

# Usuari simulador:
scenarios.d0.3103_n6to8 <- buildScenarios(d0ToSim = 1/(1 + 2*(10/9)), nToSim = n.toSim[6:8], 
                                          p01p10ToSim = p01p10.toSim, 
                                          dRange = dRange.d0.3103,
                                          nPoints = 11)
iterScenarios(scenarios.d0.3103_n6to8, seed = 1951, boot.seed = 2053,
              resultsFolder = ".\\simResultsPar_d0.3103\\",
              simId = "sim0.3103_n6to8")

