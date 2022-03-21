dSorensen2p11 <- function(d, p10, p01) {
  p11 <- 0.5 * (p10 + p01) * (1 - d) / d
  p11[p11 > 1] <- NA
  return(p11)
}



getP11ToSim <- function(dRange, p10, p01, d0, nPoints = 21) {
  if (dRange[1] < dRange[2]) {
    dmin <- dRange[1]
    dmax <- dRange[2]
  } else {
    dmin <- dRange[2]
    dmax <- dRange[1]
  }
  p11min <- dSorensen2p11(dmin, p10, p01)
  if (is.na(p11min)) {
    stop("Lowest dissimilarity to be simulated not compatible with p10 and p01")
  }
  p11max <- dSorensen2p11(dmax, p10, p01)
  if (is.na(p11max)) {
    stop("Largest dissimilarity to be simulated not compatible with p10 and p01")
  }
  # in fact, p11min > p11max, but they correspond to min and max dissimilarity
  
  if (!missing(d0)) {
    p11.0 <- dSorensen2p11(d0, p10, p01)
    if (is.na(p11.0)) {
      stop("Equivalence threshold dissimilarity d0 to be simulated not compatible with p10 and p01")
    }
    nPoints <- nPoints - 1
    dToSim <- seq(from = dmin, to = dmax, length.out = nPoints)
    id0 <- findInterval(d0, dToSim)
    if (id0 == 0) {
      dToSim <- c(d0, dToSim)
    } else if (id0 == length(dToSim)) {
      dToSim <- c(dToSim, d0)
    } else {
      dToSim <- c(dToSim[1:id0], d0, dToSim[(id0+1):length(dToSim)])
    }
    result <- dSorensen2p11(dToSim, p10, p01)
    attr(result, "d0") <- d0
  } else {
    dToSim <- seq(from = dmin, to = dmax, length.out = nPoints)
    result <- dSorensen2p11(dToSim, p10, p01)
    attr(result, "d0") <- NULL
  }
  if (any((result + p10 + p01) > 1)) {
    stop("One or more resulting p11 are not compatible with p10 or p01")
  }
  attr(result, "p10") <- p10
  attr(result, "p01") <- p01
  attr(result, "dSorensen") <- dToSim
  return(result)
}


nsor_dis <- function (ct, alpha = 0.05, z.alpha = qnorm(alpha, lower.tail = F)) {
  ct <- c(ct[4], ct[2], ct[3], ct[1])
  dis <- dSorensen(ct)
  se <- seSorensen(ct)
  du <- dis + z.alpha * se
  c(sor_dis = dis, du = du, se = se)
}

npr_rej <- function (nSim, n, p11, p1., p.1, d0, alpha = 0.05, z.alpha = qnorm(alpha, lower.tail = F), seed) {
  sor <- 1 - ((2 * p11)/(p1. + p.1))
  p <- c(p11, p1. - p11, p.1 - p11)
  if (!missing(seed)) {
    set.seed(seed)
  }
  nij <- rmultinom(n = nSim, size = n, prob = c(p, 1 - sum(p)))
  sor_samp <- apply(nij, 2, nsor_dis, alpha, z.alpha)
  rej <- sor_samp[2, ] < d0
  sor.mean <- sum(sor_samp[1, ])/nSim
  sor.mean <- mean(sor_samp[1, ])
  sor.se <- sd(sor_samp[1, ])
  se.mean <- mean(sor_samp[3, ])
  b.se <- se.mean - sor.se
  coverage <- sum(sor <= sor_samp[2, ])/nSim
  c(d = sor, Pr.Eq = sum(rej, na.rm = T)/n_val, 
    `E(sor.samp)` = sor.mean, `bias(sor.samp)` = sor.mean - sor, true.se = sor.se, 
    `E(se.sor)` = se.mean, `bias(se.sor)` = b.se, coverage = coverage)
}
