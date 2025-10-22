caribouPopSim <- function(N0, numSteps, R_samp, S_samp, interannualVar, ...) {
  for (ts in 1:numSteps) {
    if (ts == 1) {
      out <- caribouPopGrowth(rep(N0, length(R_samp)),
                              numSteps = 1,
                              interannualVar = interannualVar,
                              R_bar = R_samp, S_bar = S_samp, ...
      )
      out$id <- seq(1:nrow(out))
      out$time <- ts
      outBit <- out
    } else {
      outBit <- caribouPopGrowth(outBit$N,
                                 numSteps = 1, interannualVar = interannualVar,
                                 R_bar = R_samp, S_bar = S_samp, ...
      )
      outBit$id <- seq(1, nrow(outBit))
      outBit$time <- ts
      out <- rbind(out, outBit)
    }
  }
  return(out)
}
