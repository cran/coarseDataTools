## ----label=sourcing-----------------------------------------------------------
library(coarseDataTools)
data(simulated.outbreak.deaths)

## ----label=datapeek-----------------------------------------------------------
simulated.outbreak.deaths[15:20, ]

## ----label=preprocessing------------------------------------------------------
## set minimum number of observed cases for inclusion
min.cases <- 10

## observed cases
N.1 <- simulated.outbreak.deaths[1:60, "N"]
N.2 <- simulated.outbreak.deaths[61:120, "N"]

## subset to run analyis on times with greater than min.cases
first.t <- min(which(N.1 > min.cases & N.2 > min.cases))
last.t <- max(which(N.1 > min.cases & N.2 > min.cases))
idx.for.Estep <- first.t:last.t

## find and label the subset of times to be used for estimation routine
new.times <- seq_along(idx.for.Estep)
simulated.outbreak.deaths <- cbind(simulated.outbreak.deaths, new.times = NA)
simulated.outbreak.deaths[c(idx.for.Estep, idx.for.Estep + 60), "new.times"] <- rep(new.times, 2)

## ----label=datapeek2----------------------------------------------------------
simulated.outbreak.deaths[15:20, ]

## ----label=setValues----------------------------------------------------------
assumed.nu <- c(0, 0.3, 0.4, 0.3)
alpha.start <- rep(0, 22)

## ----label=runAnalysis, cache=TRUE, warning=FALSE-----------------------------
cfr.ests <- EMforCFR(
  assumed.nu = assumed.nu,
  alpha.start.values = alpha.start, full.data = simulated.outbreak.deaths, verb = FALSE,
  SEM.var = TRUE, max.iter = 100, tol = 1e-5
)

## ----label=estimationResults--------------------------------------------------
cfr.ests$naive.rel.cfr
cfr.ests$glm.rel.cfr
cfr.ests$EM.rel.cfr
cfr.ests$EM.rel.cfr.var.SEM

