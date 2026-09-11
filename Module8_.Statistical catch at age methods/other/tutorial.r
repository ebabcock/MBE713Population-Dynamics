install.packages(c("copula", "triangle", "coda"))
# from FLR
remotes::install_github("flr/FLCore")
install.packages("FLa4a", repos=c(FLR="https://flr.r-universe.dev", CRAN="https://cloud.r-project.org"))
library(FLa4a)
library(XML)
library(reshape2)
library(latticeExtra)

### Run model

data(ple4)
data(ple4.indices)
fit <- sca(ple4, ple4.indices)
stk <- ple4 + fit
plot(stk)
res <- residuals(fit, ple4, ple4.indices)
plot(res)
bubbles(res)
plot(fit, ple4)
plot(fit, ple4.indices)
