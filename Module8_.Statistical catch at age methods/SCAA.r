#install.packages("remotes")
#remotes::install_github("r4ss/r4ss")
library(r4ss)

#Read in SS3 output
#https://r4ss.github.io/r4ss/articles/r4ss-intro-vignette.html
replist <- SS_output(
  dir = "mako2019",  #Set equal to library all the files are in
  verbose = TRUE,
  printstats = TRUE
)
# plots the results to a webpage
SS_plots(replist)

head(replist$parameters)
rownames(replist$parameters)
replist$parameters["SR_LN(R0)",]
exp(5.4)*1000
## diagnostics

#remotes::install_github("nmfs-ost/ss3diags")

#library(ss3diags)