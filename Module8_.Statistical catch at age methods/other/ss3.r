packages<-c("devtools","shiny","shinyjs","ggplot2","reshape2","dplyr","tidyr",
            "Rcpp","rlist","viridis","shinyWidgets","shinyFiles","plyr","shinybusy",
            "truncnorm","ggpubr","flextable","officer","gridExtra","wesanderson","data.table",
            "adnuts","shinystan","shinyBS","gt","gtExtras","stringr","ggnewscale","msm",
            "EnvStats","tmvtnorm","future","parallel","parallelly","fs","tools","remotes","here","rvcheck")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

remotes::install_github("r4ss/r4ss")
remotes::install_github("chantelwetzel-noaa/HandyCode")
remotes::install_github("nwfsc-assess/nwfscDiag")

