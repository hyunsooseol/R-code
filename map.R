library(tidyverse)
data <- read.csv("mix.csv")

library(tictoc)
##############
tic()

boot.sample <- function(df) {
  df[sample(nrow(df), replace = TRUE),,]
}

boot <- map(1:1000, ~ boot.sample(data)) 

infit <- function(data){
  
  res1 <-
    mixRasch::mixRasch(
      data = data,
      steps = 1,
      model = "RSM",
      n.c = 1
    )
 
 infit<- res1$item.par$in.out[, 1]
 
}
boot.infit<- map_dfc(boot, ~ infit(data=.x))# data=.x 반드시 사용 
infit.low <- apply(boot.infit, 1, stats::quantile,probs=.025)
infit.high <- apply(boot.infit, 1, stats::quantile,probs=.975)


toc()


tic()
####################################################

boot.infit <- function(data, indices) {
  d = data[indices,]
  
  # estimate Rasch model--------
  res1 <-
    mixRasch::mixRasch(
      data = d,
      steps = 1,
      model = "RSM",
      n.c = 1
    )
  
  
  # item infit--------
  infit <- res1$item.par$in.out[, 1]
  
  return(infit)
  
}

boot.in <-
  boot::boot(data = data,
             statistic = boot.infit,
             R = 1000)

infit <- boot.in$t

infitlow = NA

for (i in 1:ncol(data)) {
  infitlow[i] <- stats::quantile(infit[, i], .025)
  
  
}

#infit lower--

infitlow <- infitlow


#infit high-

infithigh = NA

for (i in 1:ncol(data)) {
  infithigh[i] <- stats::quantile(infit[, i], .975)
  
  
}

infithigh <- infithigh
################################
toc()
















###########참고용###############
for (i in 1:ncol(data)) {
  infitlow[i] <- stats::quantile(infit[, i], .025)
  
  
}
attach(mtcars)
map(bootstraps,~lm(mpg~disp, data = .x)) %>% map(summary)

###############
bootstraps %>%
  map(~ lm(mpg ~ disp, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")
############################












