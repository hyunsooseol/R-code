
library(tidyverse)
data <- read.csv("mix.csv")

##############

boot.sample <- function(df) {
  df[sample(nrow(df), replace = TRUE),,drop=FALSE]
}

boot <- map(1:20, ~ boot.sample(data)) 

infit <- function(data){
  
  res1 <-
    mixRasch::mixRasch(
      data = data,
      steps = 1,
      model = "RSM",
      n.c = 1
    )
  
  # item infit--------
 infit<- res1$item.par$in.out[, 1]
 return(infit)
   
}

boot.infit<- map(boot, ~ infit(data=.x))# data=.x 반드시 사용 
boot.infit
# boot.infit2 <- unlist(boot.infit)# 벡터로묶을때 사용

infitd<- map(boot.infit,~as.data.frame(.)) # data frame 만들때
infitd

map(infitd, ~stats::quantile(data=.x))





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












