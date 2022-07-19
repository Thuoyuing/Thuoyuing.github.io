install.packages("rmarkdown")
tinytex::install_tinytex()
library(tidyverse)
head(mtcars)
summary(as_tibble(scale(head(select(mtcars,mpg,wt,disp),10),center = T, scale = T)))

mtcars %>% 
  select(mpg,wt,disp) %>% 
  head(10) %>% 
  scale(center = T, scale = T) %>% 
  as_tibble() %>% 
  summary()

mtcars2 <- mtcars
mtcars2[,"cyl"] <- as.factor(mtcars2[,"cyl"])
mtcar2_smry <- summary(mtcars2[,c('mpg','cyl','disp')])
mtcar2_smry

mtcars %>% 
  mutate(cyl = as.factor(cyl)) %>% 
  select(mpg,cyl,disp) %>% 
  summary() -> mtcars_smry
mtcars_smry

left_join()

a <- 2
b <- 4

FUN <- function(x,y){x^(y+1)}
a %>% FUN(b)
a %>% FUN(b,.)
