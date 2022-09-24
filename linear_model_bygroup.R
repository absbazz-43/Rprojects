
library(tidyverse)
head(Panel_Data_Reg)
w <- Panel_Data_Reg %>% 
  spread(key = Variable , value = Value)
View(w)

###### 1st way
head(w)
reg <- w %>% 
  group_by( Bank,`Bank Id`) %>% 
  do(model = lm(`Net Income(profit after tax)` ~ NPL + `NPL Ratio(%)` + `Total assets` + `Total deposits` + `Total equity` + `Total invesment` + `Total liabilities` + `Total loans and advances`, data = .))


reg$model
d <- data.frame(state=rep(c('NY', 'CA'), c(10, 10)),
                year=rep(1:10, 2),
                response=c(rnorm(10), rnorm(10)))
fitted_models = d %>% group_by(state) %>% do(model=lm(response ~ year, data = .))
fitted_models$model



######### second way
d %>% 
  split(.$state) %>% 
  map(~lm(response ~ year, data =.))


### 3rd way

d %>% 
  group_by(state) %>% 
  nest() %>% 
  mutate(model = map(data,~lm(response ~ year, data = .)), cof = map(model, "coefficients")) %>% 
  unnest(cof)

# 4th way

md <- d %>% 
  group_by(state) %>% 
  nest() %>% 
  mutate(model = map(data,~lm(response ~ year, data = .)))
md$model
