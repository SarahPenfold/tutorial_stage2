library(ggplot2)
library(tidyverse)


response %>%
  group_by(genotype) %>% 
  summarise(m = mean(sens), 
            sd = sd(sens),
            n = length(sens),
            se = sd/sqrt(n))
  
# filter out rows

response %>% 
  filter(genotype =="A2")

# select columns
response %>% 
  select(-GSH)
  
#data input
response <- read.table("response.txt", skip = 6, header = TRUE)
str(response)
ggplot(data = response,
       aes(x = GSH, y = sens, col = genotype)) +
  geom_point() +
  xlim(0, 7) +
  ylim(0, 40) +
  geom_smooth(method = "lm", se =FALSE, fullrange = TRUE)

mod <- lm(data = response, sens ~ GSH * genotype)
summary(mod)

anova(mod)

mod_2  <- update(mod, .~. -GSH:genotype)

summary(mod_2)
