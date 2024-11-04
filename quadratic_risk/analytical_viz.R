library(tidyverse)

data <- data %>%
  mutate(ce_sq = ce^2)

quad1 <- lm(util ~ ce + ce_sq, data = data)
summary(quad1)

data %>%
  ggplot()+
  aes(x=ce_sq, y=util)+
  geom_point()+
  geom_smooth(method = "lm",
              aes(color="linear"),
              se = FALSE)+
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color="quadratic"),
              se=FALSE)
