# see complete tutorial on : https://simonejdemyr.com/r-tutorials/statistics/tutorial8.html
data <- read.csv('http://www.mfilipski.com/files/ecls.csv')
library(tidyverse)
library(MatchIt)

glimpse(data)
# childid       <chr> "0001002C", "0001004C", "0001010C"
# catholic      <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
# race          <chr> "WHITE, NON-HISPANIC", "WHITE, NON-HISPANIC"
# race_white    <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
# race_black    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
# race_hispanic <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
# race_asian    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
# p5numpla      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
# p5hmage       <int> 47, 41, 43, 38, 47, 41, 31, 38, 26, 38
# p5hdage       <int> 45, 48, 55, 39, 57, 41, 32, 41, 28
# w3daded       <chr> "DOCTORATE OR PROFESSIONAL DEGREE",
# w3momed       <chr> "SOME COLLEGE", "GRADUATE/PROFESSIONAL SCHOO
# w3daded_hsb   <int> 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 
# w3momed_hsb   <int> 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 
# w3momscr      <dbl> 53.50, 34.95, 63.43, 53.50, 61.56, 38.18
# w3dadscr      <dbl> 77.50, 53.50, 53.50, 53.50, 77.50, 53.50
# w3inccat      <chr> "#50,001 TO #75,000", "#40,001 TO #50,000"
# w3income      <dbl> 62500.5, 45000.5, 62500.5, 87500.5
# w3povrty      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
# p5fstamp      <int> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1
# c5r2mtsc      <dbl> 60.043, 56.280, 55.272, 64.604, 75.721
# c5r2mtsc_std  <dbl> 0.98175332, 0.59437751, 0.49061062,
# w3income_1k   <dbl> 62.5005, 45.0005, 62.5005, 87.5005,

data %>%
  group_by(catholic) %>%
  summarise(n_students=n(),
            mean_math=mean(c5r2mtsc_std),
            std_error=sd(c5r2mtsc_std)/sqrt(n_students))

data %>%
  group_by(catholic) %>%
  select(race_white,p5hmage,w3income,p5numpla,w3momed_hsb) %>%
  summarise_all(funs(mean(., na.rm=T)))

with(data, t.test(race_white~catholic))
with(data, t.test(p5hmage~catholic))
with(data, t.test(w3income~catholic))
with(data, t.test(p5numpla~catholic))
with(data, t.test(w3momed_hsb~catholic))

m_ps <- glm(formula = catholic ~ race_white+p5hmage+w3income_1k+p5numpla+w3momed_hsb, data=data)
summary(m_ps)

prs_df <- data.frame(pr_score=predict(m_ps, type = "response"),
                     catholic=m_ps$model$catholic)

labs <- paste("Actual school type attended:",c("Catholic","Public"))
prs_df %>%
  mutate(catholic = ifelse(catholic==1,labs[1],labs[2])) %>%
  ggplot(aes(x=pr_score))+
  geom_histogram(color="white")+
  facet_wrap(~catholic)+
  xlab("Probability of going to catholic school")+
  theme_bw()

dta_clean <- 
  data %>%
  select(c5r2mtsc_std, catholic, race_white, p5hmage, w3income,w3income_1k, p5numpla, w3momed_hsb) %>%
  na.omit()

mod_match <- matchit(catholic ~ race_white+p5hmage+w3income_1k+p5numpla+w3momed_hsb,
                     method = "nearest",
                     data=dta_clean)
summary(mod_match)
plot(mod_match)
dta_m <- match.data(mod_match)

fn_bal <-
  function(data, variable)
  {
    data$variable <- data[, variable]
    if (variable == "w3income") data$variable <- data$variable/10^3
    data$catholic <- as.factor(data$catholic)
    support <- c(min(data$variable), max(data$variable))
    ggplot(data)+
      aes(x=distance, y=variable, color=catholic)+
      geom_point(alpha=0.1, size=1.3) +
      geom_smooth(method="loess", se=F)+
      xlab("Propensity Score")+
      ylab(variable)+
      theme_bw+
      ylim(support)
    }
