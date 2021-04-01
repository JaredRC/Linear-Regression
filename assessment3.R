#********* Assessment: Correlation is Not Causation *********
library(dplyr)
library(ggplot2)
# Question 1 - pval = 0.05 is 5% chance on null hypo.  
0.05 * 1000000
# Question 2 - smallest p-value and most significant

# Question 3
cor(x, y, method = "spearman")

# Question 4 - stratify & plot data

# Question 5
library(dslabs)
?admissions

# Question 6 - Women apply to more selective(difficult) majors

# Question 7 - Stratify the major

#************* Assessment: Confounding (Verified Learners only) ************
library(dslabs)
data("research_funding_rates")
research_funding_rates
# Question 1
awards2by2 <- research_funding_rates %>% 
  summarize(men = c(sum(awards_men),sum(applications_men) - sum(awards_men)),
            women = c(sum(awards_women),sum(applications_women) - sum(awards_women)) )
rownames(awards2by2)<-c("Yes","No")

# Question 2
awards2by2$men[1] / (awards2by2$men[1] + awards2by2$men[2]) * 100
awards2by2$women[1] / (awards2by2$women[1] + awards2by2$women[2]) * 100

# Question 3
chisq_test <- awards2by2 %>% chisq.test()
tidy(chisq_test)

# Question 4
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat %>% ggplot(aes(success,discipline, col = gender, size = applications)) + 
  geom_point()

