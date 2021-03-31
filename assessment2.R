#****************** Assessment: Introduction to Linear Models ********
# Question 1 - reduced because removed confounding

# Question 2
library(HistData)
data("GaltonFamilies")
fit <- lm(son ~ father, data = galton_heights)
summary(fit)

# Question 3
galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))
lm(son ~ father_centered, data = galton_heights)

# Question 4 - intercept becomes B0 + Bwhateverfixed

# Question 5 - 𝜖𝑖  are independent from each other, have expected value 0 and the 
# standard deviation 𝜎 which does not depend on i

#************ Assessment: Least Squares Estimates, part 1 ***************
# Question 1
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# Question 2 - Minimize

# Question 3
library(Lahman)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, BB_per_game = BB/G, R_per_game = R/G) %>%
  select(yearID,name,HR_per_game, BB_per_game, R_per_game)
lm(R_per_game ~ (BB_per_game + HR_per_game), data = dat)

# Question 4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# Question 5
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

predictions <- predict(model) # last option

#********** Assessment: Least Squares Estimates, part 2 **************
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     he
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
# Question 7
fit <- lm(mother ~ daughter,female_heights)

# Question 8
b0 <- fit$coefficients[1]
b1 <- fit$coefficients[2]
b0 + b1*female_heights$daughter[1]
# **********
library(Lahman)
# Question 9
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb),n = n())
sum(bat99_01$mean_singles > 0.2)
sum(bat99_01$mean_bb > 0.2)

# Question 10
r_player <- inner_join(bat_02,bat99_01, by = "playerID") %>% 
  summarize(r_singles = cor(singles,mean_singles), r_bb = cor(bb,mean_bb))

# Question 11
inner_join(bat_02,bat99_01, by = "playerID") %>% 
  ggplot(aes(mean_singles,singles)) + geom_point() +
  geom_smooth(method = "lm")
inner_join(bat_02,bat99_01, by = "playerID") %>% 
  ggplot(aes(mean_bb,bb)) + geom_point() +
  geom_smooth(method = "lm")

# Question 12
inner_join(bat_02,bat99_01, by = "playerID") %>%
  lm(singles ~ mean_singles,data = .)
inner_join(bat_02,bat99_01, by = "playerID") %>%
  lm(bb ~ mean_bb,data = .)

#********** Assessment: Tibbles, do, and broom, part 1 *************
# Question 5
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}
dat %>% group_by(HR) %>% 
  do(get_slope(.))

# Question 6
tidy(fit)

# Question 7
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)
dat %>% 
  group_by(lgID) %>% do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

#************ Assessment: Tibbles, do, and broom, part 2 **************
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

# Question 8
galton %>% group_by(pair) %>% summarize(n = n())

# Question 9
galton %>% group_by(pair) %>% 
  summarize(r = cor(childHeight,parentHeight))

# Question 10a
galton %>% group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))
0.381 * 68 - 0.381 * 67

# Question 10b
fit <- galton %>% lm(childHeight ~ parentHeight, data = .)
galton %>% group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < 0.05) %>%
  mutate(diff = conf.high - conf.low) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
motherInterv <- 0.284 + 0.309
fatherInterv <- 0.236 + 0.276
daughterInt <- 0.236 + 0.284
sonInt <- 0.276 + 0.309

#************ 2.4: Regression and Baseball **************
#******** Assessment: Regression and Baseball, part 1 *********
# Question 1
lm(R ~ BB + singles + doubles + triples + HR, data = .)

# Question 2 - the number of plate appearances per team per game, 
# averaged across all teams

# Question 3 - BB + X1B + X2B + X3B + HR
est<- coefs$estimate
est[1] + est[2]*2 + est[3]*4 + est[4]*1 + est[5]*0 + est[6]*1 #Team A
est[1] + est[2]*1 + est[3]*6 + est[4]*2 + est[5]*1 + est[6]*0 #Team B
2 + 4 + 1 + 0 + 1
1 + 6 + 2 + 1 + 0

# Question 4 - Home Runs

# Question 5 - Regression to the mean

# Question 6 - measurement error
library(ggplot2)
library(broom)

# Question 7 error is random, independent, same distribution / i

# Question 8 - Could not see the ball.

#********** Assessment: Regression and baseball, part 2 ************
fit <- Teams %>% filter(yearID == 1971) %>%
  do(tidy(lm(R ~ BB + HR, data = .),conf.int = TRUE))
# Question 9a
fit
# Question 9b
fit %>% filter(p.value < 0.05)

# Question 10
fit <- Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .),conf.int = TRUE))
fit %>% filter(term == "BB") %>%
  ggplot(aes(yearID,estimate)) +
  geom_point() + 
  geom_smooth(method = "lm")

# Question 11
fit %>% filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>% tidy()

#***** Assessment: Linear Models (Verified Learners only) *********
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)
# Question 1a
Teams_small %>% mutate(Rpg = R/G) %>%
  lm(avg_attendance ~ Rpg, data = .)
Teams_small %>% mutate(HRpg = HR/G) %>%
  lm(avg_attendance ~ HRpg, data = .)

# Question 1b
fit <- Teams_small %>%
  lm(avg_attendance ~ W, data = .)

# Question 1c
fit <- Teams_small %>% group_by(yearID) %>%
  lm(avg_attendance ~ yearID, data = .) %>% .$coef

# Question 2
Teams_small %>% mutate(Rpg = R/G, HRpg = HR/G) %>%
  summarize(cor(W,Rpg), cor(W,HRpg))

# Question 3a
fit <- Teams_small %>% 
  mutate(W_strata = round(W/10),Rpg = R/G, HRpg = HR/G) %>%
  select(W_strata,Rpg,HRpg,avg_attendance) 
fit %>% 
  group_by(W_strata) %>% filter(W_strata %in% 5:10) %>% 
  summarize(n=n(), Rtot = sum(Rpg),
            HRtot = sum(HRpg),avg_att = sum(avg_attendance))
  
# Question 3b
fit %>% group_by(W_strata) %>% filter(W_strata %in% 5:10) %>%
  summarize(mean(Rpg),mean(HRpg),
            mean(avg_attendance),
            slope = cor(avg_attendance,HRpg)*sd(HRpg)/sd(avg_attendance) )

# Question 3c
fit %>% 
  ggplot(aes(avg_attendance,HRpg)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ W_strata)
# Question 4
fit <- Teams_small %>%
  mutate(R_pg = R/G, HR_pg = HR/G) %>%
  lm(avg_attendance ~ R_pg + HR_pg + W + yearID, data = .)
tidy(fit)

# Question 5
data1 <- data.frame(R_pg = 5,HR_pg = 1.2,W = 80,yearID = 2002) #1960
data1 %>% predict(fit, newdata = .)

# Question 6
Teams2002 <- Teams %>% 
  filter(yearID == 2002) %>% 
  mutate(R_pg = R/G, HR_pg = HR/G, avg_attendance = attendance/G) %>%
  mutate(A_hat = predict(fit, newdata = .)) %>%
  summarize(c = cor(avg_attendance,A_hat))

