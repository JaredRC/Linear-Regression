#********** Assessment: Baseball as a Motivating Example ************
# Question 4
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Question 5
# Strikeouts by Pitchers

# Question 6
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Question 7
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Wins_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(Wins_per_game, E_per_game)) + 
  geom_point(alpha = 0.5)

# Question 8
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)

#******************** Assessment: Correlation *********************
# Question 4
N <- 50
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
mean(R)

# Question 5
sd(R)

# Question 6 - Independent x,y plot in a circle leaving 0 correlation

# Question 7
rho <- mean(scale(x)*scale(y))
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(R_per_game = R/G, AB_per_game = AB/G) %>%
  summarize(r = cor(R_per_game, AB_per_game)) %>% pull(r)

# Question 8
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Wins_per_game = W/G, E_per_game = E/G) %>%
  summarize(r = cor(Wins_per_game, E_per_game)) %>% pull(r)

# Question 9
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X2B_per_game = X2B/G, X3B_per_game = X3B/G) %>%
  summarize(r = cor(X2B_per_game, X3B_per_game)) %>% pull(r)

#************* Assessment: Stratification and Variance Explained, Part 1 ****
# Question 1
slope = r * s_y/s_x

# Question 2
mu_x <- 0
mu_y <- 0
s_x <- 1
s_y <- 1
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m * mu_x
galton_heights %>% 
  ggplot(aes(scale(father), scale(son))) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

# Question 3 - not many data points, large SEs, less stable than regession lines

# Question 4 - bivariate normal dist

# Question 5 - v shaped scatter

# Question 6
rho <- 0.5
rho^2

# Question 7
sd_father <- 2 # inches
sd_son <- 3 # inches
m1 <-  rho * sd_son / sd_father
m1*71 - m1*70 #If father is one inch taller, son?

#********* Assessment: Stratification and Variance Explained, Part 2 *******
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
# Question 8
mu_x <- mean(female_heights$mother)
s_x <- sd(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother,female_heights$daughter)

# Question 9
m <-  r * s_y / s_x
b <- mu_y - m * mu_x
(b + m*61) - (b + m*60)

# Question 10
r^2 # %age of outcome explained
s_y*sqrt(1-r*2) # Conditional standard deviation of the daughters' heights
s_y^2 # variance of the daughters' heights
s_y^2 * (1-r^2) # conditional variance of the daughters' heights
1-r*2

# Question 11
b + m*61
mu_y + r*((60 - mu_x)/s_x) * s_y # same outcome as above
