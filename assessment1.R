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
0.5 ^2

