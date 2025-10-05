library(tidyverse)
library(janitor)
data_sport <- read_csv(file = "cleaned_fifa_eda_stats.csv")
data_sport <- data_sport |> clean_names()
glimpse(data_sport)

install.packages("rmarkdown")

data_sport |> 
  group_by(position) |> 
  summarise(
    n = n(),
    m_wage = mean(wage, na.rm = TRUE),
    sd_wage = sd(wage, na.rm = TRUE),
    m_overall = mean(overall, na.rm = TRUE),
    sd_overall = sd(overall, na.rm = TRUE),
    m_potential = mean(potential, na.rm = TRUE),
    sd_potential = sd(potential, na.rm = TRUE),
    m_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE)
  )


#Anova position - wage

ggplot(data_sport, aes(x = position, y = wage, fill = position)) +
  geom_violin(scale = "width") +  
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(x = "POSITION", y = "WAGE") +
  theme_bw() +
  theme(legend.position = "none")


#####
repu_position <- table(data_sport$international_reputation, data_sport$position)
repu_position_test <- chisq.test(repu_position)
repu_position_test$expected

cat("Observed p-value:", repu_position_test$p.value, "\n")


# permutations chisquare
n_permutations <- 1000
permutation_stats <- numeric(n_permutations)

for (i in 1:n_permutations) {
  permuted_repu <- sample(data_sport$international_reputation)
  permuted_table <- table(data_sport$position, permuted_repu)
  permuted_result <- chisq.test(permuted_table)
  permutation_stats[i] <- permuted_result$statistic
}

observed_stat <- repu_position_test$statistic
# Calculate the p-value as the proportion of permutation statistics greater than or equal to the observed statistic
p_value_permutation <- mean(permutation_stats >= observed_stat)

permutation_stats

cat("Permutation p-value:", p_value_permutation, "\n")






#####
# chisquare position vs attacking_work_rate
repu_attack <- table(data_sport$attacking_work_rate, data_sport$position)
repu_attack_test <- chisq.test(repu_attack)
repu_attack_test$expected
cat("Observed p-value:", repu_attack_test$p.value, "\n")

# chisquare position vs deffensive_work_rate
repu_defensive <- table(data_sport$deffensive_work_rate, data_sport$position)
repu_defensive_test <- chisq.test(repu_defensive)
repu_defensive_test$expected
cat("Observed p-value:", repu_defensive_test$p.value, "\n")


# chisquare position vs body_type
repu_body <- table(data_sport$body_type, data_sport$position)
repu_body_test <- chisq.test(repu_body)
repu_body_test$expected
cat("Observed p-value:", repu_body_test$p.value, "\n")

#####
# New variable
data_sport$year_old <- ifelse(data_sport$age >= 30, "old", "young")

perm_fun <- function(x, nA, nB, R) {
  n <- nA + nB
  mean_diff <- numeric(R)
  for (i in 1:R){
    idx_a <- sample(x = 1:n, size = nA)
    idx_b <- setdiff(x = 1:n, y = idx_a)
    mean_diff[i] <- mean(x[idx_a]) - mean(x[idx_b])
  }
  return(mean_diff)
}

set.seed(19)
data_sport |> 
  group_by(year_old) |> 
  summarise(
    n = n(),
    m_wage = mean(wage, na.rm = TRUE),
    sd_wage = sd(wage, na.rm = TRUE),
    m_overall = mean(overall, na.rm = TRUE),
    sd_overall = sd(overall, na.rm = TRUE),
    m_potential = mean(potential, na.rm = TRUE),
    sd_potential = sd(potential, na.rm = TRUE),
    m_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE)
  )

# overall
diff_mean_perm <- perm_fun(data_sport$overall, nA = 3352, nB = 13291, R = 1000)
mean_a <- mean(data_sport$overall[data_sport$year_old == 'old'])
mean_b <- mean(data_sport$overall[data_sport$year_old == 'young'])
p_value_overall <- mean(abs(diff_mean_perm) < abs(mean_a - mean_b))

#  potential
diff_mean_potential <- perm_fun(data_sport$potential, nA = 3352, nB = 13291, R = 1000)
mean_a_potential <- mean(data_sport$potential[data_sport$year_old == 'old'])
mean_b_potential <- mean(data_sport$potential[data_sport$year_old == 'young'])
p_value_potential <- mean(abs(diff_mean_potential) >= abs(mean_a_potential - mean_b_potential))

#  wage
diff_mean_wage <- perm_fun(data_sport$wage, nA = 3352, nB = 13291, R = 1000)
mean_a_wage <- mean(data_sport$wage[data_sport$year_old == 'old'])
mean_b_wage <- mean(data_sport$wage[data_sport$year_old == 'young'])
p_value_wage <- mean(abs(diff_mean_wage) >= abs(mean_a_wage - mean_b_wage))

#  value
diff_mean_value <- perm_fun(data_sport$value, nA = 3352, nB = 13291, R = 1000)
mean_a_value <- mean(data_sport$value[data_sport$year_old == 'old'])
mean_b_value <- mean(data_sport$value[data_sport$year_old == 'young'])
p_value_value <- mean(abs(diff_mean_value) >= abs(mean_a_value - mean_b_value))


# p-value
cat("P-value for overall:", p_value_potential)
cat("P-value for potential:", p_value_potential)
cat("P-value for wage:", p_value_wage)
cat("P-value for value:", p_value_value)

# Ho: value  H1: Wage, overall, potential
#####
# List of features for A/B testing
features <- c('crossing', 'finishing', 'heading_accuracy', 'short_passing', 'volleys',
              'dribbling', 'curve', 'fk_accuracy', 'long_passing', 'ball_control',
              'acceleration', 'sprint_speed', 'agility', 'reactions', 'balance',
              'shot_power', 'jumping', 'stamina', 'strength', 'long_shots',
              'aggression', 'interceptions', 'positioning', 'vision', 'penalties',
              'composure', 'marking', 'standing_tackle', 'sliding_tackle',
              'gk_diving', 'gk_handling', 'gk_kicking', 'gk_positioning', 'gk_reflexes')


# Initialize result storage
results <- data.frame(Feature = features, p_value = NA, significant = NA)

# A/B testing for each feature


perm_results <- data.frame(Feature = features, p_value = NA, conclusion = NA)

perm_results

for (feature in features) {
  # Chia ra
  old <- data_sport[data_sport$year_old == 'old', feature]
  young <- data_sport[data_sport$year_old == 'young', feature]
  # Dem phan tu
  nnA=nrow(old)
  nnB=nrow(young)
  
  diff_mean_perm <- perm_fun(data_sport[[feature]], nA = nnA, nB = nnB, R = 1000)
  p_value <- mean(abs(diff_mean_perm) >= abs(mean(old[[feature]]) - mean(young[[feature]])))
  perm_results[perm_results$Feature == feature, 'p_value'] <- p_value
  perm_results[perm_results$Feature == feature, 'conclusion'] <- ifelse(p_value < 0.05, "H1", "H0")
}


perm_results

#####
### Tao bien moi
data_continents <- read_csv(file = "Countries by continents.csv")
data_continents <- data_continents |> clean_names()
colnames(data_continents)[colnames(data_continents) == 'country'] <- 'nationality'
data_sport <- merge(data_sport, data_continents[, c('nationality', 'continent')], by = 'nationality', all.x = TRUE)
data_sport$continent
print(data_sport[, c('nationality', 'continent')])


## So sanh giua asia va south america
samerica_asia <- subset(data_sport, continent %in% c("South America", "Asia"))
samerica_asia <- samerica_asia |> clean_names()

samerica_asia

features <- c('crossing', 'finishing', 'heading_accuracy', 'short_passing', 'volleys',
              'dribbling', 'curve', 'fk_accuracy', 'long_passing', 'ball_control',
              'acceleration', 'sprint_speed', 'agility', 'reactions', 'balance',
              'shot_power', 'jumping', 'stamina', 'strength', 'long_shots',
              'aggression', 'interceptions', 'positioning', 'vision', 'penalties',
              'composure', 'marking', 'standing_tackle', 'sliding_tackle',
              'gk_diving', 'gk_handling', 'gk_kicking', 'gk_positioning', 'gk_reflexes')


# Initialize result storage
perm_results <- data.frame(Feature = features, p_value = NA, conclusion = NA)

perm_results


for (feature in features) {
  # Chia ra
  samerica <- samerica_asia[samerica_asia$continent == 'South America', feature]
  asia <- samerica_asia[samerica_asia$continent == 'Asia', feature]
  # Dem phan tu
  nnA=length(samerica)
  nnB=length(asia)
  
  diff_mean_perm <- perm_fun(samerica_asia[[feature]], nA = nnA, nB = nnB, R = 1000)
  p_value <- mean(diff_mean_perm >= mean(samerica) - mean(asia))
  perm_results[perm_results$Feature == feature, 'p_value'] <- p_value
  perm_results[perm_results$Feature == feature, 'conclusion'] <- ifelse(p_value < 0.05, "H1", "H0")
}

perm_results
