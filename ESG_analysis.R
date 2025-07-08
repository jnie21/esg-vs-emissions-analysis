library(dplyr)install.packages("tidyverse")
library(tidyverse)
library(readr)
install.packages("fuzzyjoin")  
library(fuzzyjoin)


#Overview of the data set 
esg_data <- read_csv("data/data.csv")
glimpse(esg_data) 
colnames(esg_data)
summary(esg_data)
emissions <- read_csv("data/emissions_medium_granularity.csv")
glimpse(emissions)
colnames(emissions)


# Clean and select relevant ESG columns
esg_clean <- esg_data %>%
  select(
    name, ticker, industry,
    environment_score, social_score, governance_score,
    total_score, total_grade
  ) %>%
  filter(!is.na(environment_score))
# Get the latest year of emissions data per company
emissions_latest <- emissions %>%
  group_by(parent_entity) %>%
  filter(year == max(year)) %>%
  summarise(total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE)) %>%
  ungroup()
# Merge ESG scores with emissions data
esg_merged <- esg_clean %>%
  left_join(emissions_latest, by = c("name" = "parent_entity"))
glimpse(esg_merged)
summary(esg_merged$total_emissions)

#do a fuzzy merge join 
fuzzy_merged_clean <- fuzzy_merged_clean %>%
  group_by(name) %>%
  slice(1) %>%
  ungroup()
fuzzy_merged_clean %>%
  count(name) %>%
  filter(n > 1) 
glimpse(fuzzy_merged)
summary(fuzzy_merged$total_emissions) 
# now have matched ESG scores to actual emissions data for a meaningful number of companies

# first insight plot:
library(ggplot2)
ggplot(fuzzy_merged_clean, aes(x = environment_score, y = total_emissions)) +
  geom_point(alpha = 0.6) +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Environmental Score vs. CO₂ Emissions (log scale)",
    x = "ESG Environmental Score",
    y = "Total Emissions (log MtCO₂e)"
  ) +
  theme_minimal()
# The fuzzy_merged contains 122 duplicate rows per company 
#Each ESG company was matched to many emission records — not just one.

#So the same emission value was replicated across multiple rows in the plot.
#The red regression line is almost flat, with a very slight downward tilt.
#This suggests there is no strong relationship between a company’s ESG Environmental Score and its actual carbon emissions — even when emissions are logged to reduce skew.

# what does this mean? 
#Investors use ESG scores to make portfolio decisions — if they’re not backed by real outcomes, that’s financial risk.
#Shows the need for stricter ESG regulation and data transparency.

cor.test(fuzzy_merged_clean$environment_score, log10(fuzzy_merged_clean$total_emissions))
model <- lm(log10(total_emissions) ~ environment_score, data = fuzzy_merged_clean)
summary(model)
#Slope = -0.0003:
#A 1-point increase in Environmental Score is associated with a 0.03% decrease in CO₂ emissions (in log scale).
#p-value = 0.0474:
#This is just under the 0.05 threshold, so the negative relationship is statistically significant, albeit weak.
#R² = 0.0054:
#ESG score explains only 0.5% of the variance in emissions. That means 95%+ of emissions differences are due to other factors.
#Although higher ESG environmental scores are weakly associated with lower emissions, the relationship is statistically significant yet substantively small. This suggests ESG scores may not be reliable indicators of actual environmental impact — pointing to a potential gap between perception and performance.

# Natural log regression
model_ln <- lm(log(total_emissions) ~ environment_score, data = fuzzy_merged_clean)
summary(model_ln)
# Log base 2 regression
model_log2 <- lm(log2(total_emissions) ~ environment_score, data = fuzzy_merged_clean)
summary(model_log2)

# Slope is negative: ESG environmental scores are associated with lower emissions.
# p-value ≈ 0.047: Statistically significant.
# R² ≈ 0.005: Substantively very weak.

summary(lm(log10(total_emissions) ~ social_score, data = fuzzy_merged_clean))
#Not statistically significant (p ≈ 0.091)
#Suggests little to no relationship between social scores and emissions.

summary(lm(log10(total_emissions) ~ governance_score, data = fuzzy_merged_clean))

library(ggplot2)
ggplot(fuzzy_merged_clean, aes(x = environment_score, y = log10(total_emissions))) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "loess", color = "red", se = FALSE, span = 0.75) +
  labs(
    title = "ESG Environmental Score vs. CO₂ Emissions",
    subtitle = "Log-transformed emissions with LOESS smoother",
    x = "Environmental Score",
    y = "Log₁₀ of CO₂ Emissions (MtCO₂e)"
  ) +
  theme_minimal(base_size = 14)
#The LOESS curve shows a slight downward trend, suggesting that companies with higher environmental scores tend to have marginally lower log-transformed CO₂ emissions. However, the wide dispersion of points at each score level implies substantial variation in real-world emissions even among high-scoring firms.

#correlatin analysis
cor.test(fuzzy_merged_clean$environment_score, 
         log(fuzzy_merged_clean$total_emissions), 
         method = "pearson")
#Correlation coefficient (r): -0.074
#p-value: 0.047
#95% CI: from -0.146 to -0.0008

par(mfrow = c(2, 2))
plot(model_ln)
