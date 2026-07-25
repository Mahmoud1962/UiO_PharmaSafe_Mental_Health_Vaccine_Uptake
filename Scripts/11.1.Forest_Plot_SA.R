################################################################################
# Script: 11.1.Forest_Plot_SA.R
#
# Purpose:
#   Make forest plots demonstrating the resulting weighted prevalnce ratios
#   and their confidence intervals. This is done for the sensitivity analysis
#   where the exposure to mental health conditions is assessed during all
#   available lookback time.
################################################################################
library(tibble)
library(ggplot2)
library(dplyr)

# Assuming you have your vectors named like this:
# hr_group1_season1, hr_group1_season2, etc.
# lci_group1_season1, etc.
# uci_group1_season1, etc.
old_flu_all_RR <- c(1,1.03,1.02,1.03,1.03,1.,1.06,1.02,1.05)
old_flu_all_UCI <- c(1,1.04,1.03,1.04,1.04,1.02,1.8,1.04,1.11)
old_flu_all_LCI <- c(1,1.03,1.02,1.03,1.02,0.98,1.04,0.96,1)

old_flu_season1_RR <- c(1,1.10,1.05,1.1,1.15,0.92,1.28,1.08,1.19)
old_flu_season1_LCI <- c(1,1.08,1,1.07,1.09,0.81,1.15,0.96,0.86)
old_flu_season1_UCI <- c(1,1.13,1.1,1.13,1.20,1.04,1.42,1.22,1.65)

old_flu_season2_RR <- c(1,1.09,1.07,1.08,1.1,0.94,1.21,1.07,1.04)
old_flu_season2_LCI <- c(1,1.07,1.03,1.06,1.06,0.85,1.1,1.01,0.78)
old_flu_season2_UCI <- c(1,1.11,1.11,1.1,1.15,1.04,1.33,1.22,1.39)

old_flu_season3_RR <- c(1,1.06,1.04,1.06,1.07,0.91,1.24,1.09,1.15)
old_flu_season3_LCI <- c(1,1.05,1,1.04,1.03,0.83,1.15,1.01,0.93)
old_flu_season3_UCI <- c(1,1.08,1.07,1.08,1.11,0.99,1.33,1.18,1.43)

preg_covid_risk_all_RR <- c(1,1.00,1.04,0.96,0.91,NA,0.99,1.04,1.17)
preg_covid_risk_all_LCI <- c(1,0.97,0.98,0.9,0.8,NA,0.87,0.84,1)
preg_covid_risk_all_UCI <- c(1,1.05,1.1,1.03,1.03,NA,1.14,1.29,1.37)

preg_covid_norisk_all_RR <- c(1,0.96,0.96,1.01,0.99,0.97,1.01,1.04,0.96)
preg_covid_norisk_all_LCI <- c(1,0.94,0.94,1.00,0.97,0.90,0.98,0.99,0.93)
preg_covid_norisk_all_UCI <- c(1,0.97,0.97,1.03,1.02,1.04,1.04,1.1,0.99)

preg_flu_risk_all_RR <- c(1,1.03,1.05,0.97,1.14,0.85,0.96,0.92,1.09)
preg_flu_risk_all_LCI <- c(1,1.00,1.00,0.93,1.03,0.81,0.88,0.80,0.93)
preg_flu_risk_all_UCI <- c(1,1.10,1.09,1.01,1.25,0.89,1.05,1.06,1.28)

preg_flu_risk_season1_RR <- c(1,1.30,1.23,0.84,2.59,0,1,0,1.08)
preg_flu_risk_season1_LCI <- c(1,0.93,0.82,0.47,1.47,0,0.42,0,0.21)
preg_flu_risk_season1_UCI <- c(1,1.80,1.85,1.50,4.58,0,2.41,0,5.56)

preg_flu_risk_season2_RR <- c(1,1.25,1.38,0.83,1.79,0,0.52,1.22,2.54)
preg_flu_risk_season2_LCI <- c(1,0.90,0.91,0.50,0.9,0,0.16,0.28,0.97)
preg_flu_risk_season2_UCI <- c(1,1.75,2.10,1.38,3.56,0,1.71,5.27,6.67)

preg_flu_risk_season3_RR <- c(1,0.97,1.36,0.41,1.71,0,0.95,0,0.85)
preg_flu_risk_season3_LCI <- c(1,0.64,0.81,0.17,0.74,0,0.31,0,0.12)
preg_flu_risk_season3_UCI <- c(1,1.47,2.27,0.98,3.94,0,2.91,0,6.12)

preg_flu_norisk_all_RR <- c(1,1.01,1.01,1.00,1.01,0.97,1.00,1.03,0.98)
preg_flu_norisk_all_LCI <- c(1,1.00,1.00,1.00,1.00,0.92,0.99,1.01,0.96)
preg_flu_norisk_all_UCI <- c(1,1.01,1.02,1.01,1.02,1.01,1.01,1.06,1.00)

preg_flu_norisk_season1_RR <- c(1,1.01,1.01,0.98,1.1,0.86,1.07,1.22,0.68)
preg_flu_norisk_season1_LCI <- c(1,0.94,0.91,0.89,0.92,0.41,0.87,0.94,0.45)
preg_flu_norisk_season1_UCI <- c(1,1.09,1.13,1.09,1.31,1.83,1.32,1.59,1.05)

preg_flu_norisk_season2_RR <- c(1,1.07,1.09,1.08,0.97,0.81,0.87,1.16,0.99)
preg_flu_norisk_season2_LCI <- c(1,1.0,0.99,0.99,0.83,0.36,0.71,0.90,0.70)
preg_flu_norisk_season2_UCI <- c(1,1.13,1.19,1.18,1.14,1.14,1.06,1.48,1.38)

preg_flu_norisk_season3_RR <- c(1,1.12,1.14,1.07,1.14,0.6,1.05,1.30,0.85)
preg_flu_norisk_season3_LCI <- c(1,1.06,1.04,0.96,0.98,0.30,0.87,1.02,0.61)
preg_flu_norisk_season3_UCI <- c(1,1.19,1.24,1.16,1.32,1.21,1.26,1.67,1.18)

old_covid_Primary_RR <- c(1,1.11,1.11,1.11,1.09,1.08,1.17,1.09,1.07)
old_covid_Primary_LCI <- c(1,1.11,1.10,1.10,1.08,1.06,1.15,1.08,1.02)
old_covid_Primary_UCI <- c(1,1.11,1.12,1.11,1.10,1.10,1.19,1.111,1.12)

old_covid_booster1_RR <- c(1,1.09,1.08,1.08,1.07,1.05,1.16,1.09,1.05)
old_covid_booster1_LCI <- c(1,1.08,1.07,1.08,1.05,1.02,1.13,1.06,0.98)
old_covid_booster1_UCI <- c(1,1.09,1.10,1.09,1.08,1.07,1.19,1.11,1.11)

old_covid_booster2_RR <- c(1,1.03,1.03,1.04,1.00,0.98,1.11,1.05,1.02)
old_covid_booster2_LCI <- c(1,1.02,1.00,1.03,0.99,0.94,1.07,1.01,0.93)
old_covid_booster2_UCI <- c(1,1.04,1.04,1.05,1.03,1.02,1.15,1.09,1.14)

# Create vectors for grouping variables
group <- c(rep("Older Adulthood - Influenza vaccine", 36), 
           rep("Pregnancy (with risk factors) - Influenza vaccine", 36),
           rep("Pregnancy (without risk factors) - Influenza vaccine", 36), 
           rep("Older Adulthood - COVID-19 vaccine", 27),
           rep("Pregnancy (with risk factors) - COVID-19 vaccine", 9), 
           rep("Pregnancy (without risk factors) - COVID-19 vaccine", 9))
season <- rep(c(rep(c("2016/2019", "2016/2017","2017/2018","2018/2019"), each = 9),   # For Group1
                rep(c("2016/2019", "2016/2017","2017/2018","2018/2019"), each = 9),   # For Group2
                rep(c("2016/2019", "2016/2017","2017/2018","2018/2019"), each = 9),   # For Group3
                rep(c("Primary vaccination"), each = 9),
                rep(c("First Booster Dose"), each = 9),
                rep(c("Second Booster Dose"), each = 9),
                rep("2021/2023", 18)))
condition <- c(rep(c("No Mental Health Conditions","Any Mental Health Condition", "Psychiatric Comorbidity",
                     "Depression", "Anxiety", "Bipolar", "PTSD", "OCD", "ADHD"),17))
# or whatever your condition is

# Combine all HR values into one vector
estimate <- c(old_flu_all_RR, old_flu_season1_RR, old_flu_season2_RR, 
              old_flu_season3_RR, preg_flu_risk_all_RR, 
              preg_flu_risk_season1_RR, preg_flu_risk_season2_RR,
              preg_flu_risk_season3_RR, preg_flu_norisk_all_RR,
              preg_flu_norisk_season1_RR, preg_flu_norisk_season2_RR,
              preg_flu_norisk_season3_RR,old_covid_Primary_RR, old_covid_booster1_RR,
              old_covid_booster2_RR, preg_covid_risk_all_RR,
              preg_covid_norisk_all_RR)

# Similarly for lower and upper
lower <- c(old_flu_all_LCI, old_flu_season1_LCI, old_flu_season2_LCI, 
           old_flu_season3_LCI, preg_flu_risk_all_LCI, 
           preg_flu_risk_season1_LCI, preg_flu_risk_season2_LCI,
           preg_flu_risk_season3_LCI, preg_flu_norisk_all_LCI,
           preg_flu_norisk_season1_LCI, preg_flu_norisk_season2_LCI,
           preg_flu_norisk_season3_LCI,old_covid_Primary_LCI, old_covid_booster1_LCI,
           old_covid_booster2_LCI, preg_covid_risk_all_LCI,
           preg_covid_norisk_all_LCI)

upper <- c(old_flu_all_UCI, old_flu_season1_UCI, old_flu_season2_UCI, 
           old_flu_season3_UCI, preg_flu_risk_all_UCI, 
           preg_flu_risk_season1_UCI, preg_flu_risk_season2_UCI,
           preg_flu_risk_season3_UCI, preg_flu_norisk_all_UCI,
           preg_flu_norisk_season1_UCI, preg_flu_norisk_season2_UCI,
           preg_flu_norisk_season3_UCI,old_covid_Primary_UCI,old_covid_booster1_UCI,
           old_covid_booster2_UCI, preg_covid_risk_all_UCI,
           preg_covid_norisk_all_UCI)

# Create the dataframe
estimates <- tibble(
  group = group,
  season = season,
  condition = condition,
  estimate = estimate,
  lower = lower,
  upper = upper
)

main <- estimates[estimates$season %in% c("2016/2019", "2021/2023", "Primary vaccination"),]
df <- main %>%
  mutate(
    group = factor(group, levels = c("Pregnancy (without risk factors) - COVID-19 vaccine",
                                     "Pregnancy (with risk factors) - COVID-19 vaccine",
                                     "Older Adulthood - COVID-19 vaccine",
                                     "Pregnancy (without risk factors) - Influenza vaccine",
                                     "Pregnancy (with risk factors) - Influenza vaccine",
                                     "Older Adulthood - Influenza vaccine")),
    condition = factor(
      condition,
      levels = c("No Mental Health Conditions", "Any Mental Health Condition","Psychiatric Comorbidity","Depression", "Anxiety",
                 "Bipolar", "PTSD", "OCD", "ADHD"
      )
    )
  )

df <- df %>%
  filter(estimate != 0)

df <- df %>%
  mutate(
    label = ifelse(
      estimate == 1 & lower == 1 & upper == 1,
      "1.00 (Reference)",
      sprintf("%.2f (%.2f-%.2f)", estimate, lower, upper)
    )
  )


pos <- position_dodge(width = 0.6)

x_min <- min(df$lower)
x_max <- max(df$upper)

library(ggrepel)

ggplot(df, aes(x = estimate, y = group, color = factor(condition))) +
  
  ## shaded background for "Older adulthood"
  annotate( 
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = c(1.5, 3.5, 5.5), ymax = c(2.5,4.5, 6.5),
    fill = "grey90",
    alpha = 0.7
  ) +
  
  ## confidence intervals
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = pos
  ) +
  
  ## point estimates
  geom_point(
    position = pos,
    size = 2.5
  ) +
  
  ## reference line at PR = 1
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  
  ## text labels on the right
  geom_text(
    aes(
      x = x_max,
      label = label
    ),
    position = position_dodge2(width = 0.6, preserve = "single"),
    hjust = -0.1,
    size = 4,
    color = "grey30"
  ) +
  
  ## scales & labels
  scale_x_continuous(limits = c(x_min, x_max),
                     expand = expansion(mult = c(0.05, 0.25))) +
  scale_color_manual(
    values = c(
      "ADHD" = "#C07CCF",
      "OCD" = "#C97C5D",
      "PTSD" =  "#A3C586",
      "Bipolar" = "#8FA3BF",
      "Anxiety" = "#E38B8B",
      "Depression" = "#7ED1C2",
      "Psychiatric Comorbidity" = "#8FB996",
      "Any Mental Health Condition" = "#E6C65C",
      "No Mental Health Conditions" = "royalblue2"
    )
  ) +
  
  labs(
    #   title = "Prevalence ratios of vaccine uptake among individuals with mental health conditions",
    x = "Prevalence Ratio (95% CIs)",
    y = "Life Stage - Vaccine type",
    color = "Condition"
  ) +
  
  ## theme styling
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 16)
  )
#######################################################################
# We should make others for seasons 
old_seasons <- estimates[estimates$season %in% c("2016/2017", 
                                                 "2017/2018",
                                                 "2018/2019",
                                                 "First Booster Dose",
                                                 "Second Booster Dose")
                         & estimates$group %in% c("Older Adulthood - Influenza vaccine","Older Adulthood - COVID-19 vaccine"),]
df <- old_seasons %>%
  mutate(group = paste0(group, " - ", season),
         condition = factor(
           condition,
           levels = c("No Mental Health Conditions", "Any Mental Health Condition","Psychiatric Comorbidity","Depression", "Anxiety",
                      "Bipolar", "PTSD", "OCD", "ADHD"
           )
         )
  )

df <- df %>%
  mutate(group = factor(group, levels = c("Older Adulthood - COVID-19 vaccine - Second Booster Dose",
                                          "Older Adulthood - COVID-19 vaccine - First Booster Dose",
                                          "Older Adulthood - Influenza vaccine - 2018/2019",
                                          "Older Adulthood - Influenza vaccine - 2017/2018",
                                          "Older Adulthood - Influenza vaccine - 2016/2017"))
  )

df <- df %>%
  filter(estimate != 0)

df <- df %>%
  mutate(
    label = ifelse(
      estimate == 1 & lower == 1 & upper == 1,
      "1.00 (Reference)",
      sprintf("%.2f (%.2f-%.2f)", estimate, lower, upper)
    )
  )


pos <- position_dodge(width = 0.6)

x_min <- min(df$lower)
x_max <- max(df$upper)

library(ggrepel)

ggplot(df, aes(x = estimate, y = group, color = factor(condition))) +
  
  ## shaded background for "Older adulthood"
  annotate( 
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = c(1.5, 3.5), ymax = c(2.5, 4.5),
    fill = "grey90",
    alpha = 0.7
  ) +
  
  ## confidence intervals
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = pos
  ) +
  
  ## point estimates
  geom_point(
    position = pos,
    size = 2.5
  ) +
  
  ## reference line at PR = 1
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  
  ## text labels on the right
  geom_text(
    aes(
      x = x_max,
      label = label
    ),
    position = position_dodge2(width = 0.6, preserve = "single"),
    hjust = -0.1,
    size = 4,
    color = "grey30"
  ) +
  
  ## scales & labels
  scale_x_continuous(limits = c(x_min, x_max),
                     expand = expansion(mult = c(0.05, 0.25))) +
  scale_color_manual(
    values = c(
      "ADHD" = "#C07CCF",
      "OCD" = "#C97C5D",
      "PTSD" =  "#A3C586",
      "Bipolar" = "#8FA3BF",
      "Anxiety" = "#E38B8B",
      "Depression" = "#7ED1C2",
      "Psychiatric Comorbidity" = "#8FB996",
      "Any Mental Health Condition" = "#E6C65C",
      "No Mental Health Conditions" = "royalblue2"
    )
  ) +
  
  labs(
    #   title = "Prevalence ratios of vaccine uptake among individuals with mental health conditions",
    x = "Prevalence Ratio (95% CIs)",
    y = "Life Stage - Vaccine type - Season",
    color = "Condition"
  ) +
  
  ## theme styling
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 16)
  )
######################################################
# We should make others for seasons 
pregnancy_seasons <- estimates[estimates$season %in% c("2016/2017", 
                                                       "2017/2018",
                                                       "2018/2019")
                               & estimates$group %in% c("Pregnancy (with risk factors) - Influenza vaccine",
                                                        "Pregnancy (without risk factors) - Influenza vaccine"),]
df <- pregnancy_seasons %>%
  mutate(group = paste0(group, " - ", season),
         condition = factor(
           condition,
           levels = c("No Mental Health Conditions", "Any Mental Health Condition","Psychiatric Comorbidity","Depression", "Anxiety",
                      "Bipolar", "PTSD", "OCD", "ADHD"
           )
         )
  )

df <- df %>%
  mutate(group = factor(group, levels = c("Pregnancy (without risk factors) - Influenza vaccine - 2018/2019",
                                          "Pregnancy (without risk factors) - Influenza vaccine - 2017/2018",
                                          "Pregnancy (without risk factors) - Influenza vaccine - 2016/2017",
                                          "Pregnancy (with risk factors) - Influenza vaccine - 2018/2019",
                                          "Pregnancy (with risk factors) - Influenza vaccine - 2017/2018",
                                          "Pregnancy (with risk factors) - Influenza vaccine - 2016/2017"))
  )

df <- df %>%
  filter(estimate != 0)

df <- df %>%
  mutate(
    label = ifelse(
      estimate == 1 & lower == 1 & upper == 1,
      "1.00 (Reference)",
      sprintf("%.2f (%.2f-%.2f)", estimate, lower, upper)
    )
  )


pos <- position_dodge(width = 0.6)

x_min <- min(df$lower)
x_max <- max(df$upper)

library(ggrepel)

ggplot(df, aes(x = estimate, y = group, color = factor(condition))) +
  
  ## shaded background for "Older adulthood"
  annotate( 
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = c(1.5, 3.5, 5.5), ymax = c(2.5, 4.5, 6.5),
    fill = "grey90",
    alpha = 0.7
  ) +
  
  ## confidence intervals
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = pos
  ) +
  
  ## point estimates
  geom_point(
    position = pos,
    size = 2.5
  ) +
  
  ## reference line at PR = 1
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  
  ## text labels on the right
  geom_text(
    aes(
      x = x_max,
      label = label
    ),
    position = position_dodge2(width = 0.6, preserve = "single"),
    hjust = -0.1,
    size = 4,
    color = "grey30"
  ) +
  
  ## scales & labels
  scale_x_continuous(limits = c(x_min, x_max),
                     expand = expansion(mult = c(0.05, 0.25))) +
  scale_color_manual(
    values = c(
      "ADHD" = "#C07CCF",
      "OCD" = "#C97C5D",
      "PTSD" =  "#A3C586",
      "Bipolar" = "#8FA3BF",
      "Anxiety" = "#E38B8B",
      "Depression" = "#7ED1C2",
      "Psychiatric Comorbidity" = "#8FB996",
      "Any Mental Health Condition" = "#E6C65C",
      "No Mental Health Conditions" = "royalblue2"
    )
  ) +
  
  labs(
    #   title = "Prevalence ratios of vaccine uptake among individuals with mental health conditions",
    x = "Prevalence Ratio (95% CIs)",
    y = "Life Stage - Vaccine type - Season",
    color = "Condition"
  ) +
  
  ## theme styling
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 16)
  )
