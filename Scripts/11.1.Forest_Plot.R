library(tibble)
library(ggplot2)
library(dplyr)

# Assuming you have your vectors named like this:
# hr_group1_season1, hr_group1_season2, etc.
# lci_group1_season1, etc.
# uci_group1_season1, etc.
old_flu_all_RR <- c(1,1.03,1.04,1.04,1.03,1.01,1.09,0.99,1.02)
old_flu_all_UCI <- c(1,1.05,1.06,1.05,1.05,1.03,1.12,1.02,1.10)
old_flu_all_LCI <- c(1,1.02,1.02,1.02,1.02,0.96,1.06,0.96,0.95)

old_flu_season1_RR <- c(1,1.09,1.09,1.09,1.14,0.90,1.40,0.97,1.46)
old_flu_season1_LCI <- c(1,1.04,0.97,1.03,1.05,0.80,1.15,0.79,0.97)
old_flu_season1_UCI <- c(1,1.15,1.22,1.15,1.24,1.02,1.61,1.19,2.19)

old_flu_season2_RR <- c(1,1.09,1.11,1.09,1.13,0.97,1.25,0.95,1.01)
old_flu_season2_LCI <- c(1,1.04,1.01,1.04,1.05,0.87,1.08,0.8,0.66)
old_flu_season2_UCI <- c(1,1.15,1.22,1.74,1.21,1.08,1.45,1.28,1.53)

old_flu_season3_RR <- c(1,1.07,1.08,1.07,1.08,0.95,1.29,0.97,1.12)
old_flu_season3_LCI <- c(1,1.03,1,1.03,1.02,0.87,1.15,0.84,0.84)
old_flu_season3_UCI <- c(1,1.12,1.17,1.12,1.15,1.04,1.45,1.11,1.5)

preg_covid_risk_all_RR <- c(1,1.03,1.05,1,1.13,1.08,1,0.97,0.92)
preg_covid_risk_all_LCI <- c(1,0.97,0.95,0.9,0.98,0.81,0.86,0.71,0.79)
preg_covid_risk_all_UCI <- c(1,1.09,1.16,1.1,1.3,1.44,1.18,1.33,1.07)

preg_covid_norisk_all_RR <- c(1,0.98,0.98,0.99,0.98,1.02,0.98,1,0.94)
preg_covid_norisk_all_LCI <- c(1,0.97,0.96,0.96,0.95,0.96,0.95,0.93,0.91)
preg_covid_norisk_all_UCI <- c(1,1.00,1.01,1.01,1.02,1.08,1.01,1.07,0.97)

preg_flu_risk_all_RR <- c(1,1.04,1.1,0.97,0.99,1.05,1.12,0,1.21)
preg_flu_risk_all_LCI <- c(1,0.99,1.01,0.9,0.83,0.87,0.94,0,0.95)
preg_flu_risk_all_UCI <- c(1,1.1,1.21,1.04,1.17,1.28,1.33,0,1.55)

preg_flu_risk_season1_RR <- c(1,1.8,2.53,1.31,1.2,0,2.08,0,3.08)
preg_flu_risk_season1_LCI <- c(1,1.15,1.45,0.57,0.29,0,0.64,0,0.66)
preg_flu_risk_season1_UCI <- c(1,2.81,4.43,3.05,5.03,0,6.7,0,14.4)

preg_flu_risk_season2_RR <- c(1,1.04,1.58,0.1,1.11,0.63,1.97,0,3.83)
preg_flu_risk_season2_LCI <- c(1,0.61,0.74,0.01,0.31,0.08,0.67,0,1.36)
preg_flu_risk_season2_UCI <- c(1,1.76,3.35,0.71,4.05,4.8,5.79,0,10.76)

preg_flu_risk_season3_RR <- c(1,0.91,0.95,0.2,1.24,4.08,2.36,0,0)
preg_flu_risk_season3_LCI <- c(1,0.47,0.28,0.03,0.27,1.71,0.67,0,0)
preg_flu_risk_season3_UCI <- c(1,1.78,3.14,1.47,5.61,13.51,8.29,0,0)

preg_flu_norisk_all_RR <- c(1,1.02,1,1.02,1.02,1,1.03,1.08,0.99)
preg_flu_norisk_all_LCI <- c(1,1,0.99,1.01,0.99,0.96,1,1.03,0.96)
preg_flu_norisk_all_UCI <- c(1,1.03,1.02,1.04,1.05,1.04,1.06,1.14,1.02)

preg_flu_norisk_season1_RR <- c(1,1.15,1.15,1.21,0.96,1.16,1.38,1.51,0.64)
preg_flu_norisk_season1_LCI <- c(1,1.02,0.9,0.99,0.65,0.65,0.98,0.86,0.35)
preg_flu_norisk_season1_UCI <- c(1,1.31,1.47,1.47,1.42,2.04,1.96,2.65,1.19)

preg_flu_norisk_season2_RR <- c(1,1.15,1.04,1.19,1.27,1.11,1.05,1.7,0.96)
preg_flu_norisk_season2_LCI <- c(1,1.03,0.83,0.99,0.95,0.67,0.74,1.08,0.6)
preg_flu_norisk_season2_UCI <- c(1,1.29,1.32,1.42,1.7,1.85,1.49,2.67,1.52)

preg_flu_norisk_season3_RR <- c(1,1.11,1.02,1.09,1.15,0.71,1.3,1.6,1.07)
preg_flu_norisk_season3_LCI <- c(1,0.99,0.81,0.9,0.85,0.35,0.94,0.99,0.72)
preg_flu_norisk_season3_UCI <- c(1,1.25,1.29,1.32,1.57,1.42,1.8,2.62,1.61)

old_covid_Primary_RR <- c(1,1.12,1.12,1.12,1.08,1.12,1.18,1.09,1.1)
old_covid_Primary_LCI <- c(1,1.11,1.09,1.11,1.07,1.10,1.13,1.04,1.04)
old_covid_Primary_UCI <- c(1,1.12,1.15,1.13,1.1,1.14,1.24,1.13,1.18)

old_covid_booster1_RR <- c(1,1.1,1.09,1.11,1.06,1.1,1.17,1.05,1.1)
old_covid_booster1_LCI <- c(1,1.09,1.06,1.1,1.04,1.07,1.11,0.99,0.98)
old_covid_booster1_UCI <- c(1,1.11,1.13,1.12,1.08,1.13,1.23,1.11,1.16)

old_covid_booster2_RR <- c(1,1.05,1.02,1.07,1,1.08,1.11,0.98,1.03)
old_covid_booster2_LCI <- c(1,1.04,0.96,1.05,0.96,1.03,1.03,0.9,0.89)
old_covid_booster2_UCI <- c(1,1.07,1.08,1.09,1.03,1.13,1.2,1.07,1.19)

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

