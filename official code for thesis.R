#READING THE RAW DATA , ARRANGING DATAFRAMES 


library(readr)
Phytoplankton_analysis <- read_csv(
  "C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv",
  col_types = cols(
    Day =  col_number()
    ,
    Counts = col_number(),
    Factor_unit = col_number(),
    Cell_Biovolume = col_number(),
    Biomass = col_number(),
    pH = col_number(),
    Temperature = col_number(),
    Chl = col_number(),
    DO = col_number()
  )
)

library(scales)

names(Phytoplankton_analysis)

#DEFINING VALUES OF THE DATASHEET
Asa_data<-subset.data.frame(Phytoplankton_analysis, Lake == 'Asa')
Asa_data <- subset.data.frame(Phytoplankton_analysis, Lake == "Asa")

Svartberget_data <- subset.data.frame(Phytoplankton_analysis, Lake == "Svartberget")


target_treatments<-c("C1","G1","E1","C2","G2","E2","C3","G3","E3","C4","G4","E4")

#mean environmental factors per lake and treatment
library(tidyverse)
library(patchwork)


treatment_colors <- c("Control" = "#4292C6", "Gradual" = "#238b45", "Extreme" = "#cb181d")

prepare_env_summary <- function(data, variable) {
  data %>%
    filter(Day %in% c(0, 4, 8, 12), !is.na(.data[[variable]])) %>%
    mutate(
      Treatment_group = case_when(
        grepl("^C", Treatment) ~ "Control",
        grepl("^G", Treatment) ~ "Gradual",
        grepl("^E", Treatment) ~ "Extreme",
        TRUE ~ NA_character_
      ),
      Day = factor(Day, levels = c(0, 4, 8, 12))
    ) %>%
    group_by(Day, Treatment_group, Treatment) %>%
    summarise(value = mean(.data[[variable]], na.rm = TRUE), .groups = "drop") %>%
    group_by(Day, Treatment_group) %>%
    summarise(
      mean_value = mean(value),
      sd_value = sd(value),
      .groups = "drop"
    )
}


plot_env_variable <- function(summary_df, variable_label, title_text) {
  ggplot(summary_df, aes(x = Day, y = mean_value, color = Treatment_group, group = Treatment_group)) +
    geom_line(linewidth = 0.9) +
    geom_point(shape = 21, size = 2.5, fill = "white", stroke = 1) +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = 0.2, linewidth = 0.6) +
    scale_color_manual(values = treatment_colors) +
    theme_minimal(base_size = 8) +
    labs(
      x = "Sampling Day",
      y = variable_label,
      color = "Treatment",
      title = title_text
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.3, "cm"),
      legend.position = "right"
    )
}


asa_cond <- prepare_env_summary(Asa_data, "Conductivity")
svart_cond <- prepare_env_summary(Svartberget_data, "Conductivity")

plot_cond_asa <- plot_env_variable(asa_cond, "Conductivity (μS/cm)", "Dynamics of mean conductivity per treatment and sampling day in Asa")
plot_cond_svart <- plot_env_variable(svart_cond, "Conductivity (μS/cm)", "Dynamics of mean conductivity per treatment and sampling day in Svartberget")

plot_cond_asa / plot_cond_svart


asa_temp <- prepare_env_summary(Asa_data, "Temperature")
svart_temp <- prepare_env_summary(Svartberget_data, "Temperature")

plot_temp_asa <- plot_env_variable(asa_temp, "Temperature (°C)", "Dynamics of mean temperature per treatment and sampling day in Asa")
plot_temp_svart <- plot_env_variable(svart_temp, "Temperature (°C)", "Dynamics of mean temperature per treatmentand sampling day in Svartberget")

plot_temp_asa / plot_temp_svart


asa_DO <- prepare_env_summary(Asa_data, "DO")
svart_DO <- prepare_env_summary(Svartberget_data, "DO")

plot_DO_asa <- plot_env_variable(asa_DO, "Dissolved Oxygen (mg/L)", "Dynamics of mean dissolved oxygen per treatment and sampling day in Asa")
plot_DO_svart <- plot_env_variable(svart_DO, "Dissolved Oxygen (mg/L)", "Dynamics of mean dissolved oxygen per treatment and sampling day in Svartberget")

plot_DO_asa / plot_DO_svart

# ---- pH ----
asa_pH <- prepare_env_summary(Asa_data, "pH")
svart_pH <- prepare_env_summary(Svartberget_data, "pH")

plot_pH_asa <- plot_env_variable(asa_pH, "pH", "Dynamics of mean pH per treatment and sampling day in Asa")
plot_pH_svart <- plot_env_variable(svart_pH, "pH", "Dynamics of mean pH per treatment and sampling day in Svartberget")

plot_pH_asa / plot_pH_svart


#MEAN VALUES OF CONDUCTIVITY , PH , TEMPERATURE AND DO

library(dplyr)
library(ggplot2)
library(patchwork)

prepare_summary <- function(data) {
  data %>%
    mutate(Day = as.numeric(as.character(Day))) %>%
    filter(Day %in% c(0, 4, 8, 12), !is.na(Biomass)) %>%
    mutate(
      Treatment_group = case_when(
        grepl("^C", Treatment) ~ "Control",
        grepl("^G", Treatment) ~ "Gradual",
        grepl("^E", Treatment) ~ "Extreme",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Treatment_group)) %>%
    group_by(Day, Treatment_group, Treatment) %>%
    summarise(total_biomass = sum(Biomass), .groups = "drop")
}

asa_summary <- prepare_summary(Asa_data) %>%
  group_by(Day, Treatment_group) %>%
  summarise(
    mean_biomass = mean(total_biomass),
    sd_biomass = sd(total_biomass),
    .groups = "drop"
  ) %>%
  mutate(Day = factor(Day, levels = c(0, 4, 8, 12)))

svart_summary <- prepare_summary(Svartberget_data) %>%
  group_by(Day, Treatment_group) %>%
  summarise(
    mean_biomass = sum(total_biomass) / n(),
    sd_biomass = sd(total_biomass),
    .groups = "drop"
  ) %>%
  mutate(Day = factor(Day, levels = c(0, 4, 8, 12)))

treatment_colors <- c("Control" = "#4292C6", "Gradual" = "#238b45", "Extreme" = "#cb181d")

plot_asa <- ggplot(asa_summary, aes(x = Day, y = mean_biomass, color = Treatment_group, group = Treatment_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 21, size = 2.5, fill = "white", stroke = 1) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass),
                width = 0.2, linewidth = 0.6) +
  scale_color_manual(values = treatment_colors) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0.01, 0.03))) +
  theme_minimal(base_size = 8) +
  labs(
    x = "Experimental Day",
    y = "Mean Biomass (mg/L)",
    color = "Treatment",
    title = "Dynamics of mean biomass per treatment in Asa"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"),
    legend.position = "right"
  )

plot_svart <- ggplot(svart_summary, aes(x = Day, y = mean_biomass, color = Treatment_group, group = Treatment_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 21, size = 2.5, fill = "white", stroke = 1) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass),
                width = 0.2, linewidth = 0.6) +
  scale_color_manual(values = treatment_colors) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0.01, 0.03))) +
  theme_minimal(base_size = 8) +
  labs(
    x = "Experimental Day",
    y = "Mean Biomass (mg/L)",
    color = "Treatment",
    title = "Dynamics of mean biomass per treatment in Svartberget"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"),
    legend.position = "right"
  )

plot_asa / plot_svart


#MEAN BIOMASS PER TREATMENT IN ASA AND SVARTBERGET


#DIVERSITY INDEXES 


library(dplyr)
library(ggplot2)
library(tidyr)

asa_data <- Phytoplankton_analysis %>%
  filter(Lake == "Asa")

alpha_asa <- asa_data %>%
  group_by(Treatment, Day) %>%
  summarise(Alpha = n_distinct(Taxon), .groups = "drop") %>%
  summarise(Alpha = mean(Alpha)) %>%
  pull(Alpha)

gamma_asa <- asa_data %>%
  summarise(Gamma = n_distinct(Taxon)) %>%
  pull(Gamma)

beta_asa <- gamma_asa / alpha_asa

svart_data <- Phytoplankton_analysis %>%
  filter(Lake == "Svartberget")

alpha_svart <- svart_data %>%
  group_by(Treatment, Day) %>%
  summarise(Alpha = n_distinct(Taxon), .groups = "drop") %>%
  summarise(Alpha = mean(Alpha)) %>%
  pull(Alpha)

gamma_svart <- svart_data %>%
  summarise(Gamma = n_distinct(Taxon)) %>%
  pull(Gamma)

beta_svart <- gamma_svart / alpha_svart

div_df <- data.frame(
  Lake = c("Asa", "Svartberget"),
  Gamma = c(gamma_asa, gamma_svart),
  Alpha = c(alpha_asa, alpha_svart),
  Beta = c(beta_asa, beta_svart)
)

div_long <- pivot_longer(div_df, cols = c("Gamma", "Alpha", "Beta"), names_to = "Diversity", values_to = "Value")

div_long$Diversity <- factor(div_long$Diversity, levels = c("Gamma", "Alpha", "Beta"))

ggplot(div_long, aes(x = Diversity, y = Value, fill = Lake)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Asa" = "red", "Svartberget" = "blue")) +
  labs(title = "Alpha, Beta and Gamma Diversity Indices per Lake", y = "Mean Value", x = "Diversity Index") +
  theme_minimal()

#ALPHA,BETA,GAMMA DIVERSITY PER LAKE

library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

df <- read.csv("Phytoplankton_analysis.csv")

community_matrix <- df %>%
  group_by(Lake, Day, Treatment, Genus) %>%
  summarise(Counts = sum(Counts), .groups = "drop") %>%
  pivot_wider(names_from = Genus, values_from = Counts, values_fill = 0)

comm_data <- community_matrix %>%
  select(-Lake, -Day, -Treatment)

shannon <- diversity(comm_data, index = "shannon")
simpson <- diversity(comm_data, index = "simpson")
invsimpson <- 1 / simpson

div_matrix <- community_matrix %>%
  select(Lake) %>%
  mutate(
    Shannon = shannon,
    Simpson = simpson,
    Inverse_Simpson = invsimpson
  )

div_indices <- div_matrix %>%
  group_by(Lake) %>%
  summarise(across(c(Shannon, Simpson, Inverse_Simpson), mean), .groups = "drop") %>%
  pivot_longer(cols = -Lake, names_to = "Index", values_to = "Value") %>%
  mutate(Index = factor(Index, levels = c("Shannon", "Inverse_Simpson", "Simpson")))


ggplot(div_indices, aes(x = Index, y = Value, fill = Lake)) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Asa" = "red", "Svartberget" = "blue")) +
  labs(
    title = "Shannon, Simpson, and Inverse Simpson Diversity per Lake",
    x = "Diversity Index",
    y = "Mean Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )
print(div_indices)

#mean diversity indexes per lake

library(tidyverse)
library(nlme)
library(emmeans)
library(flextable)
library(webshot2)

df <- read_csv("Phytoplankton_analysis.csv") %>%
  mutate(
    Treatment = factor(substr(Treatment, 1, 1)),
    Day = factor(Day),
    Enclosure = factor(Enclosure)
  )

calc_alpha <- function(lake_name) {
  df %>%
    filter(Lake == lake_name) %>%
    group_by(Enclosure, Treatment, Day) %>%
    summarise(Alpha = n_distinct(Taxon), .groups = "drop")
}

alpha_asa <- calc_alpha("Asa")
alpha_svb <- calc_alpha("Svartberget")

ggplot(alpha_asa, aes(x = Treatment, y = Alpha, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  facet_wrap(~Day, labeller = labeller(Day = function(x) paste0("Sampling day ", x))) +
  scale_fill_manual(values = c("C" = "steelblue", "G" = "forestgreen", "E" = "firebrick")) +
  labs(title = "Alpha Diversity per Treatment and Sampling day in Asa", y = "Alpha Diversity (Richness)", x = "Treatment") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(alpha_svb, aes(x = Treatment, y = Alpha, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  facet_wrap(~Day, labeller = labeller(Day = function(x) paste0("Sampling day ", x))) +
  scale_fill_manual(values = c("C" = "steelblue", "G" = "forestgreen", "E" = "firebrick")) +
  labs(title = "Alpha Diversity per Treatment and Sampling day in Svartberget", y = "Alpha Diversity (Richness)", x = "Treatment") +
  theme_minimal() +
  theme(legend.position = "none")

library(nlme)
library(emmeans)
library(dplyr)
library(stringr)
library(flextable)
library(officer)

run_sqrt_lme_and_posthoc <- function(data, lake_name) {
  data <- data %>% mutate(Alpha_sqrt = sqrt(Alpha))
  
  model <- lme(Alpha_sqrt ~ Treatment * Day, random = ~1 | Enclosure, data = data, na.action = na.exclude)
  
  anova_df <- anova(model) %>% as.data.frame() %>%
    tibble::rownames_to_column("Effect") %>%
    mutate(Effect = str_replace_all(Effect, "Treatment", "Treatment"),
           Effect = str_replace_all(Effect, "Day", "Sampling day"),
           Effect = str_replace_all(Effect, "Treatment:Sampling day", "Treatment : Sampling day"),
           `F-value` = round(`F-value`, 3),
           `p-value` = round(`p-value`, 3),
           Significance = case_when(`p-value` <= 0.001 ~ "***",
                                    `p-value` <= 0.01 ~ "**",
                                    `p-value` <= 0.05 ~ "*",
                                    TRUE ~ "ns")) %>%
    select(`Main effects and interactions` = Effect, Df = numDF, `F-value`, `p-value`, Significance)
  
  anova_ft <- flextable(anova_df) %>%
    autofit() %>%
    fontsize(size = 10, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    bold(part = "header", bold = TRUE)
  
  save_as_image(anova_ft, path = paste0(lake_name, "_Alpha_ANOVA_sqrt.png"))
  
  posthoc_df <- emmeans(model, pairwise ~ Treatment | Day, adjust = "tukey")$contrasts %>%
    as.data.frame() %>%
    rename(Comparisons = contrast) %>%
    mutate(across(where(is.numeric), ~ round(., 3)),
           Significance = case_when(p.value <= 0.001 ~ "***",
                                    p.value <= 0.01 ~ "**",
                                    p.value <= 0.05 ~ "*",
                                    TRUE ~ "ns"))
  
  posthoc_ft <- flextable(posthoc_df) %>%
    autofit() %>%
    fontsize(size = 10, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    bold(part = "header", bold = TRUE)
  
  save_as_image(posthoc_ft, path = paste0(lake_name, "_Alpha_PostHoc_sqrt.png"))
}


run_sqrt_lme_and_posthoc(alpha_asa, "Asa")
run_sqrt_lme_and_posthoc(alpha_svb, "Svartberget")


#alpha diversity index per treatment and day and stats

library(vegan)
library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv("C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv")
svart_df <- subset(df, Lake == "Svartberget")
svart_df$Group <- factor(substr(svart_df$Treatment, 1, 1), levels = c("C", "G", "E"))

diversity_by_sample <- svart_df %>%
  group_by(Treatment, Day, Group, Genus) %>%
  summarise(Abundance = sum(Counts, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genus, values_from = Abundance, values_fill = 0) %>%
  mutate(
    Shannon = diversity(select(., -Treatment, -Day, -Group), index = "shannon"),
    Simpson = diversity(select(., -Treatment, -Day, -Group), index = "simpson"),
    InvSimpson = diversity(select(., -Treatment, -Day, -Group), index = "invsimpson")
  )

diversity_by_sample <- diversity_by_sample %>%
  mutate(Day = factor(Day, levels = c(0, 4, 8, 12)),
         BoxID = interaction(Day, Group, lex.order = TRUE))

create_summary <- function(df, metric) {
  df %>%
    group_by(BoxID, Day, Group) %>%
    summarise(
      Mean = mean(.data[[metric]], na.rm = TRUE),
      SD = sd(.data[[metric]], na.rm = TRUE),
      Min = min(.data[[metric]], na.rm = TRUE),
      Max = max(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      SD_lower = pmax(Mean - SD, Min),
      SD_upper = pmin(Mean + SD, Max)
    )
}

shannon_summary <- create_summary(diversity_by_sample, "Shannon")
simpson_summary <- create_summary(diversity_by_sample, "Simpson")
invsimpson_summary <- create_summary(diversity_by_sample, "InvSimpson")

custom_colors <- c("C" = "steelblue", "G" = "forestgreen", "E" = "firebrick")

plot_metric <- function(metric_name, y_label, summary_table) {
  ggplot(diversity_by_sample, aes(x = BoxID, y = .data[[metric_name]], fill = Group)) +
    geom_boxplot(
      coef = 0,
      outlier.shape = 21,
      outlier.size = 2.5,
      outlier.stroke = 0.6,
      outlier.colour = "black"
    ) +
    geom_errorbar(
      data = summary_table,
      aes(x = BoxID, ymin = SD_lower, ymax = SD_upper, color = Group),
      width = 0.4,
      linewidth = 0.9,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = custom_colors) +
    scale_color_manual(values = custom_colors) +
    labs(
      title = paste(y_label, "per Treatment in Svartberget"),
      x = "Sampling Day and Treatment Group",
      y = y_label
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_metric("Shannon", "Shannon Diversity Index", shannon_summary)
plot_metric("Simpson", "Simpson Diversity Index", simpson_summary)
plot_metric("InvSimpson", "Inverse Simpson Diversity Index", invsimpson_summary)


#boxplots of diversity indeces svartberget



library(vegan)
library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv("C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv")
asa_df <- subset(df, Lake == "Asa")
asa_df$Group <- factor(substr(asa_df$Treatment, 1, 1), levels = c("C", "G", "E"))

diversity_by_sample <- asa_df %>%
  group_by(Treatment, Day, Group, Genus) %>%
  summarise(Abundance = sum(Counts, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genus, values_from = Abundance, values_fill = 0) %>%
  mutate(
    Shannon = diversity(select(., -Treatment, -Day, -Group), index = "shannon"),
    Simpson = diversity(select(., -Treatment, -Day, -Group), index = "simpson"),
    InvSimpson = diversity(select(., -Treatment, -Day, -Group), index = "invsimpson")
  )

diversity_by_sample <- diversity_by_sample %>%
  mutate(Day = factor(Day, levels = c(0, 4, 8, 12)),
         BoxID = interaction(Day, Group, lex.order = TRUE))

create_summary <- function(df, metric) {
  df %>%
    group_by(BoxID, Day, Group) %>%
    summarise(
      Mean = mean(.data[[metric]], na.rm = TRUE),
      SD = sd(.data[[metric]], na.rm = TRUE),
      Min = min(.data[[metric]], na.rm = TRUE),
      Max = max(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      SD_lower = pmax(Mean - SD, Min),
      SD_upper = pmin(Mean + SD, Max)
    )
}

shannon_summary <- create_summary(diversity_by_sample, "Shannon")
simpson_summary <- create_summary(diversity_by_sample, "Simpson")
invsimpson_summary <- create_summary(diversity_by_sample, "InvSimpson")

custom_colors <- c("C" = "steelblue", "G" = "forestgreen", "E" = "firebrick")

plot_metric <- function(metric_name, y_label, summary_table) {
  ggplot(diversity_by_sample, aes(x = BoxID, y = .data[[metric_name]], fill = Group)) +
    geom_boxplot(
      coef = 0,
      outlier.shape = 21,
      outlier.size = 2.5,
      outlier.stroke = 0.6,
      outlier.colour = "black"
    ) +
    geom_errorbar(
      data = summary_table,
      aes(x = BoxID, ymin = SD_lower, ymax = SD_upper, color = Group),
      width = 0.4,
      linewidth = 0.9,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = custom_colors) +
    scale_color_manual(values = custom_colors) +
    labs(
      title = paste(y_label, "per Treatment in Asa"),
      x = "Sampling Day and Treatment Group",
      y = y_label
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_metric("Shannon", "Shannon Diversity Index", shannon_summary)
plot_metric("Simpson", "Simpson Diversity Index", simpson_summary)
plot_metric("InvSimpson", "Inverse Simpson Diversity Index", invsimpson_summary)


#boxplot of diversity indeces in Asa 

library(tidyverse)
library(nlme)
library(vegan)
library(emmeans)
library(flextable)
library(webshot2)

df_asa <- read_csv("Phytoplankton_analysis.csv") %>%
  filter(Lake == "Asa") %>%
  mutate(Treatment = factor(substr(Treatment, 1, 1)), Day = factor(Day), Enclosure = factor(Enclosure))

diversity_asa <- df_asa %>%
  group_by(Day, Enclosure, Treatment, Genus) %>%
  summarise(Total_Counts = sum(Counts, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genus, values_from = Total_Counts, values_fill = list(Total_Counts = 0)) %>%
  rowwise() %>%
  mutate(Shannon = diversity(c_across(where(is.numeric)), index = "shannon"),
         Simpson = diversity(c_across(where(is.numeric)), index = "simpson"),
         Inverse_Simpson = 1 / Simpson) %>%
  ungroup() %>%
  pivot_longer(cols = c(Shannon, Simpson, Inverse_Simpson), names_to = "Index", values_to = "Value") %>%
  mutate(Treatment_group = Treatment, Sampling_day = Day)

df_svb <- read_csv("Phytoplankton_analysis.csv") %>%
  filter(Lake == "Svartberget") %>%
  mutate(Treatment = factor(substr(Treatment, 1, 1)), Day = factor(Day), Enclosure = factor(Enclosure))

diversity_svb <- df_svb %>%
  group_by(Day, Enclosure, Treatment, Genus) %>%
  summarise(Total_Counts = sum(Counts, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genus, values_from = Total_Counts, values_fill = list(Total_Counts = 0)) %>%
  rowwise() %>%
  mutate(Shannon = diversity(c_across(where(is.numeric)), index = "shannon"),
         Simpson = diversity(c_across(where(is.numeric)), index = "simpson"),
         Inverse_Simpson = 1 / Simpson) %>%
  ungroup() %>%
  pivot_longer(cols = c(Shannon, Simpson, Inverse_Simpson), names_to = "Index", values_to = "Value") %>%
  mutate(Treatment_group = Treatment, Sampling_day = Day)

library(tidyverse)
library(nlme)
library(vegan)
library(emmeans)
library(flextable)
library(webshot2)

df_asa <- read_csv("Phytoplankton_analysis.csv") %>%
  filter(Lake == "Asa") %>%
  mutate(Treatment = factor(substr(Treatment, 1, 1)), Day = factor(Day), Enclosure = factor(Enclosure))

diversity_asa <- df_asa %>%
  group_by(Day, Enclosure, Treatment, Genus) %>%
  summarise(Total_Counts = sum(Counts, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genus, values_from = Total_Counts, values_fill = list(Total_Counts = 0)) %>%
  rowwise() %>%
  mutate(Shannon = diversity(c_across(where(is.numeric)), index = "shannon"),
         Simpson = diversity(c_across(where(is.numeric)), index = "simpson"),
         Inverse_Simpson = 1 / Simpson) %>%
  ungroup() %>%
  pivot_longer(cols = c(Shannon, Simpson, Inverse_Simpson), names_to = "Index", values_to = "Value") %>%
  mutate(Treatment_group = Treatment, Sampling_day = Day)

df_svb <- read_csv("Phytoplankton_analysis.csv") %>%
  filter(Lake == "Svartberget") %>%
  mutate(Treatment = factor(substr(Treatment, 1, 1)), Day = factor(Day), Enclosure = factor(Enclosure))

diversity_svb <- df_svb %>%
  group_by(Day, Enclosure, Treatment, Genus) %>%
  summarise(Total_Counts = sum(Counts, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genus, values_from = Total_Counts, values_fill = list(Total_Counts = 0)) %>%
  rowwise() %>%
  mutate(Shannon = diversity(c_across(where(is.numeric)), index = "shannon"),
         Simpson = diversity(c_across(where(is.numeric)), index = "simpson"),
         Inverse_Simpson = 1 / Simpson) %>%
  ungroup() %>%
  pivot_longer(cols = c(Shannon, Simpson, Inverse_Simpson), names_to = "Index", values_to = "Value") %>%
  mutate(Treatment_group = Treatment, Sampling_day = Day)

run_sqrt_lme_and_posthoc <- function(index_name, data_prefix, data_df) {
  model_data <- data_df %>% filter(Index == index_name) %>%
    mutate(Value_sqrt = sqrt(Value))
  
  model <- lme(Value_sqrt ~ Treatment_group * Sampling_day, random = ~1 | Enclosure, data = model_data, na.action = na.exclude)
  
  anova_df <- anova(model) %>% as.data.frame() %>%
    rownames_to_column("Effect") %>%
    mutate(Effect = str_replace_all(Effect, "Treatment_group", "Treatment group"),
           Effect = str_replace_all(Effect, "Sampling_day", "Sampling day"),
           Effect = str_replace_all(Effect, "Treatment group:Sampling day", "Treatment group : Sampling day"),
           `F-value` = round(`F-value`, 3),
           `p-value` = round(`p-value`, 3),
           Significance = case_when(`p-value` <= 0.001 ~ "***",
                                    `p-value` <= 0.01 ~ "**",
                                    `p-value` <= 0.05 ~ "*",
                                    TRUE ~ "ns")) %>%
    select(`Main effects and interactions` = Effect, Df = numDF, `F-value`, `p-value`, Significance)
  
  table_ft <- flextable(anova_df) %>%
    autofit() %>%
    fontsize(size = 10, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    bold(part = "header", bold = TRUE)
  
  save_as_image(table_ft, path = paste0(data_prefix, "_", index_name, "_LME_ANOVA_sqrt.png"))
  
  posthoc_df <- emmeans(model, pairwise ~ Treatment_group | Sampling_day, adjust = "tukey")$contrasts %>%
    as.data.frame() %>%
    rename(Comparisons = contrast) %>%
    mutate(across(where(is.numeric), ~ round(., 3)),
           Significance = case_when(p.value <= 0.001 ~ "***",
                                    p.value <= 0.01 ~ "**",
                                    p.value <= 0.05 ~ "*",
                                    TRUE ~ "ns"))
  
  posthoc_ft <- flextable(posthoc_df) %>%
    autofit() %>%
    fontsize(size = 10, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    bold(part = "header", bold = TRUE)
  
  save_as_image(posthoc_ft, path = paste0(data_prefix, "_", index_name, "_PostHoc_sqrt.png"))
}

run_sqrt_lme_and_posthoc("Shannon", "Asa", diversity_asa)
run_sqrt_lme_and_posthoc("Simpson", "Asa", diversity_asa)
run_sqrt_lme_and_posthoc("Inverse_Simpson", "Asa", diversity_asa)

run_sqrt_lme_and_posthoc("Shannon", "Svartberget", diversity_svb)
run_sqrt_lme_and_posthoc("Simpson", "Svartberget", diversity_svb)
run_sqrt_lme_and_posthoc("Inverse_Simpson", "Svartberget", diversity_svb)

#stats for Svartberget and Asa diversity indexes


#SAMPLING EFFORT


library(dplyr)
library(iNEXT)
library(ggplot2)

asa_abund_named <- Phytoplankton_analysis %>%
  filter(Lake == "Asa") %>%
  group_by(Taxon) %>%
  summarise(Counts = sum(as.numeric(Counts), na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(Counts), Counts > 0, is.finite(Counts)) %>%
  mutate(Counts = round(Counts)) %>%
  tibble::deframe()

asa_iNEXT <- iNEXT(asa_abund_named, q = 0, datatype = "abundance")

ggiNEXT(asa_iNEXT, type = 1) +
  labs(
    title = "Sampling Effort in Asa",
    x = "Number of Individuals (Sampling Effort)",
    y = "Species Richness"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12)
  )
#RANK ABUNDANCE CURVE FOR FURTHER EXTRAPOLATION AND RAREFACTION IN ASA
library(dplyr)
library(iNEXT)
library(ggplot2)

svartberget_abund_named <- Phytoplankton_analysis %>%
  filter(Lake == "Svartberget") %>%
  group_by(Taxon) %>%
  summarise(Counts = sum(as.numeric(Counts), na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(Counts), Counts > 0, is.finite(Counts)) %>%
  mutate(Counts = round(Counts)) %>%
  tibble::deframe()

svartberget_iNEXT <- iNEXT(svartberget_abund_named, q = 0, datatype = "abundance")

ggiNEXT(svartberget_iNEXT, type = 1) +
  labs(
    title = "Sampling Effort in Svartberget",
    x = "Number of Individuals (Sampling Effort)",
    y = "Species Richness"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12)
  )
#ABUNDANCE CURVE SVARTBERGET


#MEAN BIOMASS PER LAKE

library(dplyr)
library(ggplot2)
library(patchwork)

prepare_summary <- function(data) {
  data %>%
    mutate(Day = as.numeric(as.character(Day))) %>%
    filter(Day %in% c(0, 4, 8, 12), !is.na(Biomass)) %>%
    mutate(
      Treatment_group = case_when(
        grepl("^C", Treatment) ~ "Control",
        grepl("^G", Treatment) ~ "Gradual",
        grepl("^E", Treatment) ~ "Extreme",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Treatment_group)) %>%
    group_by(Day, Treatment_group, Treatment) %>%
    summarise(total_biomass = sum(Biomass), .groups = "drop")
}

asa_summary <- prepare_summary(Asa_data) %>%
  group_by(Day, Treatment_group) %>%
  summarise(
    mean_biomass = mean(total_biomass),
    sd_biomass = sd(total_biomass),
    .groups = "drop"
  ) %>%
  mutate(Day = factor(Day, levels = c(0, 4, 8, 12)))

svart_summary <- prepare_summary(Svartberget_data) %>%
  group_by(Day, Treatment_group) %>%
  summarise(
    mean_biomass = sum(total_biomass) / n(),
    sd_biomass = sd(total_biomass),
    .groups = "drop"
  ) %>%
  mutate(Day = factor(Day, levels = c(0, 4, 8, 12)))

treatment_colors <- c("Control" = "#4292C6", "Gradual" = "#238b45", "Extreme" = "#cb181d")

plot_asa <- ggplot(asa_summary, aes(x = Day, y = mean_biomass, color = Treatment_group, group = Treatment_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 21, size = 2.5, fill = "white", stroke = 1) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass),
                width = 0.2, linewidth = 0.6) +
  scale_color_manual(values = treatment_colors) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0.01, 0.03))) +
  theme_minimal(base_size = 8) +
  labs(
    x = "Sampling Day",
    y = "Mean Biomass (mg/L)",
    color = "Treatment",
    title = "Dynamics of mean biomass per treatment and sampling day in Asa"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"),
    legend.position = "right"
  )

plot_svart <- ggplot(svart_summary, aes(x = Day, y = mean_biomass, color = Treatment_group, group = Treatment_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 21, size = 2.5, fill = "white", stroke = 1) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass),
                width = 0.2, linewidth = 0.6) +
  scale_color_manual(values = treatment_colors) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0.01, 0.03))) +
  theme_minimal(base_size = 8) +
  labs(
    x = "Sampling Day",
    y = "Mean Biomass (mg/L)",
    color = "Treatment",
    title = "Dynamics of mean biomass per treatment and sampling day in Svartberget"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"),
    legend.position = "right"
  )

plot_asa
plot_svart



library(tidyverse)
library(nlme)
library(emmeans)
library(flextable)
library(webshot2)

df <- read_csv("Phytoplankton_analysis.csv") %>%
  mutate(
    Treatment_Group = factor(substr(Treatment, 1, 1)),
    Day = factor(Day),
    Enclosure = factor(Enclosure),
    sqrt_Biomass = sqrt(Biomass)
  )

run_lme_and_posthoc <- function(lake_name) {
  data_lake <- df %>% filter(Lake == lake_name)
  model <- lme(sqrt_Biomass ~ Treatment_Group * Day, random = ~1 | Enclosure, data = data_lake, na.action = na.exclude)
  anova_df <- anova(model) %>% as.data.frame() %>%
    rownames_to_column("Effect") %>%
    mutate(Effect = str_replace_all(Effect, "Treatment_Group", "Treatment group"),
           Effect = str_replace_all(Effect, "Day", "Sampling day"),
           Effect = str_replace_all(Effect, "Treatment group:Sampling day", "Treatment group : Sampling day"),
           `F-value` = round(`F-value`, 3),
           `p-value` = round(`p-value`, 3),
           Significance = case_when(`p-value` <= 0.001 ~ "***", `p-value` <= 0.01 ~ "**", `p-value` <= 0.05 ~ "*", TRUE ~ "ns")) %>%
    select(`Main effects and interactions` = Effect, Df = numDF, `F-value`, `p-value`, Significance)
  anova_ft <- flextable(anova_df) %>% autofit() %>% fontsize(size = 10, part = "all") %>% font(fontname = "Times New Roman", part = "all") %>% bold(part = "header", bold = TRUE)
  save_as_image(anova_ft, path = paste0(lake_name, "_sqrtBiomass_ANOVA.png"))
  posthoc_df <- emmeans(model, pairwise ~ Treatment_Group | Day, adjust = "tukey")$contrasts %>%
    as.data.frame() %>%
    rename(Comparisons = contrast) %>%
    mutate(across(where(is.numeric), ~ round(., 3)),
           Significance = case_when(p.value <= 0.001 ~ "***", p.value <= 0.01 ~ "**", p.value <= 0.05 ~ "*", TRUE ~ "ns"))
  
  posthoc_ft <- flextable(posthoc_df) %>% autofit() %>% fontsize(size = 10, part = "all") %>% font(fontname = "Times New Roman", part = "all") %>% bold(part = "header", bold = TRUE)
  save_as_image(posthoc_ft, path = paste0(lake_name, "_sqrtBiomass_PostHoc.png"))
}

run_lme_and_posthoc("Asa")
run_lme_and_posthoc("Svartberget")

#TRANSFORMED ANOVA FOR BIOMASS BASED ON SQUARE ROOT VALUES AND POST HOCS






#RELATIVE BIOMASS OF CLASSES

library(dplyr)
library(ggplot2)
library(scales)

custom_colors <- c(
  "Chrysophyceae"     = "gold",
  "Spirotrichea"      = "purple",
  "Chlorophyceae"     = "limegreen",
  "Cryptophyceae"     = "brown",
  "Cyanophyceae"      = "blue",
  "Euglenophyceae"    = "orange",
  "Baccilariophyceae" = "gray70",
  "Dinophyceae"       = "black",
  "Zygnematophyceae"  = "red",
  "Trebouxiophyceae"  = "darkgreen",
  "Choanoflagellata"  = "deeppink",
  "Oligohymenophorea" = "lightblue",
  "unidentified"      = "pink",
  "Eustigmatophyceae" = "sienna3",
  "Others"            = "white"
)

asa_all <- Phytoplankton_analysis %>%
  filter(Lake == "Asa", Day %in% c(0, 4, 8, 12))

rel_all <- asa_all %>%
  group_by(Day, Treatment, Class) %>%
  summarise(TotalBiomass = sum(Biomass), .groups = "drop") %>%
  group_by(Day, Treatment) %>%
  mutate(RelBiomass = TotalBiomass / sum(TotalBiomass)) %>%
  ungroup()

top_classes <- rel_all %>%
  group_by(Day, Treatment) %>%
  slice_max(RelBiomass, n = 4, with_ties = FALSE) %>%
  mutate(is_top = TRUE) %>%
  select(Day, Treatment, Class, is_top)

rel_lumped <- rel_all %>%
  left_join(top_classes, by = c("Day", "Treatment", "Class")) %>%
  mutate(Class = ifelse(is.na(is_top), "Others", Class)) %>%
  group_by(Day, Treatment, Class) %>%
  summarise(RelBiomass = sum(RelBiomass), .groups = "drop") %>%
  mutate(
    Day = factor(Day, levels = c(0, 4, 8, 12)),
    Treatment = factor(Treatment, levels = sort(unique(Phytoplankton_analysis$Treatment)))
  )

ggplot(rel_lumped, aes(x = Treatment, y = RelBiomass, fill = Class)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  facet_wrap(~ Day, nrow = 2, labeller = label_both, scales = "free_x") +
  scale_fill_manual(values = custom_colors, name = "Classes") +
  labs(
    title = "Relative Biomass of Leading Phytoplankton Classes in Asa (Days 0, 4, 8, 12)",
    x = "Treatments",
    y = "Relative Biomass (%)"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 11, face = "bold", lineheight = 1.2),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 10, face = "bold"),
    text = element_text(size = 11)
  )

#OVERALL IN ASA 


library(dplyr)
library(ggplot2)
library(scales)

custom_colors <- c(
  "Chrysophyceae"     = "gold",
  "Spirotrichea"      = "purple",
  "Chlorophyceae"     = "limegreen",
  "Cryptophyceae"     = "brown",
  "Cyanophyceae"      = "blue",
  "Euglenophyceae"    = "orange",
  "Baccilariophyceae" = "gray70",
  "Dinophyceae"       = "black",
  "Zygnematophyceae"  = "red",
  "Trebouxiophyceae"  = "darkgreen",
  "Choanoflagellata"  = "deeppink",
  "Oligohymenophorea" = "lightblue",
  "unidentified"      = "pink",
  "Xanthophyceae"     = "yellow3",
  "Klebsormidiaceae"  = "mediumturquoise",
  "Others"            = "white"
)

svart_all <- df %>%
  filter(Lake == "Svartberget", Day %in% c(0, 4, 8, 12))

rel_all <- svart_all %>%
  group_by(Day, Treatment, Class) %>%
  summarise(TotalBiomass = sum(Biomass), .groups = "drop") %>%
  group_by(Day, Treatment) %>%
  mutate(RelBiomass = TotalBiomass / sum(TotalBiomass)) %>%
  ungroup()

top_classes <- rel_all %>%
  group_by(Day, Treatment) %>%
  slice_max(RelBiomass, n = 4, with_ties = FALSE) %>%
  mutate(is_top = TRUE) %>%
  select(Day, Treatment, Class, is_top)

rel_lumped <- rel_all %>%
  left_join(top_classes, by = c("Day", "Treatment", "Class")) %>%
  mutate(Class = ifelse(is.na(is_top), "Others", Class)) %>%
  group_by(Day, Treatment, Class) %>%
  summarise(RelBiomass = sum(RelBiomass), .groups = "drop") %>%
  mutate(Day = factor(Day, levels = c(0, 4, 8, 12)))

ggplot(rel_lumped, aes(x = Treatment, y = RelBiomass, fill = Class)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  facet_wrap(~ Day, nrow = 2, labeller = label_both, scales = "free_x") +
  scale_fill_manual(values = custom_colors, name = "Classes") +
  labs(
    title = "Relative Biomass of Leading Phytoplankton Classes in Svartberget (Days 0, 4, 8, 12)",
    x = "Treatments",
    y = "Relative Biomass (%)"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 11, face = "bold", lineheight = 1.2),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 10, face = "bold"),
    text = element_text(size = 11)
  )

#overall relative biomass classes in Svartberget



library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(ggtext)

df <- read.csv("C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv")

asa_df <- df %>%
  filter(Lake == "Asa") %>%
  mutate(Group = substr(Treatment, 1, 1)) %>%
  group_by(Day, Treatment, Group, Class) %>%
  summarise(TotalBiomass = sum(Biomass, na.rm = TRUE), 
            Conductivity = mean(Conductivity, na.rm = TRUE),
            Temperature = mean(Temperature, na.rm = TRUE),
            DO = mean(DO, na.rm = TRUE),
            .groups = "drop")

rel_biomass_df <- asa_df %>%
  group_by(Day, Treatment) %>%
  mutate(RelativeBiomass = TotalBiomass / sum(TotalBiomass)) %>%
  ungroup()

wide_rel_biomass <- rel_biomass_df %>%
  select(Day, Treatment, Group, Class, RelativeBiomass, Conductivity, Temperature, DO) %>%
  pivot_wider(names_from = Class, values_from = RelativeBiomass, values_fill = 0)

env_data <- wide_rel_biomass %>%
  select(Treatment, Day, Group, Conductivity, Temperature, DO)

biomass_matrix <- wide_rel_biomass %>%
  select(-Treatment, -Day, -Group, -Conductivity, -Temperature, -DO)

nmds <- metaMDS(biomass_matrix, distance = "bray", trymax = 100)

fit <- envfit(nmds, env_data[, c("Conductivity", "Temperature", "DO")], perm = 999)

nmds_points <- as.data.frame(nmds$points)
nmds_points$Group <- env_data$Group
nmds_points$Day <- env_data$Day

arrow_data <- as.data.frame(fit$vectors$arrows)
arrow_data$var <- rownames(arrow_data)
arrow_data$var[arrow_data$var == "Conductivity"] <- "Cond"
colnames(arrow_data)[1:2] <- c("x", "y")

stress_val <- round(nmds$stress, 3)

ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = Group, shape = factor(Day))) +
  geom_point(size = 4, alpha = 0.9, stroke = 1.3) +
  geom_segment(data = arrow_data,
               aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(length = unit(0.25, "cm")),
               inherit.aes = FALSE,
               color = "black") +
  geom_text(data = arrow_data,
            aes(x = x * 1.15, y = y * 1.15, label = var),
            inherit.aes = FALSE,
            color = "black", size = 3.5) +
  annotate("text", 
           x = min(nmds_points$MDS1, na.rm = TRUE), 
           y = min(nmds_points$MDS2, na.rm = TRUE), 
           label = paste0("Stress = ", stress_val),
           hjust = 0, vjust = 0,
           size = 3.5, color = "gray30") +
  scale_color_manual(
    values = c("C" = "steelblue", "G" = "forestgreen", "E" = "firebrick"),
    labels = c(
      C = "<span style='color:steelblue'>Control (C)</span>",
      G = "<span style='color:forestgreen'>Gradual (G)</span>",
      E = "<span style='color:firebrick'>Extreme (E)</span>"
    ),
    guide = guide_legend(override.aes = list(shape = NA, size = 0))
  ) +
  scale_shape_manual(values = c("0" = 16, "4" = 17, "8" = 15, "12" = 4)) +
  labs(
    title = "The influence of environmental variables on relative biomass of phytoplankton organisms in Asa ",
    color = "Treatment",
    shape = " Sampling Day"
  ) +
  theme_minimal() +
  theme(
    legend.text = element_markdown(),
    legend.title = element_markdown(),
    plot.title = element_text(size = 11)
  )


#NMDS FOR THE RELATIVE BIOMASS PER CLASS IN ASA TRETAMENT AND DAY

library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(ggtext)

df <- read.csv("C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv")

svart_df <- df %>%
  filter(Lake == "Svartberget") %>%
  mutate(Group = substr(Treatment, 1, 1)) %>%
  group_by(Day, Treatment, Group, Class) %>%
  summarise(TotalBiomass = sum(Biomass, na.rm = TRUE), 
            Conductivity = mean(Conductivity, na.rm = TRUE),
            Temperature = mean(Temperature, na.rm = TRUE),
            DO = mean(DO, na.rm = TRUE),
            .groups = "drop")

rel_biomass_df <- svart_df %>%
  group_by(Day, Treatment) %>%
  mutate(RelativeBiomass = TotalBiomass / sum(TotalBiomass)) %>%
  ungroup()

wide_rel_biomass <- rel_biomass_df %>%
  select(Day, Treatment, Group, Class, RelativeBiomass, Conductivity, Temperature, DO) %>%
  pivot_wider(names_from = Class, values_from = RelativeBiomass, values_fill = 0)

env_data <- wide_rel_biomass %>%
  select(Treatment, Day, Group, Conductivity, Temperature, DO)

biomass_matrix <- wide_rel_biomass %>%
  select(-Treatment, -Day, -Group, -Conductivity, -Temperature, -DO)

nmds <- metaMDS(biomass_matrix, distance = "bray", trymax = 100)

fit <- envfit(nmds, env_data[, c("Conductivity", "Temperature", "DO")], perm = 999)

nmds_points <- as.data.frame(nmds$points)
nmds_points$Group <- env_data$Group
nmds_points$Day <- env_data$Day

arrow_data <- as.data.frame(fit$vectors$arrows)
arrow_data$var <- rownames(arrow_data)
arrow_data$var[arrow_data$var == "Conductivity"] <- "Cond"
colnames(arrow_data)[1:2] <- c("x", "y")

stress_val <- round(nmds$stress, 3)

ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = Group, shape = factor(Day))) +
  geom_point(size = 4, alpha = 0.9, stroke = 1.3) +
  geom_segment(data = arrow_data,
               aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(length = unit(0.25, "cm")),
               inherit.aes = FALSE,
               color = "black") +
  geom_text(data = arrow_data,
            aes(x = x * 1.15, y = y * 1.15, label = var),
            inherit.aes = FALSE,
            color = "black", size = 3.5) +
  annotate("text", 
           x = min(nmds_points$MDS1, na.rm = TRUE), 
           y = min(nmds_points$MDS2, na.rm = TRUE), 
           label = paste0("Stress = ", stress_val),
           hjust = 0, vjust = 0,
           size = 3.5, color = "gray30") +
  scale_color_manual(
    values = c("C" = "steelblue", "G" = "forestgreen", "E" = "firebrick"),
    labels = c(
      C = "<span style='color:steelblue'>Control (C)</span>",
      G = "<span style='color:forestgreen'>Gradual (G)</span>",
      E = "<span style='color:firebrick'>Extreme (E)</span>"
    ),
    guide = guide_legend(override.aes = list(shape = NA, size = 0))
  ) +
  scale_shape_manual(values = c("0" = 16, "4" = 17, "8" = 15, "12" = 4)) +
  labs(
    title = "The influence of environmental variables on relative biomass of phytoplankton organisms in Svartberget",
    color = "Treatment",
    shape = "Sampling Day"
  ) +
  theme_minimal() +
  theme(
    legend.text = element_markdown(),
    plot.title = element_text(size = 11)
  )


#NMDS RELATIVE BIOMASS SVARTBERGET



#COMPOSITION ANALYSIS

library(tidyverse)
library(vegan)
library(flextable)
library(webshot2)

df <- read_csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv") %>%
  mutate(Treatment = factor(substr(Treatment, 1, 1)), Day = factor(Day), Enclosure = factor(Enclosure))

run_permanova_predictor <- function(lake_name, predictor) {
  levels <- c("Class", "Taxon", "Genus", "Species")
  all_results <- data.frame()
  for (level in levels) {
    lake_df <- df %>% filter(Lake == lake_name)
    wide_df <- lake_df %>%
      group_by(Day, Enclosure, .data[[predictor]], .data[[level]]) %>%
      summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = .data[[level]], values_from = Biomass, values_fill = 0)
    meta <- wide_df %>% select(.data[[predictor]], Day)
    mat <- wide_df %>% select(where(is.numeric))
    valid_rows <- rowSums(mat) > 0
    mat <- mat[valid_rows, , drop = FALSE]
    meta <- meta[valid_rows, , drop = FALSE]
    dist <- vegdist(mat, method = "bray")
    formula <- as.formula(paste0("dist ~ ", predictor, " + Day + ", predictor, ":Day"))
    result <- adonis2(formula, data = meta, permutations = 999, by = "terms")
    result_df <- as.data.frame(result) %>%
      rownames_to_column("Effect") %>%
      filter(Effect %in% c(predictor, "Day", paste0(predictor, ":Day"))) %>%
      mutate(`Main effects and interactions` = case_when(
        Effect == predictor ~ predictor,
        Effect == "Day" ~ "Sampling day",
        Effect == paste0(predictor, ":Day") ~ paste0(predictor, " : Sampling day")),
        `F-value` = round(F, 3),
        `R²` = round(R2, 3),
        `p-value` = round(`Pr(>F)`, 3),
        Significance = case_when(`p-value` <= 0.001 ~ "***", `p-value` <= 0.01 ~ "**", `p-value` <= 0.05 ~ "*", TRUE ~ "ns"),
        Level = level) %>%
      select(Level, `Main effects and interactions`, `F-value`, `R²`, `p-value`, Significance)
    all_results <- bind_rows(all_results, result_df)
    ft <- flextable(result_df) %>%
      autofit() %>%
      fontsize(size = 10, part = "all") %>%
      font(fontname = "Times New Roman", part = "all") %>%
      bold(part = "header", bold = TRUE)
    save_as_image(ft, path = paste0(lake_name, "_", level, "_PERMANOVA_", predictor, ".png"))
  }
  all_results
}

asa_treat <- run_permanova_predictor("Asa", "Treatment")
svb_treat <- run_permanova_predictor("Svartberget", "Treatment")
asa_cond <- run_permanova_predictor("Asa", "Conductivity")
svb_cond <- run_permanova_predictor("Svartberget", "Conductivity")

asa_treat_table <- flextable(asa_treat) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)
save_as_image(asa_treat_table, path = "Asa_Global_PERMANOVA_Treatment.png")

svb_treat_table <- flextable(svb_treat) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)
save_as_image(svb_treat_table, path = "Svartberget_Global_PERMANOVA_Treatment.png")

asa_cond_table <- flextable(asa_cond) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)
save_as_image(asa_cond_table, path = "Asa_Global_PERMANOVA_Conductivity.png")

svb_cond_table <- flextable(svb_cond) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)
save_as_image(svb_cond_table, path = "Svartberget_Global_PERMANOVA_Conductivity.png")

#PERMANOVAS FOR ASA AND SVARTBERGET FOR CLASS, TAXON , GENUS AND SPECIES LEVEL

library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)

treatment_colors <- c("Control" = "steelblue", "Gradual" = "forestgreen", "Extreme" = "firebrick")

run_nmds_plot <- function(data, lake, level) {
  df <- data %>% 
    filter(Lake == lake) %>%
    mutate(
      Treatment_type = case_when(
        grepl("^C", Treatment, ignore.case = TRUE) ~ "Control",
        grepl("^G", Treatment, ignore.case = TRUE) ~ "Gradual",
        grepl("^E", Treatment, ignore.case = TRUE) ~ "Extreme",
        TRUE ~ as.character(Treatment)
      )
    )
  abundance_df <- df %>%
    group_by(Day, Treatment, .data[[level]]) %>%
    summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = {{ level }}, values_from = Biomass, values_fill = 0)
  row_meta <- abundance_df %>% select(Day, Treatment)
  mat <- abundance_df %>% select(-Day, -Treatment)
  nmds_res <- metaMDS(mat, k = 2, trymax = 100)
  stress_val <- round(nmds_res$stress, 3)
  nmds_points <- as.data.frame(nmds_res$points)
  colnames(nmds_points) <- c("NMDS1", "NMDS2")
  nmds_points$Treatment <- row_meta$Treatment
  nmds_points$Day <- as.factor(row_meta$Day)
  nmds_points$Treatment_type <- case_when(
    grepl("^C", nmds_points$Treatment, ignore.case = TRUE) ~ "Control",
    grepl("^G", nmds_points$Treatment, ignore.case = TRUE) ~ "Gradual",
    grepl("^E", nmds_points$Treatment, ignore.case = TRUE) ~ "Extreme",
    TRUE ~ as.character(nmds_points$Treatment)
  )
  ef_data <- row_meta %>%
    mutate(
      Treatment_type = case_when(
        grepl("^C", Treatment, ignore.case = TRUE) ~ "Control",
        grepl("^G", Treatment, ignore.case = TRUE) ~ "Gradual",
        grepl("^E", Treatment, ignore.case = TRUE) ~ "Extreme",
        TRUE ~ as.character(Treatment)
      ),
      Treatment_num = case_when(
        Treatment_type == "Control" ~ 1,
        Treatment_type == "Gradual" ~ 2,
        Treatment_type == "Extreme" ~ 3
      ),
      Day_num = as.numeric(Day),
      Interaction = Treatment_num * Day_num
    )
  ef <- envfit(nmds_res, ef_data[, c("Treatment_num", "Day_num", "Interaction")], permutations = 999)
  arrows <- as.data.frame(ef$vectors$arrows)
  colnames(arrows)[1:2] <- c("NMDS1", "NMDS2")
  arrows$variable <- c("treatment effect", "time effect", "interaction")
  arrow_scale <- 1.5
  arrows$NMDS1 <- arrows$NMDS1 * arrow_scale
  arrows$NMDS2 <- arrows$NMDS2 * arrow_scale
  if (tolower(level) == "taxon") {
    stress_xpos <- 0.5
    stress_ypos <- -2.0
  } else if (tolower(level) == "genus") {
    stress_xpos <- 0.5
    stress_ypos <- -1.4
  } else {
    stress_xpos <- 0.5
    stress_ypos <- -1.1
  }
  ggplot(nmds_points, aes(x = NMDS1, y = NMDS2, color = Treatment_type, shape = Day)) +
    geom_point(size = 2) +
    geom_segment(data = arrows, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.4, "cm")), inherit.aes = FALSE, color = "black", size = 0.8) +
    geom_text(data = arrows, aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = variable),
              inherit.aes = FALSE, size = 3) +
    annotate("text", x = stress_xpos, y = stress_ypos, label = paste0("stress = ", stress_val),
             size = 4) +
    scale_color_manual(values = treatment_colors) +
    labs(
      title = paste0("The effect of treatment, time and their interaction on ", 
                     tolower(level), " level in ", lake),
      x = "NMDS1",
      y = "NMDS2",
      color = "Treatment",
      shape = "Sampling day"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 11, face = "plain"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}

nmds_class_asa <- run_nmds_plot(Asa_data, "Asa", "Class")
nmds_taxon_asa <- run_nmds_plot(Asa_data, "Asa", "Taxon")
nmds_genus_asa <- run_nmds_plot(Asa_data, "Asa", "Genus")
nmds_species_asa <- run_nmds_plot(Asa_data, "Asa", "Species")

nmds_class_sv <- run_nmds_plot(Svartberget_data, "Svartberget", "Class")
nmds_taxon_sv <- run_nmds_plot(Svartberget_data, "Svartberget", "Taxon")
nmds_genus_sv <- run_nmds_plot(Svartberget_data, "Svartberget", "Genus")
nmds_species_sv <- run_nmds_plot(Svartberget_data, "Svartberget", "Species")

nmds_class_asa
nmds_taxon_asa
nmds_genus_asa
nmds_species_asa
nmds_class_sv
nmds_taxon_sv
nmds_genus_sv
nmds_species_sv
 #NMDS FOR THE EFFECT OF TREATMENT , TIME AND THEIR INTERACTION ON TAXONOMICAL LEVELS IN BOTH LAKES
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(purrr)
library(scales)
library(grid)
})

normalize_group <- function(x) {
  x <- as.character(x)
  case_when(
    grepl("^C", x, ignore.case = TRUE) ~ "Control",
    grepl("^G", x, ignore.case = TRUE) ~ "Gradual",
    grepl("^E", x, ignore.case = TRUE) ~ "Extreme",
    x %in% c("Control","Gradual","Extreme") ~ x,
    TRUE ~ NA_character_
  )
}

pairwise_similarity_with_p <- function(df, level_col, value_col = "Biomass") {
  df <- df %>%
    mutate(Day = as.integer(Day),
           Unit = as.character(Treatment),
           Group = normalize_group(Treatment)) %>%
    filter(!is.na(Group))
  days <- sort(unique(df$Day))
  map_dfr(days, function(d) {
    wide <- df %>%
      filter(Day == d) %>%
      group_by(Unit, Group, .data[[level_col]]) %>%
      summarise(value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = all_of(level_col), values_from = value, values_fill = 0) %>%
      arrange(Unit)
    if (nrow(wide) < 2) {
      tibble(Day = d,
             Comparison = c("Gradual vs Control","Extreme vs Gradual","Extreme vs Control"),
             SIM = NA_real_, p = NA_real_)
    } else {
      mat <- as.matrix(wide %>% select(-Unit, -Group))
      rs <- rowSums(mat); rs[rs == 0] <- 1
      mat <- sweep(mat, 1, rs, "/")
      meta <- wide %>% select(Unit, Group)
      pairs <- list(c("Gradual","Control"), c("Extreme","Gradual"), c("Extreme","Control"))
      out <- lapply(pairs, function(pair) {
        idx <- meta$Group %in% pair
        if (sum(idx) < 2 || length(unique(meta$Group[idx])) < 2) return(c(NA_real_, NA_real_))
        centroids <- t(sapply(pair, function(g) colMeans(mat[meta$Group == g, , drop = FALSE], na.rm = TRUE)))
        rownames(centroids) <- pair
        sim <- 1 - as.matrix(vegdist(centroids, method = "bray"))[pair[1], pair[2]]
        dsub <- vegdist(mat[idx, , drop = FALSE], method = "bray")
        grp <- factor(meta$Group[idx], levels = pair)
        a <- adonis2(dsub ~ grp, permutations = 999)
        pval <- a$`Pr(>F)`[1]
        c(sim, pval)
      })
      tibble(
        Day = d,
        Comparison = c("Gradual vs Control","Extreme vs Gradual","Extreme vs Control"),
        SIM = as.numeric(sapply(out, function(z) z[1])),
        p   = as.numeric(sapply(out, function(z) z[2]))
      )
    }
  })
}

p_to_stars_only <- function(p) {
  ifelse(is.na(p) | p >= 0.05, "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**", "*")))
}

heatmap_sim_with_stars <- function(res, file_out = NULL, day_order = c(0,4,8,12)) {
  res <- res %>%
    mutate(
      Comparison = dplyr::recode(Comparison,
                                 "Extreme vs Control" = "E vs C",
                                 "Extreme vs Gradual" = "E vs G",
                                 "Control vs Gradual" = "C vs G",
                                 "Gradual vs Control" = "C vs G"),
      Comparison = factor(Comparison, levels = c("C vs G","E vs G","E vs C")),
      Day = factor(as.character(Day), levels = as.character(day_order)),
      lab = p_to_stars_only(p)
    )
  p <- ggplot(res, aes(x = Day, y = Comparison, fill = SIM)) +
    geom_tile() +
    geom_text(aes(label = lab), size = 6, color = "black") +
    scale_fill_gradientn(
      colors = c("#fff066","#ffcc00","#ff6600","#e60000"),
      values = scales::rescale(c(0,0.33,0.66,1)),
      limits = c(0,1),
      breaks = c(0,0.5,1),
      labels = c("0","0.5","1"),
      name = "Community similarity",
      oob = scales::squish
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(1,"cm"),
      legend.key.height = unit(0.22,"cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold")
    )
  if (!is.null(file_out)) ggsave(file_out, p, width = 10, height = 7, dpi = 300, bg = "white")
  if (!interactive()) print(p)
  p
}

asa_genus <- pairwise_similarity_with_p(Asa_data, level_col = "Genus", value_col = "Biomass")
asa_taxon <- pairwise_similarity_with_p(Asa_data, level_col = "Taxon", value_col = "Biomass")
sv_genus  <- pairwise_similarity_with_p(Svartberget_data, level_col = "Genus", value_col = "Biomass")
sv_taxon  <- pairwise_similarity_with_p(Svartberget_data, level_col = "Taxon", value_col = "Biomass")

p1 <- heatmap_sim_with_stars(asa_genus, "asa_genus_similarity_heatmap.png", day_order = c(0,4,8,12))
p2 <- heatmap_sim_with_stars(asa_taxon, "asa_taxon_similarity_heatmap.png", day_order = c(0,4,8,12))
p3 <- heatmap_sim_with_stars(sv_genus,  "svartberget_genus_similarity_heatmap.png", day_order = c(0,4,8,12))
p4 <- heatmap_sim_with_stars(sv_taxon,  "svartberget_taxon_similarity_heatmap.png", day_order = c(0,4,8,12))

print(p1); print(p2); print(p3); print(p4)
#HEATMAPS OF SIMILARITY FOR TAXON AND GENUS LEVEL IN ASA AND SVARTBERGET

library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)

treatment_colors <- c("Control" = "steelblue", "Gradual" = "forestgreen", "Extreme" = "firebrick")

data <- read.csv("Phytoplankton_analysis.csv")

run_nmds_plot <- function(data, lake, level) {
  df <- data %>%
    filter(Lake == lake) %>%
    mutate(
      Treatment_group = case_when(
        grepl("^C", Treatment, ignore.case = TRUE) ~ "Control",
        grepl("^G", Treatment, ignore.case = TRUE) ~ "Gradual",
        grepl("^E", Treatment, ignore.case = TRUE) ~ "Extreme",
        TRUE ~ as.character(Treatment)
      )
    )
  
  abundance_df <- df %>%
    group_by(Treatment, Conductivity, Day, .data[[level]]) %>%
    summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = {{ level }}, values_from = Biomass, values_fill = 0)
  
  meta_info <- abundance_df %>%
    select(Treatment, Conductivity, Day)
  
  mat <- abundance_df %>%
    select(-Treatment, -Conductivity, -Day)
  
  nmds_res <- metaMDS(as.matrix(mat), k = 2, trymax = 100)
  stress_val <- round(nmds_res$stress, 3)
  
  nmds_points <- as.data.frame(nmds_res$points) %>%
    mutate(row = row_number()) %>%
    bind_cols(meta_info) %>%
    mutate(
      Treatment_group = case_when(
        grepl("^C", Treatment, ignore.case = TRUE) ~ "Control",
        grepl("^G", Treatment, ignore.case = TRUE) ~ "Gradual",
        grepl("^E", Treatment, ignore.case = TRUE) ~ "Extreme",
        TRUE ~ as.character(Treatment)
      )
    )
  
  ef_data <- nmds_points %>%
    mutate(
      Conductivity_num = as.numeric(Conductivity),
      Day_num = as.numeric(Day),
      Interaction = Conductivity_num * Day_num
    ) %>%
    select(Conductivity_num, Day_num, Interaction)
  
  ef <- envfit(nmds_res, ef_data, permutations = 999, na.rm = TRUE)
  
  arrows <- as.data.frame(ef$vectors$arrows)
  colnames(arrows)[1:2] <- c("NMDS1", "NMDS2")
  arrows$variable <- c("cond effect", "time effect", "interaction")
  arrow_scale <- 1.5
  arrows$NMDS1 <- arrows$NMDS1 * arrow_scale
  arrows$NMDS2 <- arrows$NMDS2 * arrow_scale
  
  if (lake == "Svartberget" & tolower(level) == "taxon") {
    stress_xpos <- 1.5
    stress_ypos <- -1.8
  } else if (lake == "Svartberget" & tolower(level) == "species") {
    stress_xpos <- 0.5
    stress_ypos <- -1.5
  } else if (tolower(level) == "taxon") {
    stress_xpos <- 1.0
    stress_ypos <- -1.8
  } else if (tolower(level) == "genus") {
    stress_xpos <- 1.0
    stress_ypos <- -1.6
  } else {
    stress_xpos <- 0.5
    stress_ypos <- -1.1
  }
  
  ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = Treatment_group, shape = as.factor(Day))) +
    geom_point(size = 2) +
    geom_segment(data = arrows, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.4, "cm")), inherit.aes = FALSE, color = "black", size = 0.8) +
    geom_text(data = arrows, aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = variable),
              inherit.aes = FALSE, size = 3) +
    annotate("text", x = stress_xpos, y = stress_ypos, label = paste0("stress = ", stress_val),
             size = 4) +
    scale_color_manual(values = treatment_colors) +
    labs(
      title = paste0("The effect of conductivity, time and their interaction on ", 
                     tolower(level), " level in ", lake),
      x = "NMDS1",
      y = "NMDS2",
      color = "Treatment",
      shape = "Sampling day"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 11, face = "plain"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}

nmds_class_asa <- run_nmds_plot(data, "Asa", "Class")
nmds_taxon_asa <- run_nmds_plot(data, "Asa", "Taxon")
nmds_genus_asa <- run_nmds_plot(data, "Asa", "Genus")
nmds_species_asa <- run_nmds_plot(data, "Asa", "Species")

nmds_class_sv <- run_nmds_plot(data, "Svartberget", "Class")
nmds_taxon_sv <- run_nmds_plot(data, "Svartberget", "Taxon")
nmds_genus_sv <- run_nmds_plot(data, "Svartberget", "Genus")
nmds_species_sv <- run_nmds_plot(data, "Svartberget", "Species")

nmds_class_asa
nmds_taxon_asa
nmds_genus_asa
nmds_species_asa
nmds_class_sv
nmds_taxon_sv
nmds_genus_sv
nmds_species_sv

#NMDS FOR THE EFFEC OF CONDUCTIVITY, TIME AND THEIR INTERACTION IN COMPOSITION LEVEL

library(tidyverse)
library(readr)
library(flextable)
library(webshot2)

data_path <- "C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv"

df <- read_csv(data_path, locale = locale(encoding = "UTF-8")) %>%
  mutate(
    Treatment = factor(substr(as.character(Treatment), 1, 1), levels = c("C","G","E")),
    Day = as.integer(Day),
    Class = str_squish(Class),
    Taxon = str_squish(Taxon),
    Biomass = as.numeric(Biomass)
  )

chunk_df <- function(x, n) {
  if (nrow(x) == 0) return(list())
  splits <- split(x, ceiling(seq_len(nrow(x)) / n))
  lapply(splits, function(d) { rownames(d) <- NULL; d })
}

make_tables_by_lake_day_core <- function(lake_name, day_value, trt_letter, out_dir = "tables_by_day_20_by_lake") {
  day_folder <- file.path(out_dir, paste0("Day_", day_value), lake_name)
  dir.create(day_folder, recursive = TRUE, showWarnings = FALSE)
  tab <- df %>%
    filter(Lake == lake_name, Day == day_value, Treatment == trt_letter) %>%
    group_by(Lake, Class, Taxon) %>%
    summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
    filter(Biomass > 0) %>%
    arrange(desc(Biomass), Class, Taxon) %>%
    mutate(
      `Sampling Day` = day_value,
      Biomass = formatC(Biomass, format = "f", digits = 3)
    ) %>%
    select(Lake, `Sampling Day`, Class, Taxon, Biomass)
  if (nrow(tab) == 0) return(invisible(NULL))
  pages <- chunk_df(tab, 20)
  for (i in seq_along(pages)) {
    page_df <- pages[[i]]
    ft <- flextable(page_df)
    ft <- merge_v(ft, j = ~ Lake + `Sampling Day` + Class)
    ft <- valign(ft, j = ~ Lake + `Sampling Day` + Class, valign = "top")
    ft <- align(ft, align = "left", part = "all")
    ft <- font(ft, part = "all", fontname = "Times New Roman")
    ft <- bold(ft, part = "header", bold = TRUE)
    ft <- bg(ft, part = "all", bg = "white")
    ft <- autofit(ft)
    save_as_image(ft, path = file.path(day_folder, paste0(lake_name, "_Day", day_value, "_", trt_letter, "_taxa_part_", i, ".png")))
  }
}

make_tables_by_lake_day <- function(lake_name, day_value, out_dir = "tables_by_day_20_by_lake") {
  for (trt in c("G","E")) make_tables_by_lake_day_core(lake_name, day_value, trt, out_dir)
}

make_tables_selected_days <- function(out_dir = "tables_by_day_20_by_lake") {
  lakes <- unique(df$Lake)
  days <- intersect(sort(unique(df$Day)), c(0,4,8,12))
  for (lk in lakes) for (dy in days) make_tables_by_lake_day(lk, dy, out_dir)
}

make_tables_selected_days()

#TAXA WHICH ARE FOUND IN EXTREME AND GRADUAL TREATMENTS EACH DAY IN ASA AND SVARTBERGET
library(tidyverse)
library(readr)
library(flextable)
library(webshot2)

data_path <- "C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv"

df <- read_csv(data_path, locale = locale(encoding = "UTF-8")) %>%
  mutate(
    Treatment = factor(substr(as.character(Treatment), 1, 1), levels = c("C","G","E")),
    Day = as.integer(Day),
    Class = str_squish(Class),
    Taxon = str_squish(Taxon),
    Biomass = as.numeric(Biomass)
  )

chunk_df <- function(x, n) {
  if (nrow(x) == 0) return(list())
  splits <- split(x, ceiling(seq_len(nrow(x)) / n))
  lapply(splits, function(d) { rownames(d) <- NULL; d })
}

make_tables_by_lake_day_core_control <- function(lake_name, day_value, out_dir = "tables_by_day_20_by_lake_control") {
  day_folder <- file.path(out_dir, paste0("Day_", day_value), lake_name)
  dir.create(day_folder, recursive = TRUE, showWarnings = FALSE)
  tab <- df %>%
    filter(Lake == lake_name, Day == day_value, Treatment == "C") %>%
    group_by(Lake, Class, Taxon) %>%
    summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
    filter(Biomass > 0) %>%
    arrange(desc(Biomass), Class, Taxon) %>%
    mutate(
      `Sampling Day` = day_value,
      Biomass = formatC(Biomass, format = "f", digits = 3)
    ) %>%
    select(Lake, `Sampling Day`, Class, Taxon, Biomass)
  if (nrow(tab) == 0) return(invisible(NULL))
  pages <- chunk_df(tab, 20)
  for (i in seq_along(pages)) {
    page_df <- pages[[i]]
    ft <- flextable(page_df)
    ft <- merge_v(ft, j = ~ Lake + `Sampling Day` + Class)
    ft <- valign(ft, j = ~ Lake + `Sampling Day` + Class, valign = "top")
    ft <- align(ft, align = "left", part = "all")
    ft <- font(ft, part = "all", fontname = "Times New Roman")
    ft <- bold(ft, part = "header", bold = TRUE)
    ft <- bg(ft, part = "all", bg = "white")
    ft <- autofit(ft)
    save_as_image(ft, path = file.path(day_folder, paste0(lake_name, "_Day", day_value, "_C_taxa_part_", i, ".png")))
  }
}

make_tables_by_lake_day_control <- function(lake_name, day_value, out_dir = "tables_by_day_20_by_lake_control") {
  make_tables_by_lake_day_core_control(lake_name, day_value, out_dir)
}

make_tables_selected_days_control <- function(out_dir = "tables_by_day_20_by_lake_control") {
  lakes <- unique(df$Lake)
  days <- intersect(sort(unique(df$Day)), c(0,4,8,12))
  for (lk in lakes) for (dy in days) make_tables_by_lake_day_control(lk, dy, out_dir)
}

make_tables_selected_days_control()
#TAXA WHICH ARE FOUND IN CONTROL TRATMENTS FOR EACH DAY IN ASA AND SVARTBERGET
library(tidyverse)
library(readr)
library(flextable)
library(webshot2)

data_path <- "C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv"

df <- read_csv(data_path, locale = locale(encoding = "UTF-8")) %>%
  mutate(
    Treatment = factor(substr(as.character(Treatment), 1, 1), levels = c("C","G","E")),
    Day = as.integer(Day),
    Class = str_squish(Class),
    Taxon = str_squish(Taxon),
    Biomass = as.numeric(Biomass)
  )

chunk_rows <- function(x, rows_per_table = 20) {
  if (nrow(x) == 0) return(list())
  splits <- split(x, ceiling(seq_len(nrow(x)) / rows_per_table))
  lapply(splits, function(d) { rownames(d) <- NULL; d })
}

make_class_taxa_lists_20 <- function(out_dir = "class_taxa_lists_by_day_treatment_lake_max20") {
  lakes <- unique(df$Lake)
  days <- intersect(sort(unique(df$Day)), c(0,4,8,12))
  trts <- c("C","G","E")
  for (lk in lakes) {
    for (dy in days) {
      for (tr in trts) {
        folder <- file.path(out_dir, paste0("Day_", dy), tr, lk)
        dir.create(folder, recursive = TRUE, showWarnings = FALSE)
        subdf <- df %>% filter(Lake == lk, Day == dy, Treatment == tr)
        if (nrow(subdf) == 0) next
        class_rel <- subdf %>%
          group_by(Class) %>%
          summarise(class_biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
          mutate(class_rel = class_biomass / sum(class_biomass, na.rm = TRUE)) %>%
          mutate(class_rel = replace_na(class_rel, 0))
        tbl <- subdf %>%
          filter(!is.na(Class), !is.na(Taxon), Taxon != "") %>%
          distinct(Class, Taxon) %>%
          left_join(class_rel %>% select(Class, class_rel), by = "Class") %>%
          arrange(desc(class_rel), Class, Taxon) %>%
          mutate(
            Lake = lk,
            `Sampling Day` = dy,
            `Treatment group` = tr
          ) %>%
          select(Lake, `Sampling Day`, `Treatment group`, Class, Taxon)
        if (nrow(tbl) == 0) next
        pages <- chunk_rows(tbl, rows_per_table = 20)
        for (i in seq_along(pages)) {
          ft <- flextable(pages[[i]])
          ft <- merge_v(ft, j = ~ Lake + `Sampling Day` + `Treatment group` + Class)
          ft <- valign(ft, j = ~ Lake + `Sampling Day` + `Treatment group` + Class, valign = "top")
          ft <- align(ft, align = "left", part = "all")
          ft <- font(ft, part = "all", fontname = "Times New Roman")
          ft <- bold(ft, part = "header", bold = TRUE)
          ft <- bg(ft, part = "all", bg = "white")
          ft <- autofit(ft)
          save_as_image(ft, path = file.path(folder, paste0("Classes_Taxa_", lk, "_Day", dy, "_", tr, "_part_", i, ".png")))
        }
      }
    }
  }
}

make_class_taxa_lists_20()

#TABLES WITH CORRESPODENCE BETWEEN TAXA AND CLASSES PER TREATMENT AND DAY WITHOUT BIOMASS VALUES

#END OF COMPOSITION ANALYSIS 


library(dplyr)
library(tidyr)
library(lavaan)
library(DiagrammeR)

df <- read.csv("C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv")

asa_df <- df %>%
  filter(Lake == "Asa") %>%
  group_by(Day, Treatment) %>%
  summarise(TotalBiomass = sum(Biomass, na.rm = TRUE),
            Conductivity = mean(Conductivity, na.rm = TRUE),
            .groups = "drop")

svart_df <- df %>%
  filter(Lake == "Svartberget") %>%
  group_by(Day, Treatment) %>%
  summarise(TotalBiomass = sum(Biomass, na.rm = TRUE),
            Conductivity = mean(Conductivity, na.rm = TRUE),
            .groups = "drop")

model <- '
  TotalBiomass ~ a * Conductivity
'

asa_fit <- sem(model, data = asa_df)
svart_fit <- sem(model, data = svart_df)

asa_est <- parameterEstimates(asa_fit, standardized = TRUE) %>%
  filter(lhs == "TotalBiomass", rhs == "Conductivity")
svart_est <- parameterEstimates(svart_fit, standardized = TRUE) %>%
  filter(lhs == "TotalBiomass", rhs == "Conductivity")

asa_r2 <- round(inspect(asa_fit, "rsquare")["TotalBiomass"], 3)
svart_r2 <- round(inspect(svart_fit, "rsquare")["TotalBiomass"], 3)

asa_signif <- ifelse(asa_est$pvalue < 0.001, "***",
                     ifelse(asa_est$pvalue < 0.01, "**",
                            ifelse(asa_est$pvalue < 0.05, "*", "ns")))
svart_signif <- ifelse(svart_est$pvalue < 0.001, "***",
                       ifelse(svart_est$pvalue < 0.01, "**",
                              ifelse(svart_est$pvalue < 0.05, "*", "ns")))

asa_label <- paste0("Effect = ", round(asa_est$std.all, 3), "\n",
                    asa_signif, "\n",
                    "R² = ", asa_r2)

svart_label <- paste0("Effect = ", round(svart_est$std.all, 3), "\n",
                      svart_signif, "\n",
                      "R² = ", svart_r2)

grViz(paste0("
digraph SEM_results {
  graph [layout = dot, rankdir = LR]
  node [shape = box, fontsize = 12, style=filled, fillcolor=lightgrey]

  ASA [label='Asa\n", asa_label, "']
  SVART [label='Svartberget\n", svart_label, "']

  Conductivity -> ASA [label='']
  Conductivity -> SVART [label='']

  labelloc = 't'
  label = 'Direct effects of Conductivity on Total Biomass'
}
"))

#CONDUCTIVITY VS TOTAL BOMASS SEM
library(tidyverse)
library(ape)
library(picante)
library(ggtext)

Asa_data <- read.csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")
Asa_data <- Asa_data %>%
  filter(!is.na(Species)) %>%
  mutate(
    Species_ID = paste(Class, Taxon, Genus, Species, sep = "_"),
    SiteID = paste(Treatment, Day, sep = "_")
  )

comm_matrix <- Asa_data %>%
  group_by(SiteID, Species_ID) %>%
  summarise(Abundance = sum(Counts), .groups = "drop") %>%
  pivot_wider(names_from = Species_ID, values_from = Abundance, values_fill = 0) %>%
  column_to_rownames("SiteID")

taxonomy <- Asa_data %>%
  select(Species_ID, Genus, Taxon, Class) %>%
  distinct() %>%
  column_to_rownames("Species_ID") %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

tax_dist <- dist(taxonomy)
tax_clust <- hclust(tax_dist, method = "average")
pseudo_tree <- as.phylo(tax_clust)

matched <- match.phylo.comm(pseudo_tree, comm_matrix)
tree <- matched$phy
comm <- matched$comm

pd_result <- pd(comm, tree, include.root = TRUE) %>%
  rownames_to_column("SiteID") %>%
  separate(SiteID, into = c("Treatment", "Day"), sep = "_") %>%
  mutate(
    Day = as.numeric(Day),
    Treatment_group = case_when(
      str_starts(Treatment, "C") ~ "Control",
      str_starts(Treatment, "G") ~ "Gradual",
      str_starts(Treatment, "E") ~ "Extreme",
      TRUE ~ NA_character_
    )
  )

pd_result <- pd_result %>%
  mutate(Treatment_group = factor(Treatment_group, levels = c("Control", "Extreme", "Gradual")))

pd_summary <- pd_result %>%
  group_by(Day, Treatment_group) %>%
  summarise(
    mean_PD = mean(PD),
    sd_PD = sd(PD),
    .groups = "drop"
  )

labels_colored <- c(
  "Control" = "<span style='color:steelblue'><b>Control</b></span>",
  "Extreme" = "<span style='color:firebrick'><b>Extreme</b></span>",
  "Gradual" = "<span style='color:darkgreen'><b>Gradual</b></span>"
)

ggplot() +
  geom_boxplot(
    data = pd_result,
    aes(x = as.factor(Day), y = PD, fill = Treatment_group),
    width = 0.6,
    stat = "boxplot",
    fatten = 0,
    outlier.shape = NA,
    coef = 0
  ) +
  geom_errorbar(
    data = pd_summary,
    aes(x = as.factor(Day), ymin = mean_PD - sd_PD, ymax = mean_PD + sd_PD, group = Treatment_group),
    position = position_dodge(width = 0.6),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Control" = "steelblue", "Gradual" = "darkgreen", "Extreme" = "firebrick"),
    labels = labels_colored,
    guide = guide_legend(override.aes = list(fill = NA))
  ) +
  labs(
    title = "Faith's Phylogenetic Diversity in Asa",
    x = "Experimental Day",
    y = "Faith's PD",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_markdown(size = 11, margin = margin(t = 2))
  )

#FAITH'S PHYLOGENETIC DIVERSITY IN ASA 

library(tidyverse)
library(ape)
library(picante)
library(ggtext)

Asa_data <- read.csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")

asa_data <- Asa_data %>%
  filter(Lake == "Asa", !is.na(Species)) %>%
  mutate(
    Species_ID = paste(Class, Taxon, Genus, Species, sep = "_"),
    GroupID = paste(Treatment, Day, sep = "_")
  )

comm_matrix <- asa_data %>%
  group_by(GroupID, Species_ID) %>%
  summarise(Abundance = sum(Counts), .groups = "drop") %>%
  pivot_wider(names_from = Species_ID, values_from = Abundance, values_fill = 0) %>%
  column_to_rownames("GroupID")

taxonomy <- asa_data %>%
  select(Species_ID, Genus, Taxon, Class) %>%
  distinct() %>%
  column_to_rownames("Species_ID") %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

tax_dist <- dist(taxonomy)
tax_clust <- hclust(tax_dist, method = "average")
pseudo_tree <- as.phylo(tax_clust)

matched <- match.phylo.comm(pseudo_tree, comm_matrix)
tree <- matched$phy
comm <- matched$comm

psv_result <- psv(comm, tree) %>%
  rownames_to_column("GroupID") %>%
  separate(GroupID, into = c("Treatment", "Day"), sep = "_") %>%
  mutate(
    Day = as.numeric(Day),
    Treatment_group = case_when(
      str_starts(Treatment, "C") ~ "Control",
      str_starts(Treatment, "G") ~ "Gradual",
      str_starts(Treatment, "E") ~ "Extreme",
      TRUE ~ NA_character_
    ),
    Treatment_group = factor(Treatment_group, levels = c("Control", "Extreme", "Gradual"))
  )

psv_summary <- psv_result %>%
  group_by(Day, Treatment_group) %>%
  summarise(
    mean_PSV = mean(PSVs),
    sd_PSV = sd(PSVs),
    .groups = "drop"
  )

ggplot() +
  geom_boxplot(
    data = psv_result,
    aes(x = as.factor(Day), y = PSVs, fill = Treatment_group),
    width = 0.6,
    stat = "boxplot",
    fatten = 0,
    outlier.shape = NA,
    coef = 0
  ) +
  geom_errorbar(
    data = psv_summary,
    aes(x = as.factor(Day), ymin = mean_PSV - sd_PSV, ymax = mean_PSV + sd_PSV, group = Treatment_group),
    position = position_dodge(width = 0.6),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Control" = "steelblue", "Gradual" = "darkgreen", "Extreme" = "firebrick"),
    labels = c(
      "Control" = "<span style='color:steelblue'><b>Control</b></span>",
      "Extreme" = "<span style='color:firebrick'><b>Extreme</b></span>",
      "Gradual" = "<span style='color:darkgreen'><b>Gradual</b></span>"
    ),
    guide = guide_legend(override.aes = list(fill = NA))
  ) +
  labs(
    title = "Phylogenetic Species Variability (PSV) in Asa",
    x = "Experimental Day",
    y = "PSV",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_markdown(size = 11, margin = margin(t = 2))
  )

library(openxlsx)

psv_long <- comm %>%
  rownames_to_column("GroupID") %>%
  pivot_longer(-GroupID, names_to = "Species_ID", values_to = "Abundance") %>%
  filter(Abundance > 0)

psv_long <- psv_long %>%
  separate(GroupID, into = c("Treatment", "Day"), sep = "_") %>%
  mutate(Day = as.numeric(Day)) %>%
  left_join(
    psv_result %>% 
      select(Treatment, Day, PSVs, Treatment_group),
    by = c("Treatment", "Day")
  ) %>%
  left_join(
    asa_data %>%
      select(Species_ID, Class, Taxon, Genus, Species) %>%
      distinct(),
    by = "Species_ID"
  ) %>%
  relocate(Day, Treatment, Treatment_group, Species_ID, Class, Taxon, Genus, Species, Abundance, PSVs)

write.xlsx(psv_long, file = "PSV_taxa_table_Asa.xlsx", overwrite = TRUE)

#PHYLOGENETIC DIVERSITY AND PHYLOGENETIC VARIABILITY

library(tidyverse)
library(ape)
library(picante)
library(ggtext)


data <- read.csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")


Svartberget_data <- data %>%
  filter(Lake == "Svartberget") %>%
  filter(!is.na(Species)) %>%
  mutate(
    Species_ID = paste(Class, Taxon, Genus, Species, sep = "_"),
    SiteID = paste(Treatment, Day, sep = "_")
  )


comm_matrix <- Svartberget_data %>%
  group_by(SiteID, Species_ID) %>%
  summarise(Abundance = sum(Counts), .groups = "drop") %>%
  pivot_wider(names_from = Species_ID, values_from = Abundance, values_fill = 0) %>%
  column_to_rownames("SiteID")

taxonomy <- Svartberget_data %>%
  select(Species_ID, Genus, Taxon, Class) %>%
  distinct() %>%
  column_to_rownames("Species_ID") %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

tax_dist <- dist(taxonomy)
tax_clust <- hclust(tax_dist, method = "average")
pseudo_tree <- as.phylo(tax_clust)

matched <- match.phylo.comm(pseudo_tree, comm_matrix)
tree <- matched$phy
comm <- matched$comm


pd_result <- pd(comm, tree, include.root = TRUE) %>%
  rownames_to_column("SiteID") %>%
  separate(SiteID, into = c("Treatment", "Day"), sep = "_") %>%
  mutate(
    Day = as.numeric(Day),
    Treatment_group = case_when(
      str_starts(Treatment, "C") ~ "Control",
      str_starts(Treatment, "G") ~ "Gradual",
      str_starts(Treatment, "E") ~ "Extreme",
      TRUE ~ NA_character_
    )
  )

pd_result <- pd_result %>%
  mutate(Treatment_group = factor(Treatment_group, levels = c("Control", "Extreme", "Gradual")))

pd_summary <- pd_result %>%
  group_by(Day, Treatment_group) %>%
  summarise(
    mean_PD = mean(PD),
    sd_PD = sd(PD),
    .groups = "drop"
  )

labels_colored <- c(
  "Control" = "<span style='color:steelblue'><b>Control</b></span>",
  "Extreme" = "<span style='color:firebrick'><b>Extreme</b></span>",
  "Gradual" = "<span style='color:darkgreen'><b>Gradual</b></span>"
)


ggplot() +
  geom_boxplot(
    data = pd_result,
    aes(x = as.factor(Day), y = PD, fill = Treatment_group),
    width = 0.6,
    stat = "boxplot",
    fatten = 0,
    outlier.shape = NA,
    coef = 0
  ) +
  geom_errorbar(
    data = pd_summary,
    aes(x = as.factor(Day), ymin = mean_PD - sd_PD, ymax = mean_PD + sd_PD, group = Treatment_group),
    position = position_dodge(width = 0.6),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Control" = "steelblue", "Gradual" = "darkgreen", "Extreme" = "firebrick"),
    labels = labels_colored,
    guide = guide_legend(override.aes = list(fill = NA))
  ) +
  labs(
    title = "Faith's Phylogenetic Diversity in Svartberget",
    x = "Experimental Day",
    y = "Faith's PD",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_markdown(size = 11, margin = margin(t = 2)),
    legend.key.size = unit(0, "cm")
  )

#FAITH'S PHYLOGENETIC DIVERSITY IN SVARTBERGET

library(tidyverse)
library(ape)
library(picante)
library(ggtext)
library(openxlsx)
library(writexl)

svart_data <- Asa_data %>%
  filter(Lake == "Svartberget", !is.na(Species)) %>%
  mutate(
    Species_ID = paste(Class, Taxon, Genus, Species, sep = "_"),
    GroupID = paste(Treatment, Day, sep = "_")
  )

comm_matrix_svart <- svart_data %>%
  group_by(GroupID, Species_ID) %>%
  summarise(Abundance = sum(Counts), .groups = "drop") %>%
  pivot_wider(names_from = Species_ID, values_from = Abundance, values_fill = 0) %>%
  column_to_rownames("GroupID")

taxonomy_svart <- svart_data %>%
  select(Species_ID, Genus, Taxon, Class) %>%
  distinct() %>%
  column_to_rownames("Species_ID") %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

tax_dist_svart <- dist(taxonomy_svart)
tax_clust_svart <- hclust(tax_dist_svart, method = "average")
pseudo_tree_svart <- as.phylo(tax_clust_svart)

matched_svart <- match.phylo.comm(pseudo_tree_svart, comm_matrix_svart)
tree_svart <- matched_svart$phy
comm_svart <- matched_svart$comm

psv_result_svart <- psv(comm_svart, tree_svart) %>%
  rownames_to_column("GroupID") %>%
  separate(GroupID, into = c("Treatment", "Day"), sep = "_") %>%
  mutate(
    Day = as.numeric(Day),
    Treatment_group = case_when(
      str_starts(Treatment, "C") ~ "Control",
      str_starts(Treatment, "G") ~ "Gradual",
      str_starts(Treatment, "E") ~ "Extreme",
      TRUE ~ NA_character_
    ),
    Treatment_group = factor(Treatment_group, levels = c("Control", "Extreme", "Gradual"))
  )

psv_summary_svart <- psv_result_svart %>%
  group_by(Day, Treatment_group) %>%
  summarise(
    mean_PSV = round(mean(PSVs), 4),
    sd_PSV = round(sd(PSVs), 4),
    min_PSV = round(min(PSVs), 4),
    max_PSV = round(max(PSVs), 4),
    .groups = "drop"
  )

print(psv_summary_svart)

write.xlsx(psv_long, file = "PSV_taxa_table_Svartberget.xlsx", overwrite = TRUE)
psv_long_svart <- comm_svart %>%
  rownames_to_column("GroupID") %>%
  pivot_longer(-GroupID, names_to = "Species_ID", values_to = "Abundance") %>%
  filter(Abundance > 0)

psv_long_svart <- psv_long_svart %>%
  separate(GroupID, into = c("Treatment", "Day"), sep = "_") %>%
  mutate(Day = as.numeric(Day)) %>%
  left_join(
    psv_result_svart %>% 
      select(Treatment, Day, PSVs, Treatment_group),
    by = c("Treatment", "Day")
  ) %>%
  left_join(
    svart_data %>%
      select(Species_ID, Class, Taxon, Genus, Species) %>%
      distinct(),
    by = "Species_ID"
  ) %>%
  relocate(Day, Treatment, Treatment_group, Species_ID, Class, Taxon, Genus, Species, Abundance, PSVs)

write.xlsx(psv_long_svart, file = "PSV_taxa_table_Svartberget.xlsx", overwrite = TRUE)
ggplot() +
  geom_boxplot(
    data = psv_result_svart,
    aes(x = as.factor(Day), y = PSVs, fill = Treatment_group),
    width = 0.6,
    stat = "boxplot",
    fatten = 0,
    outlier.shape = NA,
    coef = 0
  ) +
  geom_errorbar(
    data = psv_summary_svart,
    aes(x = as.factor(Day), ymin = mean_PSV - sd_PSV, ymax = mean_PSV + sd_PSV, group = Treatment_group),
    position = position_dodge(width = 0.6),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Control" = "steelblue", "Gradual" = "darkgreen", "Extreme" = "firebrick"),
    labels = c(
      "Control" = "<span style='color:steelblue'><b>Control</b></span>",
      "Extreme" = "<span style='color:firebrick'><b>Extreme</b></span>",
      "Gradual" = "<span style='color:darkgreen'><b>Gradual</b></span>"
    ),
    guide = guide_legend(override.aes = list(fill = NA))
  ) +
  labs(
    title = "Phylogenetic Species Variability (PSV) in Svartberget",
    x = "Experimental Day",
    y = "PSV",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_markdown(size = 11, margin = margin(t = 2))
  )

#psv in svartberget
library(dplyr)
library(rstatix)
library(flextable)

pd_result_svb <- pd_result
psv_result_svb <- psv_result_svart

pd_result <- pd_result %>%
  mutate(PD_sqrt = sqrt(PD))

psv_result <- psv_result %>%
  mutate(PSVs_sqrt = sqrt(PSVs))

pd_result_svb <- pd_result_svb %>%
  mutate(PD_sqrt = sqrt(PD))

psv_result_svb <- psv_result_svb %>%
  mutate(PSVs_sqrt = sqrt(PSVs))

asa_table_data <- data.frame(
  Lake = "Asa",
  Metric = c("PD", "PD", "PD", "PSV", "PSV", "PSV"),
  Test = c("Treatment", "Sampling day", "Treatment*Sampling day", "Treatment", "Sampling day", "Treatment*Sampling day"),
  p_value = round(c(
    anova_test(pd_result, dv = PD_sqrt, between = c(Treatment_group, Day))$p[1],
    anova_test(pd_result, dv = PD_sqrt, between = c(Treatment_group, Day))$p[2],
    anova_test(pd_result, dv = PD_sqrt, between = c(Treatment_group, Day))$p[3],
    anova_test(psv_result, dv = PSVs_sqrt, between = c(Treatment_group, Day))$p[1],
    anova_test(psv_result, dv = PSVs_sqrt, between = c(Treatment_group, Day))$p[2],
    anova_test(psv_result, dv = PSVs_sqrt, between = c(Treatment_group, Day))$p[3]
  ), 3)
) %>%
  mutate(Significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    TRUE ~ "ns"
  )) %>%
  mutate(Lake = replace(Lake, 2:n(), ""))

svb_table_data <- data.frame(
  Lake = "Svartberget",
  Metric = c("PD", "PD", "PD", "PSV", "PSV", "PSV"),
  Test = c("Treatment", "Sampling day", "Treatment*Sampling day", "Treatment", "Sampling day", "Treatment*Sampling day"),
  p_value = round(c(
    anova_test(pd_result_svb, dv = PD_sqrt, between = c(Treatment_group, Day))$p[1],
    anova_test(pd_result_svb, dv = PD_sqrt, between = c(Treatment_group, Day))$p[2],
    anova_test(pd_result_svb, dv = PD_sqrt, between = c(Treatment_group, Day))$p[3],
    anova_test(psv_result_svb, dv = PSVs_sqrt, between = c(Treatment_group, Day))$p[1],
    anova_test(psv_result_svb, dv = PSVs_sqrt, between = c(Treatment_group, Day))$p[2],
    anova_test(psv_result_svb, dv = PSVs_sqrt, between = c(Treatment_group, Day))$p[3]
  ), 3)
) %>%
  mutate(Significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    TRUE ~ "ns"
  )) %>%
  mutate(Lake = replace(Lake, 2:n(), ""))

asa_table <- flextable(asa_table_data) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)

svb_table <- flextable(svb_table_data) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)

asa_table
svb_table


#PD AND PSV REPEATED MEASURED ANOVA TESTS


library(tidyverse)


asa_biomass_by_day <- Asa_data %>%
  filter(Lake == "Asa") %>%
  group_by(Day, Taxon) %>%
  summarise(Total_Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop")


complete_taxa <- asa_biomass_by_day %>%
  complete(Taxon, Day = c(0, 4, 8, 12), fill = list(Total_Biomass = 0))


biomass_wide <- complete_taxa %>%
  pivot_wider(names_from = Day, values_from = Total_Biomass, names_prefix = "Day_")

biomass_decline <- biomass_wide %>%
  mutate(
    Decline_D0_D4 = Day_4 - Day_0,
    Decline_D4_D8 = Day_8 - Day_4,
    Decline_D8_D12 = Day_12 - Day_8
  )

print(head(biomass_decline, 10))

#ASA DECLINING TAXA 

library(tidyverse)

svart_biomass_by_day <- Svartberget_data %>%
  group_by(Day, Taxon) %>%
  summarise(Total_Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop")

svart_complete_taxa <- svart_biomass_by_day %>%
  complete(Taxon, Day = c(0, 4, 8, 12), fill = list(Total_Biomass = 0))

svart_biomass_wide <- svart_complete_taxa %>%
  pivot_wider(names_from = Day, values_from = Total_Biomass, names_prefix = "Day_")

svart_decline <- svart_biomass_wide %>%
  mutate(
    Decline_D0_D4 = Day_4 - Day_0,
    Decline_D4_D8 = Day_8 - Day_4,
    Decline_D8_D12 = Day_12 - Day_8
  )

print(head(svart_decline, 10))
install.packages("writexl")
library(writexl)

write_xlsx(biomass_decline, "Asa_biomass_decline.xlsx")
write_xlsx(svart_decline, "Svartberget_biomass_decline.xlsx")

#SVARTBERGET DECLINING BIOMASS

library(dplyr)
library(writexl)


asa_taxa <- Asa_data %>%
  filter(Lake == "Asa") %>%
  distinct(Taxon) %>%
  arrange(Taxon)

svart_taxa <- Svartberget_data %>%
  distinct(Taxon) %>%
  arrange(Taxon)


only_in_asa <- asa_taxa %>%
  filter(!Taxon %in% svart_taxa$Taxon)

only_in_svart <- svart_taxa %>%
  filter(!Taxon %in% asa_taxa$Taxon)



write_xlsx(only_in_asa, "Taxa_only_in_Asa.xlsx")
write_xlsx(only_in_svart, "Taxa_only_in_Svartberget.xlsx")


#TAXA ONLY IN ASA AND SVARTBERGET


if (!require(writexl)) install.packages("writexl")
library(dplyr)
library(tidyr)
library(writexl)

df <- read.csv("C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv")

get_rare_taxa <- function(data, lake_name, lower = 0.01, upper = 0.19) {
  data %>%
    filter(Lake == lake_name) %>%
    mutate(Sample_ID = paste0(Lake, "_Day", Day, "_", Treatment)) %>%
    group_by(Sample_ID, Taxon) %>%
    summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
    group_by(Sample_ID) %>%
    mutate(Total_Biomass = sum(Biomass),
           Relative_Biomass = Biomass / Total_Biomass) %>%
    filter(Relative_Biomass >= lower, Relative_Biomass <= upper) %>%
    arrange(Sample_ID, desc(Relative_Biomass))
}

rare_asa <- get_rare_taxa(df, "Asa")
rare_svb <- get_rare_taxa(df, "Svartberget")

write_xlsx(rare_asa, "Rare_Taxa_Asa.xlsx")
write_xlsx(rare_svb, "Rare_Taxa_Svartberget.xlsx")


#Rare taxa in Asa and Svartberget
library(dplyr)
library(tidyr)
library(writexl)

df <- read.csv("C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv")

get_moderate_taxa <- function(data, lake_name, lower = 0.2, upper = 0.4) {
  data %>%
    filter(Lake == lake_name) %>%
    group_by(Taxon) %>%
    summarise(Total_Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
    filter(Total_Biomass >= lower & Total_Biomass <= upper) %>%
    arrange(desc(Total_Biomass))
}

moderate_asa <- get_moderate_taxa(df, "Asa")
moderate_svb <- get_moderate_taxa(df, "Svartberget")

write_xlsx(moderate_asa, "Moderate_Taxa_Asa.xlsx")
write_xlsx(moderate_svb, "Moderate_Taxa_Svartberget.xlsx")


#MODERATE TAXA ASA SVARTBERGET

library(dplyr)
library(writexl)
library(readr)

df <- read_csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")

get_dominant_taxa_by_group <- function(data, lake_name, threshold = 0.65) {
  data %>%
    filter(Lake == lake_name, !is.na(Biomass)) %>%
    group_by(Treatment, Day, Taxon) %>%
    summarise(Total_Biomass = sum(Biomass), .groups = "drop") %>%
    group_by(Treatment, Day) %>%
    mutate(
      Group_Total = sum(Total_Biomass),
      Relative_Contribution = round((Total_Biomass / Group_Total) * 100, 2)
    ) %>%
    filter(Relative_Contribution >= threshold * 100) %>%
    arrange(Treatment, Day, desc(Relative_Contribution)) %>%
    ungroup()
}

dominant_asa <- get_dominant_taxa_by_group(df, "Asa")
dominant_svb <- get_dominant_taxa_by_group(df, "Svartberget")

write_xlsx(dominant_asa, "Dominant_Taxa_Asa_Treatment_Day.xlsx")
write_xlsx(dominant_svb, "Dominant_Taxa_Svb_Treatment_Day.xlsx")



#DOMINANT TAXA ASA AND SVARTBERGET




library(tidyverse)
library(ARTool)
library(flextable)


df <- read_csv("Phytoplankton_analysis.csv") %>%
  mutate(
    Treatment_Group = factor(substr(Treatment, 1, 1)),
    Day = factor(Day),
    Enclosure = factor(Enclosure),
    sqrt_Biomass = sqrt(Biomass)
  )


run_art_summary <- function(lake_name) {
  art_model <- art(sqrt_Biomass ~ Treatment_Group * Day + (1 | Enclosure), 
                   data = df %>% filter(Lake == lake_name))
  
  anova_art_df <- anova(art_model) %>%
    as.data.frame() %>%
    rownames_to_column("Effect") %>%
    mutate(
      `p-value` = formatC(`Pr(>F)`, format = "e", digits = 2),
      Significance = case_when(
        as.numeric(`Pr(>F)`) <= 0.001 ~ "***",
        as.numeric(`Pr(>F)`) <= 0.01 ~ "**",
        as.numeric(`Pr(>F)`) <= 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    select(
      `Main effects and interactions` = Effect,
      `Df` = Df,
      `F-value` = `F`,
      `p-value`,
      Significance
    )
  
  flextable(anova_art_df) %>%
    autofit() %>%
    fontsize(size = 10, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    bold(part = "header", bold = TRUE)
}

asa_table <- run_art_summary("Asa")
svartberget_table <- run_art_summary("Svartberget")

asa_table
svartberget_table


#TRANSFORMED ANOVA BASED ON LOGARITHMIC VALUES 


library(dplyr)
library(readr)

df <- read.csv("C:\\Users\\kostk\\OneDrive\\Υπολογιστής\\THESIS\\EXPERIMENTAL PART\\data\\FINAL EXCEL FOR R-STUDIO\\Phytoplankton_analysis.csv")

top_counts <- function(data, lake_name) {
  data %>%
    filter(Lake == lake_name) %>%
    mutate(Sample_ID = paste0(Lake, "_Day", Day, "_", Treatment)) %>%
    group_by(Sample_ID, Taxon) %>%
    summarise(Counts = sum(Counts, na.rm = TRUE), .groups = "drop") %>%
    group_by(Sample_ID) %>%
    slice_max(order_by = Counts, n = 10, with_ties = FALSE) %>%
    arrange(Sample_ID, desc(Counts))
}

top_asa <- top_counts(df, "Asa")
top_svb <- top_counts(df, "Svartberget")

write_csv(top_asa, "Top_10_Frequent_Taxa_Asa.csv")
write_csv(top_svb, "Top_10_Frequent_Taxa_Svartberget.csv")

#10 frequent taxa in Asa and in Svartberget



library(dplyr)
library(readr)
library(stringr)

df <- read.csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")

morphology_summary <- df %>%
  mutate(Morphology = case_when(
    str_detect(tolower(information), "filament") ~ "Filamentous taxa",
    str_detect(tolower(information), "colony") ~ "Colonial taxa",
    str_detect(tolower(information), "unicellular") ~ "Unicellular taxa",
    TRUE ~ "Other"
  )) %>%
  group_by(Lake, Day, Treatment, Taxon, Morphology) %>%
  summarise(Total_Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  group_by(Lake, Day, Treatment) %>%
  mutate(Relative_Biomass = Total_Biomass / sum(Total_Biomass)) %>%
  select(Lake, Day, Treatment, Taxon, Morphology, Relative_Biomass)
write_csv(
  morphology_summary,
  "C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Relative_Biomass_Morphology_Taxa_Asa_Svartberget.csv"
)

#csv with all the colonial, filamentous and unicellular taxa for Asa and Svartberget


library(dplyr)
library(ggplot2)
library(stringr)

svart_df <- read.csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")

svart_overall <- svart_df %>%
  filter(Lake == "Svartberget", Treatment %in% c(paste0("C", 1:5), paste0("G", 1:5), paste0("E", 1:5))) %>%
  mutate(
    info_lower = tolower(information),
    Morphology = case_when(
      str_detect(info_lower, "filament") ~ "Filamentous",
      str_detect(info_lower, "colony") ~ "Colonial",
      str_detect(info_lower, "unicellular") ~ "Unicellular",
      TRUE ~ "Other"
    ),
    Group = case_when(
      str_starts(Treatment, "C") ~ "C",
      str_starts(Treatment, "G") ~ "G",
      str_starts(Treatment, "E") ~ "E"
    )
  ) %>%
  group_by(Day, Group, Morphology) %>%
  summarise(Total_Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  group_by(Day, Group) %>%
  mutate(Relative_Biomass = Total_Biomass / sum(Total_Biomass)) %>%
  ungroup()

ggplot(svart_overall, aes(x = Group, y = Relative_Biomass, fill = Morphology)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Day, nrow = 1) +
  scale_fill_manual(values = c("Filamentous" = "black", "Colonial" = "orange", "Unicellular" = "brown")) +
  labs(
    title = str_wrap("Relative biomass of colonial, filamentous and unicellular taxa per treatment in Svartberget", width = 60),
    x = "Treatment Group",
    y = "Relative Biomass",
    fill = "Morphology"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 11, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#overall svartberget

library(dplyr)
library(ggplot2)
library(stringr)

asa_df <- read.csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")

asa_overall <- asa_df %>%
  filter(Lake == "Asa", Treatment %in% c(paste0("C", 1:5), paste0("G", 1:5), paste0("E", 1:5))) %>%
  mutate(
    info_lower = tolower(information),
    Morphology = case_when(
      str_detect(info_lower, "filament") ~ "Filamentous",
      str_detect(info_lower, "colony") ~ "Colonial",
      str_detect(info_lower, "unicellular") ~ "Unicellular",
      TRUE ~ "Other"
    ),
    Group = case_when(
      str_starts(Treatment, "C") ~ "C",
      str_starts(Treatment, "G") ~ "G",
      str_starts(Treatment, "E") ~ "E"
    )
  ) %>%
  group_by(Day, Group, Morphology) %>%
  summarise(Total_Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  group_by(Day, Group) %>%
  mutate(Relative_Biomass = Total_Biomass / sum(Total_Biomass)) %>%
  ungroup()

ggplot(asa_overall, aes(x = Group, y = Relative_Biomass, fill = Morphology)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Day, nrow = 1) +
  scale_fill_manual(values = c("Filamentous" = "black", "Colonial" = "orange", "Unicellular" = "brown")) +
  labs(
    title = str_wrap("Relative biomass of colonial, filamentous and unicellular taxa per treatment in Åsa", width = 60),
    x = "Treatment Group",
    y = "Relative Biomass",
    fill = "Morphology"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 11, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#overall Asa 



library(dplyr)
library(ggplot2)
library(vegan)
library(stringr)
library(tidyr)
library(factoextra)

df <- read.csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")

asa_pca_data <- df %>%
  filter(Lake == "Asa", str_detect(Treatment, "^[CGE]")) %>%
  mutate(
    Day = as.character(Day),
    Morphology = case_when(
      str_detect(tolower(information), "filament") ~ "filamentous",
      str_detect(tolower(information), "colony") ~ "colonial",
      str_detect(tolower(information), "unicellular") ~ "unicellular",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Morphology)) %>%
  group_by(Day, Treatment, Morphology) %>%
  summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Morphology, values_from = Biomass, values_fill = 0)

env_data <- df %>%
  filter(Lake == "Asa", str_detect(Treatment, "^[CGE]")) %>%
  mutate(Day = as.character(Day)) %>%
  dplyr::select(Day, Treatment, pH, Temperature, Conductivity, DO) %>%
  distinct()

merged <- left_join(asa_pca_data, env_data, by = c("Day", "Treatment"))

biomass_matrix <- merged %>% dplyr::select(filamentous, colonial, unicellular)

pca_result <- prcomp(biomass_matrix, scale. = TRUE)

var_exp <- round(summary(pca_result)$importance[2, 1:2] * 100, 1)

env_vars <- merged %>% dplyr::select(pH, Temperature, Conductivity, DO)
envfit_result <- envfit(pca_result, env_vars, permutations = 999)
env_vectors <- as.data.frame(scores(envfit_result, display = "vectors"))
env_vectors$Variable <- rownames(env_vectors)

pca_scores <- as.data.frame(pca_result$x)
pca_scores$Treatment <- merged$Treatment
pca_scores$Day <- as.factor(merged$Day)
pca_scores$TreatmentGroup <- case_when(
  str_starts(pca_scores$Treatment, "C") ~ "Control",
  str_starts(pca_scores$Treatment, "G") ~ "Gradual",
  str_starts(pca_scores$Treatment, "E") ~ "Extreme"
)

morph_df <- merged %>%
  pivot_longer(cols = c(filamentous, colonial, unicellular), names_to = "Morphology", values_to = "Biomass") %>%
  group_by(Day, Treatment) %>%
  slice_max(order_by = Biomass, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(Day, Treatment, Morphology)

final_df <- left_join(pca_scores, morph_df, by = c("Day", "Treatment"))

day_shapes <- c("0" = 16, "4" = 17, "8" = 15, "12" = 4)

ggplot(final_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = TreatmentGroup, shape = Day), size = 2.5, alpha = 0.85) +
  stat_ellipse(aes(group = Morphology, linetype = Morphology), size = 0.8, color = "black") +
  geom_segment(data = env_vectors,
               mapping = aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
               arrow = arrow(length = unit(0.15, "cm")), color = "gray30", inherit.aes = FALSE) +
  geom_text(data = env_vectors,
            mapping = aes(x = PC1 * 3.2, y = PC2 * 3.2, label = Variable),
            color = "gray10", size = 2.5, inherit.aes = FALSE) +
  scale_color_manual(
    values = c("Control" = "steelblue", "Gradual" = "darkgreen", "Extreme" = "firebrick")
  ) +
  scale_shape_manual(
    values = day_shapes,
    breaks = c("0", "4", "8", "12"),
    labels = c("Day 0", "Day 4", "Day 8", "Day 12")
  ) +
  labs(
    title = str_wrap("PCA of biomass of colonial, filamentous and unicellular taxa in Åsa across days and treatments", width = 60),
    x = paste0("PC1 (", var_exp[1], "%)"),
    y = paste0("PC2 (", var_exp[2], "%)"),
    color = "Treatment",
    shape = "Day",
    linetype = "Morphology"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "plain"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.6, "cm"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

#PCA FOR ASA TAXA 

library(dplyr)
library(ggplot2)
library(vegan)
library(stringr)
library(tidyr)
library(factoextra)

df <- read.csv("C:/Users/kostk/OneDrive/Υπολογιστής/THESIS/EXPERIMENTAL PART/data/FINAL EXCEL FOR R-STUDIO/Phytoplankton_analysis.csv")

svb_pca_data <- df %>%
  filter(Lake == "Svartberget", str_detect(Treatment, "^[CGE]")) %>%
  mutate(
    Day = as.character(Day),
    Morphology = case_when(
      str_detect(tolower(information), "filament") ~ "filamentous",
      str_detect(tolower(information), "colony") ~ "colonial",
      str_detect(tolower(information), "unicellular") ~ "unicellular",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Morphology)) %>%
  group_by(Day, Treatment, Morphology) %>%
  summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Morphology, values_from = Biomass, values_fill = 0)

env_data <- df %>%
  filter(Lake == "Svartberget", str_detect(Treatment, "^[CGE]")) %>%
  mutate(Day = as.character(Day)) %>%
  dplyr::select(Day, Treatment, pH, Temperature, Conductivity, DO) %>%
  distinct()

merged <- left_join(svb_pca_data, env_data, by = c("Day", "Treatment"))

biomass_matrix <- merged %>% dplyr::select(filamentous, colonial, unicellular)

pca_result <- prcomp(biomass_matrix, scale. = TRUE)

var_exp <- round(summary(pca_result)$importance[2, 1:2] * 100, 1)

env_vars <- merged %>% dplyr::select(pH, Temperature, Conductivity, DO)
envfit_result <- envfit(pca_result, env_vars, permutations = 999)
env_vectors <- as.data.frame(scores(envfit_result, display = "vectors"))
env_vectors$Variable <- rownames(env_vectors)

pca_scores <- as.data.frame(pca_result$x)
pca_scores$Treatment <- merged$Treatment
pca_scores$Day <- as.factor(merged$Day)
pca_scores$TreatmentGroup <- case_when(
  str_starts(pca_scores$Treatment, "C") ~ "Control",
  str_starts(pca_scores$Treatment, "G") ~ "Gradual",
  str_starts(pca_scores$Treatment, "E") ~ "Extreme"
)

morph_df <- merged %>%
  pivot_longer(cols = c(filamentous, colonial, unicellular), names_to = "Morphology", values_to = "Biomass") %>%
  group_by(Day, Treatment) %>%
  slice_max(order_by = Biomass, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(Day, Treatment, Morphology)

final_df <- left_join(pca_scores, morph_df, by = c("Day", "Treatment"))

day_shapes <- c("0" = 16, "4" = 17, "8" = 15, "12" = 4)

ggplot(final_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = TreatmentGroup, shape = Day), size = 2.5, alpha = 0.85) +
  stat_ellipse(aes(group = Morphology, linetype = Morphology), linewidth = 0.8, color = "black")+
  geom_segment(data = env_vectors,
               mapping = aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
               arrow = arrow(length = unit(0.15, "cm")), color = "gray30", inherit.aes = FALSE) +
  geom_text(data = env_vectors,
            mapping = aes(x = PC1 * 3.2, y = PC2 * 3.2, label = Variable),
            color = "gray10", size = 2.5, inherit.aes = FALSE) +
  scale_color_manual(
    values = c("Control" = "steelblue", "Gradual" = "darkgreen", "Extreme" = "firebrick")
  ) +
  scale_shape_manual(
    values = day_shapes,
    breaks = c("0", "4", "8", "12"),
    labels = c("Day 0", "Day 4", "Day 8", "Day 12")
  ) +
  labs(
    title = str_wrap("The effect of environmental variables on the biomass of colonial, filamentous and unicellular taxa in Svartberget across days and treatments", width = 60),
    x = paste0("PC1 (", var_exp[1], "%)"),
    y = paste0("PC2 (", var_exp[2], "%)"),
    color = "Treatment",
    shape = "Day",
    linetype = "Morphology"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "plain"),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.7, "cm"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

#PCA FOR SVARTBERGET UNICELLULAR-FILAMENTOUS COLONIAL TAXA

#PCA FOR ASA AND TAXA

library(dplyr)
library(tidyr)
library(rstatix)
library(flextable)
library(officer)
library(webshot2)

asa_df <- read.csv("Phytoplankton_analysis.csv")

asa_df <- asa_df %>%
  mutate(
    Treatment_group = case_when(
      grepl("^C", Treatment) ~ "Control",
      grepl("^G", Treatment) ~ "Gradual",
      grepl("^E", Treatment) ~ "Extreme"
    )
  )

asa_data <- asa_df %>%
  filter(Lake == "Asa",
         Treatment %in% c(paste0("C", 1:5), paste0("G", 1:5), paste0("E", 1:5)),
         information %in% c("filament", "colony", "unicellular")) %>%
  group_by(Day, Treatment, information, Treatment_group) %>%
  summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  group_by(Day, Treatment) %>%
  mutate(Total_Biomass = sum(Biomass)) %>%
  ungroup() %>%
  mutate(Rel_Bio = Biomass / Total_Biomass,
         Day = factor(Day),
         Treatment = factor(Treatment),
         Treatment_group = factor(Treatment_group))

analyze_table <- function(morph_value, morph_label) {
  df <- asa_data %>% 
    filter(information == morph_value) %>%
    mutate(sqrt_Rel_Bio = sqrt(Rel_Bio))
  
  aov_model <- aov(sqrt_Rel_Bio ~ Treatment_group * Day, data = df)
  aov_summary <- car::Anova(aov_model, type = 3) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Effect")
  
  aov_table <- aov_summary %>%
    filter(Effect %in% c("Treatment_group", "Day", "Treatment_group:Day")) %>%
    mutate(
      Effect = case_when(
        Effect == "Treatment_group" ~ "Treatment",
        Effect == "Day" ~ "Sampling day",
        Effect == "Treatment_group:Day" ~ "Treatment:Sampling day",
        TRUE ~ Effect
      ),
      df = Df,
      F = sprintf("%.3f", `F value`),
      `p-value` = sprintf("%.3f", `Pr(>F)`),
      Significance = case_when(
        `Pr(>F)` <= 0.001 ~ "***",
        `Pr(>F)` <= 0.01 ~ "**",
        `Pr(>F)` <= 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    select(Effect, df, F, `p-value`, Significance) %>%
    mutate(Morphology = ifelse(row_number() == 1, morph_label, ""))
  
  return(aov_table)
}

table_filamentous <- analyze_table("filament", "Filamentous taxa")
table_colonial <- analyze_table("colony", "Colonial taxa")
table_unicellular <- analyze_table("unicellular", "Unicellular taxa")

final_table <- bind_rows(table_filamentous, table_colonial, table_unicellular) %>%
  mutate(Lake = ifelse(row_number() == 1, "Asa", "")) %>%  
  select(Lake, Morphology, everything())

flextable_final <- flextable(final_table) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)

save_as_image(flextable_final, path = "asa_anova_final.png")

flextable_final


#SQUARED ANOVA FOR TREATMENT EFFECT ON COLONIAL UNICELLULAR FILAMENTOUS AND COLONIAL TAXA IN ASA

library(dplyr)
library(rstatix)
library(flextable)
library(webshot2)

asa_df <- read.csv("Phytoplankton_analysis.csv")

asa_df <- asa_df %>%
  mutate(
    Treatment_group = case_when(
      grepl("^C", Treatment) ~ "Control",
      grepl("^G", Treatment) ~ "Gradual",
      grepl("^E", Treatment) ~ "Extreme"
    )
  )

asa_data <- asa_df %>%
  filter(Lake == "Asa",
         Treatment %in% c(paste0("C", 1:5), paste0("G", 1:5), paste0("E", 1:5)),
         information %in% c("filament", "colony", "unicellular")) %>%
  group_by(Day, Treatment, information, Treatment_group) %>%
  summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  group_by(Day, Treatment) %>%
  mutate(Total_Biomass = sum(Biomass)) %>%
  ungroup() %>%
  mutate(Rel_Bio = Biomass / Total_Biomass)

analyze_table <- function(morph_value, morph_label) {
  df <- asa_data %>%
    filter(information == morph_value) %>%
    mutate(sqrt_Rel_Bio = sqrt(Rel_Bio),
           Day = factor(Day),
           Treatment = factor(Treatment),
           Treatment_group = factor(Treatment_group))
  
  aov_res <- tryCatch({
    anova_test(
      data = df,
      dv = sqrt_Rel_Bio,
      wid = Treatment,
      within = Day,
      between = Treatment_group
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(aov_res)) {
    return(tibble(
      Morphology = morph_label,
      Effect = "Analysis failed",
      df = NA,
      F = NA,
      `p-value` = NA,
      Significance = NA
    ))
  }
  
  aov_table <- aov_res$ANOVA %>%
    filter(Effect %in% c("Treatment_group", "Day", "Treatment_group:Day")) %>%
    mutate(
      Effect = case_when(
        Effect == "Treatment_group" ~ "Treatment",
        Effect == "Day" ~ "Sampling day",
        Effect == "Treatment_group:Day" ~ "Treatment:Sampling day",
        TRUE ~ Effect
      ),
      df = as.character(DFn),  
      Significance = case_when(
        p <= 0.001 ~ "***",
        p <= 0.01 ~ "**",
        p <= 0.05 ~ "*",
        TRUE ~ "ns"
      ),
      `p-value` = sprintf("%.3f", p),
      F = sprintf("%.3f", F),
      Morphology = ifelse(row_number() == 1, morph_label, "")
    ) %>%
    select(Morphology, Effect, df, F, `p-value`, Significance)
  
  return(aov_table)
}

table_filamentous <- analyze_table("filament", "Filamentous taxa")
table_colonial <- analyze_table("colony", "Colonial taxa")
table_unicellular <- analyze_table("unicellular", "Unicellular taxa")

final_table <- bind_rows(table_filamentous, table_colonial, table_unicellular) %>%
  mutate(Lake = ifelse(row_number() == 1, "Asa", "")) %>%
  select(Lake, everything())

flextable_final <- flextable(final_table) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)

save_as_image(flextable_final, path = "asa_sqrt_anova.png")

flextable_final

#CONDUCTIVITY EFFECT ON TREATMENT , SAMPLING DAY AND INTERACTION IN ASA 

library(dplyr)
library(rstatix)
library(flextable)
library(officer)
library(car)
library(tibble)
library(webshot2) 

df <- read.csv("Phytoplankton_analysis.csv")

Svartberget_data <- df %>%
  filter(Lake == "Svartberget",
         Treatment %in% c(paste0("C", 1:5), paste0("G", 1:5), paste0("E", 1:5)),
         information %in% c("filament", "colony", "unicellular")) %>%
  mutate(
    Treatment_group = case_when(
      grepl("^C", Treatment) ~ "Control",
      grepl("^G", Treatment) ~ "Gradual",
      grepl("^E", Treatment) ~ "Extreme"
    )
  ) %>%
  group_by(Day, Treatment) %>%
  mutate(Total_Biomass = sum(Biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Rel_Bio = Biomass / Total_Biomass)

analyze_table <- function(morph_value, morph_label) {
  df <- Svartberget_data %>%
    filter(information == morph_value) %>%
    mutate(sqrt_Rel_Bio = sqrt(Rel_Bio)) %>%
    mutate(
      Treatment = factor(Treatment),
      Day = factor(Day),
      Treatment_group = factor(Treatment_group)
    )
  
  if (nrow(df) < 3) {
    return(tibble(
      Morphology = morph_label,
      Effect = "Insufficient data",
      df = NA,
      F = NA,
      `p-value` = NA,
      Significance = NA
    ))
  }
  
  aov_model <- aov(sqrt_Rel_Bio ~ Treatment_group * Day, data = df)
  aov_summary <- car::Anova(aov_model, type = 3) %>%
    as.data.frame() %>%
    rownames_to_column(var = "Effect")
  
  aov_table <- aov_summary %>%
    filter(Effect %in% c("Treatment_group", "Day", "Treatment_group:Day")) %>%
    mutate(
      Effect = case_when(
        Effect == "Treatment_group" ~ "Treatment",
        Effect == "Day" ~ "Sampling day",
        Effect == "Treatment_group:Day" ~ "Treatment:Sampling day",
        TRUE ~ Effect
      ),
      df = Df,
      F = sprintf("%.3f", `F value`),
      `p-value` = sprintf("%.3f", `Pr(>F)`),
      Significance = case_when(
        `Pr(>F)` <= 0.001 ~ "***",
        `Pr(>F)` <= 0.01 ~ "**",
        `Pr(>F)` <= 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    select(Effect, df, F, `p-value`, Significance) %>%
    mutate(Morphology = ifelse(row_number() == 1, morph_label, ""))
  
  return(aov_table)
}

table_filamentous <- analyze_table("filament", "Filamentous taxa")
table_colonial <- analyze_table("colony", "Colonial taxa")
table_unicellular <- analyze_table("unicellular", "Unicellular taxa")

final_table <- bind_rows(table_filamentous, table_colonial, table_unicellular) %>%
  mutate(Lake = ifelse(row_number() == 1, "Svartberget", "")) %>%
  select(Lake, Morphology, everything())

flextable_final <- flextable(final_table) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)

save_as_image(flextable_final, path = "svartberget_anova_final.png")

flextable_final

#SVARTBERGET EFFECT OF TREATMENT ON FILAMENTOUS, COLONIAL AND UNICELLULAR TAXA

library(dplyr)
library(broom)
library(flextable)
library(webshot2)

df <- read.csv("Phytoplankton_analysis.csv")

svart_data <- df %>%
  filter(Lake == "Svartberget",
         Treatment %in% c(paste0("C", 1:5), paste0("G", 1:5), paste0("E", 1:5)),
         information %in% c("filament", "colony", "unicellular")) %>%
  group_by(Day, Treatment, information) %>%
  summarise(Biomass = sum(Biomass, na.rm = TRUE),
            Conductivity = mean(Conductivity, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(Day, Treatment) %>%
  mutate(Total_Biomass = sum(Biomass)) %>%
  ungroup() %>%
  mutate(Rel_Bio = Biomass / Total_Biomass)

analyze_table <- function(morph_value, morph_label) {
  df_sub <- svart_data %>%
    filter(information == morph_value) %>%
    mutate(sqrt_Rel_Bio = sqrt(Rel_Bio),
           Day = factor(Day))
  
  if (nrow(df_sub) == 0) {
    return(tibble(
      Morphology = morph_label,
      Effect = "Analysis failed",
      df = NA,
      F = NA,
      `p-value` = NA,
      Significance = NA
    ))
  }
  
  model <- tryCatch({
    lm(sqrt_Rel_Bio ~ Conductivity * Day, data = df_sub)
  }, error = function(e) { NULL })
  
  if (is.null(model)) {
    return(tibble(
      Morphology = morph_label,
      Effect = "Analysis failed",
      df = NA,
      F = NA,
      `p-value` = NA,
      Significance = NA
    ))
  }
  
  aov_table <- broom::tidy(anova(model)) %>%
    filter(term %in% c("Conductivity", "Day", "Conductivity:Day")) %>%
    mutate(
      Effect = case_when(
        term == "Conductivity" ~ "Conductivity",
        term == "Day" ~ "Sampling day",
        term == "Conductivity:Day" ~ "Conductivity:Sampling day",
        TRUE ~ term
      ),
      df = as.character(df),
      F = sprintf("%.3f", statistic),
      `p-value` = sprintf("%.3f", p.value),
      Significance = case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        TRUE ~ "ns"
      ),
      Morphology = ifelse(row_number() == 1, morph_label, "")
    ) %>%
    select(Morphology, Effect, df, F, `p-value`, Significance)
  
  return(aov_table)
}

table_filamentous <- analyze_table("filament", "Filamentous taxa")
table_colonial <- analyze_table("colony", "Colonial taxa")
table_unicellular <- analyze_table("unicellular", "Unicellular taxa")

final_table <- bind_rows(table_filamentous, table_colonial, table_unicellular) %>%
  mutate(Lake = ifelse(row_number() == 1, "Svartberget", "")) %>%
  select(Lake, everything())

flextable_final <- flextable(final_table) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header", bold = TRUE)

save_as_image(flextable_final, path = "svartberget_conductivity_day.png")

flextable_final

#EFFECT OF CONDUCTIVITY, TIME AND THEIR INTERACTION ON FILAMENTOUS, UNICELLULAR AND COLONIAL TAXA

