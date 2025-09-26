# ────────────────────────────────────────────────────────────────
# ==== Load Required Libraries (Install if missing) ====
# ────────────────────────────────────────────────────────────────

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  FactoMineR, factoextra, ggplot2, grid, corrplot,
  patchwork, ggridges, DataExplorer, pROC, rstatix,
  viridis, dplyr, vcd, correlation, tidyverse, 
  gridExtra, viridis
)

# ────────────────────────────────────────────────────────────────
# ==== Load and Inspect Dataset ==== 
# ────────────────────────────────────────────────────────────────

cirrhosis <- read.csv("cirrhosis.csv")
str(cirrhosis)
summary(cirrhosis)

# Remove rows with missing values
cirrhosis <- cirrhosis %>%
  na.omit()

cirrhosis$Stage <- as.factor(cirrhosis$Stage)

# ────────────────────────────────────────────────────────────────
# ==== Visualize Distributions: Histograms for Numerical Variables ====
# ────────────────────────────────────────────────────────────────

# Select only numeric variables
numeric_vars <- cirrhosis[, sapply(cirrhosis, is.numeric)]

# Create list of histograms
plots <- lapply(names(numeric_vars), function(var) {
  ggplot(cirrhosis, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(title = var, x = var, y = "Frequency") +
    theme_minimal()
})

# Plot all histograms together
do.call(gridExtra::grid.arrange, plots)

# ────────────────────────────────────────────────────────────────
# ==== Bar Plots for Categorical Variables ====
# ────────────────────────────────────────────────────────────────

# List of categorical variables to plot
cat_vars <- c("Sex", "Ascites", "Hepatomegaly", "Spiders", "Edema", "Drug", "Status", "Stage")

# Initialize list to store plots
plots <- list()

for (var in cat_vars) {
  plots[[var]] <- ggplot(cirrhosis, aes_string(x = var, fill = var)) +
    geom_bar(color = "black") +
    labs(title = paste("Distribution of", var),
         x = var, y = "Frequency") +
    theme_minimal() +
    scale_fill_viridis(discrete = TRUE) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

# Combine and display all plots
combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 4))
print(combined_plot)

# Save combined plot
ggsave("all_categorical_distributions.png", combined_plot, width = 16, height = 12, dpi = 300)

# ────────────────────────────────────────────────────────────────
# ==== Class Balancing Check and Recoding ==== 
# ────────────────────────────────────────────────────────────────

# Check class distribution
table(cirrhosis$Stage)
table(cirrhosis$Status)

# Recode Stage into grouped levels: 1&2 → '1', 3 → '2', 4 → '3'
cirrhosis <- cirrhosis %>%
  mutate(
    Stage_group = factor(case_when(
      Stage %in% c(1, 2) ~ "1",
      Stage == 3 ~ "2",
      Stage == 4 ~ "3"
    ), levels = c("1", "2", "3"))
  )

# Recode Status: C, CL → 'alive'; D → 'dead'
cirrhosis <- cirrhosis %>%
  mutate(
    Status_group = factor(case_when(
      Status %in% c("C", "CL") ~ "alive",
      Status == "D" ~ "dead"
    ), levels = c("alive", "dead"))
  )

# Remove original columns
cirrhosis$Stage <- NULL
cirrhosis$Status <- NULL
cirrhosis$ID <- NULL

# ────────────────────────────────────────────────────────────────
# ==== Grouped Bar Charts by Outcome Groups ==== 
# ────────────────────────────────────────────────────────────────

# Function to create grouped bar chart
plot_grouped_bar <- function(data, var, group_var) {
  data %>%
    group_by(.data[[group_var]], .data[[var]]) %>%
    summarise(Count = n()) %>%
    ggplot(aes(x = .data[[var]], y = Count, fill = .data[[group_var]])) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste(var, "vs", group_var),
         x = var,
         y = "Frequency",
         fill = group_var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate plots for both outcome groups
vars_to_plot <- c("Sex", "Ascites", "Hepatomegaly", "Spiders", "Edema", "Drug")
plots_status <- lapply(vars_to_plot, function(var) plot_grouped_bar(cirrhosis, var, "Status_group"))
plots_stage <- lapply(vars_to_plot, function(var) plot_grouped_bar(cirrhosis, var, "Stage_group"))

# Combine and print final layout
final_layout <- wrap_plots(mapply(`+`, plots_status, plots_stage, SIMPLIFY = FALSE), ncol = 2) +
  plot_annotation(title = "Variable Distributions by Status and Stage Group")
print(final_layout)

# ────────────────────────────────────────────────────────────────
# ==== Hypothesis Testing: Fisher's Exact Test ==== 
# ────────────────────────────────────────────────────────────────

for (var in cat_vars) {
  table <- table(cirrhosis$Stage_group, cirrhosis[[var]])
  fisher_test <- fisher.test(table)
  print(paste("Fisher's Exact Test for", var))
  print(fisher_test)
}

for (var in cat_vars) {
  table <- table(cirrhosis$Status_group, cirrhosis[[var]])
  fisher_test <- fisher.test(table)
  print(paste("Fisher's Exact Test for", var))
  print(fisher_test)
}

# ────────────────────────────────────────────────────────────────
# ==== Normality Check: Shapiro-Wilk Test ==== 
# ────────────────────────────────────────────────────────────────

# Identify numeric variables
num_vars <- cirrhosis %>% select(where(is.numeric)) %>% names()

# Apply Shapiro-Wilk test
shapiro_results <- lapply(num_vars, function(var) {
  test <- shapiro.test(cirrhosis[[var]])
  return(list(
    variable = var,
    statistic = test$statistic,
    p.value = test$p.value
  ))
})

# Convert results to a dataframe
shapiro_df <- do.call(rbind, shapiro_results) %>%
  as_tibble() %>%
  mutate(normal = ifelse(p.value >= 0.05, "Yes", "No"))

print(shapiro_df)

# ────────────────────────────────────────────────────────────────
# ==== Log Transformation of Non-Normal Variables ==== 
# ────────────────────────────────────────────────────────────────

# Exclude Age since it's already normal
log_vars <- setdiff(num_vars, c("Age"))

# Apply log transformation
cirrhosis_log <- cirrhosis %>%
  mutate(across(all_of(log_vars), ~ log1p(.x)))

# Keep grouping variables
cirrhosis_log$Status_group <- cirrhosis$Status_group
cirrhosis_log$Stage_group <- cirrhosis$Stage_group

# ────────────────────────────────────────────────────────────────
# ==== Boxplots and Outlier Detection ==== 
# ────────────────────────────────────────────────────────────────

# Pivot data for plotting
data_long <- cirrhosis %>%
  select(where(is.numeric), Stage_group) %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value")

# Boxplot by Stage_group
ggplot(data_long, aes(x = Stage_group, y = value)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Boxplot by Disease Stage", x = "Stage Group", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Outlier detection
outliers_list <- data_long %>%
  group_by(Stage_group, variable) %>%
  summarise(outliers = list(boxplot.stats(value)$out), .groups = "drop") %>%
  unnest(outliers) %>%
  rename(outlier_value = outliers)

# Summary of outliers
outlier_summary <- outliers_list %>%
  group_by(Stage_group, variable) %>%
  summarise(n_outliers = n())

print(outlier_summary)

# ────────────────────────────────────────────────────────────────
# ==== Violin Plots for Visualizing Distribution by Group ==== 
# ────────────────────────────────────────────────────────────────

num_vars <- cirrhosis_log[, sapply(cirrhosis_log, is.numeric)]
num_vars$Stage_group <- cirrhosis_log$Stage_group
num_vars$Status_group <- cirrhosis_log$Status_group

create_violin_plot <- function(data, var_name, group_var) {
  ggplot(data, aes_string(x = group_var, y = var_name, fill = group_var)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.2) +
    labs(title = var_name) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Create violin plots by Status_group
plots_status <- Filter(Negate(is.null), lapply(names(num_vars), function(var) {
  if (var %in% c("Status_group", "Stage_group")) return(NULL)
  create_violin_plot(num_vars, var, "Status_group")
}))

wrap_plots(plots_status, ncol = 3) + plot_annotation(title = "Violin Plots by Status_group")

# Create violin plots by Stage_group
plots_stage <- Filter(Negate(is.null), lapply(names(num_vars), function(var) {
  if (var %in% c("Status_group", "Stage_group")) return(NULL)
  create_violin_plot(num_vars, var, "Stage_group")
}))

wrap_plots(plots_stage, ncol = 3) + plot_annotation(title = "Violin Plots by Stage_group")




# ────────────────────────────────────────────────────────────────
# ==== Correlation Matrix ==== 
# ────────────────────────────────────────────────────────────────

# Add numeric version of Stage_group for correlation
cirrhosis$Stage_group_num <- as.numeric(as.character(cirrhosis$Stage_group))

# Compute correlation matrix
cor_matrix <- cor(select(cirrhosis, where(is.numeric)), use = "pairwise.complete.obs")

# Plot correlation matrix
corrplot::corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "white",
  col = viridis::inferno(100),
  tl.col = "navy",
  tl.srt = 45,
  diag = FALSE,
  number.cex = 0.8,
  tl.cex = 0.8,
  cl.pos = "r",
  cl.cex = 0.6,
  title = "Correlation Matrix",
  mar = c(0, 0, 1, 0)
)

# ────────────────────────────────────────────────────────────────
# ==== Save the Cleaned Dataset ==== 
# ────────────────────────────────────────────────────────────────

write.csv(cirrhosis, "cirrhosis_cleaned.csv", row.names = FALSE)