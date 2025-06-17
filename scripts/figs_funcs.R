############################## Global Vars ##############################################

# ---- Plot Styling ----

library(ggplot2)

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_line(color = "grey90")
    )
)


style_kable_default <- function(kable_obj, df = NULL) {
  # Guard clause: skip if df is missing or not a data frame
  if (is.null(df) || !is.data.frame(df)) {
    warning("No valid df provided; skipping row_spec.")
    return(
      kable_obj %>%
        kableExtra::kable_styling(
          full_width = FALSE,
          bootstrap_options = c("hover", "condensed", "responsive"),
          font_size = 13,
          position = "center"
        )
    )
    
  }
    kable_obj %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("hover", "condensed", "responsive"),
      font_size = 13,
      position = "center"
    ) %>%
    kableExtra::row_spec(0:nrow(df), extra_css = "color: black;")
}

############################## FIGURES ##############################################

#---- Figures

library(ggplot2)
library(gridExtra)
library(dplyr)

numeric_targets <- c("length_of_stay")

# Generate histograms for each numeric target
hist_list <- lapply(numeric_targets, function(target) {
  ggplot(data_hosp, aes(x = .data[[target]])) +
    geom_histogram(color = "black", fill = "darkgreen", bins = 100, alpha = 0.7) +  
    xlab(target) +
    ylab("Frequency") +
    theme_light() +
    ggtitle(paste("Distribution of", target)) +
    theme(plot.title = element_text(size = 8, face = "bold"),
          axis.title.x = element_blank())
})

binary_targets <- c("hospital_expire_flag", "readmitted", "mortality_30_day")

# Generate bar charts for each binary target
bar_list <- lapply(binary_targets, function(target) {
  ggplot(data_hosp, aes(x = factor(.data[[target]]))) +
    geom_bar(color = "black", fill = "darkgreen", alpha = 0.7) +
    xlab(target) +
    ylab("Count") +
    theme_light() +
    ggtitle(paste(target)) +
    theme(plot.title = element_text(size = 8, face = "bold"),
          axis.title.x = element_blank())
})

# Create summary table
binary_summary_table <- bind_rows(
  data.frame(Target = "hospital_expire_flag", table(data_hosp$hospital_expire_flag)) %>% mutate(Proportion = Freq / sum(Freq)),
  data.frame(Target = "readmitted", table(data_hosp$readmitted)) %>% mutate(Proportion = Freq / sum(Freq)),
  data.frame(Target = "mortality_30_day", table(data_hosp$mortality_30_day)) %>% mutate(Proportion = Freq / sum(Freq))
) 

caption_html <- paste0(
  "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
  "Binary Target Summary Table",
  "</div>"
)

colnames(binary_summary_table) <- c("Target", "Category", "Count", "Proportion")
binary_df <- binary_summary_table

# Step 2: Render as styled kable object
binary_summary_table <- binary_df %>%
  kable(format = "html", caption = caption_html) %>%
  style_kable_default(binary_df)

# Save plots and summary table as a list to be called from Rmd
plots <- list(hist_list = hist_list, bar_list = bar_list, summary_table = binary_summary_table)

#---------------------------------------------------------------------------

#---- Figure # 
readmitted_patients <- data_hosp %>%
  filter(readmitted == "Yes") %>%  # Filter only patients with at least one readmission
  distinct(subject_id)  # Get unique patient IDs

set.seed(42)  # For reproducibility
example_patient <- sample(readmitted_patients$subject_id, 1)

patient_history <- data_hosp %>%
  filter(subject_id == example_patient) %>%  # Pull all records for this patient
  arrange(admittime)  # Order by admission time to check sequence


admission_counts <- data_hosp %>%
  group_by(subject_id) %>%
  summarize(num_admissions = n())

# Define bins for admissions
admission_counts <- admission_counts %>%
  mutate(binned_admissions = cut(num_admissions,
                                 breaks = c(1, 2, 4, 6, 10, 15, 20, Inf),  # Bins
                                 labels = c("1","2-3","4-5","6-9","10-15", "15-20","20+"),
                                 include.lowest = TRUE))

# Plot histogram of the number of admissions per patient with binning
figure_4_4_7 <- ggplot(admission_counts, aes(x = binned_admissions)) +
  geom_bar(color = "black", fill = "darkgreen", alpha = 0.8) +
  labs(title = "Number of Admissions Per Patient",
       x = "Admissions Count",
       y = "Patients Count")


#---------------------------------------------------------------------------



############################## FUNCTIONS ##############################################


#################
###   EDA     ###
#################

library(ggplot2)
library(dplyr)
library(gridExtra)

# Step 1: Readmitted Patients
readmitted_flags <- data_hosp %>%
  group_by(subject_id) %>%
  summarize(any_readmission = ifelse(any(readmitted == "Yes"), "Yes", "No")) %>%
  mutate(any_readmission = factor(any_readmission, levels = c("No", "Yes")))  # Ensure factor levels match scale_fill_manual()

# Step 2: One Admission Per Patient
cleaned_patient_data <- data_hosp %>%
  left_join(readmitted_flags, by = "subject_id") %>%  # Merge readmission flag
  group_by(subject_id) %>%
  filter(ifelse(any_readmission == "Yes", readmitted == "Yes", row_number() == 1)) %>%  # Convert factor to logical
  ungroup()  

plot_readmission_by_category <- function(dataset, category_var) {
  ggplot(dataset, aes(x = .data[[category_var]], fill = any_readmission)) +
    geom_bar(color = "black", alpha = 0.8,linewidth = 0.3) +
    labs(title = paste(category_var),
         x = NULL,
         y = "Patients") +  # Removed legend label
    scale_fill_manual(values = c("No" = "lightblue", "Yes" = "red"),
                      drop = FALSE) +  # Ensures all levels appear even if missing
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
}

#---------------------------------------------------------------------------

plot_feature_distribution <- function(df, column, target) {
  # Check if column exists
  if (!(column %in% colnames(df))) {
    stop(paste("Column", column, "not found in the dataset"))
  }
  
  # Ensure target variable exists
  if (!(target %in% colnames(df))) {
    stop(paste("Target variable", target, "not found in the dataset"))
  }
  
  # Convert target to factor if not already
  df[[target]] <- as.factor(df[[target]])
  
  # Convert binary features to factor with Yes/No labels
  unique_vals <- unique(df[[column]])
  if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
    df[[column]] <- factor(df[[column]], levels = c(0, 1), labels = c("No", "Yes"))
  }
  
  # Remove 0 values for visualization (but keep in dataset)
  filtered_data <- df[df[[column]] != 0, , drop = FALSE]
  
  if (is.numeric(df[[column]])) {
    # Stacked density plot based on target variable
    p <- ggplot(filtered_data, aes(x = .data[[column]], fill = .data[[target]])) +
      geom_density(alpha = 0.8, position = "stack", color = "black", linewidth = 0.3) +  # Stacked density
      labs(title = paste("Stacked Density of", column, "by", target), 
           x = column, y = "Density", fill = target)

  } else {
    # Categorical features: Stacked bar chart
    p <- ggplot(df, aes(x = .data[[column]], fill = .data[[target]])) +
      geom_bar(position = "fill", alpha = 0.8, color = "black", linewidth = 0.3) +  # Add outlines
      labs(title = paste("Stacked Category Distribution of", column, "by", target), 
           x = column, y = "Proportion", fill = target) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
             plot.background = element_rect(fill = "white", color = NA),
             panel.background = element_rect(fill = "white", color = NA))
  }
  
  return(p)
}

#---------------------------------------------------------------------------

plot_horizontal_box <- function(data, features) {
  plot_list <- list()
  
  for (feature in features) {
    p <- ggplot(data, aes(x = .data[[feature]], y = "")) +
      geom_boxplot(fill = "lightblue", outlier.colour = "red", alpha = 0.7) +
      labs(title = paste("Box Plot of", feature), x = "Values", y = "")
    
    plot_list[[feature]] <- p
  }
  
  return(plot_list)
}

#---------------------------------------------------------------------------

plot_qq_by_variable <- function(data, features, title_prefix = "Q-Q Plot for", drop_zeros = TRUE) {
  plot_list <- list()
    for (feature in features) {
    plot_data <- if (drop_zeros) data[data[[feature]] != 0 & !is.na(data[[feature]]), , drop = FALSE] else data
    
    p <- ggplot(plot_data, aes(sample = .data[[feature]])) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(
        title = paste(title_prefix, feature),
        x = "Theoretical Quantiles", y = "Sample Quantiles"
      )
      
    plot_list[[feature]] <- p 
  }
    return(plot_list)
}

#---------------------------------------------------------------------------

check_normality_shapiro <- function(data, vars, sample_size = 500, seed = 43, remove_zeros = FALSE) {
  set.seed(seed)
  data_sample <- data[sample(nrow(data), min(nrow(data), sample_size)), ]
  
  results <- lapply(vars, function(col) {
    col_data <- na.omit(data_sample[[col]])
    if (remove_zeros) col_data <- col_data[col_data != 0]
    
    if (length(col_data) > 3) {
      test <- shapiro.test(col_data)
      data.frame(Feature = col, statistic = test$statistic, p.value = test$p.value)
    } else {
      data.frame(Feature = col, statistic = NA, p.value = NA)
    }
  })
  
  dplyr::bind_rows(results) %>%
    dplyr::arrange(p.value)
}

render_normality_table <- function(results_df, 
                                   title = "Shapiro-Wilk Normality Test Results", 
                                   digits = 5, 
                                   end = 0.6) {
  df <- results_df %>%
    arrange(statistic) %>%
    dplyr::select(Feature, statistic, p.value) %>%
  mutate(p.value = format(p.value, scientific = TRUE, digits = digits))
  
  caption_html <- paste0(
    "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
    title,
    "</div>"
  )
  
  df %>%
    kable("html", escape = FALSE, caption = caption_html) %>%
    style_kable_default(df) %>%
    column_spec(3, width = "120px", background = spec_color(df$statistic, end = end, direction = 1))
}

#---------------------------------------------------------------------------

log_transform_vars <- function(data, vars, suffix = "_log", filter_expr = NULL) {
  for (v in vars) {
    if (!is.numeric(data[[v]])) next
    
    transformed <- if (!is.null(filter_expr)) {
      result <- rep(NA, nrow(data))
      rows <- eval(parse(text = filter_expr), envir = data)
      result[rows] <- log(data[[v]][rows] + 1)
      result
    } else {
      log(data[[v]] + 1)
    }
    
    data[[paste0(v, suffix)]] <- transformed
  }
  return(data)
}

#---------------------------------------------------------------------------

replace_features_by_suffix <- function(base_list, all_features, suffix = "_log") {
  sapply(base_list, function(f) {
    transformed_name <- paste0(f, suffix)
    if (transformed_name %in% all_features) {
      transformed_name
    } else {
      f
    }
  }, USE.NAMES = FALSE)
}

#---------------------------------------------------------------------------

###########################
### Statistical_tests  ###
###########################

######## Parametric    ############################

## Logistic Regression (binary targets)
run_logistic_models <- function(data, binary_targets, predictors) {
  models <- list()
  for (target in binary_targets) {
    formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
    model <- glm(formula, data = data, family = "binomial")
    models[[target]] <- model
  }
  return(models)
}

#---------------------------------------------------------------------------

render_logit_summary_table <- function(model, 
                                       target_name = "Target", 
                                       p_filter = 1000, 
                                       exclude_terms = NULL,
                                       digits = 4,
                                       begin = 0.3,
                                       end = 0.7,
                                       rank_by = "p.value") {
  # compute odds ratios
  tidy_output <- broom::tidy(model) %>%
    dplyr::mutate(OR = exp(estimate)) %>%
    dplyr::filter(p.value < p_filter)
  
  # optional filter
  if (!is.null(exclude_terms)) {
    tidy_output <- tidy_output %>%
      filter(!term %in% exclude_terms)
  }
  
  # rank and sort
  tidy_output <- tidy_output %>%
    dplyr::mutate(Rank = dplyr::dense_rank(!!rlang::sym(rank_by))) %>%
    dplyr::arrange(Rank) %>%
    dplyr::select(Rank, term, estimate, OR, statistic, p.value)
  
  
  title <- paste0("Predictors for: ", target_name)
  caption_html <- paste0(
    "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
    title,
    "</div>"
  )
  
  # style table
  tidy_output %>%
    knitr::kable("html", digits = digits, escape = FALSE,
                 caption = caption_html
    ) %>%
    style_kable_default(tidy_output) %>%
    kableExtra::column_spec(3, width = "120px", background = spec_color(tidy_output$estimate, begin = begin, end = end)) %>%
    kableExtra::column_spec(4, width = "120px", background = spec_color(tidy_output$OR, begin = begin, end = end)) %>%
    kableExtra::column_spec(5, width = "120px", background = spec_color(tidy_output$statistic, begin = begin, end = end)) %>%
    kableExtra::column_spec(6, width = "120px", background = spec_color(tidy_output$p.value, direction = -1, begin = begin, end = end))
}

#---------------------------------------------------------------------------

## Linear Regression (continuous targets i.e. length of stay)
run_linear_models <- function(data, continuous_targets, predictors) {
  models <- list()
  for (target in continuous_targets) {
    formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
    model <- lm(formula, data = data)
    models[[target]] <- model
  }
  return(models)
}

#---------------------------------------------------------------------------

render_lm_summary_table <- function(model, 
                                    target_name = "Target", 
                                    exclude_terms = NULL,
                                    digits = 4,
                                    begin = begin,
                                    end = 0.8,
                                    rank_by = "p.value") {
  # summary
  tidy_output <- broom::tidy(model)
  
  # filter
  if (!is.null(exclude_terms)) {
    tidy_output <- tidy_output %>%
      dplyr::filter(!term %in% exclude_terms)
  }
  
  # sort
  tidy_output <- tidy_output %>%
    dplyr::mutate(Rank = dplyr::dense_rank(!!rlang::sym(rank_by))) %>%
    dplyr::arrange(Rank) %>%
    dplyr::select(Rank, term, estimate, statistic, p.value)
  
  title <- paste0("Predictors for: ", target_name)
  caption_html <- paste0(
    "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
    title,
    "</div>"
  )
  
  # Style table
  tidy_output %>%
    knitr::kable("html", digits = digits, escape = FALSE,
                 caption = caption_html) %>%
    style_kable_default(tidy_output) %>%
    kableExtra::column_spec(3, width = "120px", background = spec_color(tidy_output$estimate, begin = begin, end = end)) %>%
    kableExtra::column_spec(4, width = "120px", background = spec_color(tidy_output$statistic, begin = begin, end = end)) %>%
    kableExtra::column_spec(5, width = "120px", background = spec_color(tidy_output$p.value, direction = -1, begin = begin, end = end))
}

#---------------------------------------------------------------------------

render_vif_summary_table <- function(model, 
                                     target_name = "Target", 
                                     digits = 4,
                                     begin = 0.2,
                                     end = 0.8,
                                     rank_by = "GVIF^(1/(2*Df))") {

  #  VIF
  vif_df <- as.data.frame(car::vif(model))
  vif_df$term <- rownames(vif_df)
  rownames(vif_df) <- NULL
  
  # Rank
  vif_df <- vif_df %>%
    dplyr::mutate(Rank = dplyr::dense_rank(-!!rlang::sym(rank_by))) %>%
    dplyr::arrange(Rank) %>%
    dplyr::select(Rank, term, dplyr::everything())
  
  title <- paste0("VIF Summary for: ", target_name)
  caption_html <- paste0(
    "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
    title,
    "</div>"
  )
  
  # style table
  vif_df %>%
    kable("html", digits = digits, escape = FALSE,
          caption = title) %>%
    style_kable_default(vif_df) %>%
    column_spec(
      which(colnames(vif_df) == rank_by),
      background = spec_color(vif_df[[rank_by]], direction = -1, begin= begin, end = end)
    )
}

#---------------------------------------------------------------------------

run_t_tests <- function(data, numeric_vars, target_var, target_label = "Outcome") {
  if (!is.numeric(data[[target_var]])) {
    target_numeric <- as.numeric(as.factor(data[[target_var]]))
  } else {
    target_numeric <- data[[target_var]]
  }
  
  results <- lapply(numeric_vars, function(var) {
    if (length(unique(na.omit(data[[var]]))) < 2) return(NULL)
    test <- t.test(data[[var]] ~ target_numeric, na.action = na.omit)
    data.frame(Feature = var, P_Value = test$p.value, Test = "t-test", stringsAsFactors = FALSE)
  })
  
  results_df <- do.call(rbind, results)
  results_df$Target <- target_label
  results_df <- results_df[order(results_df$P_Value), ]
  return(results_df)
}

#---------------------------------------------------------------------------


render_anova_summary_table <- function(data, 
                                       target, 
                                       predictors, 
                                       title = NULL, 
                                       digits = 5,
                                       begin = 0.2,
                                       end = 0.7) {
  # ANOVA for each predictor
  results <- lapply(predictors, function(pred) {
    formula <- as.formula(paste(target, "~", pred))
    test <- aov(formula, data = data)
    summary_stats <- summary(test)[[1]]
    p_val <- summary_stats[["Pr(>F)"]][1]
    
    data.frame(Predictor = pred, P_Value = p_val, stringsAsFactors = FALSE)
  })
  
  results_df <- do.call(rbind, results) %>%
    arrange(P_Value)
  
  # Caption title
  if (is.null(title)) {
    title <- paste0("ANOVA: Predictors of ", target)
  }
  
  caption_html <- paste0(
    "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
    title,
    "</div>"
  )
  
  # tidy table
  results_df %>%
    kable("html", digits = digits, escape = FALSE, caption = caption_html) %>%
    style_kable_default(df = results_df) %>%
    kableExtra::column_spec(2, width = "120px", background = spec_color(results_df$P_Value, direction = -1, begin= begin, end = end))
}

######## Non-Parametric    ############################

#---------------------------------------------------------------------------

run_wilcoxon_tests <- function(data, continuous_vars, binary_var, target_label = NULL) {
  results <- lapply(continuous_vars, function(cont_var) {
    test <- wilcox.test(data[[cont_var]] ~ data[[binary_var]], na.action = na.omit)
    data.frame(
      Feature = cont_var,
      Target = ifelse(is.null(target_label), binary_var, target_label),
      P_Value = test$p.value,
      stringsAsFactors = FALSE
    )
  })
  
  results_df <- do.call(rbind, results)
  results_df <- results_df[order(results_df$P_Value), ]
  return(results_df)
}

#---------------------------------------------------------------------------

render_wilcoxon_summary_table <- function(results_df, 
                                          title = "Wilcoxon Test Results", 
                                          digits = 4,
                                          begin = 0.2,
                                          end = 0.7) {
  df <- results_df %>%
    arrange(desc(P_Value)) %>%
    mutate(Rank = dense_rank(desc(P_Value))) %>%
    dplyr::select(Rank, Feature, Target, P_Value)
  
  caption_html <- paste0(
    "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
    title,
    "</div>"
  )
  
  df %>%
    kable("html", digits = digits, escape = FALSE,
          caption = caption_html) %>%
    style_kable_default(df) %>%
    column_spec(4, width = "120px", background = spec_color(df$P_Value, begin = begin, end = end, direction = -1))
}


#---------------------------------------------------------------------------

run_kruskal_tests <- function(data, numeric_vars, target_var) {
  target_numeric <- as.numeric(as.factor(data[[target_var]]))
  data[[paste0(target_var, "_numeric")]] <- target_numeric
  
  results <- lapply(numeric_vars, function(col) {
    test <- kruskal.test(data[[col]] ~ data[[paste0(target_var, "_numeric")]], data = data)
    return(data.frame(Feature = col, P_Value = test$p.value))
  })
  
  df <- do.call(rbind, results)
  df$Target <- target_var
  df <- df[order(df$P_Value), ]
  return(df)
}


#---------------------------------------------------------------------------

plot_correlation_heatmap <- function(data, vars) {
  cor_matrix <- cor(data[, vars], use = "complete.obs", method = "pearson")
  cor_melt <- melt(cor_matrix)
  
  ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name = "Pearson\nCorrelation") +
    geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_line(color = "grey90"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_blank(),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    ggtitle("Correlation Heatmap")
}

#---------------------------------------------------------------------------

run_fishers_tests <- function(data, binary_vars, target_var, target_label = "Outcome") {
  results <- lapply(binary_vars, function(var) {
    if (length(unique(data[[var]])) != 2) return(NULL)
    if (length(unique(data[[target_var]])) != 2) return(NULL)
    
    tbl <- table(data[[var]], data[[target_var]])
    if (any(tbl == 0)) return(NULL)  # skip if any cell is zero
    
    test <- fisher.test(tbl)
    
    # Extract fields safely
    odds_ratio <- if ("estimate" %in% names(test)) unname(test$estimate) else NA
    ci_low <- if (!is.null(test$conf.int)) test$conf.int[1] else NA
    ci_high <- if (!is.null(test$conf.int)) test$conf.int[2] else NA
    
    data.frame(
      Feature = var,
      P_Value = test$p.value,
      Odds_Ratio = odds_ratio,
      CI_Lower = ci_low,
      CI_Upper = ci_high,
      Test = "Fisher",
      stringsAsFactors = FALSE
    )
  })
  
  results_df <- do.call(rbind, results)
  results_df$Target <- target_label
  results_df <- results_df[order(results_df$P_Value), ]
  return(results_df)
}

#---------------------------------------------------------------------------

render_fisher_summary_table <- function(results_df, 
                                        title = "Fisher's Exact Test Results", 
                                        digits = 4, 
                                        begin = 0.2,
                                        end = 0.8) {
  
  caption_html <- paste0(
    "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
    title,
    "</div>"
  )
  
  # Sort by Target, then Odds Ratio (descending = stronger association)
  table_df <- results_df %>%
    dplyr::arrange(Target, desc(Odds_Ratio)) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::select(Rank, Feature, Target, Odds_Ratio, CI_Lower, CI_Upper, P_Value)
  
  # Render HTML table
  knitr::kable(table_df, "html", digits = digits, escape = FALSE,
               caption = caption_html,
               col.names = c("Rank", "Feature", "Target", "Odds Ratio", "CI Lower", "CI Upper", "P-Value")) %>%
    style_kable_default(table_df) %>%
    kableExtra::column_spec(4, width = "100px", background = spec_color(table_df$Odds_Ratio, begin = begin, end = end)) %>%
    kableExtra::column_spec(7, width = "100px", background = spec_color(table_df$P_Value, begin = begin, end = end, direction = -1))
}


#---------------------------------------------------------------------------

run_chisq_tests_df <- function(data, target_var, cat_vars = NULL) {
  if (is.null(cat_vars)) {
    cat_vars <- names(data)[sapply(data, is.factor) & names(data) != target_var]
  }
  
  results <- lapply(cat_vars, function(col) {
    tbl <- table(data[[col]], data[[target_var]])
    if (all(dim(tbl) > 1)) {
      test <- chisq.test(tbl)
      return(data.frame(
        Feature = col,
        P_Value = test$p.value,
        Statistic = test$statistic
      ))
    } else {
      return(data.frame(
        Feature = col,
        P_Value = NA,
        Statistic = NA
      ))
    }
  })
  
  result_df <- do.call(rbind, results)
  result_df <- result_df[order(result_df$P_Value), ]
  return(result_df)
}

run_chisq_for_targets <- function(data, target_vars, cat_vars = NULL) {
  results <- do.call(rbind, lapply(target_vars, function(target) {
    df <- run_chisq_tests_df(data, target_var = target, cat_vars = cat_vars)
    df$Target <- target
    return(df)
  }))
  return(results)
}

#---------------------------------------------------------------------------

render_chisq_summary_table <- function(results_df, 
                                       title = "Chi-Squared Test Results", 
                                       digits = 4, 
                                       begin = 0.2,
                                       end = 0.8) {
  
  caption_html <- paste0(
    "<div style='text-align:center; font-weight:bold; color:black; font-size:16px;'>",
    title,
    "</div>"
  )
  
  # Sort, rank, prepare
  table_df <- results_df %>%
    dplyr::arrange(Target, Statistic) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::select(Rank, Feature, Target, Statistic, P_Value)
  
  # Render HTML table
  knitr::kable(table_df, "html", digits = digits, escape = FALSE,
               caption = caption_html,
               col.names = c("Rank", "Feature", "Target", "Chi-Squared Statistic", "P-Value")) %>%
    style_kable_default(table_df) %>%
    kableExtra::column_spec(5, width = "120px", 
                            background = spec_color(table_df$Statistic, begin = begin, end = end))
}

#---------------------------------------------------------------------------

######################
### Model Training ###
######################

## Split & Train Single Model - With Performance to CSV export
## Function can split time based 

customSummary <- function(data, lev = NULL, model = NULL) {
  obs <- data$obs
  prob_yes <- data$Yes
  pred_class <- ifelse(prob_yes >= 0.5, "Yes", "No")
  pred_class <- factor(pred_class, levels = lev)
  
  # Compute Metrics
  roc_auc <- yardstick::roc_auc_vec(truth = obs, estimate = prob_yes, event_level = "second")
  pr_auc <- yardstick::pr_auc_vec(truth = obs, estimate = prob_yes, event_level = "second")
  precision <- yardstick::precision_vec(truth = obs, estimate = pred_class, event_level = "second")
  recall <- as.numeric(yardstick::recall_vec(truth = obs, estimate = pred_class, event_level = "second"))
  f1_score <- yardstick::f_meas_vec(truth = obs, estimate = pred_class, event_level = "second")
  sens <- yardstick::sens_vec(truth = obs, estimate = pred_class, event_level = "second")
  spec <- yardstick::spec_vec(truth = obs, estimate = pred_class, event_level = "second")
  accuracy <- yardstick::accuracy_vec(truth = obs, estimate = pred_class)
  kappa <- yardstick::kap_vec(truth = obs, estimate = pred_class)  # Add Cohen's Kappa
  
  # Return all metrics
  c(Accuracy = accuracy, Kappa = kappa, ROC = roc_auc, PR_AUC = pr_auc, 
    Precision = precision, Recall = recall, F1 = f1_score, Specificity = spec, Sensitivity = sens)
}

# Generalized Training Function with Time-Based Splitting & Correct Summary Function
train_single_model <- function(model_name, target, feature_set_name, selected_features, dataset, dataset_name, csv_path, tuneGrid = NULL, time_slices = 5) {
  
  # Sort dataset by time (to ensure chronological training)
  dataset <- dataset[order(dataset$admittime), ]
  
  # Define Time-Based Cross-Validation with Correct Summary Function
  ctrl <- if (is.numeric(dataset[[target]])) {
    trainControl(
      method = "timeslice",
      initialWindow = round(0.7 * nrow(dataset)),  # Train on 70% of historical data
      #horizon = round(0.3 * nrow(dataset)),        # Test on 30% of future data
      horizon = round(0.3 * nrow(dataset)),  # Increase test set to 30% of data, 
      fixedWindow = TRUE,
      #skip = round(nrow(dataset) / (time_slices + 1)),  # Number of skipped observations per window
      skip = max(1, round(nrow(dataset) / (time_slices + 1))),
      summaryFunction = defaultSummary,
      savePredictions = "final",
      verboseIter = TRUE
    )
  } else {
    trainControl(
      method = "timeslice",
      initialWindow = round(0.7 * nrow(dataset)),  
      horizon = round(0.3 * nrow(dataset)),
      #horizon = min(100, round(0.2 * nrow(dataset))), 
      fixedWindow = TRUE,
      #skip = round(nrow(dataset) / (time_slices + 1)),  # Number of skipped observations per window
      skip = max(1, round(nrow(dataset) / (time_slices + 1))),
      classProbs = TRUE,
      summaryFunction = customSummary,
      savePredictions = "final",
      sampling = "smote",
      verboseIter = TRUE
    )
  }
  
  # Construct formula
  formula <- as.formula(paste(target, "~", paste(selected_features, collapse = " + ")))
  
  # Train Model
  model <- train(formula, data = dataset, method = model_name, trControl = ctrl, tuneGrid = tuneGrid)
  
  results <- model$results  
  
  if (nrow(results) == 0) {
    results <- data.frame(
      RMSE = NA, Rsquared = NA, MAE = NA,  # baseline regression metrics
      Accuracy = NA, Kappa = NA, ROC = NA, PR_AUC = NA,
      Sensitivity = NA, Specificity = NA, Precision = NA, Recall = NA, F1 = NA,
      stringsAsFactors = FALSE
    )
  }
  
  # Timestamp of Model Run
  model_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")  # Format as "YYYY-MM-DD HH:MM:SS"
  
  # Extract AIC & BIC if applicable
  final_model <- model$finalModel
  results$AIC <- if ("glm" %in% class(final_model) || "lm" %in% class(final_model)) AIC(final_model) else NA
  results$BIC <- if ("glm" %in% class(final_model) || "lm" %in% class(final_model)) BIC(final_model) else NA
  
  
  all_metrics <- c(
    "Accuracy",    # Standard classification metric (useful for balanced datasets)
    "Kappa",       # Agreement metric useful for imbalanced datasets
    "ROC",         # Area under the ROC curve (classification)
    "PR_AUC",      # Precision-Recall AUC (better for highly imbalanced datasets)
    "Sensitivity", # Sensitivity (Recall) - True Positive Rate
    "Specificity", # Specificity - True Negative Rate
    "Precision",   # Precision (Positive Predictive Value)
    "Recall",      # Recall (Sensitivity)
    "F1",          # F1-Score (harmonic mean of Precision & Recall)
    "RMSE",        # Root Mean Squared Error (regression)
    "Rsquared",    # R² - Variance explained (regression)
    "MAE",         # Mean Absolute Error (regression)
    "AIC",         # Akaike Information Criterion
    "BIC"          # Bayesian Information Criterion
  )
  
  # Ensure all metrics exist, setting missing ones to NA
  for (metric in all_metrics) {
    if (!(metric %in% colnames(results))) {
      results[[metric]] <- NA  # Fill missing columns with NA for consistency
    }
  }
  
  # Ensure numerical values are stored with higher precision
  results[all_metrics] <- lapply(results[all_metrics], function(x) {
    round(as.numeric(x), 6)  # Increase to 6 decimal places
  })
  
  
  # Store tuning parameters for each row in results
  if (!is.null(tuneGrid)) {
    results$Tuning_Params <- apply(model$results[, names(tuneGrid), drop = FALSE], 1, function(row) {
      paste(names(row), "=", row, collapse = "; ")
    })
  } else {
    results$Tuning_Params <- "Default"
  }
  
  # Standardize Output Format
  results$Data_Set <- dataset_name
  results$Timestamp <- model_timestamp
  results$Target <- target
  results$Model <- model_name
  results$Feature_Set <- feature_set_name
  results$Features_Used <- paste(selected_features, collapse = ", ")
  results$Validation_Type <- "Time-Based CV"
  results$Time_Slices <- time_slices
  results$Description <- paste("Model trained on", feature_set_name, "for", target)
  
  
  final_results <- results[, c("Timestamp","Data_Set", "Target", "Model", "Feature_Set", "Features_Used", "Validation_Type", "Time_Slices", "Tuning_Params", "Description", all_metrics), drop = FALSE]
  
  # Save to CSV (Append or Create New)
  if (file.exists(csv_path)) {
    existing_results <- read.csv(csv_path)  # Read existing results
    final_results <- rbind(existing_results, final_results)  # Append new results
  }
  
  write.csv(final_results, csv_path, row.names = FALSE)  # Write everything back
  
  cat("\n Model results saved to:", csv_path, "\n")
  
  print(final_results)
  
  return(model)
  
}

#---------------------------------------------------------------------------

## Split & Train Single Model 2 - With Performance to CSV export
## Function can split time based or by patient, default CV, but 70/30 for final

train_single_model2 <- function(model_name, target, feature_set_name, selected_features, dataset, dataset_name, csv_path, 
                               tuneGrid = NULL, split_type = "patient", train_size = 0.7, time_slices = 5, cv_splits = 5, final_model = FALSE) {
  
  set.seed(42)  # Ensure reproducibility
  
  # Train-test split based on selected method
  if (split_type == "patient") {
    unique_patients <- unique(dataset$subject_id)
    train_patients <- sample(unique_patients, size = round(train_size * length(unique_patients)), replace = FALSE)
    train_data <- dataset[dataset$subject_id %in% train_patients, ]
    test_data <- dataset[!dataset$subject_id %in% train_patients, ]
  } else if (split_type == "time") {
    dataset <- dataset[order(dataset$admittime), ]  
    train_size_count <- round(train_size * nrow(dataset))  
    train_data <- dataset[1:train_size_count, ]
    test_data <- dataset[(train_size_count + 1):nrow(dataset), ]
  } else {
    stop("Invalid split_type. Use 'time' or 'patient'.")
  }
  
  # Define train control based on whether this is the final model
    if (final_model) {
        ctrl <- if (is.numeric(dataset[[target]])) {
          trainControl(
            method = "none",
            summaryFunction = defaultSummary,
            savePredictions = "final",
            verboseIter = TRUE
          )
      } else {
        trainControl(
          method = "none",
          classProbs = TRUE,
          summaryFunction = customSummary,
          savePredictions = "final",
          verboseIter = TRUE
        )
      }
  } else {
    ctrl <- if (is.numeric(dataset[[target]])) {
      trainControl(
        method = if (split_type == "time") "timeslice" else "cv",
        number = if (split_type == "patient") cv_splits else NA,  # number of splits
        initialWindow = if (split_type == "time") round(train_size * nrow(dataset)) else NA,
        horizon = if (split_type == "time") round((1 - train_size) * nrow(dataset)) else NA,
        fixedWindow = if (split_type == "time") TRUE else NA,
        skip = if (split_type == "time") max(1, round(nrow(dataset) / (time_slices + 1))) else NA,
        summaryFunction = defaultSummary,
        savePredictions = "final",
        verboseIter = TRUE
      )
    } else {
      trainControl(
        method = if (split_type == "time") "timeslice" else "cv",
        number = if (split_type == "patient") cv_splits else NA,  # number of splits
        initialWindow = if (split_type == "time") round(train_size * nrow(dataset)) else NA,
        horizon = if (split_type == "time") round((1 - train_size) * nrow(dataset)) else NA,
        fixedWindow = if (split_type == "time") TRUE else NA,
        skip = if (split_type == "time") max(1, round(nrow(dataset) / (time_slices + 1))) else NA,
        classProbs = TRUE,
        summaryFunction = customSummary,
        savePredictions = "final",
        sampling = "smote",  # Only apply SMOTE when using CV, not final model
        verboseIter = TRUE
      )
    }
  }

  # Construct formula
  formula <- as.formula(paste(target, "~", paste(selected_features, collapse = " + ")))
  
  # Train Model
  model <- train(
    formula,
    data = if (final_model) train_data else dataset,
    method = model_name,
    trControl = ctrl,
    tuneGrid = tuneGrid
  )
  
  results <- if (final_model) data.frame() else model$results
  
  if (final_model) {
    is_classification <- is.factor(dataset[[target]])
    
    if (is_classification) {
      prob_yes <- predict(model, newdata = test_data, type = "prob")[, "Yes"]
      obs <- test_data[[target]]
      eval_data <- data.frame(obs = obs, Yes = prob_yes)
      summary_vals <- customSummary(eval_data, lev = levels(obs))
      results <- as.data.frame(as.list(summary_vals))  # convert to 1-row df
    } else {
      preds <- predict(model, newdata = test_data)
      actual <- test_data[[target]]
      summary_vals <- list(
        RMSE = caret::RMSE(preds, actual),
        MAE  = caret::MAE(preds, actual),
        Rsquared = caret::R2(preds, actual),
        MSE  = mean((preds - actual)^2)
      )
      results <- as.data.frame(summary_vals)
    }
  }
  
  # Timestamp of Model Run
  model_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")  # Format as "YYYY-MM-DD HH:MM:SS"
  
  # Extract AIC & BIC if applicable
  model_final <- model$finalModel  # Rename to avoid conflict
  
  results$AIC <- if ("glm" %in% class(model_final) || "lm" %in% class(model_final)) AIC(model_final) else NA
  results$BIC <- if ("glm" %in% class(model_final) || "lm" %in% class(model_final)) BIC(model_final) else NA
  
  
  all_metrics <- c(
    "Accuracy",    # Standard classification metric (useful for balanced datasets)
    "Kappa",       # Agreement metric useful for imbalanced datasets
    "ROC",         # Area under the ROC curve (classification)
    "PR_AUC",      # Precision-Recall AUC (better for highly imbalanced datasets)
    "Sensitivity", # Sensitivity (Recall) - True Positive Rate
    "Specificity", # Specificity - True Negative Rate
    "Precision",   # Precision (Positive Predictive Value)
    "Recall",      # Recall (Sensitivity)
    "F1",          # F1-Score (harmonic mean of Precision & Recall)
    "RMSE",        # Root Mean Squared Error (regression)
    "Rsquared",    # R² - Variance explained (regression)
    "MAE",         # Mean Absolute Error (regression)
    "AIC",         # Akaike Information Criterion
    "BIC"          # Bayesian Information Criterion
  )
  
  # Set missing metrics to NA
  for (metric in all_metrics) {
    if (!(metric %in% colnames(results))) {
      results[[metric]] <- NA  # Fill missing columns with NA for consistency
    }
  }
  
  # stored numerical values with higher precision
  results[all_metrics] <- lapply(results[all_metrics], function(x) {
    round(as.numeric(x), 6)  # Increase to 6 decimal places
  })
  
  
  # Store tuning parameters for each row in results
  if (!is.null(tuneGrid)) {
    if (final_model) {
      # For final models, just log the single grid row you passed
      results$Tuning_Params <- paste(names(tuneGrid), "=", unlist(tuneGrid[1, ]), collapse = "; ")
    } else {
      # For CV models with multiple rows in results
      results$Tuning_Params <- apply(model$results[, names(tuneGrid), drop = FALSE], 1, function(row) {
        paste(names(row), "=", row, collapse = "; ")
      })
    }
  } else {
    results$Tuning_Params <- "Default"
  }
  
  # Validation Type
  if (final_model) {
    validation_type <- "Final Model (No CV)"
  } else if (split_type == "time") {
    validation_type <- "Time-Based CV"
  } else {
    validation_type <- paste0(cv_splits, "-Fold Cross-Validation")
  }
  
  # Split Type
  split_method <- if (split_type == "time") {
    paste0("Time-Split (", round(train_size * 100), "% Train / ", round((1 - train_size) * 100), "% Test)")
  } else {
    paste0("Patient-Split (", round(train_size * 100), "% Train / ", round((1 - train_size) * 100), "% Test)")
  }
  
  split_feature <- ifelse(split_type == "time", "Time", "Patient")
  
  # Compute Train/Test Row Counts
  train_count <- nrow(train_data)
  test_count <- nrow(test_data)
  
  # Standardize Output Format
  results$Data_Set <- dataset_name
  results$Timestamp <- model_timestamp
  results$Target <- target
  results$Model <- model_name
  results$Feature_Set <- feature_set_name
  results$Features_Used <- paste(selected_features, collapse = ", ")
  results$Validation_Type <- validation_type
  results$Split_Type <- split_method
  results$Split_Feature <- split_feature  # Now simply "Time" or "Patient"
  results$Splits <- ifelse(final_model, NA, ifelse(split_type == "time", time_slices, cv_splits))
  results$Train_Percentage <- paste0(round(train_size * 100), "%")
  results$Description <- paste("Model trained on", feature_set_name, "for", target)
  results$Train_Count <- nrow(train_data)
  results$Test_Count <- nrow(test_data)
  
  
  final_results <- results[, c("Timestamp","Data_Set", "Target", "Model", "Feature_Set", "Features_Used", "Validation_Type", "Split_Type", "Split_Feature" ,"Splits", "Train_Percentage" ,"Tuning_Params", "Description", "Train_Count","Test_Count", all_metrics), drop = FALSE]
  
  # model$train_data <- train_data
  model$test_data  <- test_data
  
  # Save to CSV (Append or Create New)
  if (file.exists(csv_path)) {
    existing_results <- read.csv(csv_path)  # Read existing results
    final_results <- rbind(existing_results, final_results)  # Append new results
  }
  
  write.csv(final_results, csv_path, row.names = FALSE)  # Write everything back
  
  cat("\n Model results saved to:", csv_path, "\n")
  
  print(final_results)

  return(model)
  
}

#---------------------------------------------------------------------------


## Split & Train Single Model 3 - With Performance to CSV export
## Function can split time based or by patient, default CV, but 70/30 for final
## Fixed CV split with leakage :C

train_single_model3 <- function(model_name, target, feature_set_name, selected_features, dataset, dataset_name, csv_path, 
                                tuneGrid = NULL, split_type = "patient", train_size = 0.7, time_slices = 5, cv_splits = 5, final_model = FALSE) {
  
  set.seed(42)  # Ensure reproducibility
  
  # --- Train/Test Split (only used for final_model = TRUE)
  if (split_type == "patient") {
    unique_patients <- unique(dataset$subject_id)
    train_patients <- sample(unique_patients, size = round(train_size * length(unique_patients)), replace = FALSE)
    train_data <- dataset[dataset$subject_id %in% train_patients, ]
    test_data <- dataset[!dataset$subject_id %in% train_patients, ]
  } else {
    stop("Currently only 'patient' split_type is supported.")
  }
  
  # --- Train Control Setup
  if (final_model) {
    ctrl <- if (is.numeric(dataset[[target]])) {
      trainControl(
        method = "none",
        summaryFunction = defaultSummary,
        savePredictions = "final",
        verboseIter = TRUE
      )
    } else {
      trainControl(
        method = "none",
        classProbs = TRUE,
        summaryFunction = customSummary,
        savePredictions = "final",
        verboseIter = TRUE
      )
    }
  } else {
    # Cross-validation folds by unique patient
    set.seed(42)
    all_patients <- unique(dataset$subject_id)
    patient_folds <- createFolds(all_patients, k = cv_splits)
    
    # Translate patient IDs to row indices in dataset
    index <- lapply(patient_folds, function(p_ids) {
      which(dataset$subject_id %in% all_patients[p_ids])
    })
    
    ctrl <- if (is.numeric(dataset[[target]])) {
      trainControl(
        method = "cv",
        number = cv_splits,
        index = index,
        summaryFunction = defaultSummary,
        savePredictions = "final",
        verboseIter = TRUE
      )
    } else {
      trainControl(
        method = "cv",
        number = cv_splits,
        index = index,
        classProbs = TRUE,
        summaryFunction = customSummary,
        savePredictions = "final",
        sampling = "smote",
        verboseIter = TRUE
      )
    }
  }
  
  # Construct formula
  formula <- as.formula(paste(target, "~", paste(selected_features, collapse = " + ")))
  
  # Train Model
  model <- train(
    formula,
    data = if (final_model) train_data else dataset,
    method = model_name,
    trControl = ctrl,
    tuneGrid = tuneGrid
  )
  
  results <- if (final_model) data.frame() else model$results
  
  if (final_model) {
    is_classification <- is.factor(dataset[[target]])
    
    if (is_classification) {
      prob_yes <- predict(model, newdata = test_data, type = "prob")[, "Yes"]
      obs <- test_data[[target]]
      eval_data <- data.frame(obs = obs, Yes = prob_yes)
      summary_vals <- customSummary(eval_data, lev = levels(obs))
      results <- as.data.frame(as.list(summary_vals))  # convert to 1-row df
    } else {
      preds <- predict(model, newdata = test_data)
      actual <- test_data[[target]]
      summary_vals <- list(
        RMSE = caret::RMSE(preds, actual),
        MAE  = caret::MAE(preds, actual),
        Rsquared = caret::R2(preds, actual),
        MSE  = mean((preds - actual)^2)
      )
      results <- as.data.frame(summary_vals)
    }
  }
  
  # Timestamp of Model Run
  model_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")  # Format as "YYYY-MM-DD HH:MM:SS"
  
  # Extract AIC & BIC if applicable
  model_final <- model$finalModel  # Rename to avoid conflict
  
  results$AIC <- if ("glm" %in% class(model_final) || "lm" %in% class(model_final)) AIC(model_final) else NA
  results$BIC <- if ("glm" %in% class(model_final) || "lm" %in% class(model_final)) BIC(model_final) else NA
  
  
  all_metrics <- c(
    "Accuracy",    # Standard classification metric (useful for balanced datasets)
    "Kappa",       # Agreement metric useful for imbalanced datasets
    "ROC",         # Area under the ROC curve (classification)
    "PR_AUC",      # Precision-Recall AUC (better for highly imbalanced datasets)
    "Sensitivity", # Sensitivity (Recall) - True Positive Rate
    "Specificity", # Specificity - True Negative Rate
    "Precision",   # Precision (Positive Predictive Value)
    "Recall",      # Recall (Sensitivity)
    "F1",          # F1-Score (harmonic mean of Precision & Recall)
    "RMSE",        # Root Mean Squared Error (regression)
    "Rsquared",    # R² - Variance explained (regression)
    "MAE",         # Mean Absolute Error (regression)
    "AIC",         # Akaike Information Criterion
    "BIC"          # Bayesian Information Criterion
  )
  
  # Set missing metrics to NA
  for (metric in all_metrics) {
    if (!(metric %in% colnames(results))) {
      results[[metric]] <- NA  # Fill missing columns with NA for consistency
    }
  }
  
  # stored numerical values with higher precision
  results[all_metrics] <- lapply(results[all_metrics], function(x) {
    round(as.numeric(x), 6)  # Increase to 6 decimal places
  })
  
  
  # Store tuning parameters for each row in results
  if (!is.null(tuneGrid)) {
    if (final_model) {
      # For final models, just log the single grid row you passed
      results$Tuning_Params <- paste(names(tuneGrid), "=", unlist(tuneGrid[1, ]), collapse = "; ")
    } else {
      # For CV models with multiple rows in results
      results$Tuning_Params <- apply(model$results[, names(tuneGrid), drop = FALSE], 1, function(row) {
        paste(names(row), "=", row, collapse = "; ")
      })
    }
  } else {
    results$Tuning_Params <- "Default"
  }
  
  # Validation Type
  if (final_model) {
    validation_type <- "Final Model (No CV)"
  } else if (split_type == "time") {
    validation_type <- "Time-Based CV"
  } else {
    validation_type <- paste0(cv_splits, "-Fold Cross-Validation")
  }
  
  # Split Type
  split_method <- if (split_type == "time") {
    paste0("Time-Split (", round(train_size * 100), "% Train / ", round((1 - train_size) * 100), "% Test)")
  } else {
    paste0("Patient-Split (", round(train_size * 100), "% Train / ", round((1 - train_size) * 100), "% Test)")
  }
  
  split_feature <- ifelse(split_type == "time", "Time", "Patient")
  
  # Compute Train/Test Row Counts
  train_count <- nrow(train_data)
  test_count <- nrow(test_data)
  
  # Standardize Output Format
  results$Data_Set <- dataset_name
  results$Timestamp <- model_timestamp
  results$Target <- target
  results$Model <- model_name
  results$Feature_Set <- feature_set_name
  results$Features_Used <- paste(selected_features, collapse = ", ")
  results$Validation_Type <- validation_type
  results$Split_Type <- split_method
  results$Split_Feature <- split_feature  # Now simply "Time" or "Patient"
  results$Splits <- ifelse(final_model, NA, ifelse(split_type == "time", time_slices, cv_splits))
  results$Train_Percentage <- paste0(round(train_size * 100), "%")
  results$Description <- paste("Model trained on", feature_set_name, "for", target)
  results$Train_Count <- nrow(train_data)
  results$Test_Count <- nrow(test_data)
  
  
  final_results <- results[, c("Timestamp","Data_Set", "Target", "Model", "Feature_Set", "Features_Used", "Validation_Type", "Split_Type", "Split_Feature" ,"Splits", "Train_Percentage" ,"Tuning_Params", "Description", "Train_Count","Test_Count", all_metrics), drop = FALSE]
  
  # model$train_data <- train_data
  model$test_data  <- test_data
  
  # Save to CSV (Append or Create New)
  if (file.exists(csv_path)) {
    existing_results <- read.csv(csv_path)  # Read existing results
    final_results <- rbind(existing_results, final_results)  # Append new results
  }
  
  write.csv(final_results, csv_path, row.names = FALSE)  # Write everything back
  
  cat("\n Model results saved to:", csv_path, "\n")
  
  print(final_results)
  
  return(model)
  
}


#---------------------------------------------------------------------------

## Call Model Train Fucntion

train_and_assign <- function(
    train_function = get("train_function", envir = .GlobalEnv),  
    model_type, feature_set_name, selected_features, tuneGrid = NULL, 
    dataset = get("data", envir = .GlobalEnv), 
    dataset_name = get("dataset_name", envir = .GlobalEnv),
    target = get("target", envir = .GlobalEnv),
    file_name = get("file_name", envir = .GlobalEnv),
    slices = get("slices", envir = .GlobalEnv),
    model_run = get("model_run", envir = .GlobalEnv),
    split_type = get("split_type", envir = .GlobalEnv),
    train_size = get("train_size", envir = .GlobalEnv),
    cv_splits = get("cv_splits", envir = .GlobalEnv),
    final_model = get("final_model", envir = .GlobalEnv)
) {
  
  # Define dynamic model name
  model_name <- paste0(model_type, "_", gsub(" ", "_", tolower(feature_set_name)), "_", target, "_", model_run)
  
  # Call the selected training function dynamically
  trained_model <- do.call(train_function, list(
    model_name = model_type,
    target = target,
    feature_set_name = feature_set_name,
    selected_features = selected_features,
    dataset = dataset,
    dataset_name = dataset_name,
    csv_path = here("results", file_name),
    time_slices = slices,
    tuneGrid = tuneGrid,
    split_type = split_type,
    train_size = train_size,
    cv_splits = cv_splits,
    final_model = final_model
  ))
  
  # Assign the trained model dynamically
  assign(model_name, trained_model, envir = .GlobalEnv)
  
  return(model_name)
}

#---------------------------------------------------------------------------

feature_selection <- function(
    model, 
    target, 
    features, 
    dataset,
    fast_mode = TRUE,
    return_stats = FALSE
) {
  model_type <- model$method
  selected_features <- NULL
  is_classification <- is.factor(dataset[[target]])
  
  if (model_type %in% c("rf", "gbm", "xgbTree", "svmRadial", "svmLinear", "nnet", "naive_bayes")) {
    
    imp <- varImp(model, scale = TRUE)
    imp_df <- imp$importance
      
    if (!"Overall" %in% colnames(imp_df)) {
      overall_col <- colnames(imp_df)[1]
      imp_df$Overall <- imp_df[[overall_col]]
    }
    
    imp_df <- imp_df %>%
      tibble::rownames_to_column("feature") %>%
      dplyr::mutate(rank = rank(-Overall)) %>%
      dplyr::arrange(rank)
    
    selected_features <- imp_df$feature[imp_df$Overall >= 1]
    
    if (return_stats) return(imp_df)
    
  } else if (model_type %in% c("glm", "bayesglm")) {
    
    formula <- as.formula(paste(target, "~", paste(features, collapse = "+")))
    family_used <- if (is_classification) binomial() else gaussian()
    full_model <- glm(formula, data = dataset, family = family_used)
    step_model <- stepAIC(full_model, direction = "backward", trace = FALSE)
    selected_features <- names(coef(step_model))[-1]
    
    if (return_stats) {
      return(tibble::tibble(
        feature = selected_features,
        rank = seq_along(selected_features)
      ))
    }
    
  } else if (!is.null(varImp(model, scale = FALSE))) {
    
    imp <- varImp(model, scale = TRUE)
    imp_df <- imp$importance
      
    if (!"Overall" %in% colnames(imp_df)) {
      stop(paste("No 'Overall' column found in importance for model:", model_type))
    }
    
    imp_df <- imp_df %>%
      tibble::rownames_to_column("feature") %>%
      dplyr::mutate(rank = rank(-Overall)) %>%
      dplyr::arrange(rank)
    
    selected_features <- imp_df$feature[imp_df$Overall >= 1]
    
    if (return_stats) return(imp_df)
    
  } else {
    stop(paste("Feature selection not implemented for model type:", model_type))
  }
  
  return(selected_features)
}


#----------------------------------------------------

get_model_data_and_features <- function(model_obj, target) {
  if (!is.null(model_obj$trainingData)) {
    data <- model_obj$trainingData
    data[[target]] <- data$.outcome  
    features <- setdiff(names(data), c(".outcome", target))
    return(list(data = data, features = features))
  }
}


#----------------------------------------------------

get_training_ready_features <- function(model_type, feature_set, target, model_run, feature_selection_results, original_feature_list) {
  key <- paste(model_type, feature_set, target, model_run, sep = "_")
  model_result <- feature_selection_results[[key]]
  
  if (is.null(model_result)) {
    warning("Missing model result for: ", key)
    return(character(0))
  }
  
  removed_feats <- model_result$removed
  usable_feats <- setdiff(original_feature_list, removed_feats)
  
  return(usable_feats)
}


########################
### Model Evaluation ###
########################


add_predictions_to_models <- function(models, target_col = "hospital_expire_flag") {
  for (i in seq_along(models)) {
    model <- models[[i]]
    
    test_data <- model$test_data
    probs <- predict(model, newdata = test_data, type = "prob")[, "Yes"]

    pred_df <- data.frame(
      rowIndex = 1:nrow(test_data),
      obs = test_data[[target_col]],
      Yes = probs
    )
    
    model$pred <- pred_df
    
    models[[i]] <- model
  }
  return(models)
}

### ROC AUC Plot

plot_roc_curves <- function(models, model_names = names(models), target, plot_title = "ROC Curves") {
  roc_curves <- list()
  legend_labels <- character()
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- model_names[i]
    
    # Extract model type (first part before "_")
    model_type <- strsplit(model_name, "_")[[1]][1]
    
    # Detect if SDoH or Base
    base_or_sdoh <- if (grepl("sdoh", tolower(model_name))) "(SDoH)" else "(Base)"
    
    # Create clean legend label
    label <- paste(model_type, base_or_sdoh)
    
    test_preds <- model$pred  
    test_preds_filtered <- test_preds[test_preds$rowIndex %in% unique(test_preds$rowIndex), ]
    
    actual_values <- test_preds_filtered$obs
    pred_probs <- if ("Yes" %in% names(test_preds_filtered)) {
      test_preds_filtered$Yes
    } else if ("pred_prob" %in% names(test_preds_filtered)) {
      test_preds_filtered$pred_prob
    } else {
      stop("Prediction probability column not found.")
    }
    
    roc_obj <- pROC::roc(response = actual_values, predictor = pred_probs)
    # Store curve with unique internal key
    internal_label <- paste0(model_name, "_", i)
    roc_curves[[internal_label]] <- roc_obj
    legend_labels <- c(legend_labels, label)
  }
  
  # Plot the first ROC curve
  plot.roc(roc_curves[[1]], col = 1, main = paste(plot_title, "-", target),
           xlab = "False Positive Rate (1 - Specificity)",
           ylab = "True Positive Rate (Sensitivity)", lwd = 2, legacy.axes = TRUE)
  
  # Overlay others
  for (i in 2:length(roc_curves)) {
    plot.roc(roc_curves[[i]], col = i, add = TRUE, lwd = 2, lty = i)
  }
  
  abline(0, 1, col = "gray", lty = 2)
  
  # Legend with short names
  legend("bottomright", legend = legend_labels, col = 1:length(roc_curves), lwd = 2, lty = 1:length(roc_curves))
}

#---------------------------------------------------------------------------

### PR AUC Plot

library(PRROC)

plot_pr_curves <- function(models, model_names, target, plot_title = "PR AUC Curves") {
  pr_curves <- list()
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- model_names[i]
    
    # Extract test predictions stored in `model$pred`
    test_preds <- model$pred  
    
    # Filter to only test set predictions (based on rowIndex)
    test_preds_filtered <- test_preds[test_preds$rowIndex %in% unique(test_preds$rowIndex), ]
    
    # Extract actual test labels and predicted probabilities
    actual_values <- test_preds_filtered$obs  # True labels
    pred_probs <- test_preds_filtered$Yes  # Predicted probabilities for "Yes"
    
    # Convert actual values to numeric (1 = "Yes", 0 = "No")
    actual_values_numeric <- ifelse(actual_values == "Yes", 1, 0)
    
    # Compute Precision-Recall Curve
    pr_obj <- pr.curve(scores.class0 = pred_probs, weights.class0 = actual_values_numeric, curve = TRUE)
    
    # Store PR Curve for later comparison
    pr_curves[[model_name]] <- pr_obj
  }
  
  # Set up plot margins to leave space for legend
  par(mar = c(5, 4, 4, 8) + 0.1)  # Increase right margin for legend space
  
  # Define colors and line types
  colors <- rainbow(length(pr_curves))
  line_types <- rep(1:6, length.out = length(pr_curves))  # Cycle through different line types
  
  # Plot PR Curves
  plot(pr_curves[[1]]$curve, type = "l", col = colors[1], lwd = 2, lty = line_types[1], 
       xlab = "Recall", ylab = "Precision", 
       main = paste(plot_title, "-", target), 
       xlim = c(0, 1), ylim = c(0, 1))
  
  for (i in 2:length(pr_curves)) {
    lines(pr_curves[[i]]$curve, col = colors[i], lwd = 2, lty = line_types[i])
  }
  
  # random guess line
  p <- mean(actual_values_numeric)  # Proportion of positive cases
  abline(h = p, col = "black", lwd = 2, lty = 2)
  
  # Move the legend **outside** the plot area (to the right)
  legend("topright", inset = c(-0.3, 0), legend = model_names, col = colors, lwd = 2, lty = line_types, 
         bty = "n", cex = 0.8, xpd = TRUE)
}

#---------------------------------------------------------------------------

## Prediciton Probabilites Plot

extract_predicted_probs <- function(models) {
  if (!is.list(models)) models <- list(models)
  results_list <- list()
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    if (!"pred" %in% names(model) || nrow(model$pred) == 0) next
    
    test_preds <- model$pred
    test_preds_filtered <- test_preds[test_preds$rowIndex %in% unique(test_preds$rowIndex), ]
    if (nrow(test_preds_filtered) == 0) next
    
    results_list[[model_name]] <- data.frame(
      predicted_prob = test_preds_filtered$Yes,
      actual = test_preds_filtered$obs,
      model = model_name
    )
  }
  if (length(results_list) > 0) return(do.call(rbind, results_list))
  return(NULL)
}

library(ggplot2)

plot_prediction_histograms <- function(results_df) {
  if (is.null(results_df)) return(warning("No valid predictions found!"))
  
  for (model_name in unique(results_df$model)) {
    print(
      ggplot(subset(results_df, model == model_name), aes(x = predicted_prob, fill = actual)) +
        geom_histogram(binwidth = 0.05, position = "dodge", alpha = 0.7) +
        scale_fill_manual(values = c("No" = "lightblue", "Yes" = "red")) +
        labs(title = paste("Prediction Probability Distribution -", model_name),
             x = "Predicted Probability", y = "Count")
    )
  }
}


#---------------------------------------------------------------------------

## Numeric Plots

evaluate_model <- function(model, target, model_name) {
  test_data <- model$test_data
  predictions <- predict(model, newdata = test_data)
  actuals <- test_data[[target]]
  
  df <- data.frame(Predicted = predictions, Actual = actuals, Residuals = actuals - predictions)
  
  p1 <- ggplot(df, aes(x = Predicted, y = Actual)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
    ggtitle(paste("Actual vs. Predicted -", model_name)) +
    xlab("Predicted") + ylab("Actual")
  
  p2 <- ggplot(df, aes(x = Residuals)) +
    geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.6) +
    ggtitle(paste("Residuals -", model_name)) +
    xlab("Residuals") + ylab("Count")
  
  return(list(actual_vs_pred = p1, residual_hist = p2))
}

#---------------------------------------------------------------------------


