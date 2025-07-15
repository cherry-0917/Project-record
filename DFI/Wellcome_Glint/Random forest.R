Sys.setenv(LANG = "en")
library(readr)
library(dplyr)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint")
#Without duplicate
data = read.csv("./score_wellcome.csv")
newdata = read.csv("./score_wellcome_duplicate.csv")
#duplicate--> not use
#tdata <-read.csv("./score_wellcome_new.csv",header = TRUE)
#overall
odata = read.csv("./score_overall.csv")
o1 = odata%>%filter(Group ==1)
o2 = odata%>%filter(Group ==2)
train_models <- function(target_string, variables, data, base_dir, var_labels, number_cols, interactions = FALSE) {
  # Create directory structure
  new_dir <- base_dir
  dir.create(new_dir, recursive = TRUE, showWarnings = FALSE)
  setwd(new_dir)
  # Load required libraries
  library(dplyr)
  library(randomForest)
  library(caret)
  library(pdp)
  library(ggplot2)
  library(gridExtra)
  library(tidyr)
  library(knitr)
  library(glmnet)
  library(xgboost)
  # Create model formula
  if(target_string %in% variables) {
    variables <- setdiff(variables, target_string)
    warning(paste("Removed target variable", target_string, "from predictors"))
  }
  
  formula <- as.formula(paste(target_string, "~", paste(variables, collapse = " + ")))
  
  # Train/test split
  set.seed(123)
  trainIndex <- createDataPartition(data[[target_string]], p = 0.7, list = FALSE)
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  # # Median imputation using training data only
  # preProc <- preProcess(train_data_raw, method = "medianImpute")
  # train_data <- predict(preProc, train_data_raw)
  # test_data <- predict(preProc, test_data_raw)
  
  # Train Benchmark Models
  model_list <- list()
  model_summaries <- list()
  # Linear Regression (OLS)
  lm_model <- train(
    formula,
    data = train_data,
    method = "lm",
    trControl = trainControl(method = "none")
  )
  lm_summary <- capture.output(summary(lm_model$finalModel))
  writeLines(lm_summary, "lm_summary.txt")
  model_summaries$LM <- lm_summary
  
  # Train Random Forest
  set.seed(123)
  rf_model <- randomForest(
    formula,
    data = train_data,
    ntree = 40,
    mtry = max(floor(sqrt(length(variables)/3)), 1),  # Dynamic mtry
    importance = TRUE
  )
  
  rf_summary <- capture.output(
    cat("Random Forest Model Summary\n"),
    # cat("Number of trees:", rf_model$ntree, "\n"),
    # cat("Variables per split:", rf_model$mtry, "\n"),
    print(rf_model),
    cat("\nVariable Importance:\n"),
    print(importance(rf_model))
  )
  writeLines(rf_summary, "rf_summary.txt")
  model_summaries$RandomForest <- rf_summary
  
  rf_cv <- train(
    formula,
    data = train_data,
    method = "rf",
    trControl = trainControl(method = "cv", number = 10),
    ntree = 40,
    tuneGrid = data.frame(mtry = max(floor(round(sqrt(length(variables)/3))), 1))
  )
  
  
  # Evaluate all models on test set
  evaluate_model <- function(model, test_data) {
    preds <- predict(model, newdata = test_data)
    postResample(preds, test_data[[target_string]])
  }

  model_performance <- list(
    LM = evaluate_model(lm_model, test_data),
    RandomForest = evaluate_model(rf_model, test_data)
  )
  
  # Save performance comparison
  perf_df <- do.call(rbind, model_performance) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Model")
  
  perf_md <- knitr::kable(perf_df, format = "markdown", digits = 4)
  writeLines(perf_md, "model_performance_comparison.md")
  
  # Partial dependence plots function
  generate_pd_plots <- function(model, vars, train_data, number_cols, var_labels) {
    plot_list <- lapply(vars, function(var) {
      pd <- partial(model, pred.var = var, train = train_data)
      means <- sapply(variables, function(var) mean(train_data[[var]]))
      # window_size <- 13
      # max_slope <- -Inf
      # x_start <- NA
      # x_end <- NA
      # for (i in 1:(nrow(train_data) - window_size + 1)) {
      #   segment <- train_data[i:(i + window_size - 1), ]
      #   model_segment <- lm(target_string ~ get(var), data = segment)
      #   slope <- abs(coef(model_segment)[2])
      #   if (slope > max_slope) {
      #     max_slope <- slope
      #     x_start <- segment[[var]][1]
      #     x_end <- segment[[var]][window_size]
      #   }
      # }
      # # Print the maximum slope and its range (optional)
      # annotation_text <- paste("Max. Slope range:", x_start, "-", x_end)
      plot_title <- var_labels[[var]] 
      ggplot(pd, aes(x = .data[[var]], y = yhat)) +
        geom_line(color = "steelblue", linewidth = 1) +
        scale_x_continuous(limits=c(50,100))+
        scale_y_continuous(limits=c(50,100))+
        geom_vline(xintercept = means[var], color = "blue",linetype = "dashed", size=1)+
        #geom_vline(xintercept = xmax_slope, color = "red", size=1)+
        labs(title = plot_title, x = "", y = "Partial Dependence") +
        theme_bw() +
        theme(plot.title = element_text(size = 10))
    })
    grid.arrange(grobs = plot_list, ncol = number_cols)
  }

  
  # Generate and save plots
  pd_plot <- generate_pd_plots(rf_model, variables, train_data, number_cols, var_labels)
  ggsave("./rf_partial_plot.png", plot = pd_plot, width = 15, height = 10)
  
  imp <- importance(rf_model)
  imp_mse <- imp[, "%IncMSE"]        # Mean decrease in accuracy
  imp_node <- imp[, "IncNodePurity"] # Node purity increase
  
  base_values <- sapply(variables, function(var) mean(train_data[[var]], na.rm = TRUE))
  observations_at_max <- sapply(variables, function(var) sum(train_data[[var]] == 100, na.rm = TRUE))

  # Calculate max_pred at 80 and 90 per variable
  base_point <- as.data.frame(t(base_values), stringsAsFactors = FALSE)
  colnames(base_point) <- names(base_values)
  
  # Function to calculate max predictions
  calculate_max_pred <- function(value, base_point, rf_model) {
    sapply(colnames(base_point), function(var) {
      test_point <- base_point  # Start with baseline
      test_point[1, var] <- value  # Modify only this variable
      predict(rf_model, newdata = test_point)
    })
  }
  
  # Calculate predictions
  max_pred_at_80_per_var <- calculate_max_pred(80, base_point, rf_model)
  max_pred_at_90_per_var <- calculate_max_pred(90, base_point, rf_model)
  
  saturated_vars <- round(max_pred_at_90_per_var - max_pred_at_80_per_var,2) < 0.5
  max_attainable <- ifelse(saturated_vars, 80, 100)
    
  # Create Calculate proportional increases based on importance
  compute_proportional_increases <- function(rf_model, base_values, target_score, max_attainable) {
    # Calculate baseline prediction
    base_point <- as.data.frame(t(base_values), stringsAsFactors = FALSE)
    colnames(base_point) <- names(base_values)
    base_pred <- predict(rf_model, newdata = base_point)
    
    # If already at target, return zeros
    if (base_pred >= target_score) {
      return(rep(0, length(base_values)))
    }
    
    # Calculate importance weights
    #weights <- imp_mse / sum(imp_mse)
    
    max_increase_per_var <- max_attainable - base_values
    
    # Initial proportional increases
    gap <- target_score - base_pred
    increases <- pmin(gap * 10, max_increase_per_var)  # Initial scaling factor
    
    # Iterative adjustment
    max_iter <- 100
    tolerance <- 0.01
    current_pred <- base_pred
    
    for (iter in 1:max_iter) {
      test_values <- base_values + increases
      test_point <- as.data.frame(t(test_values), stringsAsFactors = FALSE)
      colnames(test_point) <- names(base_values)
      current_pred <- predict(rf_model, newdata = test_point)
      
      if (current_pred >= target_score - tolerance) break
      
      # Calculate remaining gap
      remaining_gap <- target_score - current_pred
      
      # Adjust increases proportionally
      adjustment <- remaining_gap * 5  # Adjustment factor
      increases <- pmin(increases + adjustment, max_increase_per_var)  # Cap by max attainable
    }
    
    return(increases)
  }
  
  # Calculate for both targets
  req_inc_70 <- compute_proportional_increases(rf_model, base_values, 70, max_attainable)
  req_inc_75 <- compute_proportional_increases(rf_model, base_values, 75, max_attainable)
  

  
  # # Calculate max prediction when all variables at 100
  # max_data_point <- as.data.frame(t(rep(100, length(base_values))), stringsAsFactors = FALSE)
  # colnames(max_data_point) <- names(base_values)
  # max_pred <- predict(rf_model, newdata = max_data_point)
  # 
  # max_pred_value <- numeric(length(variables))
  # max_pred_score <- numeric(length(variables))
  # 
  # for (i in seq_along(variables)) {
  #   var <- variables[i]
  #   grid <- seq(0, 100, by = 0.5)  # Fine-grained grid
  #   predictions <- numeric(length(grid))
  #   
  #   for (j in seq_along(grid)) {
  #     test_point <- base_point
  #     test_point[1, var] <- grid[j]
  #     predictions[j] <- predict(rf_model, newdata = test_point)
  #   }
  #   
  #   idx <- which.max(predictions)
  #   max_pred_value[i] <- grid[idx]
  #   max_pred_score[i] <- predictions[idx]
  # }
  
  n_capped_after <- sapply(variables, function(var) {
    inc <- req_inc_70[var]
    
    if (is.na(inc)) {
      # If no solution found (variable can't reach target even at 100)
      return(sum(train_data[[var]] == 100, na.rm = TRUE)) # Original capped count
    }
    
    # Calculate new values after increase (capped at 100)
    new_values <- pmin(train_data[[var]] + inc, 100)
    
    # Count observations at exactly 100 after increase
    sum(new_values == 100, na.rm = TRUE)
  })
  
  # annotation_texts <- sapply(variables, function(var) {
  #   window_size <- 13
  #   max_slope <- -Inf
  #   x_start <- NA
  #   x_end <- NA
  #   
  #   for (i in 1:(nrow(train_data) - window_size + 1)) {
  #     segment <- train_data[i:(i + window_size - 1), ]
  #     formula_str <- as.formula(paste(target_string, "~", var))
  #     model_segment <- lm(formula_str, data = segment)
  #     slope <- abs(coef(model_segment)[2])
  #     if (slope > max_slope) {
  #       max_slope <- slope
  #       x_start <- segment[[var]][1]
  #       x_end <- segment[[var]][window_size]
  #     }
  #   }
  #   paste(x_start, "-", x_end)
  # })
  

  imp_df <- data.frame(
    #Variable = variables,
    Imp = imp_mse,
    IncNodePurity = imp_node,
    Imp_Rank_MSE = rank(-imp_mse, ties.method = "min"),
    Imp_Rank_Gini = rank(-imp_node, ties.method = "min"),
    Baseline = base_values,
    #req_inc_70 = req_inc_70,   
    #req_inc_75 = req_inc_75,   
    #max_pred_at_80 = max_pred_at_80_per_var,  # Per variable
    #max_pred_at_90 = max_pred_at_90_per_var,
    #max_value = max_pred_value, # value at max prediction
    #max_pred_score = max_pred_score, # Per variable
    #Max_slope_range = annotation_texts,
    n_capped = n_capped_after,
    stringsAsFactors = FALSE
  ) %>%
    arrange(Imp_Rank_MSE)
  
  md_table <- knitr::kable(imp_df, format = "markdown", digits = 4)
  writeLines(md_table, "variable_importance.txt")

  
  # Overfitting Check 
  train_pred <- predict(rf_model, newdata = train_data)
  train_perf <- postResample(train_pred, train_data[[target_string]])
  
  test_pred <- predict(rf_model, newdata = test_data)
  test_perf <- postResample(test_pred, test_data[[target_string]])
  
  # Calculate performance differences
  rmse_diff <- test_perf["RMSE"] - train_perf["RMSE"]
  rsq_diff <- train_perf["Rsquared"] - test_perf["Rsquared"]
  mae_diff <- test_perf["MAE"] - train_perf["MAE"]
  
  # Determine if overfitting is significant
  is_overfit <- ifelse(
    (rmse_diff > 0.5 * train_perf["RMSE"]) |  # Test RMSE > 150% of train RMSE
      (rsq_diff > 0.15) |                       # R² drops more than 0.15
      (test_perf["Rsquared"] < 0.5) |           # Test R² unacceptably low
      (mae_diff > 0.5 * train_perf["MAE"]),     # Test MAE > 150% of train MAE
    "Yes", 
    "No"
  )
  
  overfit_check <- data.frame(
    Dataset = c("Training", "Test"),
    RMSE = c(train_perf["RMSE"], test_perf["RMSE"]),
    Rsquared = c(train_perf["Rsquared"], test_perf["Rsquared"]),
    MAE = c(train_perf["MAE"], test_perf["MAE"]),  # Added MAE column
    Overfitting = c(NA, is_overfit)  # Only show for test set
  )
  
  # Save overfitting check
  overfit_md <- knitr::kable(overfit_check, format = "markdown", digits = 4)
  writeLines(overfit_md, "overfit_check.md")


  return(list(
    model = rf_model,
    importance = imp_df,
    md_table = md_table,
    performance = model_performance,
    overfit = overfit_check,
    partial_plot = pd_plot,
    increases_70 = req_inc_70,  
    increases_75 = req_inc_75,  
    #max_pred = max_pred,        # Maximum prediction
    benchmark_models = list(
      lm = lm_model
    ),
    model_summaries = model_summaries
  ))
}

run_analysis <- function(data, targets, variables_list, base_dir, var_labels, number_cols, interactions = FALSE) {
  results <- list()
  
  for (target in targets) {
    cat("Running analysis for:", target, "\n")
    target_vars <- variables_list[[target]]
    
    # Create target-specific directory
    target_dir <- file.path(base_dir, target)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Run modeling
    results[[target]] <- train_models(
      target_string = target,
      variables = target_vars,
      data = data,
      base_dir = target_dir,
      var_labels = var_labels,
      number_cols = number_cols,
      interactions = interactions
    )
  }
  
  return(results)
}

new_names <- c(
  "Recommend my Manager" = "avg_score_Q7",
  "Action after Feedback" = "avg_score_Q8",
  "Asked Opinion" = "avg_score_Q9",
  "Training for Confidence" = "avg_score_Q10",
  "Career Opportunities" = "avg_score_Q11",
  "Recommend our Stores" = "avg_score_Q12",
  "Trust each other" = "avg_score_Q15",
  "Trust Leadership" = "avg_score_Q16",
  "Provide Feedback for Improvement" = "avg_score_Q17",
  "Satisfied with Recognition" = "avg_score_Q18",
  "Great Customers Performance" = "avg_score_Q19",
  "Proud of Company" = "avg_score_Q20",
  "Consider Customer's Needs" = "avg_score_Q21",
  "Empowered to Make Decisions" = "avg_score_Q22",
  "Meaningful Work" = "avg_score_Q23",
  "Given Enough Resources" = "avg_score_Q24",
  "Live Company Values" = "avg_score_Q25",
  "Being Cared" = "avg_score_Q26",
  "Equal Opportunity for Success" = "avg_score_Q27",
  "Free to Speak my Mind" = "avg_score_Q28",
  "Given Technology Resources" = "avg_score_Q29",
  "Supporting Communities" = "avg_score_Q30",
  "SSC Collaborate Effectively" = "avg_score_Q31",
  "SSC Good Team Communicate" = "avg_score_Q32",
  "Leadership Team Demostrates Commitment" = "avg_score_Q33",
  "Excited about Company's Future" = "avg_score_Q34",
  "SSC Adapt Changes" = "avg_score_Q35",
  "SSC deliver commitments" = "avg_score_Q36",
  "Clear other's Responsiblity" = "avg_score_Q37",
  "Being held accountable for work" = "avg_score_Q38"
)
reversed_names <- setNames(names(new_names), new_names)
create_variable_labels <- function(predictor_vars) {
  sapply(predictor_vars, function(var) {
    if (var %in% names(reversed_names)) {
      reversed_names[[var]]
    } else {
      var
    }
  }, USE.NAMES = TRUE)
}
# Define survey specifications
survey_specs <- list(
  Gallup = list(
    data = data,  
    predictor_vars = paste0("avg_score_Q", c(10,17,18,22,24,29)),
    targets = c("Satisfy_score1", "Manager_score", "Belonging_score"),
    base_dir = "C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Gallup",
    interactions = FALSE,
    number_cols = 3
  ),
  Glint = list(
    data = data,  
    predictor_vars = paste0("avg_score_Q", c(7:12, 15:38)),
    targets = c("Satisfy_score2", "avg_score_Q2", "avg_score_Q4", "avg_score_Q5", "avg_score_Q6"),
    base_dir = "C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Glint",
    interactions = FALSE,  # Disabled due to high dimensionality
    number_cols = 5
  )
)
# Create variables lists for each survey
create_variables_list <- function(spec) {
  setNames(
    lapply(spec$targets, function(x) spec$predictor_vars),
    spec$targets
  )
}

# Run analyses for both surveys
all_results <- list()

for (survey in names(survey_specs)) {
  spec <- survey_specs[[survey]]
  cat("\nRunning analysis for:", survey, "\n")
  
  var_labels <- create_variable_labels(spec$predictor_vars)
  all_results[[survey]] <- run_analysis(
    data = spec$data,
    targets = spec$targets,
    variables_list = create_variables_list(spec),
    base_dir = spec$base_dir,
    var_labels = var_labels,  # Pass var_labels to run_analysis
    number_cols = spec$number_cols,  # Pass number_cols to run_analysis
    interactions = spec$interactions
  )
}

# Additional analysis for overall directories
overall_specs <- list(
  Overall_Gallup = list(
    data = o2,
    base_dir = "C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Overall/Same/Gallup",
    number_cols = 3
  ),
  Overall_Glint = list(
    data = o2,
    base_dir = "C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Overall/Same/Glint",
    number_cols = 5
  )
)

# Run overall analysis (using all predictors for each target)
for (overall in names(overall_specs)) {
  spec <- overall_specs[[overall]]
  survey_type <- ifelse(grepl("Gallup", overall), "Gallup", "Glint")
  
  # Get all predictor variables from original spec
  predictor_vars <- survey_specs[[survey_type]]$predictor_vars
  targets <- survey_specs[[survey_type]]$targets
  
  variables_list <- setNames(
    rep(list(predictor_vars), length(targets)),
    targets
  )
  var_labels <- create_variable_labels(predictor_vars)
  cat("\nRunning OVERALL analysis for:", overall, "\n")
  
  all_results[[overall]] <- run_analysis(
    data = spec$data,
    targets = targets,
    variables_list = variables_list,
    base_dir = spec$base_dir,
    var_labels = var_labels,
    number_cols = spec$number_cols,
    interactions = FALSE  # Disable interactions for overall
  )
}

# Create final objects
create_result_objects <- function(results, survey_name) {
  list(
    model = results$model,
    importance = results$importance,
    md_table = results$md_table,
    performance = results$performance,
    overfit = results$overfit,
    partial_plot = results$partial_plot,
    increases = results$increases
  )
}

# Generate final objects
final_results <- list()
for (survey in names(all_results)) {
  final_results[[survey]] <- lapply(
    all_results[[survey]], 
    create_result_objects, 
    survey_name = survey
  )
}

#result view
#E.g. final_results$Gallup$Satisfy_score

#########
importance_s_clean <- final_results$Glint$Satisfy_score2$importance
importance_q2_clean <- final_results$Glint$avg_score_Q2$importance
importance_q4_clean <- final_results$Glint$avg_score_Q4$importance
importance_q5_clean <- final_results$Glint$avg_score_Q5$importance
importance_q6_clean <- final_results$Glint$avg_score_Q6$importance



imp_list <- list(
  "2Q_engagement" = importance_s_clean,
  "Satisfactory-work" = importance_q2_clean,
  "Future Success" = importance_q4_clean,
  "Belonging" = importance_q5_clean,
  "Retention" = importance_q6_clean
)
new_names <- c(
  "Recommend my Manager" = "avg_score_Q7",
  "Action after Feedback" = "avg_score_Q8",
  "Asked Opinion" = "avg_score_Q9",
  "Training for Confidence" = "avg_score_Q10",
  "Career Opportunities" = "avg_score_Q11",
  "Recommend our Stores" = "avg_score_Q12",
  "Trust each other" = "avg_score_Q15",
  "Trust Leadership" = "avg_score_Q16",
  "Provide Feedback for Improvement" = "avg_score_Q17",
  "Satisfied with Recognition" = "avg_score_Q18",
  "Great Customers Performance" = "avg_score_Q19",
  "Proud of Company" = "avg_score_Q20",
  "Consider Customer's Needs" = "avg_score_Q21",
  "Empowered to Make Decisions" = "avg_score_Q22",
  "Meaningful Work" = "avg_score_Q23",
  "Given Enough Resources" = "avg_score_Q24",
  "Live Company Values" = "avg_score_Q25",
  "Being Cared" = "avg_score_Q26",
  "Equal Opportunity for Success" = "avg_score_Q27",
  "Free to Speak my Mind" = "avg_score_Q28",
  "Given Technology Resources" = "avg_score_Q29",
  "Supporting Communities" = "avg_score_Q30"
)
name_mapping <- setNames(names(new_names), unname(new_names))

# Get all variables across all importance tables
all_vars <- unique(unlist(lapply(imp_list, rownames)))

# Create base dataframe with original and descriptive names
imp_combine_df <- data.frame(
  Question = all_vars,
  Variables = ifelse(all_vars %in% names(name_mapping),
                     name_mapping[all_vars],
                     all_vars),
  stringsAsFactors = FALSE
)

# Define model names in desired order
model_names <- c("2Q_engagement", "Satisfactory-work", "Future Success", "Belonging", "Retention")

# Add rank columns for each model
for(model in model_names) {
  
  if(model %in% names(imp_list)) {
    imp_df <- imp_list[[model]]
    
    # Create temp df with summed ranks
    temp_df <- data.frame(
      Question = rownames(imp_df),
      Sum = imp_df$Imp_Rank_MSE + imp_df$Imp_Rank_Gini,
      stringsAsFactors = FALSE
    )
    
    # Calculate rank (1 = most important)
    temp_df$Rank <- rank(temp_df$Sum, ties.method = "min")
    
    # Merge ranks into main df
    imp_combine_df[[model]] <- temp_df$Rank[match(imp_combine_df$Question, temp_df$Question)]
  }
}

# Reorder columns
imp_combine_df <- imp_combine_df[, c("Question", "Variables", model_names)]

max_rank <- max(sapply(imp_combine_df[model_names], max, na.rm = TRUE), na.rm = TRUE)
imp_combine_df[model_names] <- lapply(imp_combine_df[model_names], 
                                      function(x) ifelse(is.na(x), max_rank + 1, x))

# Add sum column (total of all model scores)
imp_combine_df$Total_Sum <- rowSums(imp_combine_df[model_names])

# Order by Total_Sum (ascending - lower sum = more important)
imp_combine_df <- imp_combine_df[order(imp_combine_df$Total_Sum), ]

# Reorganize columns to put Total_Sum last
imp_combine_df <- imp_combine_df[, c("Question", "Variables", model_names, "Total_Sum")]


# Add Top 10 count column
imp_combine_df$Top_10_count <- rowSums(imp_combine_df[model_names] <= 10)

# Add Group column based on conditions
imp_combine_df$Group <- with(imp_combine_df, 
                             ifelse(Total_Sum < 35, "High",
                                    ifelse(Total_Sum >= 35 & Total_Sum < 60 & Top_10_count >= 2, "Medium-up",
                                           ifelse(Total_Sum >= 60 & Total_Sum < 65 & Top_10_count >= 2, "Medium-low",
                                           "Low")))
)

imp_combine_df <- imp_combine_df[, c("Question", "Variables", model_names, "Top_10_count", "Total_Sum", "Group")]

######
importance_s <- final_results$Gallup$Satisfy_score1$importance
importance_b <- final_results$Gallup$Manager_score$importance
importance_m <- final_results$Gallup$Belonging_score$importance

g_imp_list <- list(
  "Satisfatory score" = importance_s,
  "Belonging score" = importance_b,
  "Manager score" = importance_m
)
all_vars <- unique(unlist(lapply(g_imp_list, rownames)))

imp_combine_g <- data.frame(
  Question = all_vars,
  Variables = ifelse(all_vars %in% names(name_mapping),
                     name_mapping[all_vars],
                     all_vars),
  stringsAsFactors = FALSE
)

# Define model names
model_names_g <- names(g_imp_list)  # ["Satisfatory score", "Belonging score", "Manager score"]

max_rank <- max(sapply(g_imp_list, function(x) max(nrow(x), na.rm = TRUE)), na.rm = TRUE)

for(model in model_names_g) {
  
  if(model %in% names(g_imp_list)) {
    imp_df <- g_imp_list[[model]]
    
    temp_df <- data.frame(
      Question = rownames(imp_df),
      Sum = imp_df$Imp_Rank_MSE, #+ imp_df$Imp_Rank_Gini,
      stringsAsFactors = FALSE
    )
    
    temp_df$Rank <- rank(temp_df$Sum, ties.method = "min")
    imp_combine_g[[model]] <- temp_df$Rank[match(imp_combine_g$Question, temp_df$Question)]
  }
}

imp_combine_g$Total_Sum <- rowSums(imp_combine_g[model_names_g])
imp_combine_g$Top_3_count <- rowSums(imp_combine_g[model_names_g] <= 3)

# Reorder columns
imp_combine_g <- imp_combine_g[, c("Question", "Variables", model_names_g, 
                                   "Top_3_count", "Total_Sum")]

# Sort by Total_Sum (ascending = most important first)
imp_combine_g <- imp_combine_g[order(imp_combine_g$Total_Sum), ]

# View result
print(imp_combine_g)


gallup_lookup <- setNames(
  ifelse(imp_combine_g$Top_3_count >= 2, "Yes", "No"),
  imp_combine_g$Question
)

# Add "In Gallup" column to imp_combine_df
imp_combine_df$In_Gallup <- ifelse(
  imp_combine_df$Question %in% names(gallup_lookup),
  gallup_lookup[imp_combine_df$Question],
  "No"
)

imp_combine_df <- imp_combine_df[, 
                                 c("Question", "Variables", model_names, "Top_10_count", "Total_Sum", "Group", "In_Gallup")
]
rownames(imp_combine_df) <- FALSE
# View result
print(imp_combine_df)

setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Glint")
output = knitr::kable(imp_combine_df, caption = "Importance comparsion", format="markdown", row.names = FALSE)
# Add explanatory comments below the table
comments <- c(
  "",
  "### Notes:",
  "- In each outcomes, it calculated the Sum of Imp-MSE and IMP-Gini",
  "- Top 10 count: How many times that variables is in Top 10 in the model",
  "- High: Total_Sum<35; Medium-up: 35<=Total_Sum<60; Medium-low: 60<=Total_Sum<65 and Top_10_count>=2",
  "- In Gallup: Yes if Top 3 count >= 2 of the relevant variables in Gallup model",
  "",
  ""
)
# Combine table and comments
output <- c(output, comments)
path = "./Importance_comparsion.md"
writeLines(output, path)

#####
importance_s <- final_results$Overall_Glint$Satisfy_score2$importance
importance_q2 <- final_results$Overall_Glint$avg_score_Q2$importance
importance_q4 <- final_results$Overall_Glint$avg_score_Q4$importance
importance_q5 <- final_results$Overall_Glint$avg_score_Q5$importance
importance_q6 <- final_results$Overall_Glint$avg_score_Q6$importance



imp_list <- list(
  "2Q_engagement" = importance_s,
  "Satisfactory-work" = importance_q2,
  "Future Success" = importance_q4,
  "Belonging" = importance_q5,
  "Retention" = importance_q6
)
new_names <- c(
  "Recommend my Manager" = "avg_score_Q7",
  "Action after Feedback" = "avg_score_Q8",
  "Asked Opinion" = "avg_score_Q9",
  "Training for Confidence" = "avg_score_Q10",
  "Career Opportunities" = "avg_score_Q11",
  "Recommend our Stores" = "avg_score_Q12",
  "Trust each other" = "avg_score_Q15",
  "Trust Leadership" = "avg_score_Q16",
  "Provide Feedback for Improvement" = "avg_score_Q17",
  "Satisfied with Recognition" = "avg_score_Q18",
  "Great Customers Performance" = "avg_score_Q19",
  "Proud of Company" = "avg_score_Q20",
  "Consider Customer's Needs" = "avg_score_Q21",
  "Empowered to Make Decisions" = "avg_score_Q22",
  "Meaningful Work" = "avg_score_Q23",
  "Given Enough Resources" = "avg_score_Q24",
  "Live Company Values" = "avg_score_Q25",
  "Being Cared" = "avg_score_Q26",
  "Equal Opportunity for Success" = "avg_score_Q27",
  "Free to Speak my Mind" = "avg_score_Q28",
  "Given Technology Resources" = "avg_score_Q29",
  "Supporting Communities" = "avg_score_Q30",
  "SSC Collaborate Effectively" = "avg_score_Q31",
  "SSC Good Team Communicate" = "avg_score_Q32",
  "Leadership Team Demostrates Commitment" = "avg_score_Q33",
  "Excited about Company's Future" = "avg_score_Q34",
  "SSC Adapt Changes" = "avg_score_Q35",
  "SSC deliver commitments" = "avg_score_Q36",
  "Clear other's Responsiblity" = "avg_score_Q37",
  "Being held accountable for work" = "avg_score_Q38"
)
name_mapping <- setNames(names(new_names), unname(new_names))


all_vars <- unique(unlist(lapply(imp_list, rownames)))

imp_combine_df <- data.frame(
  Question = all_vars,
  Variables = ifelse(all_vars %in% names(name_mapping),
                     name_mapping[all_vars],
                     all_vars),
  stringsAsFactors = FALSE
)


model_names <- c("2Q_engagement", "Satisfactory-work", "Future Success", "Belonging", "Retention")


for(model in model_names) {
  
  if(model %in% names(imp_list)) {
    imp_df <- imp_list[[model]]
    
    # Create temp df with summed ranks
    temp_df <- data.frame(
      Question = rownames(imp_df),
      Sum = imp_df$Imp_Rank_MSE + imp_df$Imp_Rank_Gini,
      stringsAsFactors = FALSE
    )
    
    # Calculate rank (1 = most important)
    temp_df$Rank <- rank(temp_df$Sum, ties.method = "min")
    
    # Merge ranks into main df
    imp_combine_df[[model]] <- temp_df$Rank[match(imp_combine_df$Question, temp_df$Question)]
  }
}


imp_combine_df <- imp_combine_df[, c("Question", "Variables", model_names)]

max_rank <- max(sapply(imp_combine_df[model_names], max, na.rm = TRUE), na.rm = TRUE)
imp_combine_df[model_names] <- lapply(imp_combine_df[model_names], 
                                      function(x) ifelse(is.na(x), max_rank + 1, x))

imp_combine_df$Total_Sum <- rowSums(imp_combine_df[model_names])

imp_combine_df <- imp_combine_df[order(imp_combine_df$Total_Sum), ]

imp_combine_df <- imp_combine_df[, c("Question", "Variables", model_names, "Total_Sum")]


imp_combine_df$Top_10_count <- rowSums(imp_combine_df[model_names] <= 10)

imp_combine_df$Group <- with(imp_combine_df, 
                             ifelse(Total_Sum < 35, "High",
                                    ifelse(Total_Sum >= 35 & Total_Sum < 60 & Top_10_count >= 2, "Medium-up",
                                           ifelse(Total_Sum >= 60 & Total_Sum <= 65 & Top_10_count >= 2, "Medium-low",
                                                  "Low")))
)

imp_combine_df <- imp_combine_df[, c("Question", "Variables", model_names, "Top_10_count", "Total_Sum", "Group")]
imp_combine_df

importance_s_o <- final_results$Overall_Gallup$Satisfy_score1$importance
importance_b_o <- final_results$Overall_Gallup$Manager_score$importance
importance_m_o <- final_results$Overall_Gallup$Belonging_score$importance

o_imp_list <- list(
  "Satisfatory score" = importance_s_o,
  "Belonging score" = importance_b_o,
  "Manager score" = importance_m_o
)
all_vars <- unique(unlist(lapply(o_imp_list, rownames)))

imp_combine_o <- data.frame(
  Question = all_vars,
  Variables = ifelse(all_vars %in% names(name_mapping),
                     name_mapping[all_vars],
                     all_vars),
  stringsAsFactors = FALSE
)

# Define model names
model_names_o <- names(o_imp_list)  # ["Satisfatory score", "Belonging score", "Manager score"]

max_rank <- max(sapply(o_imp_list, function(x) max(nrow(x), na.rm = TRUE)), na.rm = TRUE)

for(model in model_names_o) {
  
  if(model %in% names(o_imp_list)) {
    imp_df <- o_imp_list[[model]]
    
    temp_df <- data.frame(
      Question = rownames(imp_df),
      Sum = imp_df$Imp_Rank_MSE + imp_df$Imp_Rank_Gini,
      stringsAsFactors = FALSE
    )
    
    temp_df$Rank <- rank(temp_df$Sum, ties.method = "min")
    imp_combine_o[[model]] <- temp_df$Rank[match(imp_combine_o$Question, temp_df$Question)]
  }
}

imp_combine_o$Total_Sum <- rowSums(imp_combine_o[model_names_o])
imp_combine_o$Top_3_count <- rowSums(imp_combine_o[model_names_o] <= 3)
imp_combine_o$Top_2_count <- rowSums(imp_combine_o[model_names_o] <= 2)
# Reorder columns
imp_combine_o <- imp_combine_o[, c("Question", "Variables", model_names_o, 
                                   "Top_2_count","Top_3_count", "Total_Sum")]

# Sort by Total_Sum (ascending = most important first)
imp_combine_o <- imp_combine_o[order(imp_combine_o$Total_Sum), ]

# View result
print(imp_combine_o)


gallup_lookup1 <- setNames(
  ifelse(imp_combine_o$Top_2_count >= 2, "Yes", "No"),
  imp_combine_o$Question
)
gallup_lookup2 <- setNames(
  ifelse(imp_combine_o$Top_3_count >= 2, "Yes", "No"),
  imp_combine_o$Question
)

# Add "In Gallup" column to imp_combine_df
imp_combine_df$In_Gallup1 <- ifelse(
  imp_combine_df$Question %in% names(gallup_lookup1),
  gallup_lookup1[imp_combine_df$Question],
  "No"
)
imp_combine_df$In_Gallup2 <- ifelse(
  imp_combine_df$Question %in% names(gallup_lookup2),
  gallup_lookup2[imp_combine_df$Question],
  "No"
)
imp_combine_df <- imp_combine_df[, 
                                 c("Question", "Variables", model_names, "Top_10_count", "Total_Sum", "Group", "In_Gallup1","In_Gallup2")
]
# View result
print(imp_combine_df)


setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Overall/Same")
output = knitr::kable(imp_combine_df, caption = "Importance comparison -- Overall", format="markdown", row.names = FALSE)
# Add explanatory comments below the table
comments <- c(
  "",
  "### Notes:",
  "- In each outcomes, it calculated the Sum of Imp-MSE and IMP-Gini",
  "- Top 10 count: How many times that variables is in Top 10 in the model",
  "- High: Total_Sum<35; Medium-up: 35<=Total_Sum<60; Medium-low: 60<=Total_Sum<=65 and Top_10_count>=2",
  "- In Gallup1: Yes if Top 2 count >= 2 of the relevant variables in Gallup model",
  "- In Gallup2: Yes if Top 3 count >= 2 of the relevant variables in Gallup model",
  "",
  ""
)
# Combine table and comments
output <- c(output, comments)
path = "./Importance_comparsion(Overall).md"
writeLines(output, path)

setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Overall")
# 1. Read all lines
lines <- readLines("./Importance_comparsion_updated(Overall).md")

# 2. Find table lines (all that start with a "|")
table_lines <- grep("^\\|", lines, value = TRUE)

# 3. Remove alignment row (starts with "|:")
table_lines <- table_lines[!grepl("^\\|:", table_lines)]

# 4. Remove leading/trailing pipes and collapse spaces
table_lines <- gsub("^\\||\\|$", "", table_lines)
table_lines <- gsub("\\s*\\|\\s*", "|", table_lines)

# 5. Write to a temp file and read as data.frame
tmp <- tempfile(fileext = ".csv")
writeLines(table_lines, tmp)
df <- read.csv(tmp, header = TRUE, sep = "|", check.names = FALSE, stringsAsFactors = FALSE)

library(dplyr)
df <- df %>%
  rename(
    "2Q_engagement(Q1&Q2)" = "2Q_engagement",
    "Satisfactory-work(Q3)" = "Satisfactory-work",
    "Future Success(Q5)"    = "Future Success", 
    "Belonging(Q4)"         = "Belonging", 
    "Retention(Q35)"        = "Retention"
  )
new_question <- c(
  "Q17" = "avg_score_Q7",
  "Q36" = "avg_score_Q8",
  "Q30" = "avg_score_Q9",
  "Q31" = "avg_score_Q10",
  "Q32" = "avg_score_Q11",
  "Q33" = "avg_score_Q12",
  "Q12" = "avg_score_Q15",
  "Q15" = "avg_score_Q16",
  "Q16" = "avg_score_Q17",
  "Q29" = "avg_score_Q18",
  "Q8" = "avg_score_Q19",
  "Q10" = "avg_score_Q20",
  "Q9" = "avg_score_Q21",
  "Q25" = "avg_score_Q22",
  "Q26" = "avg_score_Q23",
  "Q27" = "avg_score_Q24",
  "Q18" = "avg_score_Q25",
  "Q28" = "avg_score_Q26",
  "Q13" = "avg_score_Q27",
  "Q24" = "avg_score_Q28",
  "Q6" = "avg_score_Q29",
  "Q7" = "avg_score_Q30",
  "Q19" = "avg_score_Q31",
  "Q11" = "avg_score_Q32",
  "Q14" = "avg_score_Q33",
  "Q34" = "avg_score_Q34",
  "Q20" = "avg_score_Q35",
  "Q21" = "avg_score_Q36",
  "Q22" = "avg_score_Q37",
  "Q23" = "avg_score_Q38"
)
q_mapping <- setNames(names(new_question), unname(new_question))

df$`New Question No.` <- q_mapping[df$Question]

# Reorder columns: put "New Question No." after "Question"
nms <- names(df)
pos <- which(nms == "Question")
df <- df[, c(nms[1:pos], "New Question No.", nms[(pos+1):(length(nms)-1)])]
print(df)
output = knitr::kable(df, caption = "Importance comparison -- Overall", format="markdown", row.names = FALSE)
# Add explanatory comments below the table
comments <- c(
  "",
  "### Notes:",
  "- In each outcomes, it calculated the Sum of Imp-MSE and IMP-Gini",
  "- Top 10 count: How many times that variables is in Top 10 in the model",
  "- High: Total_Sum<35; Medium-up: 35<=Total_Sum<60; Medium-low: 60<=Total_Sum<=65 and Top_10_count>=2",
  "- In Gallup1: Yes if Top 2 count >= 2 of the relevant variables in Gallup model",
  "- In Gallup2: Yes if Top 3 count >= 2 of the relevant variables in Gallup model",
  "",
  ""
)
# Combine table and comments
output <- c(output, comments)
path = "./Importance_comparsion_updated(Overall).md"
writeLines(output, path)
write.csv(df, file = "Importance_comparison_output(SSC).csv", row.names = FALSE)





run_analysis <- function(survey_type, analysis_type, data_source) {
  # Define configurations
  config <- list(
    Gallup = list(
      predictor_vars = paste0("avg_score_Q", c(6,11,16,22,25,27,29)),
      targets = c("Satisfy_score1", "Manager_score", "Belonging_score")
    ),
    Glint = list(
      predictor_vars = paste0("avg_score_Q", c(7:12, 15:30)),
      targets = c("Satisfy_score2", "avg_score_Q2", "avg_score_Q4", "avg_score_Q5", "avg_score_Q6")
    )
  )
  
  # Set base directory
  base_dirs <- list(
    Gallup = "C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/Gallup",
    Glint = "C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/Glint",
    Overall_Gallup = "C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/Overall/Gallup/Store",
    Overall_Glint = "C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/Overall/Glint/Store"
  )
  
  # Determine directory key
  dir_key <- if(analysis_type == "Regular") survey_type else paste0("Overall_", survey_type)
  
  # Get configuration
  cfg <- config[[survey_type]]
  base_dir <- base_dirs[[dir_key]]
  
  # Run analysis
  results <- list()
  for (target in cfg$targets) {
    target_dir <- file.path(base_dir, target)
    cat("\nRunning", analysis_type, survey_type, "for target:", target, "\n")
    
    results[[target]] <- randomforest_model(
      target_string = target,
      variables = cfg$predictor_vars,
      data = data_source,
      base_dir = target_dir
    )
  }
  
  return(results)
}

# Regular Gallup analysis
gallup_results <- run_analysis(
  survey_type = "Gallup",
  analysis_type = "Regular",
  data_source = data
)

# Regular Glint analysis
glint_results <- run_analysis(
  survey_type = "Glint",
  analysis_type = "Regular",
  data_source = data
)

# Overall Gallup analysis
overall_gallup_results <- run_analysis(
  survey_type = "Gallup",
  analysis_type = "Overall",
  data_source = odata
)

# Overall Glint analysis
overall_glint_results <- run_analysis(
  survey_type = "Glint",
  analysis_type = "Overall",
  data_source = odata
)






