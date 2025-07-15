library(readr)

setwd("C:/Users/tiip/Downloads/Wellcome_Glint")
#setwd("C:/Users/choyinch/Downloads/Wellcome_Glint")
data = read.csv("./score_wellcome.csv")
# variables = c("avg_score_Q7", "avg_score_Q8", 
#               "avg_score_Q9", "avg_score_Q10",
#               "avg_score_Q11", "avg_score_Q12", "avg_score_Q15", 
#               "avg_score_Q16", "avg_score_Q17", "avg_score_Q18",
#               "avg_score_Q19", "avg_score_Q20", "avg_score_Q21",
#               "avg_score_Q22", "avg_score_Q23", "avg_score_Q24",
#               "avg_score_Q25", "avg_score_Q26", "avg_score_Q27", 
#               "avg_score_Q28", "avg_score_Q29", "avg_score_Q30")
# data_standardized <- data %>% 
#   select(all_of(variables)) %>%
#   mutate(across(everything(), ~scale(.) %>% as.vector()))
# 
# # Replace the original columns with the standardized columns
# data_combined <- data %>%
#   mutate(across(all_of(variables), ~ data_standardized[[cur_column()]]))
model = function(target, target_string){
  new_dir = paste("C:/Users/tiip/Downloads/Wellcome_Glint/ManagerToEmployee/Glint", target_string, sep="/")
  #new_dir = paste("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Glint", target_string, sep="/")

  dir.create(new_dir)
  setwd(new_dir)
  
  # Least Square Method
  print("Done Least Square Method:")
  sink("./least_square.txt")
  reg1 <- lm(target ~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
               avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
               avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
               avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
               avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
               avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
               avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
             data = data_combined)
  print(summary(reg1))
  sink()
  
  
  # Cook Distance for Least Square Method
  print("Doing Cooking Distance:")
  cooksd <- cooks.distance(reg1)
  cooks_data <- data.frame(
    Observation = 1:length(cooksd),
    CooksD = cooksd,
    Store = data_combined$store_name  
  )
  
  
  # Threshold Plot
  library(ggrepel)
  library(ggplot2)
  print("Doing Threshold Plot:")
  threshold <- 4 / nrow(cooks_data)
  my_plot = ggplot(cooks_data, aes(x = Observation, y = CooksD)) +
    geom_point(shape = 19, color = "steelblue") +
    geom_text_repel(
      aes(label = ifelse(CooksD > threshold, Store, "")),
      size = 3,
      box.padding = 0.5,  # Adjust spacing around labels
      max.overlaps = Inf,  # Show all labels regardless of overlap
      min.segment.length = 0.1,  # Always draw segments
      direction = "both",  # Allow labels to move in all directions
      segment.color = "gray"  # Add lines connecting labels to points
    ) +
    geom_hline(
      yintercept = threshold,
      color = "red",
      linetype = "dashed"
    ) +
    labs(
      title = paste("Cook's Distance -", target_string),
      y = "Cook's Distance",
      x = "Observation Index"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave("./threshold_plot.png", plot = my_plot, width = 10, height = 6, units = "in")
  
  
  # Outlier Plot
  library(dplyr)
  print("Doing Outlier Plot:")
  threshold <- 4 / nrow(cooks_data)
  
  # outliers_list <- cooks_data %>%
  #  filter(CooksD > threshold) %>%
  #  select(Store, CooksD) %>%
  #  arrange(desc(CooksD))  # Sort by highest influence
  
  
  outliers_list <- cooks_data[cooks_data$CooksD > threshold, c("Store", "CooksD")]
  outliers_list <- outliers_list[order(-outliers_list$CooksD), ]
  
  write.csv(outliers_list, paste("./cooks_distance_outliers_", target_string, ".csv", sep=""), row.names = FALSE)
  
  
  
  # Multicollinearity(VIF>5)
  print("Doing Multicollinearity:")
  sink("./multicollinearity.txt")
  print(car::vif(reg1))
  sink()
  
  
  # Build regression using only features with importance score higher than median
  feature_names = names(importance$lmg[importance$lmg>median(importance$lmg)])
  formula_string = paste("target ~", paste(feature_names, collapse = " + "))
  formula = as.formula(formula_string)
  reg2 = lm(formula, data=data)
  sink("./least_square_features_median.txt")
  print(summary(reg2))
  sink()
  
  # Weighting Importance
  library(relaimpo)
  print("Doing Weighting Importance Again:")
  sink("./weighting_importance_features_median.txt")
  importance_features_median = calc.relimp(reg2, type = "lmg", rela = TRUE)
  print(importance_features_median)
  sink()
  
  library(glmnet)
  # Fit Lasso Regression
  print("Doing Lasso Regression:")
  sink("./lasso_regression.txt")
  lasso_x <- model.matrix(target ~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
                            avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
                            avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
                            avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
                            avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
                            avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
                            avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
                          data = data_combined)
  lasso_y <- target
  # Fit Lasso model
  lasso_model <- glmnet(lasso_x, lasso_y, alpha = 1)  # alpha=1 for Lasso
  cv_lasso <- cv.glmnet(lasso_x, lasso_y, alpha = 1)
  best_lambda_lasso <- cv_lasso$lambda.min
  print(coef(lasso_model, s = best_lambda_lasso))
  sink()
  
  # Extract non-zero coefficients from Lasso
  lasso_coefs <- coef(lasso_model, s = best_lambda_lasso)
  lasso_coefs <- as.matrix(lasso_coefs)
  selected_vars <- rownames(lasso_coefs)[lasso_coefs != 0]
  
  # Remove intercept from selected variables
  selected_vars <- selected_vars[selected_vars != "(Intercept)"]
  print(selected_vars)
  
  # Calculate importance metrics (absolute values of non-zero coefficients)
  importance_metrics <- abs(lasso_coefs[lasso_coefs != 0]) 
  
  # Get the names of the non-zero coefficients
  feature_names <- rownames(lasso_coefs)[lasso_coefs != 0]
  
  # Check if there are non-zero coefficients
  if (length(importance_metrics) > 0) {
    # Calculate total importance
    total_importance <- sum(importance_metrics)
    
    # Calculate percentage of importance
    percentage_importance <- (importance_metrics / total_importance) * 100
    
    # Create a data frame for better readability
    importance_df <- data.frame(
      Feature = feature_names,
      Coefficients = importance_metrics,
      Percentage = percentage_importance
    )
    
    # Sort by percentage importance
    importance_df <- importance_df[order(-importance_df$Percentage), ]
    
    # Print the importance data frame
    sink("./weighting_importance_lasso.txt")
    print(importance_df)
    sink()
  } else {
    print("No non-zero coefficients found.")
  }
  
  process_importance <- function(model) {
    library(vip)
    imp = vi(model, type = "gcv") %>%
      filter(Variable %in% selected_vars)%>%
      as.data.frame() %>%
      mutate(Importance = signif(Importance, 4)) %>%
      mutate(Importance = format(Importance, scientific = FALSE)) %>%
      bind_rows(
        data.frame(Variable = "sum", Importance = format(signif(sum(as.numeric(.$Importance)), 4), scientific = FALSE))
      )
    #print()
    return(imp)
  }
  # Calculate variable importance for lasso
  print("Calculating Variable Importance for Lasso")
  sink("./var_importance_lasso.txt")
  importance <- process_importance(lasso_model)
  print(importance)
  plot = vip(lasso_model, n = length(selected_vars))
  ggsave("./importance_plot_lasso.png", plot, width = 10, height = 6, units = "in")
  sink()
  
  # Fit Ridge Regression
  print("Doing Ridge Regression:")
  sink("./ridge_regression.txt")
  ridge_x <- model.matrix(target ~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
                            avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
                            avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
                            avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
                            avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
                            avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
                            avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
                          data =data_combined)
  ridge_y <- target 
  
  ridge_model <- glmnet(ridge_x, ridge_y, alpha = 0)  # alpha=0 for Ridge
  cv_ridge <- cv.glmnet(ridge_x, ridge_y, alpha = 0)
  best_lambda_ridge <- cv_ridge$lambda.min
  
  print(coef(ridge_model, s = best_lambda_ridge))
  sink()
  
  
  # Get predictions from both models
  print("Get predictions from both models:")
  ols_pred <- predict(reg1)  # OLS predictions
  lasso_pred <- predict(lasso_model, newx = lasso_x, s = best_lambda_lasso)  # Lasso predictions
  ridge_pred <- predict(ridge_model, newx = ridge_x, s = best_lambda_ridge)  # Ridge predictions (lambda = 10)
  
  # Create plotting data
  plot_data <- data.frame(
    Actual = ridge_y,
    OLS = ols_pred,
    Lasso = as.vector(lasso_pred),
    Ridge = as.vector(ridge_pred)  # Convert matrix to vector
  )
  
  # Plot predictions vs actual for both models
  my_plot = ggplot(plot_data, aes(x = Actual)) +
    geom_point(aes(y = OLS, color = "OLS"), alpha = 0.6, size = 2) +
    geom_point(aes(y = Lasso, color = "Lasso"), alpha = 0.6, size = 2) +
    geom_point(aes(y = Ridge, color = "Ridge"), alpha = 0.6, size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  # Perfect prediction line
    labs(
      title = "Overall Model Comparison: OLS vs. Lasso vs. Ridge",
      x = paste("Actual ", target_string),
      y = paste("Predicted ", target_string),
      color = "Model"
    ) +
    scale_color_manual(values = c("OLS" = "blue", "Lasso" = "green", "Ridge" = "red")) +
    theme_minimal() +
    coord_fixed(ratio = 1)
  ggsave("./model_comparison.png", plot = my_plot, width = 10, height = 6, units = "in")
  
  # For one DV Scatterplot matrix
  library(GGally)
  print("Doing Scatterplot:")
  my_plot = ggpairs(data, 
                    columns = c(target_string, "avg_score_Q7", "avg_score_Q8", 
                                "avg_score_Q9", "avg_score_Q10",
                                "avg_score_Q11", "avg_score_Q12", "avg_score_Q15", 
                                "avg_score_Q16", "avg_score_Q17", "avg_score_Q18",
                                "avg_score_Q19", "avg_score_Q20", "avg_score_Q21",
                                "avg_score_Q22", "avg_score_Q23", "avg_score_Q24",
                                "avg_score_Q25", "avg_score_Q26", "avg_score_Q27", 
                                "avg_score_Q28", "avg_score_Q29", "avg_score_Q30"),
                    upper = list(continuous = wrap("cor", size = 4)),
                    lower = list(continuous = wrap("smooth", alpha = 0.3)))
  ggsave("./scatterplot.png", plot = my_plot, width = 10, height = 6, units = "in")
  
  
  # GAM Model
  library(mgcv)
  print("Doing GAM:")
  # Define the variables as a character vector
  
  update_gam <- function(gam_model,data, filename_gam, filename_updated_gam) {
    sink(filename_gam)
    print(summary(gam_model))
    sink()
    
    gam_summary <- summary(gam_model)
    smooth_terms <- gam_summary$s.table
    terms_to_update <- rownames(smooth_terms)[smooth_terms[, "edf"] >= 0.99 & smooth_terms[, "edf"] <= 1.10]
    print(terms_to_update)
    
    formula_gam <- formula(gam_model)
    formula_updated <- formula_gam
    
    for (term in terms_to_update) {
      variable_name <- gsub("s\\((.*)\\)", "\\1", term) # Extract variable name from "s(var)"
      formula_updated <- update.formula(formula_updated, paste(". ~ . -", term, "+", variable_name))
    }
    
    updated_gam <- gam(formula_updated, data =data_combined, family = gaussian(link = 'log'))
    
    sink(filename_updated_gam)
    print(summary(updated_gam))
    sink()
    
    return(updated_gam)
  }
  
  variables <- c("avg_score_Q7", "avg_score_Q8", 
                 "avg_score_Q9", "avg_score_Q10",
                 "avg_score_Q11", "avg_score_Q12", "avg_score_Q15", 
                 "avg_score_Q16", "avg_score_Q17", "avg_score_Q18",
                 "avg_score_Q19", "avg_score_Q20", "avg_score_Q21",
                 "avg_score_Q22", "avg_score_Q23", "avg_score_Q24",
                 "avg_score_Q25", "avg_score_Q26", "avg_score_Q27", 
                 "avg_score_Q28", "avg_score_Q29", "avg_score_Q30")
  
  # Create the GAM formula
  gam_formula <- as.formula(paste(target_string, "~", paste(paste0("s(", variables, ")"), collapse = " + ")))
  
  gam_1 <- gam(gam_formula, data =data_combined, family = gaussian(link = 'log'))
  
  
  gam_formula <- as.formula(paste(target_string, "~", paste(paste0("s(", selected_vars, ")"), collapse = " + ")))
  gam_2 <- gam(gam_formula, data =data_combined, family = gaussian(link = 'log'))
  
  gam_1A <- update_gam(
    gam_model = gam_1, 
    data =data_combined,
    filename_gam = "./GAM_1.txt", 
    filename_updated_gam = "./GAM_1A.txt"
  )
  gam_3 <- update_gam(
    gam_model = gam_2, 
    data =data_combined,
    filename_gam = "./GAM_2.txt", 
    filename_updated_gam = "./GAM_3.txt"
  )
  
  library(ggeffects)
  library(gratia)
  library(RColorBrewer)
  print("Doing ANOVA:")
  plot_obj1 <- ggeffects::ggpredict(gam_1)
  plot_obj2 <- ggeffects::ggpredict(gam_2)
  custom_colors <- colorRampPalette(brewer.pal(9, "Set1"))(22)
  #colors <- c(brewer.pal(9, "Set1"), brewer.pal(9, "Set2"),brewer.pal(9, "Set3"))
  my_plot = plot(plot_obj1, facets = TRUE) + labs(x = "avg_score") + scale_color_manual(values = custom_colors)
  ggsave("./gam_overall_plot.png", my_plot, width = 10, height = 6, units = "in")
  
  my_plot = plot(plot_obj2, facets = TRUE) + labs(x = "avg_score") + scale_color_manual(values = custom_colors)
  ggsave("./gam_overall_plot(selected).png", my_plot, width = 10, height = 6, units = "in")
  
  my_plot = gratia::draw(gam_1)
  ggsave("./gam_individual_plot.png", plot = my_plot, width = 8, height = 6, units = "in")
  
  my_plot = gratia::draw(gam_2)
  ggsave("./gam_individual_plot(selected).png", plot = my_plot, width = 8, height = 6, units = "in")
  
  sink("./ANOVA.txt")
  print(anova(reg1, gam_1, gam_1A, gam_2, gam_3, test = "Chisq"))
  sink()
  
  process_importance <- function(model, data) {
    library(vip)
    imp = vi(model, method = "firm", train = data_combined) %>%
      as.data.frame() %>%
      mutate(Importance = signif(Importance, 4)) %>%
      mutate(Importance = format(Importance, scientific = FALSE)) %>%
      bind_rows(
        data.frame(Variable = "sum", Importance = format(signif(sum(as.numeric(.$Importance)), 4), scientific = FALSE))
      )
    #print()
    return(imp)
  }
  # Calculate variable importance for gam_3
  print("Calculating Variable Importance for GAMs")
  sink("./var_importance.txt")
  var_importance <- process_importance(gam_2, data_combined)
  print(var_importance)
  plot2 = vip(gam_2, method = "firm", ice = TRUE, train = data_combined, n = length(selected_vars))
  ggsave("./importance_plot_gam.png", plot2, width = 10, height = 6, units = "in")
  sink()
  
  return(list(reg1=reg1, gam_1=gam_1, gam_1A=gam_1A, gam_2=gam_2, gam_3=gam_3, importance=importance, 
              selected_vars=selected_vars, var_importance = var_importance ))
}
# For Satisfaction Score
target =data_combined$Satisfy_score
target_string = "Satisfy_score"
result = model(target, target_string)
reg1_s = result$reg1
reg2_s = result$reg2
gam1_s = result$gam_1
gam1A_s = result$gam_1A
gam2_s = result$gam_2
gam3_s = result$gam_3
importance_s = result$importance
importance_s_features_median = result$importance_features_median
selected_vars_s = result$selected_vars
var_importance_s = result$var_importance

# For Q2
target =data_combined$avg_score_Q2
target_string = "avg_score_Q2"
result = model(target, target_string)
reg1_q2 = result$reg1
reg2_q2 = result$reg2
gam1_q2 = result$gam_1
gam2_q2 = result$gam_2
gam3_q2 = result$gam_3
importance_q2 = result$importance
importance_q2_features_median = result$importance_features_median
selected_vars_q2 = result$selected_vars
var_importance_q2 = result$var_importance

# For Q4
target = data_combined$avg_score_Q4
target_string = "avg_score_Q4"
result = model(target, target_string)
reg1_q4 = result$reg1
reg2_q4 = result$reg2
gam1_q4 = result$gam_1
gam2_q4 = result$gam_2
gam3_q4 = result$gam_3
importance_q4 = result$importance
importance_q4_features_median = result$importance_features_median
selected_vars_q4 = result$selected_vars
var_importance_q4 = result$var_importance

# For Q5
target = data_combined$avg_score_Q5
target_string = "avg_score_Q5"
result = model(target, target_string)
reg1_q5 = result$reg1
reg2_q5 = result$reg2
gam1_q5 = result$gam_1
gam2_q5 = result$gam_2
gam3_q5 = result$gam_3
importance_q5 = result$importance
importance_q5_features_median = result$importance_features_median
selected_vars_q5 = result$selected_vars
var_importance_q5 = result$var_importance

# For Q6
target = data_combined$avg_score_Q6
target_string = "avg_score_Q6"
result = model(target, target_string)
reg1_q6 = result$reg1
reg2_q6 = result$reg2
gam1_q6 = result$gam_1
gam2_q6 = result$gam_2
gam3_q6 = result$gam_3
importance_q6 = result$importance
importance_q6_features_median = result$importance_features_median
selected_vars_q6 = result$selected_vars
var_importance_q6 = result$var_importance

library(gridExtra)
library(broom)
library(texreg)
#setwd("C:/Users/tiip/Downloads/Wellcome_Glint/ManagerToEmployee/Glint")
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate")

models <- list(gallup_model_s, overall_gallup_model_s, gallup_model_m, overall_gallup_model_m,gallup_model_b, overall_gallup_model_b)
screenreg(models, custom.model.names = c("Satisfy_score(Wellcome)", "Satisfy_score(Overall)", "Manager_score(Wellcome)",
                                         "Manager_score(Overall)", "Belonging_score(Wellcome)", "Belonging_score(overall)"),
          file = "./models_summary_gallup.txt")

models <- list(reg2_s, reg2_q2, reg2_q4, reg2_q5, reg2_q6)
screenreg(models, custom.model.names = c("OLS 2(2Q_Engagement)", "OLS 2(Satisfaction-work)", "OLS 2(Future Success)",
                                         "OLS 2(Belonging)", "OLS 2(Retention)"),
          file = "./models_summary_OLS_features_median.txt")

models <- list(gam2_s, gam2_q2, gam2_q4, gam2_q5, gam2_q6)
screenreg(models, custom.model.names = c("GAM 2(2Q_Engagement)", "GAM 2(Satisfaction-work)", "GAM 2(Future Success)",
                                         "GAM 2(Belonging)", "GAM 2(Retention)"),
          file = "./models_summary_gam2.txt")

models <- list(gam3_s, gam3_q2, gam3_q4, gam3_q5, gam3_q6)
screenreg(models, custom.model.names = c("GAM 3(2Q_Engagement)", "GAM 3(Satisfaction-work)", "GAM 3(Future Success)",
                                         "GAM 3(Belonging)", "GAM 3(Retention)"),
          file = "./models_summary_gam3.txt")

# Importance
# Lasso
# Clean Lasso importance with zero-value filtering
clean_lasso_importance <- function(imp_df) {
  imp_df %>%
    # Convert to numeric and filter zeros (keep sum)
    mutate(Importance = as.numeric(Importance)) %>%
    filter(Variable == "sum" | Importance > 0 & Sign == "POS") %>%
    {setNames(.$Importance, .$Variable)}
}

# Process importance components (modify names as needed)
importance_s_clean <- clean_lasso_importance(importance_s)
importance_q2_clean <- clean_lasso_importance(importance_q2)
importance_q4_clean <- clean_lasso_importance(importance_q4)
importance_q5_clean <- clean_lasso_importance(importance_q5)
importance_q6_clean <- clean_lasso_importance(importance_q6)

# Create named list for Lasso models
lasso_imp_list <- list(
  "2Q_engagement" = importance_s_clean,
  "Satisfactory-work" = importance_q2_clean,
  "Future Success" = importance_q4_clean,
  "Belonging" = importance_q5_clean,
  "Retention" = importance_q6_clean
)

# Create combined matrix
lasso_vars <- unique(unlist(lapply(lasso_imp_list, names)))
lasso_combined_mat <- do.call(cbind, lapply(lasso_imp_list, function(x) {
  x[match(lasso_vars, names(x))]
}))

# Convert to dataframe and handle sum row
lasso_combined_df <- as.data.frame(lasso_combined_mat)
rownames(lasso_combined_df) <- lasso_vars

# Force sum to last row
lasso_sum_row <- lasso_combined_df["sum", , drop = FALSE]
lasso_combined_df <- lasso_combined_df[rownames(lasso_combined_df) != "sum", ]
lasso_combined_df <- rbind(lasso_combined_df, lasso_sum_row)
rownames(lasso_combined_df)[nrow(lasso_combined_df)] <- "sum"

# Generate formatted table
lasso_output <- knitr::kable(
  lasso_combined_df,
  caption = "Lasso Variable Importance Comparison",
  format = "markdown",
  align = c("l", rep("c", ncol(lasso_combined_df)))
)

writeLines(lasso_output, "./lasso_importance_metrics.md")
# Gam
clean_importance <- function(var_imp_df) {
  var_imp_df %>%
    mutate(Importance = as.numeric(Importance)) %>%  # Convert to numeric
    {setNames(.$Importance, .$Variable)}             # Keep all rows including 'sum'
}

# Process importance data (preserving sum rows)
var_importance_s_clean <- clean_importance(var_importance_s)
var_importance_q2_clean <- clean_importance(var_importance_q2)
var_importance_q4_clean <- clean_importance(var_importance_q4)
var_importance_q5_clean <- clean_importance(var_importance_q5)
var_importance_q6_clean <- clean_importance(var_importance_q6)
# Create named list with sum preserved
imp_list <- list(
  "2Q_engagement" = var_importance_s_clean,
  "Satisfactory-work" = var_importance_q2_clean,
  "Future Success" = var_importance_q4_clean,
  "Belonging" = var_importance_q5_clean,
  "Retention" = var_importance_q6_clean
)

# Create combined matrix with sum row
all_vars <- unique(unlist(lapply(imp_list, names)))
combined_mat <- do.call(cbind, lapply(imp_list, function(x) {
  x[match(all_vars, names(x))]
}))

# Create final dataframe
combined_df <- as.data.frame(combined_mat)
rownames(combined_df) <- all_vars
sum_row <- combined_df["sum", , drop = FALSE]
combined_df <- combined_df[rownames(combined_df) != "sum", ]
combined_df <- rbind(combined_df, sum_row)
rownames(combined_df)[nrow(combined_df)] <- "sum"

# Generate final table
output <- knitr::kable(
  combined_df,
  caption = "Relative Importance Comparison(GAMs)",
  format = "markdown",
  align = c("l", rep("c", ncol(combined_df)))
)

writeLines(output, "./importance_metrics.md")
#Selected variable
# List of selected variables for each target model
selected_vars_list <- list(
  "2Q_engagement" = selected_vars_s,
  "Satisfactory-work" = selected_vars_q2,
  "Future Success" = selected_vars_q4,
  "Belonging" = selected_vars_q5,
  "Retention" = selected_vars_q6
)

# Get all unique variable names
all_vars <- unique(unlist(selected_vars_list))

# Initialize the results data frame
results_df <- data.frame(variable = all_vars)

# Fill the data frame with presence (1) or absence (0) of each variable
for (target in names(selected_vars_list)) {
  results_df[[target]] <- ifelse(results_df$variable %in% selected_vars_list[[target]], 1, 0)
}
results_df <- results_df[order(results_df$variable), ]
# View the final table
#print(results_df)
output = knitr::kable(results_df, caption = "Relative IVs Comparison", format="markdown")
path = "./relative_ivs.md"
writeLines(output, path)
