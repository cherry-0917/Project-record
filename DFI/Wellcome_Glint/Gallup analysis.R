library(readr)

#setwd("C:/Users/tiip/Downloads/Wellcome_Glint")
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint")
data = read.csv("./score_wellcome.csv")

model = function(target, target_string){
  #new_dir = paste("C:/Users/tiip/Downloads/Wellcome_Glint/ManagerToEmployee", target_string, sep="/")
  new_dir = paste("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Gallup", target_string, sep="/")
  dir.create(new_dir)
  setwd(new_dir)
  
  # Least Square Method
  print("Done Least Square Method:")
  sink("./least_square.txt")
  reg1 <- lm(target ~ avg_score_Q10 + avg_score_Q17 + avg_score_Q18 + avg_score_Q22+ 
               avg_score_Q24 + avg_score_Q29, 
             data = data)
  print(summary(reg1))
  sink()
  
  
  # Cook Distance for Least Square Method
  print("Doing Cooking Distance:")
  cooksd <- cooks.distance(reg1)
  cooks_data <- data.frame(
    Observation = 1:length(cooksd),
    CooksD = cooksd,
    Store = data$store_name  
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
  
  
  # Weighting Importance
  library(relaimpo)
  library(vip)
  print("Doing Weighting Importance:")
  sink("./weighting_importance.txt")
  #importance = calc.relimp(reg1, type = "lmg", rela = TRUE)
  importance = vi(reg1)
  print(importance)
  sink()
  
  # Multicollinearity(VIF>5)
  print("Doing Multicollinearity:")
  sink("./multicollinearity.txt")
  print(car::vif(reg1))
  sink()
  
  
  # Fit Ridge Regression
  library(glmnet)
  print("Doing Ridge Regression:")
  sink("./ridge_regression.txt")
  x <- model.matrix(target ~ avg_score_Q10 + avg_score_Q17 + avg_score_Q18 + avg_score_Q22+ 
                      avg_score_Q24 + avg_score_Q29, 
                    data = data)
  y <- target 
  
  ridge_model <- glmnet(x, y, alpha = 0)  # alpha=0 for Ridge
  cv_ridge <- cv.glmnet(x, y, alpha = 0)
  best_lambda <- cv_ridge$lambda.min
  
  print(coef(ridge_model, s = best_lambda))
  sink()
  
  
  # Get predictions from both models
  print("Get predictions from both models:")
  ols_pred <- predict(reg1)  # OLS predictions
  ridge_pred <- predict(ridge_model, newx = x, s = 10)  # Ridge predictions (lambda = 10)
  
  # Create plotting data
  plot_data <- data.frame(
    Actual = y,
    OLS = ols_pred,
    Ridge = as.vector(ridge_pred)  # Convert matrix to vector
  )
  
  # Plot predictions vs actual for both models
  my_plot = ggplot(plot_data, aes(x = Actual)) +
    geom_point(aes(y = OLS, color = "OLS"), alpha = 0.6, size = 2) +
    geom_point(aes(y = Ridge, color = "Ridge"), alpha = 0.6, size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  # Perfect prediction line
    labs(
      title = "Overall Model Comparison: OLS vs. Ridge",
      x = paste("Actual ", target_string),
      y = paste("Predicted ", target_string),
      color = "Model"
    ) +
    scale_color_manual(values = c("OLS" = "blue", "Ridge" = "red")) +
    theme_minimal() +
    coord_fixed(ratio = 1)
  ggsave("./both_models.png", plot = my_plot, width = 10, height = 6, units = "in")
  
  # For one DV Scatterplot matrix
  library(GGally)
  print("Doing Scatterplot:")
  my_plot = ggpairs(data, 
                    columns = c(target_string, "avg_score_Q10", "avg_score_Q17","avg_score_Q18",
                                "avg_score_Q22",  "avg_score_Q24","avg_score_Q29"),
                    upper = list(continuous = wrap("cor", size = 4)),
                    lower = list(continuous = wrap("smooth", alpha = 0.3)))
  ggsave("./scatterplot.png", plot = my_plot, width = 10, height = 6, units = "in")
  
  # GAM Model
  library(mgcv)
  print("Doing GAM:")
  
  gam_1 <- gam(target ~ avg_score_Q10 + avg_score_Q17 + avg_score_Q18 + avg_score_Q22+ 
                 avg_score_Q24 + avg_score_Q29,
               data = data, family = gaussian(link = 'log'))
  sink("./GAM_1.txt")
  print(summary(gam_1))
  sink()
  
  update_gam <- function(gam_model, data, filename_gam, filename_updated_gam) {
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
    
    updated_gam <- gam(formula_updated, data = data, family = gaussian(link = 'log'))
    
    sink(filename_updated_gam)
    print(summary(updated_gam))
    sink()
    
    return(updated_gam)
  }
  
  gam_2 <- gam(
    as.formula(paste(target_string, "~ s(avg_score_Q10) + s(avg_score_Q17) + s(avg_score_Q18) + 
                   s(avg_score_Q22) + s(avg_score_Q24)  + s(avg_score_Q29)")),
    data = data, 
    family = gaussian(link = 'log')
  )
  
  gam_3 <- update_gam(
    gam_model = gam_2, 
    data = data,
    filename_gam = "./GAM_2.txt", 
    filename_updated_gam = "./GAM_3.txt"
  )
  
  library(ggeffects)
  library(gratia)
  print("Doing ANOVA:")
  plot_obj <- ggeffects::ggpredict(gam_2)
  my_plot = plot(plot_obj, facets = TRUE) + labs(x = "avg_score")
  ggsave("./gam_overall_plot.png", my_plot)
  
  my_plot = gratia::draw(gam_2)
  ggsave("./gam_individual_plot.png", my_plot)
  
  sink("./ANOVA.txt")
  print(anova(reg1, gam_1, gam_2, gam_3, test = "Chisq"))
  sink()
  
  return(list(reg1=reg1, gam_1=gam_1, gam_2=gam_2, gam_3=gam_3, importance=importance))
}

# Gallup

# For Satisfaction Score = (Q1+Q2)/2
target = data$Satisfy_score
target_string = "Satisfy_score"
result = model(target, target_string)
reg1_s = result$reg1
gam1_s = result$gam_1
gam2_s = result$gam_2
gam3_s = result$gam_3
importance_s = result$importance

# For Manager Score
target = data$Manager_score
target_string = "Manager_score"
result = model(target, target_string)
reg1_m = result$reg1
gam1_m = result$gam_1
gam2_m = result$gam_2
gam3_m = result$gam_3
importance_m = result$importance


# For Belonging Score
# Version 1: Take Belonging Score = (Q5+Q6+Q15+Q16) / 4
target = data$Belonging_score
target_string = "Belonging_score"
result = model(target, target_string)
reg1_b = result$reg1
gam1_b = result$gam_1
gam2_b = result$gam_2
gam3_b = result$gam_3
importance_b = result$importance

# Version 2: Take Q5,Q6,Q15,Q16 separately
# Q4
target = data$avg_score_Q5
target_string = "avg_score_Q5"
result = model(target, target_string)
reg1_q5 = result$reg1
gam1_q5 = result$gam_1
gam2_q5 = result$gam_2
gam3_q5 = result$gam_3
importance_q5 = result$importance

# Q12
target = data$avg_score_Q6
target_string = "avg_score_Q6"
result = model(target, target_string)
reg1_q6 = result$reg1
gam1_q6 = result$gam_1
gam2_q6 = result$gam_2
gam3_q6 = result$gam_3
importance_q6 = result$importance

# Q15
target = data$avg_score_Q15
target_string = "avg_score_Q15"
result = model(target, target_string)
reg1_q15 = result$reg1
gam1_q15 = result$gam_1
gam2_q15 = result$gam_2
gam3_q15 = result$gam_3
importance_q15 = result$importance

# Q16
target = data$avg_score_Q16
target_string = "avg_score_Q16"
result = model(target, target_string)
reg1_q16 = result$reg1
gam1_q16 = result$gam_1
gam2_q16 = result$gam_2
gam3_q16 = result$gam_3
importance_q16 = result$importance

library(gridExtra)
library(broom)
library(texreg)
#setwd("C:/Users/tiip/Downloads/Wellcome_Glint/ManagerToEmployee")
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Gallup")

models <- list(reg1_s, reg1_m, reg1_b, reg1_q5, reg1_q6, reg1_q15,reg1_q16)
screenreg(models, custom.model.names = c("OLS 1(Satisfy score)", "OLS 2(Manager score)", "OLS 3(Belonging score)",
                                         "OLS 4(Belonging-Q5)", "OLS 5(Belonging-Q6)", 
                                         "OLS 6(Belonging-Q15)", "OLS 7(Belonging-Q16)"), 
          file = "./models_summary_OLS.txt")

models <- list(gam2_s, gam2_m, gam2_b, gam2_q5, gam2_q6, gam2_q15, gam2_q16)
screenreg(models, custom.model.names = c("GAM 2(Satisfy score)", "GAM 2(Manager score)", "GAM 2(Belonging score)",
                                         "GAM 2(Belonging-Q5)", "GAM 2(Belonging-Q6)",
                                         "GAM 2(Belonging-Q15)", "GAM 2(Belonging-Q16)"), 
          file = "./models_summary_gam2.txt")

models <- list(gam3_s, gam3_m, gam3_b, gam3_q5, gam3_q6, gam3_q15,gam3_q16)
screenreg(models, custom.model.names = c("GAM 3(Satisfy score)", "GAM 3(Manager score)", "GAM 3(Belonging score)",
                                         "GAM 3(Belonging-Q5)", "GAM 3(Belonging-Q6)",
                                         "GAM 3(Belonging-Q15)", "GAM(Belonging-Q16)"), 
          file = "./models_summary_gam3.txt")



# Importance 
# Create list of importance vectors with names
imp_list <- list(
  Satisfactory_score = setNames(importance_s$Importance, importance_s$Variable),
  Manager_score = setNames(importance_m$Importance, importance_m$Variable),
  Belonging_score = setNames(importance_b$Importance, importance_b$Variable),
  Belonging_score_Q5 = setNames(importance_q5$Importance, importance_q5$Variable),
  Belonging_score_Q6 = setNames(importance_q6$Importance, importance_q6$Variable),
  Belonging_score_Q15 = setNames(importance_q15$Importance, importance_q15$Variable),
  Belonging_score_Q16 = setNames(importance_q16$Importance, importance_q16$Variable)
)

# Create list of sign vectors
sign_list <- list(
  Satisfactory_score = setNames(importance_s$Sign, importance_s$Variable),
  Manager_score = setNames(importance_m$Sign, importance_m$Variable),
  Belonging_score = setNames(importance_b$Sign, importance_b$Variable),
  Belonging_score_Q5 = setNames(importance_q5$Sign, importance_q5$Variable),
  Belonging_score_Q6 = setNames(importance_q6$Sign, importance_q6$Variable),
  Belonging_score_Q15 = setNames(importance_q15$Sign, importance_q15$Variable),
  Belonging_score_Q16 = setNames(importance_q16$Sign, importance_q16$Variable)
)

# Get all unique variable names
all_vars <- unique(unlist(lapply(imp_list, names)))

# Create combined matrix for importance and signs
imp_mat <- sapply(imp_list, function(x) x[match(all_vars, names(x))])
sign_mat <- sapply(sign_list, function(x) x[match(all_vars, names(x))])

# Combine importance and signs into formatted strings
combined_char <- matrix("", nrow = length(all_vars), ncol = length(imp_list))
for(i in 1:nrow(imp_mat)) {
  for(j in 1:ncol(imp_mat)) {
    imp_val <- imp_mat[i, j]
    sign_val <- sign_mat[i, j]
    if(!is.na(imp_val) && !is.na(sign_val)) {
      combined_char[i, j] <- paste0(round(imp_val, 3), " (", sign_val, ")")
    } else if(!is.na(imp_val)) {
      combined_char[i, j] <- as.character(round(imp_val, 3))
    }
  }
}

# Convert to dataframe
combined_df <- as.data.frame(combined_char)
rownames(combined_df) <- all_vars
colnames(combined_df) <- names(imp_list)

# Order by average importance
avg_imp <- rowMeans(imp_mat, na.rm = TRUE)
combined_df <- combined_df[order(avg_imp, decreasing = TRUE), ]

# Add total row (importance only, no sign)
total_imp <- colSums(imp_mat, na.rm = TRUE)
total_row <- as.character(round(total_imp, 3))
combined_df <- rbind(combined_df, Total = total_row)

output = knitr::kable(combined_df, caption = "Relative Importance Comparison", format="markdown")
path = "./importance_metrics.md"
writeLines(output, path)
