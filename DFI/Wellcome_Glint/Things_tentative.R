library(readr)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint")
data = read.csv("./score_wellcome_new(try_only).csv")
library(dplyr)
variables <- c("avg_score_Q7", "avg_score_Q8", 
               "avg_score_Q9", "avg_score_Q10",
               "avg_score_Q11", "avg_score_Q12", "avg_score_Q15", 
               "avg_score_Q16", "avg_score_Q17", "avg_score_Q18",
               "avg_score_Q19", "avg_score_Q20", "avg_score_Q21",
               "avg_score_Q22", "avg_score_Q23", "avg_score_Q24",
               "avg_score_Q25", "avg_score_Q26", "avg_score_Q27", 
               "avg_score_Q28", "avg_score_Q29", "avg_score_Q30")

#####Shapley value
#train_data <- as.matrix(data[, variables])
train_data <- as.data.frame(data[, variables])
train_labels <- data$Satisfy_score
library(xgboost)
# Custom predict function that handles data type conversion
xgb_predict <- function(model, newdata) {
  predict(model, as.matrix(newdata))
}
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)
xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror")
# Create predictor with DATA FRAME (required by iml)
library(iml)
predictor <- Predictor$new(
  model = xgb_model,
  data = train_data,  # Must be data frame
  y = train_labels,
  predict.fun = xgb_predict  # Use our custom predictor
)
# Method that always works for single-row extraction
x_interest <- train_data[1, , drop = FALSE]  # Critical: keep as data frame
rownames(x_interest) <- NULL  # Clean up row names

# Verify structure matches exactly:
stopifnot(
  identical(colnames(x_interest), colnames(train_data)),
  is.data.frame(x_interest)
)
shapley <- Shapley$new(
  predictor,
  x.interest = x_interest
)
print(shapley$results)
shapley$plot()


library(ggplot2)
library(dplyr)
library(purrr)

# Function to compute SHAP values for a single row
compute_shap <- function(row_index) {
  x_interest <- train_data[row_index, , drop = FALSE]
  rownames(x_interest) <- NULL
  shapley <- Shapley$new(
    predictor,
    x.interest = x_interest,
    sample.size = 316  # Adjust for speed/accuracy tradeoff
  )
  return(shapley$results)
}

# Compute SHAP values for all rows (this will take time)
shap_values <- map_dfr(1:nrow(train_data), compute_shap, .id = "row_id")
shap_values_clean <- shap_values %>%
  mutate(
    # Extract numeric value after "="
    feature_value_numeric = as.numeric(gsub(".*=", "", feature.value)),
    
    # Clean feature names by removing values
    feature_clean = gsub("=.*", "", feature.value)
  )

# 2. Create summary plot with corrected data
shap_summary_plot <- shap_values_clean %>%
  group_by(feature_clean) %>%
  mutate(mean_abs_shap = mean(abs(phi))) %>%
  ungroup() %>%
  mutate(feature_clean = reorder(feature_clean, -mean_abs_shap)) %>%
  ggplot(aes(x = phi, y = feature_clean, color = feature_value_numeric)) +
  geom_point(alpha = 0.5, position = position_jitter(height = 0.2)) +
  scale_color_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = median(shap_values_clean$feature_value_numeric, na.rm = TRUE)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "SHAP Value (Impact on Prediction)",
    y = "Feature",
    color = "Feature Value",
    title = "SHAP Summary Plot"
  ) +
  theme_minimal()
print(shap_summary_plot)

####GAM individual plot in smooth term
library(ggeffects)
library(ggplot2)
library(mgcv)
gam_formula <- as.formula(paste("Satisfy_score", "~", paste(paste0("s(", variables, ")"), collapse = " + ")))
gam_2 <- gam(gam_formula, data =data, family = gaussian(link = 'log'))

# Generate predictions with 1-unit intervals
pred <- ggpredict(gam_2, 
                  terms = "avg_score_Q12 [0:100 by=1]",
                  back.transform = TRUE)

# Plot with non-linear visualization
library(ggeffects)
library(ggplot2)
#Q12
pred_Q12 <- ggpredict(gam_2, 
                  terms = "avg_score_Q12 [0:100 by=1]",
                  back.transform = TRUE)
# ggplot(pred, aes(x, predicted)) + 
#   geom_line() + 
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   labs(x = "Q12 Score", y = "Predicted Satisfaction Score")
#Q20
observed_min <- min(data$avg_score_Q20)
observed_max <- max(data$avg_score_Q20)
pred_Q20 <- ggpredict(gam_2, 
                  terms = paste0("avg_score_Q20 [", observed_min, ":", observed_max, " by=1]"),
                  back.transform = TRUE)
# ggplot(pred, aes(x, predicted)) + 
#   geom_line() + 
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   labs(x = "Q20 Score", y = "Predicted Satisfaction Score")
#Q29
observed_min <- min(data$avg_score_Q29)
observed_max <- max(data$avg_score_Q29)
pred_Q29 <- ggpredict(gam_2, 
                      terms = paste0("avg_score_Q29 [", observed_min, ":", observed_max, " by=1]"),
                      back.transform = TRUE)
library(gridExtra)

# Generate plots and save as objects
p1 <- ggplot(pred_Q12, aes(x, predicted)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Q12 Score", y = "Predicted Satisfaction Score")

p2 <- ggplot(pred_Q20, aes(x, predicted)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Q20 Score", y = "Predicted Satisfaction Score")

p3 <- ggplot(pred_Q29, aes(x, predicted)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Q29 Score", y = "Predicted Satisfaction Score")
# Arrange plots side-by-side
grid.arrange(p1, p2, p3, ncol = 2)

####Stepwise ---> it only for linear regression, then is similar with Lasso and ridge --> need or not????
target =data$Satisfy_score
# Fit the full model
all <- lm(target ~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
            avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
            avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
            avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
            avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
            avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
            avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
          data = data)

# Perform stepwise regression
intercept_only <- lm(target ~ 1, data=data)  # Model with only intercept
both <- step(intercept_only, direction='both', scope=formula(all), trace=)
# Check the ANOVA and coefficients
anova_results <- both$anova
coefficients <- both$coefficients

# Display results
print(anova_results)
print(coefficients)

####importance check
###calc.relimp --> only can check linear model and in average percenatge
library(relaimpo)
calc.relimp(both, type = "lmg", rela = TRUE)
###vi --> check all model but without showing percentage
library(vip)
vi(both)
vi_gam <- vi(gam_2,
             method = "permute",
             target = "Satisfy_score",
             train = data[, c("Satisfy_score", variables)],
             metric = "rsq",
             pred_wrapper = function(object, newdata) {
               predict(object, newdata = newdata, type = "response")
               },
             nsim = 30  # Increase for stability
)
print(vi_gam, n=22)
library(fastshap)
library(shapviz)
# Custom prediction wrapper with explicit data structure control
pred_wrapper <- function(object, newdata) {
     # Convert to data frame with original column names
     newdata_df <- as.data.frame(newdata)
     colnames(newdata_df) <- variables  # Ensure variables matches model terms
     
     # Return predictions
     predict(object, newdata = newdata_df, type = "response")
}
# Convert SHAP visualization object PROPERLY
shap_values <- explain(
     gam_2,
     X = data[, variables, drop = FALSE],  # Keep as data frame
     pred_wrapper = pred_wrapper,
     nsim = 30
)
shap_viz <- shapviz(
     object = as.matrix(shap_values),  # SHAP values matrix
     X = as.data.frame(data[, variables])  # Feature data frame
)
# Verify successful creation
class(shap_viz)
sv_importance(shap_viz, show_numbers = TRUE)
##Penalized in Gam(L1-norm) --> bs = ts
gam_penalized_formula <- as.formula(paste("Satisfy_score", "~", paste(paste0("s(", variables, ", bs = 'ts')"), collapse = " + ")))

gam_penalized <- gam(
  gam_penalized_formula,
  data = data,
  #method = "REML",
  family = gaussian(link = 'log')
)
summary(gam_penalized)
library(ggeffects)
library(gratia)
library(RColorBrewer)
plot_obj1 <- ggeffects::ggpredict(gam_penalized)
custom_colors <- colorRampPalette(brewer.pal(9, "Set1"))(22)
plot(plot_obj1, facets = TRUE) + labs(x = "avg_score") + scale_color_manual(values = custom_colors)
gratia::draw(gam_penalized)
###bs="ts" has Smoothness penalty and Shrinkage penalty, so
###When the shrinkage penalty dominates (due to weak/no signal), 
### the smooth term collapses to a horizontal line at y=0. 
### y=0 means the variable has no meaningful effect on the outcome and don't meaningfully predict `Satisfy_score`

###Make comparison of importance with Gallup and Glint GAM model in same IVs

formula <- as.formula(paste("Satisfy_score", "~", paste(paste0("s(avg_score_Q6) + s(avg_score_Q11) + s(avg_score_Q16) + 
                                                                 s(avg_score_Q22) + s(avg_score_Q25) + 
                                                               s(avg_score_Q27) + s(avg_score_Q29)"))))

target =data$Satisfy_score
gam_1 <- gam(
  formula,
  data = data,
  #method = "REML",
  family = gaussian(link = 'log')
)
# Custom VI function for GAMs (using absolute t-value)
gam_vi <- function(object) {
  sum_tbl <- summary(object)$s.table
  vip_df <- data.frame(
    Variable = gsub("s\\((.*)\\)", "\\1", rownames(sum_tbl)),
    Importance = abs(sum_tbl[, "F"]),  # Using F-statistic magnitude
    stringsAsFactors = FALSE
  )
  return(vip_df)
}
vi_gam1 <- gam_vi(gam_1)
vi_gam2 <- gam_vi(gam_2)
# Merge results for common variables
common_vars <- intersect(vi_gam1$Variable, vi_gam2$Variable)
vi_comparison <- merge(
  vi_gam1[vi_gam1$Variable %in% common_vars, ],
  vi_gam2[vi_gam2$Variable %in% common_vars, ],
  by = "Variable",
  suffixes = c(".gam1", ".gam2")
)

# Visual comparison
library(ggplot2)
ggplot(vi_comparison, aes(x = Importance.gam1, y = Importance.gam2)) +
  geom_point() +
  #geom_abline(slope = 1, linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = Variable)) +
  labs(title = "Variable Importance Comparison")
###Above 2 are More Importance!!!
##Permutation Importance (Model-Agnostic)
library(DALEX)

# Function to compute permutation importance
get_perm_vi <- function(model, data, target) {
  explainer <- explain(
    model,
    data = data,
    y = data[[target]],
    label = deparse(substitute(model))
  )
  vi <- model_parts(explainer, type = "difference")
  return(vi)
}

# Compute for both models
set.seed(123)
vi_perm1 <- get_perm_vi(gam_1, data, "Satisfy_score")
vi_perm2 <- get_perm_vi(gam_2, data, "Satisfy_score")

# Filter to common variables and compare
library(dplyr)
comparison_df <- bind_rows(
  vi_perm1 %>% filter(variable %in% common_vars) %>% mutate(model = "gam1"),
  vi_perm2 %>% filter(variable %in% common_vars) %>% mutate(model = "gam2")
)

comparison_df <- comparison_df %>%
  group_by(variable, model) %>%
  summarise(dropout_loss = mean(dropout_loss)) %>%  # Remove duplicates
  ungroup()
library(ggrepel)
ggplot(comparison_df, 
       aes(x = dropout_loss, 
           y = reorder(variable, dropout_loss),
           fill = model)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(
    aes(label = round(dropout_loss, 2),  # Direct column reference
        group = model),
    position = position_dodge(width = 0.8),  # Match bar dodge width
    hjust = -0.15,
    size = 3.5,
    color = "black"
  ) +
  scale_fill_manual(
    name = "Model Version",
    labels = c("gam1" = "Gallup Model", "gam2" = "Glint Model"),
    values = c("#1f77b4", "#ff7f0e")
  ) +
  labs(x = "Performance Drop (Higher = More Important)", y = "") +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10)
  ) +
  expand_limits(x = max(comparison_df$dropout_loss) * 1.2)


####Direct GAM Metrics Comparison
# Extract key metrics
extract_gam_metrics <- function(model) {
  sum_tbl <- summary(model)$s.table
  model_sum <- summary(model)
  data.frame(
    Variable = rownames(sum_tbl),
    edf = sum_tbl[, "edf"],
    p_value = sum_tbl[, "p-value"],
    R_sq = model_sum$r.sq,          # Adjusted R-squared
    Deviance_explained = model_sum$dev.expl,
    stringsAsFactors = FALSE
  )
}

# Compare common variables
metrics_gam1 <- extract_gam_metrics(gam_1)
metrics_gam2 <- extract_gam_metrics(gam_2)

comparison_metrics <- merge(
  metrics_gam1,
  metrics_gam2,
  by = "Variable",
  suffixes = c(".gam1", ".gam2")
)

# Filter to variables present in both models
comparison_metrics <- comparison_metrics %>%
  filter(
    Variable %in% c("s(avg_score_Q11)", "s(avg_score_Q16)", "s(avg_score_Q16)",
                             "s(avg_score_Q22)", "s(avg_score_Q25)", 
                             "s(avg_score_Q27)", "s(avg_score_Q29)")) %>% # List common terms
      select(
        Variable,
        edf.gam1, edf.gam2,
        p_value.gam1, p_value.gam2,
        R_sq.gam1, R_sq.gam2,
        Deviance_explained.gam1, Deviance_explained.gam2
      )
#R_sq explain Gaussian/normal responses;Deviance generalizes R_sq to non‐Gaussian models
knitr::kable(comparison_metrics)

####Randon Forest
library(randomForest)
library(caret) 
trainIndex <- createDataPartition(tdata$Satisfy_score, p = .7, 
                                 list = FALSE, 
                                 times = 1)
train_data <- tdata[trainIndex, ]
test_data <- tdata[-trainIndex, ]
model_matrix <- model.matrix(Satisfy_score~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
                               avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
                               avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
                               avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
                               avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
                               avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
                               avg_score_Q28 + avg_score_Q29 + avg_score_Q30, tdata)
n_predictors <- ncol(model_matrix) - 1  # Subtract 1 for intercept
rf_model <- randomForest(Satisfy_score~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
                            avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
                            avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
                            avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
                            avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
                            avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
                            avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
                        data = data)

rf_model <- randomForest(Satisfy_score~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
                           avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
                           avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
                           avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
                           avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
                           avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
                           avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
                         data = data)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance/Glint")
generate_pd_plots <- function(model, variables, ncol = 5) {
  library(pdp)
  library(ggplot2)
  library(gridExtra)
  # Number of columns in final grid
  # Create empty list to store plots
  plot_list <- list()
  
  # Generate partial dependence plots for each variable
  for (var in variables) {
    pd_data <- partial(model, pred.var = var)
    
    p <- ggplot(pd_data, aes(x = .data[[var]], y = yhat)) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(title = var, x = var, y = "Partial Dependence") +
      theme_bw() +
      theme(
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 10)
      )
    
    plot_list[[var]] <- p
  }
  
  # Combine and return plots
  grid.arrange(grobs = plot_list, ncol = ncol)
}
combined_plot_rf2_new <- generate_pd_plots(model = rf_model, variables = variables)
ggsave("./Satisfy_partial_plot.png", combined_plot_rf2, width = 10, height = 6, units = "in")

rf2<- randomForest(Satisfy_score~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
                     avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
                     avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
                     avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
                     avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
                     avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
                     avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
                   ntree=11,mtry = max(floor(11/3), 1),
                   data = data)
print(rf2)
importance(rf2)
rf2_new<- randomForest(Satisfy_score~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
                     avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
                     avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
                     avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
                     avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
                     avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
                     avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
                   ntree=n_predictors/2,mtry = 3,
                   data = data)
print(rf2_new)
means <- sapply(variables, function(var) mean(new_data[[var]], na.rm = TRUE))
print(means)
{library(randomForest)
set.seed(123)

# Create parameter grid (exclude mtry=0 and ntree=0)
grid <- expand.grid(
  mtry = 1:22,       # Valid mtry: 1 to 22 predictors
  ntree = seq(5, 100, by = 5)  # Valid ntree: 5,10,...,100
)

# Store results
results <- data.frame()

# Full grid search
for(i in 1:nrow(grid)) {
  model <- randomForest(
    Satisfy_score~ avg_score_Q7+avg_score_Q8+avg_score_Q9+avg_score_Q10+
      avg_score_Q11 + avg_score_Q12 + avg_score_Q15 + 
      avg_score_Q16 + avg_score_Q17 + avg_score_Q18 +
      avg_score_Q19 + avg_score_Q20 + avg_score_Q21 +
      avg_score_Q22 + avg_score_Q23 + avg_score_Q24 +
      avg_score_Q25 + avg_score_Q26 + avg_score_Q27 + 
      avg_score_Q28 + avg_score_Q29 + avg_score_Q30,
    data = data,
    mtry = grid$mtry[i],
    ntree = grid$ntree[i]
  )
  
  # Calculate R-squared
  predictions <- predict(model, data)
  R_squared <- 1 - sum((data$Satisfy_score - predictions)^2) / sum((data$Satisfy_score - mean(data$Satisfy_score))^2)
  
  # Record R-squared
  results <- rbind(results, data.frame(
    mtry = grid$mtry[i],
    ntree = grid$ntree[i],
    R_squared = R_squared
  ))
  # Progress tracking
  cat("Completed", i, "/", nrow(grid), "\n")
}

# Find optimal combination
best_params <- results[which.max(results$R_squared), ]
cat("Best parameters:\n")
print(best_params)


importance(rf_model)
varImpPlot(rf_model, main = "Variable Importance")}

generate_pd_plots <- function(model, variables, ncol = 5) {
  library(pdp)
  library(ggplot2)
  library(gridExtra)
  # Number of columns in final grid
  # Create empty list to store plots
  plot_list <- list()
  
  # Generate partial dependence plots for each variable
  for (var in variables) {
    pd_data <- partial(model, pred.var = var)
    
    p <- ggplot(pd_data, aes(x = .data[[var]], y = yhat)) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(title = var, x = var, y = "Partial Dependence") +
      theme_bw() +
      theme(
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 10)
      )
    
    plot_list[[var]] <- p
  }
  
  # Combine and return plots
  grid.arrange(grobs = plot_list, ncol = ncol)
}

combined_plot_rf2_new <- generate_pd_plots(model = rf2, variables = variables)
print(combined_plot_rf2_new)

create_pd_importance_plot <- function(model, title = "Partial Dependence with Importance Coloring",
                                      n_colors = 22,
                                      palette = "Set1") {
  library(RColorBrewer)
  library(dplyr)
  # 1. Get variable importance
  imp <- importance(model)
  imp_df <- data.frame(
    variable = rownames(imp),
    importance = imp[, 1],
    stringsAsFactors = FALSE
  ) %>% 
    arrange(desc(importance))
  
  # 2. Create color mapping
  color_values <- colorRampPalette(brewer.pal(9, palette))(n_colors)
  names(color_values) <- imp_df$variable
  
  # 3. Generate partial dependence data
  combined_data <- do.call(rbind, lapply(imp_df$variable, function(var) {
    pd <- partial(model, pred.var = var)
    data.frame(
      x_value = pd[[var]],
      yhat = pd$yhat,
      variable = factor(var, levels = imp_df$variable),
      stringsAsFactors = FALSE
    )
  })) %>% 
    left_join(imp_df, by = "variable")
  
  # 4. Create visualization
  ggplot(combined_data, aes(x = x_value, y = yhat, 
                            color = variable, 
                            group = variable)) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(
      name = "Variables (Importance)",
      values = color_values,
      labels = function(x) {
        exact_imp <- imp_df$importance[match(x, imp_df$variable)]
        sprintf("%s (%.1f)", x, exact_imp)
      },
      guide = guide_legend(
        override.aes = list(size = 1.5),
        title.position = "top",
        label.position = "right"
      )
    ) +
    labs(
      x = "Average Score", 
      y = "Predicted Satisfaction Score",
      title = title
    ) +
    theme_bw() +
    theme(
      legend.box = "vertical",
      legend.key.height = unit(0.8, "cm"),
      legend.title.align = 0.5
    )
}
imp_plot_rf2<- create_pd_importance_plot(rf2)
#imp_plot_rf2_new <- create_pd_importance_plot(rf2_new)
imp_plot_rf2
#imp_plot_rf2_new
library(dplyr)
tdata_imputed <- tdata %>%
  mutate(across(
    starts_with("avg_score"),
    ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x)
  ))

# Create synthetic data using imputed values
new_data <- tdata_imputed %>%
  mutate(across(
    starts_with("avg_score"),
    ~ pmin(.x + 20, 100)
  ))
library(tidyr)
boosted_vars <- new_data %>%
  select(starts_with("avg_score")) %>%
  summarise(across(everything(), ~ sum(.x == 100))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_capped") %>%
  filter(n_capped > 0) %>%          # Keep only variables that hit 100
  arrange(n_capped) %>%             # Sort from LOWEST to HIGHEST
  print(n=28)
# Merge the two tables by variable name
combined_table <- boosted_vars %>%
  left_join(
    importance(rf2) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("variable"),
    by = "variable"
  ) %>%
  mutate(
    importance_order = rank(-IncNodePurity, ties.method = "min")  # Rank descending
  ) %>%
  select(variables = variable, n_capped, IncNodePurity, importance_order)%>%
  filter(importance_order <= 15) %>%
  filter(IncNodePurity >=1000)

# Display formatted table
#print(combined_table, n=22)
knitr::kable(combined_table,format = "simple", digits = 2)

library(dplyr)

# Assume you have a fitted model named `model`
# Create a function to predict Satisfy_score for given average scores
predict_satisfaction <- function(avg_scores) {
  new_data <- data.frame(
    avg_score_Q7 = avg_scores[1],
    avg_score_Q8 = avg_scores[2],
    avg_score_Q9 = avg_scores[3],
    avg_score_Q10 = avg_scores[4],
    avg_score_Q11 = avg_scores[5],
    avg_score_Q12 = avg_scores[6],
    avg_score_Q15 = avg_scores[7],
    avg_score_Q16 = avg_scores[8],
    avg_score_Q17 = avg_scores[9],
    avg_score_Q18 = avg_scores[10],
    avg_score_Q19 = avg_scores[11],
    avg_score_Q20 = avg_scores[12],
    avg_score_Q21 = avg_scores[13],
    avg_score_Q22 = avg_scores[14],
    avg_score_Q23 = avg_scores[15],
    avg_score_Q24 = avg_scores[16],
    avg_score_Q25 = avg_scores[17],
    avg_score_Q26 = avg_scores[18],
    avg_score_Q27 = avg_scores[19],
    avg_score_Q28 = avg_scores[20],
    avg_score_Q29 = avg_scores[21],
    avg_score_Q30 = avg_scores[22]
  )
  prediction <- predict(rf2, newdata = new_data)
  
  # Check if prediction is NA
  if (is.na(prediction)) {
    warning("Prediction returned NA for avg_scores: ", paste(avg_scores, collapse = ", "))
  }
  
  return(prediction)
}

# Find the required increases
required_increases <- numeric(length = 22)  # Assuming 20 questions

# Loop through each average score question
for (i in 1:length(required_increases)) {
  current_score <- mean(tdata[[paste0("avg_score_Q", i)]], na.rm = TRUE)  # Current average score
  increment <- 0
  
  # Increment until the predicted score reaches 80
  while (TRUE) {
    predicted_score <- predict_satisfaction(c(rep(current_score + increment, 22)))
    
    # Check if prediction is NA
    if (is.na(predicted_score)) {
      break  # Exit the loop if prediction fails
    }
    
    if (predicted_score >= 80) {
      break  # Exit the loop if the desired score is reached
    }
    
    increment <- increment + 1  # Increment by 1 (or any suitable delta)
  }
  
  required_increases[i] <- increment
}

# Display required increases
names(required_increases) <- variables
print(required_increases)



simulate_increments <- function(increment_values, variables) {
  # Create new scores based on the increment values
  new_scores <- sapply(1:length(variables), function(i) {
    current_score <- mean(tdata[[variables[i]]], na.rm = TRUE)
    current_score + increment_values[i]
  })
  
  # Print the current combination of variable names and their scores
  combined_output <- paste(variables, new_scores, sep = ": ")
  print(paste("Testing combinations:", paste(combined_output, collapse = ", ")))
  
  # Predict new satisfaction score
  predicted_score <- predict_satisfaction(new_scores)
  return(predicted_score)
}

# Example: Test different combinations of increases
for (i in 10:15) {
  increments <- rep(i, length(variables))  # Testing uniform increases
  predicted_score <- simulate_increments(increments, variables)
  print(paste("With uniform increase of", i, "predicted score:", predicted_score))
}


{# # Function to find the optimal combination
# find_optimal_combination <- function(variables, target_score, max_increment) {
#   # Initialize variables to store best combination
#   best_combination <- NULL
#   best_increase <- Inf
#   best_num_vars <- Inf
#   
#   # Loop through all combinations of variables
#   for (i in 1:length(variables)){
#     for (increment in 0:max_increment) {
#       # Create a vector to hold increments for the current combination
#       increments <- rep(0, length(variables))
#       increments[i] <- increment
#       
#       # Get the predicted score for the current combination
#       predicted_score <- simulate_increments(increments, variables)
#       
#       # Check if we meet the target score
#       if (predicted_score >= target_score) {
#         total_increase <- sum(increments)
#         num_vars <- sum(increments > 0)
#         
#         # Check if this combination is better
#         if (total_increase < best_increase || 
#             (total_increase == best_increase && num_vars < best_num_vars)) {
#           best_combination <- increments
#           best_increase <- total_increase
#           best_num_vars <- num_vars
#         }
#       }
#     }
#   }
#   
#   return(list(
#     combination = best_combination,
#     total_increase = best_increase,
#     num_vars = best_num_vars
#   ))
# }
# 
# # Set parameters
# target_score <- 80
# max_increment <- 15  # Adjust as needed
# 
# # Find the optimal combination
# optimal_result <- find_optimal_combination(variables, target_score, max_increment)
# 
# # Display the results
# print(optimal_result)
}

find_optimal_combination <- function(variables, target_score, max_increment, max_vars) {
  # Initialize variables to store the best combination
  best_combination <- NULL
  best_increase <- Inf
  best_num_vars <- Inf
  
  # Loop through combinations of variable indices with a limit on the number of variables
  for (num_vars in 1:min(max_vars, length(variables))) {
    for (indices in combn(1:length(variables), num_vars, simplify = FALSE)) {
      # Test all integer increments for the selected variables
      for (increments in 1:max_increment) {
        # Create a vector to hold increments for the current combination
        current_increments <- rep(0, length(variables))
        
        # Apply the integer increment to the selected variables
        for (index in indices) {
          current_increments[index] <- increments
        }
        
        # Get the predicted score for the current combination
        predicted_score <- simulate_increments(current_increments, variables)
        
        # Check if we meet the target score
        if (predicted_score >= target_score) {
          total_increase <- sum(current_increments)
          num_vars_used <- sum(current_increments > 0)
          
          # Check if this combination is better
          if (total_increase < best_increase || 
              (total_increase == best_increase && num_vars_used < best_num_vars)) {
            best_combination <- current_increments
            best_increase <- total_increase
            best_num_vars <- num_vars_used
          }
        }
      }
    }
  }
  
  return(list(
    combination = best_combination,
    total_increase = best_increase,
    num_vars = best_num_vars
  ))
}

# Set parameters
target_score <- 80
max_increment <- 15  # Maximum integer increment
max_vars <- 15       # Maximum number of variables to test

# Find the optimal combination
optimal_result <- find_optimal_combination(variables, target_score, max_increment, max_vars)

# Display the results
print(optimal_result)
o2 = odata%>%filter(Group ==2)
nrow(o2)
t1 <- lm(Belonging_score ~ avg_score_Q10 + avg_score_Q17 + avg_score_Q18 + avg_score_Q22+ 
             avg_score_Q24 + avg_score_Q29 + avg_score_Q32 +avg_score_Q37, 
           data = o1)
summary(t1)

##6, 16,29
##16,17,25,29
##6,16,17,27,29

#SSC
##10,22,24 (~0.9052)
##intercept, 17 (~0.7448)
##10,32 (~0.9402)








{####Things try
library(dplyr)
tdata <-read.csv("./score_wellcome_new.csv")
variable <-c("Turnover", "Satisfy_score","Manager_score","Belonging_score")
data_standardized <- tdata %>% 
  dplyr::select(all_of(variable)) %>%
  mutate(across(everything(), ~scale(.) %>% as.vector()))

# Replace the original columns with the standardized columns
data_combined <- tdata %>%
  mutate(across(all_of(variable), ~ data_standardized[[cur_column()]]))
target1 <-data_combined$productivity_sum
target2 <-data_combined$SOP
#If the DV has extreme scales (e.g., values in millions), normalization can stabilize training.
scaled_target1 <- scale(data_combined$productivity_sum)
scaled_target2 <- scale(data_combined$SOP)
#For Productivity
formula <- as.formula(paste("productivity_sum", "~", paste(paste0("Turnover + Satisfy_score + Manager_score + 
                                                                 Belonging_score"))))
model1 <- lm(formula, data = data_combined)
formula <- as.formula(paste("productivity_sum", "~", paste(paste0("s(Turnover) + s(Satisfy_score) + s(Manager_score) + 
                                                                 s(Belonging_score)"))))
gmodel1 <- gam(formula, data=data_combined, family = gaussian(link = 'log'))
#For Profitability
lm_formula2 <- reformulate(var, response = "SOP")
model2 <- lm(lm_formula2, data = data_combined)
formula <- as.formula(paste("SOP", "~", paste(paste0("s(Turnover) + s(Satisfy_score) + s(Manager_score) + 
                                                                 s(Belonging_score)"))))
gmodel2 <- gam(formula, data=data_combined, family = gaussian(link = 'log'))
library(broom)
library(texreg)
models<-list(model1, model2)
screenreg(models, custom.model.names = c("OLS 1(Productivity)", "OLS 2(Profitability)"))
models<-list(gmodel1, gmodel2)
screenreg(models, custom.model.names = c("GAM 1(Productivity)", "GAM 2(Profitability)"))}
