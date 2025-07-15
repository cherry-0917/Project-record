Sys.setenv(LANG = "en")
library(readr)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint")
library(dplyr)
library(tidyverse)
tdata <-read.csv("./score_wellcome_new.csv",header = TRUE) #%>%
#   mutate(cluster = factor(Store_Cluster, labels = c("Urban Hub - Local (Wellcome)", "Urban Hub - Local (Wellcome)", "Tourist Border(Wellcome)", "Residential - Western & Top Income (Upscale)",
#                                               "Residential - Local (Wellcome)", "Residential - High Income (Wellcome)", "Office - Local (Wellcome)", "Office - High Income (Upscale)"))) %>%
#   mutate(scale = factor(Format_y, labels = c("Mass", "Upscales")))
trydata <-read.csv("./score_wellcome_new(try_only).csv",header = TRUE)
head(tdata,1)
variable1 <-c("Satisfy_score","Manager_score","Belonging_score","X2023.Turnover", "MSP_sum", "workload")
variable <-c("X2023.Turnover", "MSP_sum", "workload")
#variable <-c("X2023.Turnover", "MSP_sum", "workload", "adj_satisfy_rank","adj_manager_rank","adj_belonging_rank")
#vari<-c("avg_turnover", "MSP_sum", "workload", "adj_satisfy_rank","adj_manager_rank","adj_belonging_rank")
scale_params <- list()

# Calculate and store scaling parameters
for (var in c(variable, "productivity_sum", "SOP_sum","SOP...T.Area")) {
  scale_params[[var]] <- list(
    mean = mean(tdata[[var]], na.rm = TRUE),
    sd = sd(tdata[[var]], na.rm = TRUE)
  )
}
data_standardized1 <- tdata %>%
  mutate(
    across(c(variable, productivity_sum, SOP_sum,SOP...T.Area), 
           ~ (. - scale_params[[cur_column()]]$mean) / scale_params[[cur_column()]]$sd)
  ) %>%
  mutate(
    scaled_target1 = productivity_sum,
    scaled_target2A = SOP_sum,
    scaled_target2B = SOP...T.Area,
    cluster = factor(Store.Cluster),
    scale = factor(Format_y)
  )
{# scale_params <- list()
# for (var in c(vari, "productivity_mean", "SOP_mean")) {
#   scale_params[[var]] <- list(
#     mean = mean(trydata[[var]], na.rm = TRUE),
#     sd = sd(trydata[[var]], na.rm = TRUE)
#   )
# }
# data_standardized2 <- trydata %>%
#   mutate(
#     across(c(vari, productivity_mean, SOP_mean), 
#            ~ (. - scale_params[[cur_column()]]$mean) / scale_params[[cur_column()]]$sd)
#   ) %>%
#   mutate(
#     scaled_target1 = productivity_mean,
#     scaled_target2 = SOP_mean,
#     cluster = factor(Store.Cluster),
#     scale = factor(Format_y)
#   )
# target1 <-data_combined$productivity_sum
# target2 <-data_combined$SOP_sum
#If the DV has extreme scales (e.g., values in millions), normalization can stabilize training.
# scaled_target1 <- scale(data_combined$productivity_mean)
# scaled_target2 <- scale(data_combined$SOP_mean)
  }  
##Productivity
data_standardized1$cluster <- relevel(data_standardized1$cluster, ref = "Residential - Local (Wellcome)")
data_standardized1$scale <- relevel(data_standardized1$scale, ref = "Mass")
contcont1A <- lm(scaled_target1 ~ X2023.Turnover  + adj_satisfy_rank  +
                  adj_manager_rank  + adj_belonging_rank  +
                  MSP_sum  + workload + cluster + scale, 
                data = data_standardized1)
contcont1B <- lm(data_standardized2$productivity_mean ~ (X2023.Turnover  + adj_satisfy_rank  +
                  adj_manager_rank  + adj_belonging_rank  +
                  MSP_sum  + workload) * cluster * scale, 
                data = data_standardized1)
summary(contcont1A)
summary(contcont1B)
library(broom)
coefficients_table <- tidy(contcont1A) %>% filter(!is.na(estimate)) %>%
  select(term, estimate, std.error, p.value)
model_stats <- glance(contcont1A) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value) %>%
  rename(`Residual SE` = sigma,`F-statistic` = statistic,`F-p.value` = p.value)
combined_data1 <- bind_rows(coefficients_table,model_stats %>%
    pivot_longer(everything(), names_to = "term", values_to = "estimate") %>%
    mutate(std.error = NA_real_, p.value = NA_real_))
output1 <- knitr::kable(combined_data,digits = 3,format = "markdown",
  col.names = c("Term", "Estimate", "Std.Error", "p-value"),
  caption = "Combined Model Results")
coefficients_table <- tidy(contcont1B) %>%
  filter(!is.na(estimate)) %>%
  select(term, estimate, std.error, p.value)
model_stats <- glance(contcont1B) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value) %>%
  rename(`Residual SE` = sigma,`F-statistic` = statistic,`F-p.value` = p.value)
combined_data2 <- bind_rows(coefficients_table,model_stats %>%
    pivot_longer(everything(), names_to = "term", values_to = "estimate") %>%
    mutate(std.error = NA_real_, p.value = NA_real_))
output2 <- knitr::kable(combined_data,digits = 3,format = "markdown",
  col.names = c("Term", "Estimate", "Std.Error", "p-value"),
  caption = "Combined Model Results")


contco1A <- lm(scaled_target2 ~ (X2023.Turnover  + adj_satisfy_rank  +
                                     adj_manager_rank  + adj_belonging_rank  +
                                     MSP_sum  + workload)* cluster * scale, 
                 data = data_standardized1)
contco1B <- lm(scaled_target2 ~ (X2023.Turnover  + adj_satisfy_rank  +
                                     adj_manager_rank  + adj_belonging_rank  +
                                     MSP_sum  + workload) * cluster * scale, 
                 data = data_standardized1)
summary(contco1A)
summary(contco1B)
data_standardized2$cluster <- relevel(data_standardized2$cluster, ref = "Residential - Local (Wellcome)")
data_standardized2$scale <- relevel(data_standardized2$scale, ref = "Mass")
contcont2A <- lm(scaled_target1 ~ (avg_turnover  + adj_satisfy_rank  +
                   adj_manager_rank  + adj_belonging_rank  +
                   MSP_sum  + workload)* cluster * scale, 
                 data = data_standardized2)
contcont2B <- lm(scaled_target1 ~ (avg_turnover  + adj_satisfy_rank  +
                   adj_manager_rank  + adj_belonging_rank  +
                   MSP_sum  + workload)* cluster * scale, 
                 data = data_standardized2)
summary(contcont2A)
summary(contcont2B)
contco2A <- lm(scaled_target2 ~ (avg_turnover  + adj_satisfy_rank  +
                                     adj_manager_rank  + adj_belonging_rank  +
                                     MSP_sum  + workload)* cluster * scale, 
                 data = data_standardized2)
contco2B <- lm(scaled_target2 ~ (avg_turnover  + adj_satisfy_rank  +
                                     adj_manager_rank  + adj_belonging_rank  +
                                     MSP_sum  + workload)* cluster * scale, 
                 data = data_standardized2)
summary(contco2A)
summary(contco2B)
data_standardized1$cluster <- relevel(data_standardized$cluster, ref = "Residential - Local (Wellcome)")
data_standardized1$scale <- relevel(data_standardized$scale, ref = "Mass")
contcon1 <- lm(scaled_target1 ~ X2023.Turnover  + Satisfy_score  +
                 Manager_score  + Belonging_score  +
                 MSP_sum  + workload, 
               data = data_standardized1)
summary(contcon1)
library(randomForest)
library(caret) 
trainIndex <- createDataPartition(data_standardized1$SOP_sum, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data_standardized1[trainIndex, ]
test_data <- data_standardized1[-trainIndex, ]
rf_model <- randomForest(SOP_sum ~ X2023.Turnover + adj_satisfy_rank + adj_manager_rank + 
                           adj_belonging_rank + MSP_sum + workload + cluster + scale+interaction,
                         data = data_standardized1, importance = TRUE, ntree = 60, mtry=8)
print(rf_model)
plot(rf_model, main = "OOB Error Rate vs. Number of Trees")
library(caret)
# Define the control using 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)
# Train the model using cross-validation
rf_cv_model <- train(productivity_sum ~ X2023.Turnover + adj_satisfy_rank + adj_manager_rank + 
                       adj_belonging_rank + MSP_sum + workload + cluster + scale +interaction, 
                     data = data_standardized1,
                     method = "rf",
                     trControl = train_control,
                     ntree = 60)

print(rf_cv_model)
# Create interaction term in the data frame
data_standardized1$interaction <- (data_standardized1$X2023.Turnover + 
                                    data_standardized1$adj_satisfy_rank + 
                                    data_standardized1$adj_manager_rank + 
                                    data_standardized1$adj_belonging_rank + 
                                    data_standardized1$MSP_sum + 
                                    data_standardized1$workload) * 
  as.numeric(interaction(data_standardized1$cluster, data_standardized1$scale))
library(Boruta)
boruta_res <- Boruta(SOP_sum ~ X2023.Turnover + adj_satisfy_rank + adj_manager_rank + 
                       adj_belonging_rank + MSP_sum + workload + cluster + scale,
                     data = data_standardized1)
getSelectedAttributes(boruta_res)
importance(rf_model)
variables = c("adj_satisfy_rank","adj_manager_rank","adj_belonging_rank","X2023.Turnover", "MSP_sum", "workload")#, "cluster", "scale")
generate_pd_plots <- function(model, variables, ncol = 3, means, sds, dv_mean, dv_sd) {
  library(pdp)
  library(ggplot2)
  library(gridExtra)
  
  # Create empty list to store plots
  plot_list <- list()
  
  # Generate partial dependence plots for each variable
  for (var in variables) {
    pd_data <- partial(model, pred.var = var)
    
    # Unscale the independent variables if needed
    if (var %in% names(means)) {
      pd_data[[var]] <- pd_data[[var]] * sds[[var]] + means[[var]]
    }
    
    # Unscale the dependent variable predictions
    pd_data$yhat <- pd_data$yhat * dv_sd + dv_mean
    
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
{means <- c(
  X2023.Turnover = mean(trydata$X2023.Turnover, na.rm = TRUE),
  MSP_sum = mean(trydata$MSP_sum, na.rm = TRUE),
  workload = mean(trydata$workload, na.rm = TRUE),
  adj_satisfy_rank = mean(trydata$adj_satisfy_rank, na.rm = TRUE),
  adj_manager_rank = mean(trydata$adj_manager_rank, na.rm = TRUE),
  adj_belonging_rank = mean(trydata$adj_belonging_rank, na.rm = TRUE)
)

sds <- c(
  X2023.Turnover = sd(trydata$X2023.Turnover, na.rm = TRUE),
  MSP_sum = sd(trydata$MSP_sum, na.rm = TRUE),
  workload = sd(trydata$workload, na.rm = TRUE),
  adj_satisfy_rank = sd(trydata$adj_satisfy_rank, na.rm = TRUE),
  adj_manager_rank = sd(trydata$adj_manager_rank, na.rm = TRUE),
  adj_belonging_rank = sd(trydata$adj_belonging_rank, na.rm = TRUE)
)
dv_mean <- mean(trydata$SOP_sum, na.rm = TRUE)
dv_sd <- sd(trydata$SOP_sum, na.rm = TRUE)}
# Generate plots
#variables <- c("Satisfy_score", "Manager_score", "Belonging_score", "X2023.Turnover", "MSP_sum", "workload")
combined_plot_rf2<- generate_pd_plots(model = rf_model, variables = variables, means = means, sds = sds, dv_mean = dv_mean, dv_sd = dv_sd)
ggsave("./profitabilty_partial_plot.png", combined_plot_rf2, width = 10, height = 6, units = "in")

{# library(mgcv)
# gam_1 <-gam(scaled_target1 ~ X2023.Turnover + s(X2023.Turnover, by = cluster), data = data_standardized,family = gaussian(link = 'log'))
# summary(gam_1)
# contpy1 <- lm(scaled_target1 ~ poly(X2023.Turnover,2)*(Store_Cluster + Format_y), data = data_standardized)
# summary(contpy1)
# library(ggplot2)
# ggplot(data, aes(x = scaled_target1, y = X2023.Turnover, color = as.factor(c("Store_Cluster","Format_y")))) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2) * as.numeric(M), se = FALSE) +
#   labs(title = "Moderation Analysis in Non-Linear Model",
#        x = "Independent Variable (X)",
#        y = "Dependent Variable (Y)",
#        color = "Moderator (M)")
# contpy1 <- lm(scaled_target1 ~ poly(X2023.Turnover,2) + poly(Satisfy_score,2) *X2023.Turnover * Satisfy_score* 
#                 (Store_Cluster + Format_y), data = data_standardized)
# summary(contpy1)
}
catcat1 <- lm(scaled_target1 ~ 
                (X2023.Turnover + Satisfy_score_Rank + Manager_score_Rank + 
                   Belonging_score_Rank + MSP_sum + workload) * 
                (cluster + scale), 
              data = data_standardized)
summary(catcat1)
cat1 <- lm(scaled_target1 ~ 
                (X2023.Turnover + Satisfy_score + avg_score_Q2 + 
                   avg_score_Q3 + avg_score_Q4 + avg_score_Q5 +
                   avg_score_Q6 + MSP_sum + workload) * 
                (cluster + scale), 
              data = data)
summary(cat1)


data_standardized$cluster <- relevel(data_standardized$cluster, ref = "Residential - Local (Wellcome)")
data_standardized$scale <- relevel(data_standardized$scale, ref = "Mass")

trycat1<-lm(scaled_target1 ~ 
              (X2023.Turnover + Satisfy_score_Rank + Manager_score_Rank + 
                 Belonging_score_Rank + MSP_sum + workload) * cluster, data = data_standardized)
summary(trycat1)



##SOP
contcont2 <- lm(scaled_target2 ~ X2023.Turnover + Satisfy_score + 
                  Manager_score + Belonging_score + MSP_sum + workload, data = data_standardized1)
summary(contcont2)
catcat2 <- lm(SOP_sum ~ 
                (X2023.Turnover + Satisfy_score_Rank + Manager_score_Rank + 
                   Belonging_score_Rank + MSP_sum + workload) * 
                (cluster + scale), 
              data = data_standardized)
summary(catcat2)
cat2 <- lm(scaled_target2 ~ 
             (X2023.Turnover + Satisfy_score + avg_score_Q2 + 
                avg_score_Q3 + avg_score_Q4 + avg_score_Q5 +
                avg_score_Q6 + MSP_sum + workload) * 
             (Store.Cluster + Format_y), 
           data = data_standardized)
summary(cat2)
generate_model_table <- function(model, file_path) {
  library(broom)
  library(dplyr)
  library(tidyr)
  # Process coefficients
  coefficients_table <- tidy(model) %>% 
    filter(!is.na(estimate)) %>%
    select(term, estimate, std.error, p.value)
  
  # Process model statistics
  model_stats <- glance(model) %>%
    select(r.squared, adj.r.squared, sigma, statistic, p.value) %>%
    rename(`Residual SE` = sigma,
           `F-statistic` = statistic,
           `F-p.value` = p.value)
  
  # Combine data
  combined_data <- bind_rows(
    coefficients_table,
    model_stats %>% 
      pivot_longer(everything(), names_to = "term", values_to = "estimate") %>%
      mutate(std.error = NA_real_, p.value = NA_real_)
  )
  
  # Create and save table
  output <- knitr::kable(
    combined_data,
    digits = 3,
    format = "markdown",
    col.names = c("Term", "Estimate", "Std.Error", "p-value"),
    caption = "Combined Model Results"
  )
  
  writeLines(as.character(output), file_path)
}

{# library(broom)
# coefficients_table <- tidy(catcat2) %>% filter(!is.na(estimate)) %>%
#   select(term, estimate, std.error, p.value)
# model_stats <- glance(catcat2) %>%
#   select(r.squared, adj.r.squared, sigma, statistic, p.value) %>%
#   rename(`Residual SE` = sigma,`F-statistic` = statistic,`F-p.value` = p.value)
# combined_data <- bind_rows(coefficients_table,model_stats %>%
#     pivot_longer(everything(), names_to = "term", values_to = "estimate") %>%
#     mutate(std.error = NA_real_, p.value = NA_real_))
# output <- knitr::kable(combined_data,digits = 3,format = "markdown",
#   col.names = c("Term", "Estimate", "Std.Error", "p-value"),
#   caption = "Combined Model Results")
# writeLines(as.character(output), "./OLS_productivity_with_moderator.md")
# coefficients_table <- tidy(catcat2) %>%
#   filter(!is.na(estimate)) %>%
#   select(term, estimate, std.error, p.value)
# model_stats <- glance(catcat2) %>%
#   select(r.squared, adj.r.squared, sigma, statistic, p.value) %>%
#   rename(`Residual SE` = sigma,`F-statistic` = statistic,`F-p.value` = p.value)
# combined_data <- bind_rows(coefficients_table,model_stats %>%
#     pivot_longer(everything(), names_to = "term", values_to = "estimate") %>%
#     mutate(std.error = NA_real_, p.value = NA_real_))
# output <- knitr::kable(combined_data,digits = 3,format = "markdown",
#   col.names = c("Term", "Estimate", "Std.Error", "p-value"),
#   caption = "Combined Model Results")
# writeLines(as.character(output), "./OLS_profitability_with_moderator.md")
}
unscale_model <- function(model, dep_var, file_suffix) {
  # Get dependent variable stats
  dep_mean <- mean(tdata[[dep_var]])
  dep_sd <- sd(tdata[[dep_var]])
  
  # Define variables
  scaled_ivs <- c("X2023.Turnover", "MSP_sum", "workload")
  scaled_dvs <- c("productivity_mean", "SOP_mean")
  factor_vars <- c("cluster", "scale")
  
  # Get valid terms
  valid_terms <- rownames(summary(model)$coefficients)
  
  # Unscale coefficients
  unscaled_coefs <- sapply(valid_terms, function(term) {
    if (term == "(Intercept)") {
      coef(model)[term] * dep_sd + dep_mean
    } else {
      components <- unlist(strsplit(term, ":"))
      # Calculate scaling factor for each component
      scaling_factors <- sapply(components, function(c) {
        if(any(sapply(factor_vars, function(fv) startsWith(c, fv)))) {
          1  # Factor variables not scaled
        } else if(c %in% scaled_ivs) {
          scale_params[[c]]$sd  # Scaled IVs
        } else if(c %in% scaled_dvs) {
          dep_sd  # Scaled DVs
        } else {
          1  # Unscaled variables (Satisfy_score, avg_score_Q*)
        }
      })
      coef(model)[term] * (dep_sd / prod(scaling_factors))
    }
  })
  # Unscale standard errors
  unscaled_se <- sapply(valid_terms, function(term) {
    if (term == "(Intercept)") {
      summary(model)$coefficients[term, "Std. Error"] * dep_sd
    } else {
      se <- summary(model)$coefficients[term, "Std. Error"]
      components <- unlist(strsplit(term, ":"))
      scaling_factors <- sapply(components, function(c) {
        if(any(sapply(factor_vars, function(fv) startsWith(c, fv)))) {
          1
        } else if(c %in% scaled_ivs) {
          scale_params[[c]]$sd
        } else if(c %in% scaled_dvs) {
          dep_sd
        } else {
          1
        }
      })
      
      se * (dep_sd / prod(scaling_factors))
    }
  })
  
  # Create results table
  results_table <- data.frame(
    Term = valid_terms,
    Estimate = unscaled_coefs,
    Std.Error = unscaled_se,
    t.value = unscaled_coefs / unscaled_se,
    p.value = 2 * pt(-abs(unscaled_coefs / unscaled_se), df = model$df.residual)
  )
  
  # Save output
  writeLines(
    knitr::kable(results_table, digits = 3),
    paste0("./OLS_", file_suffix, "_unscaled.md")
  )
  
  return(results_table)
}
# Main pipeline
process_model <- function(model, dep_var, file_suffix) {
  # Generate scaled model table
  generate_model_table(model, paste0("./OLS_", file_suffix, "_scaled.md"))
  
  # Generate unscaled results
  unscaled_results <- unscale_model(model, dep_var, file_suffix)
  
  # Return both results
  list(
    scaled = model,
    unscaled = unscaled_results
  )
}
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance/Gallup")
productivity_models <- process_model(catcat1, "productivity_sum", "productivity")
sop_models <- process_model(catcat2, "SOP_sum", "profitability")
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance/Glint")
productivity_models <- process_model(cat1, "productivity_sum", "productivity")
sop_models <- process_model(cat2, "SOP_sum", "profitability")

###Random Forest trying
##SOP
library(randomForest)
library(caret) 
trainIndex <- createDataPartition(data_standardized$SOP_mean, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data_standardized[trainIndex, ]
test_data <- data_standardized[-trainIndex, ]
# Fit the random forest model including the interaction term
# rf_model <- randomForest(target_variable ~ X + M + interaction, data = data)
rf_model <- randomForest(SOP_mean ~ X2023.Turnover + Satisfy_score + Manager_score + 
                               Belonging_score + MSP_sum + workload  + 
                               (X2023.Turnover + Satisfy_score + Manager_score + 
                                  Belonging_score + MSP_sum + workload) * 
                               (cluster + scale),
                             data=data_standardized,
                         mtry = 56,
                         ntree = 65)
print(rf_model)
model_matrix <- model.matrix(SOP_mean ~ X2023.Turnover + Satisfy_score + Manager_score + 
                               Belonging_score + MSP_sum + workload  + 
                               (X2023.Turnover + Satisfy_score + Manager_score + 
                                  Belonging_score + MSP_sum + workload) * 
                               (cluster + scale), data_standardized)
n_predictors <- ncol(model_matrix) - 1  # Subtract 1 for intercept
print(n_predictors)
library(Boruta)
# Create interaction term in the data frame
data_standardized$interaction <- (data_standardized$X2023.Turnover + 
                                    data_standardized$Satisfy_score + 
                                    data_standardized$Manager_score + 
                                    data_standardized$Belonging_score + 
                                    data_standardized$MSP_sum + 
                                    data_standardized$workload) * 
  as.numeric(interaction(data_standardized$cluster, data_standardized$scale))
boruta_res <- Boruta(SOP_mean ~ X2023.Turnover + Satisfy_score + Manager_score + 
                       Belonging_score + MSP_sum + workload  + 
                       interaction, data = data_standardized)
getSelectedAttributes(boruta_res)
#importance(rf_model)
set.seed(123)

# Create parameter grid (exclude mtry=0 and ntree=0)
grid <- expand.grid(
  mtry = 1:62,       # Valid mtry: 1 to 22 predictors
  ntree = seq(5, 100, by = 5)  # Valid ntree: 5,10,...,100
)

# Store results
results <- data.frame()

# Full grid search
for(i in 1:nrow(grid)) {
  model <- randomForest(
    SOP_mean ~ X2023.Turnover + Satisfy_score + Manager_score + 
      Belonging_score + MSP_sum + workload  + 
      (X2023.Turnover + Satisfy_score + Manager_score + 
         Belonging_score + MSP_sum + workload) * 
      (cluster + scale),
    data=data_standardized,
    mtry = grid$mtry[i],
    ntree = grid$ntree[i]
  )
  
  # Calculate R-squared
  predictions <- predict(model, data_standardized)
  R_squared <- 1 - sum((data_standardized$SOP_mean - predictions)^2) / sum((data_standardized$SOP_mean - mean(data_standardized$SOP_mean))^2)
  
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



























































































# library(emmeans)
# effar <- round(mean(data_combined$Satisfy_score) + sd(data_combined$Satisfy_score))
# effr  <- round(mean(data_combined$Satisfy_score))
# effbr <- round(mean(data_combined$Satisfy_score) - sd(data_combined$Satisfy_score))
# mylist <- list(Satisfy_score = c(effbr, effr, effar))
# 
# # Compute simple slopes
# emtrends(contcont1, ~ Satisfy_score, var = "X2023._Turnover", at = mylist)
# 
# mylist <- list(X2023._Turnover = seq(0, 4, by = 0.4),
#                Satisfy_score = c(effbr, effr, effar))
# emmip(contcont1, Satisfy_score ~ X2023._Turnover, at = mylist, CIs = TRUE)
# dat$gender <- relevel(dat$gender, ref = "female")