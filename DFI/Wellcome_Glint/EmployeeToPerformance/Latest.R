Sys.setenv(LANG = "en")
library(readr)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint")
library(dplyr)
library(tidyverse)
library(gridExtra)
library(broom)
library(texreg)
tdata <-read.csv("./score_wellcome_duplicate.csv",header = TRUE)
data<-read.csv("./score_wellcome_nonduplicate.csv",header = TRUE)
variable <-c("X2023.Turnover", "MSP_sum", "workload", 
             "shrinkage.....of.Sales")
dv<-c("productivity_sum","SOP...T.Area")
score_variable1 <-c("Satisfy_score1","Satisfy_score2","Manager_score", "Belonging_score")
score_variable2 <- paste0("avg_score_Q", c(1:12, 15:30))
#scale_params <- list()
tdata <- tdata %>%
  mutate(
    cluster = factor(Store.Cluster),
    scale = factor(Format_y)
  )
tdata$scale   <- relevel(tdata$scale, ref = "Mass")
tdata$cluster   <- relevel(tdata$cluster, ref = "Residential - Local (Wellcome)")
target_string = "SOP...T.Area"
formula <- as.formula(paste(target_string, "~", paste(score_variable2, collapse = " + ")))
formula1 <- update(formula, . ~ . *(scale+cluster))
reg1<-lm(formula1, data=tdata)
summary(reg1)
target_string = "productivity_sum"
formula <- as.formula(paste(target_string, "~", paste(score_variable2, collapse = " + ")))
formula1 <- update(formula, . ~ . *(scale+cluster))
reg2<-lm(formula1, data=data)
#summary(reg2)
target_string = "X2023.Turnover"
formula <- as.formula(paste(target_string, "~", paste(score_variable2, collapse = " + ")))
formula1 <- update(formula, . ~ . *(scale+cluster))
reg3<-lm(formula1, data=tdata)
#summary(reg3)
target_string = "shrinkage.....of.Sales"
formula <- as.formula(paste(target_string, "~", paste(score_variable2, collapse = " + ")))
formula1 <- update(formula, . ~ . *(scale+cluster))
reg4<-lm(formula1, data=tdata)
#summary(reg4)
target_string = "MSP_sum"
formula <- as.formula(paste(target_string, "~", paste(score_variable2, collapse = " + ")))
formula1 <- update(formula, . ~ . *(scale+cluster))
reg5<-lm(formula1, data=tdata)
#summary(reg5)
library(broom)
library(dplyr)
p_to_stars <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else if (p < 0.1) return(".")
  else return("")
}

model_list <- list(reg1 = reg1, reg2 = reg2, reg3 = reg3, reg4 = reg4, reg5 = reg5)

results <- bind_rows(
  lapply(names(model_list), function(name) {
    broom::tidy(model_list[[name]]) %>%
      mutate(model = name) %>%
      select(model, everything())
  })
) %>%
  mutate(stars = sapply(p.value, p_to_stars))
results

# Write to CSV
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance")
write.csv(results, "regression_results.csv", row.names = FALSE)

library(broom)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# -- Significance stars function
p_to_stars <- function(p) {
  if (is.na(p)) "" 
  else if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else if (p < 0.1) "."
  else ""
}

# -- Model list with display names for columns
model_list <- list(
  "reg1(SOP...T.Area)" = reg1,
  "reg2(productivity_sum)" = reg2,
  "reg3(X2023.Turnover)" = reg3,
  "reg4(shrinkage.....of.Sales)" = reg4,
  "reg5(MSP_sum)" = reg5
)

# -- Get total effect for each avg_score_Qx (main + interaction)
get_total_effects <- function(model, model_name) {
  tidy_mod <- tidy(model) %>%
    mutate(stars = sapply(p.value, p_to_stars))
  
  # Find all main avg_score_Qx terms
  main_terms <- tidy_mod %>%
    filter(str_detect(term, "^avg_score_Q\\d+$")) %>%
    pull(term)
  
  res_list <- list()
  
  for (mt in main_terms) {
    main_row <- tidy_mod %>% filter(term == mt)
    if (main_row$stars %in% c("*","**","***")) {
      res_list[[length(res_list)+1]] <- data.frame(
        term = mt,
        total_effect = main_row$estimate,
        stars = main_row$stars,
        model = model_name,
        Sign = ifelse(main_row$estimate > 0, "Positive", "Negative"),
        stringsAsFactors = FALSE
      )
    }
    # all interactions of this main term with cluster/scale
    int_rows <- tidy_mod %>%
      filter(str_detect(term, paste0("^", mt, ":(scale|cluster).*|^(scale|cluster).*:", mt)))
    if (nrow(int_rows) > 0) {
      for (i in 1:nrow(int_rows)) {
        int_row <- int_rows[i,]
        total_effect <- main_row$estimate + int_row$estimate
        # Use main p-value for stars (since joint p-value is not trivial to calculate)
        if (int_row$stars %in% c("*","**","***")) {
          eff_name <- paste0(mt, "+", int_row$term)
          res_list[[length(res_list)+1]] <- data.frame(
            term = eff_name,
            total_effect = total_effect,
            stars = int_row$stars,
            model = model_name,
            Sign = ifelse(total_effect > 0, "Positive", "Negative"),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  results <- bind_rows(res_list)
  # Only keep terms with *
  results <- results %>% filter(stars %in% c("*","**","***"))
  results <- results %>%
    mutate(estimate = round(total_effect, 3)) %>%
    select(Sign, term, estimate, stars, model)
  results
}

# -- Collect all total effects for all models
all_effects <- map_dfr(names(model_list), function(mn) get_total_effects(model_list[[mn]], mn))
all_effects <- all_effects %>%
  mutate(estimate_stars = paste0(estimate, stars))
# -- Wide format: each model as a column
wide_effects <- all_effects %>%
  pivot_wider(
    names_from = model,
    values_from = c(estimate, stars),
    names_glue = "{model}_{.value}"
  ) %>%
  # If you want to reorder columns so each model's estimate is followed by its stars:
  select(Sign, term, sort(tidyselect::peek_vars()))

# -- Split into positive and negative tables
pos_table <- wide_effects %>% filter(Sign == "Positive")
neg_table <- wide_effects %>% filter(Sign == "Negative")


# make_reveal_table <- function(wide_df, model_col, decreasing = TRUE) {
#   # Only terms significant in this model
#   filtered <- wide_df %>%
#     filter(!is.na(.data[[model_col]]))
#   # Extract numeric coefficient
#   filtered <- filtered %>%
#     mutate(.coef = as.numeric(str_extract(.data[[model_col]], "^-?\\d+\\.?\\d*")))
#   # Sort by coefficient
#   filtered <- filtered %>%
#     arrange(if (decreasing) desc(.coef) else .coef)
#   # Remove helper
#   filtered <- filtered %>%
#     select(-.coef)
#   filtered
# }
# pos_table_reveal_3star <- make_reveal_table(pos_table_3star, "reg1(SOP...T.Area)", decreasing = TRUE)
# neg_table_reveal_3star <- make_reveal_table(neg_table_3star, "reg1(SOP...T.Area)", decreasing = FALSE)
# pos_table_reveal
# neg_table_reveal
# -- Output CSVs
write.csv(pos_table, "positive_table.csv", row.names = FALSE)
write.csv(neg_table, "negative_table.csv", row.names = FALSE)

target_string = "productivity_sum"
formula <- as.formula(paste(target_string, "~", paste(variable, collapse = " + ")))
formula1 <- update(formula, . ~ . *(scale+cluster))
oreg1<-lm(formula, data=tdata)
summary(oreg1)
target_string = "SOP...T.Area"
formula <- as.formula(paste(target_string, "~", paste(variable, collapse = " + ")))
formula1 <- update(formula, . ~ . *(scale+cluster))
oreg2<-lm(formula, data=tdata)
summary(oreg2)

final_model_list<-list()
all_effects <- map_dfr(names(model_list), function(mn) get_total_effects(model_list[[mn]], mn))
all_effects <- all_effects %>%
  mutate(estimate_stars = paste0(estimate, stars))



screenreg(model_list, #custom.model.names = c("ALL 1(SOP...Sales)", "ALL 2(SOP...T.Area)",
                       #                  "GL 1(SOP...Sales)", "GL 2(SOP...T.Area)",
                        #                 "GA 1(SOP...Sales)", "GA 2(SOP...T.Area)"),
          file = "./models_summary.txt")

model<-list(oreg1,oreg2)
screenreg(model, custom.model.names = c("Productivity_sum", "SOP...T.Area)"),
          file = "./ETP_models_summary1.txt")


{# # Calculate and store scaling parameters
# for (var in c(variable, score_variable, variables, "productivity_sum","SOP...T.Area", "SOP...Sales")) {
#   scale_params[[var]] <- list(
#     mean = mean(tdata[[var]], na.rm = TRUE),
#     sd = sd(tdata[[var]], na.rm = TRUE)
#   )
# }
# data_standardized1 <- tdata %>%
#   mutate(
#     across(c(variable, score_variable, variables, productivity_sum,SOP...T.Area,SOP...Sales), 
#            ~ (. - scale_params[[cur_column()]]$mean) / scale_params[[cur_column()]]$sd)
#   ) %>%
#   mutate(
#     scaled_target1 = productivity_sum,
#     scaled_target2B = SOP...Sales,
#     scaled_target2A = SOP...T.Area,
#     cluster = factor(Store.Cluster),
#     scale = factor(Format_y)
#   )
}  
# urban ="Urban Hub - Local (Wellcome)", "Urban Hub - High Income (Upscale)"
# residential = "Residential - Western & Top Income (Upscale)","Residential - Local (Wellcome)", "Residential - High Income (Wellcome)"
# office = "Office - Local (Wellcome)", "Office - High Income (Upscale)"
urban<-data_standardized1 %>% filter(cluster =="Urban Hub - Local (Wellcome)"|
                                     cluster =="Urban Hub - High Income (Upscale)")
residential<-data_standardized1 %>% filter(cluster =="Residential - Western & Top Income (Upscale)"|
                                           cluster =="Residential - Local (Wellcome)"|
                                           cluster =="Residential - High Income (Wellcome)")
office<-data_standardized1 %>% filter(cluster =="Office - Local (Wellcome)"|
                                      cluster ==  "Office - High Income (Upscale)")
mass<-data_standardized1 %>% filter(scale =="Mass")
upper<-data_standardized1 %>% filter(scale =="Upscales")
#Note 1: All store in urban are Mass
table(urban$scale)
ucont1 <- lm(productivity_sum ~ X2023.Turnover  + Satisfy_score1 + 
               Manager_score   + Belonging_score   +
               avg_score_Q4 + avg_score_Q25 +
               MSP_sum  + workload,
                 data = urban)
#summary(ucont1)

ucont3 <- lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score1 + 
              Manager_score + Belonging_score  +
              avg_score_Q4 + avg_score_Q25 +
              MSP_sum  + workload, 
            data = urban)
ucat1<-lm(productivity_sum ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload,
          data = urban)
ucat2<-lm(SOP...Sales ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload,
          data = urban)
ucat3<-lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload,
          data = urban)


#summary(ucont3)
#summary(ucont3)
table(residential$scale)
residential$scale <- as.factor(residential$scale)
rcont1 <- lm(productivity_sum ~ X2023.Turnover+ Satisfy_score1   +
               Manager_score   + Belonging_score   +
               MSP_sum  + workload,
             data = residential)
#summary(rcont1)

rcont3 <- lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score1 + 
              Manager_score + Belonging_score  +
              avg_score_Q4 + avg_score_Q25 +
              MSP_sum  + workload, 
            data = residential)
rcat1<-lm(productivity_sum ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload,
          data = residential)
rcat3<-lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload,
          data = residential)
#summary(rcont3)
table(office$scale)
ocont1 <- lm(productivity_sum ~ X2023.Turnover+ Satisfy_score1 +
               Manager_score   + Belonging_score   +
               avg_score_Q4 + avg_score_Q25 +
               MSP_sum  + workload,
             data = office)
#summary(ocont1)

ocont3 <- lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score1 + 
              Manager_score + Belonging_score  +
               avg_score_Q4 + avg_score_Q25 +
              MSP_sum  + workload, 
            data = office)
ocat1<-lm(productivity_sum ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload,
          data = office)
ocat3<-lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload,
          data = office)
#summary(ocont3)
cont1 <- lm(productivity_sum ~ X2023.Turnover  + Satisfy_score1 + 
              Manager_score + Belonging_score  +
              avg_score_Q4 + avg_score_Q25 +
              MSP_sum  + workload+ shrinkage.....of.Sales, 
               data = data_standardized1)
cont2<-lm(SOP...Sales ~ X2023.Turnover  + Satisfy_score1 + 
            Manager_score + Belonging_score  +
            avg_score_Q4 + avg_score_Q25 +
            MSP_sum  + workload+ shrinkage.....of.Sales, 
          data = data_standardized1)
cont3 <- lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score1 + 
              Manager_score + Belonging_score  +
              avg_score_Q4 + avg_score_Q25 +
              MSP_sum  + workload+ shrinkage.....of.Sales, 
            data = data_standardized1)
cat1<-lm(productivity_sum ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload + shrinkage.....of.Sales,
          data = data_standardized1)
cat2<-lm(SOP...Sales ~ X2023.Turnover  + Satisfy_score2 + 
           avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
           avg_score_Q25 +
           MSP_sum  + workload + shrinkage.....of.Sales,
         data = data_standardized1)
cat3<-lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score2 + 
            avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
            avg_score_Q25 +
            MSP_sum  + workload + shrinkage.....of.Sales,
          data = data_standardized1)
predictor_vars <- c(variables,variable)
formula <- as.formula(paste("productivity_sum", "~", paste(predictor_vars, collapse = " + ")))
reg1<-lm(formula, data = data_standardized1)
formula <- as.formula(paste("SOP...Sales", "~", paste(predictor_vars, collapse = " + ")))
reg2<-lm(formula, data = data_standardized1)
formula <- as.formula(paste("SOP...T.Area", "~", paste(predictor_vars, collapse = " + ")))
reg3<-lm(formula, data = data_standardized1)
reg3M<-lm(formula, data = mass)
reg3S<-lm(formula, data = upper)
reg3U<-lm(formula, data = urban)
reg3R<-lm(formula, data = residential)
reg3o<-lm(formula, data = office)
upper2<-upper %>% filter(cluster !=  "Office - High Income (Upscale)")
reg3S2<-lm(formula, data = upper2)
summary(reg3S2)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance")
sink("./ANOVA_productivity.txt")
print(anova(cont1,cat1, reg1, test = "F"))
sink()
sink("./ANOVA_SOP_T.Area.txt")
print(anova(cont3, cat3,reg3, test = "F"))
sink()
sink("./ANOVA_SOP_Sales.txt")
print(anova(cont2, cat2, reg2, test = "F"))
sink()
library(gridExtra)
library(broom)
library(texreg)
models <- list(reg1, cont1, cat1)
screenreg(models, custom.model.names = c("OLS 1(All question)", "OLS 2(Gallup)", "OLS 3(Glint)"),
          file = "./models_summary_productivity_sum.txt")
models <- list(reg2, reg3, cont3,cat3, cont2, cat2)
screenreg(models, custom.model.names = c("ALL 1(SOP...Sales)", "ALL 2(SOP...T.Area)", 
                                         "GL 1(SOP...Sales)", "GL 2(SOP...T.Area)",
                                         "GA 1(SOP...Sales)", "GA 2(SOP...T.Area)"),
          file = "./models_summary_SOP.txt")
#summary(cont3)
model3 <- list(reg3, reg3M, reg3S, reg3U, reg3R, reg3o)
screenreg(model3, custom.model.names = c("OLS 1(Overall", "OLS 2(Mass)", "OLS 3(Upperscale)",
                                         "OLS 4(Urban)", "OLS 5(Residential)", 
                                         "OLS 6(Office)"),
          file = "./models_summary_SOPinTArea.txt")
unscale_model <- function(model, dep_var, file_suffix, data, scale_params, scaled_ivs, factor_vars = c("cluster", "scale")) {
  # Get stats for dependent variable
  dep_mean <- mean(data[[dep_var]], na.rm = TRUE)
  dep_sd <- sd(data[[dep_var]], na.rm = TRUE)
  
  # Get coefficient names/terms
  valid_terms <- rownames(summary(model)$coefficients)
  coefs_std <- coef(model)
  ses_std <- summary(model)$coefficients[, "Std. Error"]
  
  # Function to get scaling factor for a term/component
  get_scaling_factor <- function(term) {
    components <- unlist(strsplit(term, ":"))
    sapply(components, function(c) {
      # Remove backticks if present
      c <- gsub("`", "", c)
      # Factors not scaled
      if(any(startsWith(c, factor_vars))) {
        1
      } else if(c %in% scaled_ivs) {
        scale_params[[c]]$sd
      } else {
        1 # Not scaled
      }
    })
  }
  
  # Unscale coefficients (except intercept)
  unscaled_coefs <- numeric(length(valid_terms))
  unscaled_se <- numeric(length(valid_terms))
  
  for(i in seq_along(valid_terms)) {
    term <- valid_terms[i]
    if(term == "(Intercept)") {
      # Special case for intercept
      # Reconstruct unscaled intercept:
      # b0_unscaled = mean_y - sum(bi_std * mean_xi * (sd_y/sd_xi))
      sum_adj <- 0
      for(j in seq_along(valid_terms)) {
        t2 <- valid_terms[j]
        if(t2 != "(Intercept)") {
          components <- unlist(strsplit(t2, ":"))
          # Only main effects (not interactions) count for mean adjustment
          if(length(components) == 1) {
            c <- components[1]
            if(c %in% scaled_ivs) {
              sum_adj <- sum_adj + coefs_std[t2] * mean(data[[c]], na.rm=TRUE) * (dep_sd / scale_params[[c]]$sd)
            }
          }
        }
      }
      unscaled_coefs[i] <- dep_mean - sum_adj
      # Standard Error for intercept: not directly back-transformable, so NA
      unscaled_se[i] <- NA
    } else {
      scaling_factors <- get_scaling_factor(term)
      scaling_product <- prod(scaling_factors)
      unscaled_coefs[i] <- coefs_std[term] * (dep_sd / scaling_product)
      unscaled_se[i] <- ses_std[term] * (dep_sd / scaling_product)
    }
  }
  
  # Calculate t and p values (skip intercept SE)
  t_value <- unscaled_coefs / unscaled_se
  t_value[is.na(unscaled_se)] <- NA
  p_value <- 2 * pt(-abs(t_value), df = model$df.residual)
  
  # Assemble results table
  results_table <- data.frame(
    Term = valid_terms,
    Estimate = unscaled_coefs,
    Std.Error = unscaled_se,
    t.value = t_value,
    p.value = p_value
  )
  
  # Save output
  writeLines(
    knitr::kable(results_table, digits = 3),
    paste0("./OLS_", file_suffix, "_unscaled.md")
  )
  
  return(results_table)
}

scaled_ivs <- c(variable, score_variable, variables, "productivity_sum", "SOP...T.Area", "SOP...Sales")
# Remove duplicates:
scaled_ivs <- unique(scaled_ivs)

unscaled_results <- unscale_model(
  reg3, 
  dep_var = "SOP...T.Area", 
  file_suffix = "SOP...T.Area", 
  data = tdata, 
  scale_params = scale_params, 
  scaled_ivs = scaled_ivs
)
unscaled_results




library(randomForest)
library(caret)
library(pdp)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(knitr)
set.seed(123)
for (var in c(variable, score_variable, variables, "productivity_sum","SOP...T.Area", "SOP...Sales")) {
  scale_params[[var]] <- list(
    mean = mean(data[[var]], na.rm = TRUE),
    sd = sd(data[[var]], na.rm = TRUE)
  )
}
data_standardized <- data %>%
  mutate(
    across(c(variable, score_variable, variables, productivity_sum,SOP...T.Area,SOP...Sales), 
           ~ (. - scale_params[[cur_column()]]$mean) / scale_params[[cur_column()]]$sd)
  ) %>%
  mutate(
    scaled_target1 = productivity_sum,
    scaled_target2B = SOP...Sales,
    scaled_target2A = SOP...T.Area,
    cluster = factor(Store.Cluster),
    scale = factor(Format_y)
  )
target_string = "SOP...Sales"
formula <- as.formula(paste(target_string, "~", paste(predictor_vars, collapse = " + ")))
trainIndex <- createDataPartition(data_standardized[[target_string]], p = 0.7, list = FALSE)
train_data <- data_standardized[trainIndex, ]
test_data <- data_standardized[-trainIndex, ]
rf_model <- randomForest(
  formula,
  data = train_data,
  ntree = round(nrow(train_data)/6,-1),
  mtry = max(floor(sqrt(length(variables)/3)), 1),  # Dynamic mtry
  importance = TRUE
)
print(rf_model)
generate_pd_plots <- function(model, vars, train_data, number_cols) {
  plot_list <- lapply(vars, function(var) {
    pd <- partial(model, pred.var = var, train = train_data)
    means <- sapply(variables, function(var) mean(tdata[[var]], na.rm = TRUE))
    # Plotting
    ggplot(pd, aes(x = .data[[var]], y = yhat)) + 
      geom_line(color = "steelblue", linewidth = 1) +
      scale_x_continuous(limits = c(50, 100)) +
      scale_y_continuous(limits = c(75,78)) +
      geom_vline(xintercept = means[var], color = "blue", linetype = "dashed", size = 1) +
      labs(title = vars, x = "", y = "Partial Dependence") +
      theme_bw() +
      theme(plot.title = element_text(size = 10))
  })
  
  # Filter out NULL plots
  plot_list <- Filter(Negate(is.null), plot_list)
  
  grid.arrange(grobs = plot_list, ncol = number_cols)
}


generate_pd_plots <- function(model, vars, train_data, number_cols = 3) {
  plot_list <- lapply(vars, function(var) {
    # Try to compute PDP, skip and return NULL if error
    pd <- tryCatch(
      partial(model, pred.var = var, train = train_data, plot = FALSE),
      error = function(e) {message(sprintf("Skipping %s: %s", var, e$message)); NULL}
    )
    if (is.null(pd)) return(NULL)
    # Plot
    ggplot(pd, aes_string(x = var, y = "yhat")) +
      geom_line(color = "steelblue", linewidth = 1) +
      # scale_x_continuous(limits = c(50, 100)) +
      # scale_y_continuous(limits = c(0.07,0.095)) +
      labs(title = var, x = var, y = "Partial Dependence") +
      theme_bw() +
      theme(plot.title = element_text(size = 10))
  })
  # Remove NULLs (failed plots)
  plot_list <- Filter(Negate(is.null), plot_list)
  # Arrange in a grid
  do.call(grid.arrange, c(plot_list, ncol = number_cols))
}


# Generate and save plots
generate_pd_plots(rf_model, predictor_vars, train_data, number_cols = 4)

pd <- partial(rf_model, pred.var = var, train = train_data, plot = FALSE)
ggplot(pd, aes_string(x = var, y = "yhat")) +
  geom_line() +
  labs(title = var, x = paste0(var, " (original scale)"), y = "Partial Dependence")
# Back-transform x-axis
mean_val <- scale_params[[var]]$mean
sd_val <- scale_params[[var]]$sd
pd[[var]] <- pd[[var]] * sd_val + mean_val

back_generate_pd_plots <- function(model, vars, train_data, number_cols = 3) {
  plot_list <- lapply(vars, function(var) {
    pd <- tryCatch(
      partial(model, pred.var = var, train = train_data, plot = FALSE),
      error = function(e) {message(sprintf("Skipping %s: %s", var, e$message)); NULL}
    )
    if (is.null(pd)) return(NULL)
    # Back-transform
    mean_val <- scale_params[[var]]$mean
    sd_val <- scale_params[[var]]$sd
    pd[[var]] <- pd[[var]] * sd_val + mean_val
    # Plot
    ggplot(pd, aes_string(x = var, y = "yhat")) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(title = var, x = paste0(var, " (original scale)"), y = "Partial Dependence") +
      theme_bw() +
      theme(plot.title = element_text(size = 10))
  })
  plot_list <- Filter(Negate(is.null), plot_list)
  do.call(grid.arrange, c(plot_list, ncol = number_cols))
}
back_generate_pd_plots(rf_model, predictor_vars, train_data, number_cols = 4)
mcont1<-lm(productivity_sum ~ X2023.Turnover  + Satisfy_score1 + 
             Manager_score + Belonging_score  +
             avg_score_Q4 + avg_score_Q25 +
             MSP_sum  + workload,
           data =mass)
#summary(mcont1)

mcont3 <- lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score1 + 
              Manager_score + Belonging_score  +
               avg_score_Q4 + avg_score_Q25 +
              MSP_sum  + workload, 
            data = mass)
mcat1<-lm(productivity_sum ~ X2023.Turnover  + Satisfy_score2 + 
           avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
           avg_score_Q25 +
           MSP_sum  + workload,
         data = mass)
mcat3<-lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score2 + 
           avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
           avg_score_Q25 +
           MSP_sum  + workload,
         data = mass)
#summary(mcont3)
ucon1<-lm(productivity_sum ~ X2023.Turnover  + Satisfy_score1 + 
             Manager_score + Belonging_score  +
             avg_score_Q4 + avg_score_Q25 +
             MSP_sum  + workload,
           data =upper)
ucon3 <- lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score1 + 
               Manager_score + Belonging_score  +
               avg_score_Q4 + avg_score_Q25 +
               MSP_sum  + workload, 
             data = upper)
#summary(ucont3)
uca1<-lm(productivity_sum ~ X2023.Turnover  + Satisfy_score2 + 
           avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
           avg_score_Q25 +
           MSP_sum  + workload,
         data = upper)
uca3<-lm(SOP...T.Area ~ X2023.Turnover  + Satisfy_score2 + 
           avg_score_Q2   + avg_score_Q4 + avg_score_Q5 + avg_score_Q6 +
           avg_score_Q25 +
           MSP_sum  + workload,
         data = upper)

library(gridExtra)
library(broom)
library(texreg)
#setwd("C:/Users/tiip/Downloads/Wellcome_Glint/ManagerToEmployee")
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance")
#Gallup
models <- list(cont1, ucont1, rcont1, ocont1, mcont1, ucon1)
screenreg(models, custom.model.names = c("OLS 1(Overall", "OLS 2(Urban)", "OLS 3(Residential)",
                                         "OLS 4(Office)", "OLS 5(Mass)", 
                                         "OLS 6(Upperscale)"),
          file = "./models_summary_productivity_sum.txt")

model2 <- list(cont3, ucont3, rcont3, ocont3, mcont3, ucon3)
screenreg(model2, custom.model.names = c("OLS 1(Overall", "OLS 2(Urban)", "OLS 3(Residential)",
                                         "OLS 4(Office)", "OLS 5(Mass)", 
                                         "OLS 6(Upperscale)"),
          file = "./models_summary_SOP.txt")
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance/Glint")
#Glint
model <- list(cat1, ucat1, rcat1, ocat1, mcat1, uca1)
screenreg(model, custom.model.names = c("OLS 1(Overall", "OLS 2(Urban)", "OLS 3(Residential)",
                                         "OLS 4(Office)", "OLS 5(Mass)", 
                                         "OLS 6(Upperscale)"),
          file = "./models_summary_productivity_sum.txt")

model3 <- list(cat3, ucat3, rcat3, ocat3, mcat3, uca3)
screenreg(model3, custom.model.names = c("OLS 1(Overall", "OLS 2(Urban)", "OLS 3(Residential)",
                                         "OLS 4(Office)", "OLS 5(Mass)", 
                                         "OLS 6(Upperscale)"),
          file = "./models_summary_SOP.txt")


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
unscale_model <- function(model, dep_var, file_suffix) {
  # Get dependent variable stats
  dep_mean <- mean(tdata[[dep_var]])
  dep_sd <- sd(tdata[[dep_var]])
  
  # Define variables
  scaled_ivs <- c("X2023.Turnover", "MSP_sum", "workload")
  scaled_dvs <- c("productivity_sum", "SOP...T.Area","")
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
productivity_models <- process_model(cont1, "productivity_sum", "productivity")
sop_tarea_models<-process_model(cont3, "SOP...T.Area", "profitability in T.Area")

setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance/Glint")
productivity_models <- process_model(cat1, "productivity_sum", "productivity")
sop_models <- process_model(cat3, "SOP...T.Area", "profitability")






library(mgcv)
gam_formula <- as.formula(paste("SOP...T.Area", "~", paste(paste0("s(", variable1, ")", collapse = " + "))))
gam<-gam(SOP...T.Area ~ Satisfy_score + Manager_score + Belonging_score + 
           X2023.Turnover + MSP_sum + workload,  
         data = data_standardized1,
         family = gaussian(link = 'log'))
summary(gam)
gam1 <- gam(
  gam_formula,
  data = data_standardized1,
  family = gaussian(link = 'log')
)
summary(gam1)

gam<-gam(SOP...T.Area ~ Satisfy_score + Manager_score + Belonging_score + 
           X2023.Turnover + MSP_sum + workload,  
         data = data_standardized1,
         family = gaussian(link = 'log'))

















library(caret)
# Define the control using 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)
# Train the model using cross-validation
rf_cv_model <- train(productivity_sum ~ X2023.Turnover + Satisfy_score + 
                       Manager_score + Belonging_score + MSP_sum + workload, 
                     data = data_standardized1,
                     method = "rf",
                     trControl = train_control,
                     ntree = 60)

print(rf_cv_model)
library(randomForest)
trainIndex <- createDataPartition(data_standardized1$productivity_sum, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data_standardized1[trainIndex, ]
test_data <- data_standardized1[-trainIndex, ]
rf_model <- randomForest(productivity_sum ~ X2023.Turnover + Satisfy_score + 
                           Manager_score + Belonging_score + MSP_sum + workload, 
                         data = data_standardized1,ntree=100)
print(rf_model)
plot(rf_model, main = "OOB Error Rate vs. Number of Trees")
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
  X2023.Turnover = mean(tdata$X2023.Turnover, na.rm = TRUE),
  MSP_sum = mean(tdata$MSP_sum, na.rm = TRUE),
  workload = mean(tdata$workload, na.rm = TRUE),
  Satisfy_score  = mean(tdata$Satisfy_score , na.rm = TRUE),
  Manager_score  = mean(tdata$Manager_score , na.rm = TRUE),
  Belonging_score  = mean(tdata$Belonging_score , na.rm = TRUE)
)
  
  sds <- c(
    X2023.Turnover = sd(tdata$X2023.Turnover, na.rm = TRUE),
    MSP_sum = sd(tdata$MSP_sum, na.rm = TRUE),
    workload = sd(tdata$workload, na.rm = TRUE),
    Satisfy_score  = sd(tdata$Satisfy_score , na.rm = TRUE),
    Manager_score  = sd(tdata$Manager_score , na.rm = TRUE),
    Belonging_score  = sd(tdata$Belonging_score , na.rm = TRUE)
  )}
dv_mean <- mean(tdata$productivity_sum, na.rm = TRUE)
dv_sd <- sd(tdata$productivity_sum, na.rm = TRUE)
# Generate plots
#variables <- c("Satisfy_score", "Manager_score", "Belonging_score", "X2023.Turnover", "MSP_sum", "workload")
combined_plot_rf2<- generate_pd_plots(model = rf_model, variables = variables, means = means, sds = sds, dv_mean = dv_mean, dv_sd = dv_sd)