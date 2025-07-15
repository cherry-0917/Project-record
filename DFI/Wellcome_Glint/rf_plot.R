Sys.setenv(LANG = "en")
rm(list=ls())
library(readr)
library(dplyr)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint")
data = read.csv("./score_wellcome_nonduplicate.csv")
dat = read.csv("./score_overall.csv")
newdata = read.csv("./score_wellcome_duplicate.csv")
#library(dplyr)
library(randomForest)
library(caret)
library(pdp)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(knitr)

#setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Glint/avg_score_Q6")

target_string = "productivity_sum"
predictor_vars = paste0("avg_score_Q", c(1:12, 15:30))
#predictor_vars = paste0("avg_score_Q", c(10,17,18,22,24,29))
formula <- as.formula(paste(target_string, "~", paste(predictor_vars, collapse = " + ")))
mass<-tdata %>% filter(scale =="Mass")
upper<-tdata %>% filter(scale =="Upscales")
set.seed(123)
trainIndex <- createDataPartition(upper[[target_string]], p = 0.7, list = FALSE)
train_data <-upper[trainIndex, ]
test_data <- upper[-trainIndex, ]
rf_model <- randomForest(
  formula,
  data = train_data,
  ntree = round(nrow(train_data)/6,-1),
  mtry = max(floor(sqrt(length(predictor_vars)/3)), 1),  # Dynamic mtry
  importance = TRUE
)
generate_pd_plots <- function(model, vars, train_data, number_cols, var_labels) {
  plot_list <- lapply(vars, function(var) {
    pd <- partial(model, pred.var = var, train = train_data)
    means <- sapply(score_variable2, function(var) mean(train_data[[var]], na.rm = TRUE))
    # Plotting
    ggplot(pd, aes(x = .data[[var]], y = yhat)) + 
      geom_line(color = "steelblue", linewidth = 1) +
      #scale_x_continuous(limits = c(50, 100)) +
      #scale_y_continuous(limits = c(0,67)) +
      geom_vline(xintercept = means[var], color = "blue", linetype = "dashed", size = 1) +
      labs(title = var_labels[var], x = "", y = "Partial Dependence") +
      theme_bw() +
      theme(plot.title = element_text(size = 10))
  })
  
  # Filter out NULL plots
  plot_list <- Filter(Negate(is.null), plot_list)
  
  grid.arrange(grobs = plot_list, ncol = number_cols)
}

pd_plot <- generate_pd_plots(rf_model, predictor_vars, train_data, 5, name_mapping)
print(rf_model)
rf_summary <- capture.output(
  cat("Random Forest Model Summary\n"),
  # cat("Number of trees:", rf_model$ntree, "\n"),
  # cat("Variables per split:", rf_model$mtry, "\n"),
  print(rf_model),
  cat("\nVariable Importance:\n"),
  print(importance(rf_model))
)
writeLines(rf_summary, "rf_summary.txt")
{# rf_cv <- train(
#   formula,
#   data = train_data,
#   method = "rf",
#   trControl = trainControl(method = "cv", number = 10),
#   ntree = 40,
#   tuneGrid = data.frame(mtry = max(floor(round(sqrt(length(variables)/3))), 1))
# )
# # Overfitting Check 
# train_pred <- predict(rf_model, newdata = train_data)
# train_perf <- postResample(train_pred, train_data[[target_string]])
# 
# test_pred <- predict(rf_model, newdata = test_data)
# test_perf <- postResample(test_pred, test_data[[target_string]])
# 
# # Calculate performance differences
# rmse_diff <- test_perf["RMSE"] - train_perf["RMSE"]
# rsq_diff <- train_perf["Rsquared"] - test_perf["Rsquared"]
# mae_diff <- test_perf["MAE"] - train_perf["MAE"]
# 
# # Determine if overfitting is significant
# is_overfit <- ifelse(
#   (rmse_diff > 0.5 * train_perf["RMSE"]) |  # Test RMSE > 150% of train RMSE
#     (rsq_diff > 0.15) |                       # R² drops more than 0.15
#     (test_perf["Rsquared"] < 0.5) |           # Test R² unacceptably low
#     (mae_diff > 0.5 * train_perf["MAE"]),     # Test MAE > 150% of train MAE
#   "Yes", 
#   "No"
# )
# overfit_check <- data.frame(
#   Dataset = c("Training", "Test"),
#   RMSE = c(train_perf["RMSE"], test_perf["RMSE"]),
#   Rsquared = c(train_perf["Rsquared"], test_perf["Rsquared"]),
#   MAE = c(train_perf["MAE"], test_perf["MAE"]),  # Added MAE column
#   Overfitting = c(NA, is_overfit)  # Only show for test set
# )
# overfit_check
}
means<- sapply(variables, function(var) mean(dat[[var]], na.rm = TRUE))
dv_mean <- 74 #mean(train_data[[target_string]], na.rm = TRUE)
new_names <- c(
  "1 Team Satisfaction" = "avg_score_Q1",
  "2 Work Satisfaction" = "avg_score_Q2",
  "3 Company Satisfaction" = "avg_score_Q3",
  "4 Future Success" = "avg_score_Q4",
  "5 Belonging" = "avg_score_Q5", 
  "6 Retention" = "avg_score_Q6",
  "7 Recommend my Manager" = "avg_score_Q7",
  "8 Action after Feedback" = "avg_score_Q8",
  "9 Asked Opinion" = "avg_score_Q9",
  "10 Training for Confidence" = "avg_score_Q10",
  "11 Career Opportunities" = "avg_score_Q11",
  "12 Recommend our Stores" = "avg_score_Q12",
  "15 Trust each other" = "avg_score_Q15",
  "16 Trust Leadership" = "avg_score_Q16",
  "17 Provide Feedback for Improvement" = "avg_score_Q17",
  "18 Satisfied with Recognition" = "avg_score_Q18",
  "19 Great Customers Performance" = "avg_score_Q19",
  "20 Proud of Company" = "avg_score_Q20",
  "21 Consider Customer's Needs" = "avg_score_Q21",
  "22 Empowered to Make Decisions" = "avg_score_Q22",
  "23 Meaningful Work" = "avg_score_Q23",
  "24 Given Enough Resources" = "avg_score_Q24",
  "25 Live Company Values" = "avg_score_Q25",
  "26 Being Cared" = "avg_score_Q26",
  "27 Equal Opportunity for Success" = "avg_score_Q27",
  "28 Free to Speak my Mind" = "avg_score_Q28",
  "29 Given Technology Resources" = "avg_score_Q29",
  "30 Supporting Communities" = "avg_score_Q30"
)
name_mapping <- setNames(names(new_names), unname(new_names))
new_scores <- c(
  avg_score_Q8 = 90,
  avg_score_Q11 = 83,
  #avg_score_Q12 = 85,
  avg_score_Q18 = 90,
  avg_score_Q19 = 85,
  avg_score_Q20 = 92,
  avg_score_Q23 = 91,
  avg_score_Q26 = 90,
  avg_score_Q27 = 90,
  avg_score_Q29 = 95,
  avg_score_Q30 = 90
)
new_data <- as.data.frame(t(means))
new_data[names(new_scores)] <- new_scores
test_row <- means
test_row[names(new_scores)] <- new_scores  # set selected vars to their max (from new_scores)
max_pred <- as.numeric(predict(rf_model, as.data.frame(t(test_row))))
print(max_pred)
# Predict the score using the trained model
predicted_score <- predict(rf_model, new_data)
increase_from_mean <- new_scores - means[names(new_scores)]
increase <- predicted_score  - dv_mean
combine_df <- data.frame(
  Selected_variables = name_mapping[names(new_scores)],
  Increase_between_mean = increase_from_mean,
  Score = new_scores,
  Predicted_score = predicted_score,
  Increase_in_total = increase
)

print(combine_df)

library(gtools) # for combinations
find_best_combo_grid <- function(target_score, rf_model, means, max_scores, selected_vars, step=2) {
  nvars <- length(selected_vars)
  for (k in 1:nvars) {
    combos <- gtools::combinations(n=nvars, r=k, v=selected_vars)
    for (i in 1:nrow(combos)) {
      vars <- combos[i,]
      grids <- lapply(vars, function(v) unique(c(seq(means[v], max_scores[v], by=step), max_scores[v])))
      names(grids) <- vars
      grid_mat <- expand.grid(grids)
      for (j in 1:nrow(grid_mat)) {
        test_row <- means
        test_row[vars] <- as.numeric(grid_mat[j,])
        pred <- as.numeric(predict(rf_model, as.data.frame(t(test_row))))
        if (pred >= target_score) {
          return(list(
            vars = vars,
            values = as.numeric(grid_mat[j,]),
            increases = as.numeric(grid_mat[j,]) - means[vars],
            pred = pred
          ))
        }
      }
    }
    gc()
  }
  return(NULL)
}
targets <- c(75, 76, 77,78,79,80)
results_list <- list()
for (target in targets) {
  res <- find_best_combo_grid(target, rf_model, means, new_scores, names(new_scores), step=1)
  if (!is.null(res)) {
    df <- data.frame(
      Target = target,
      Variable = res$vars,
      Score = res$values,
      Increase = res$increases,
      Predicted_score = res$pred
    )
    results_list[[as.character(target)]] <- df
  } else {
    cat("Target", target, "not reachable with selected variables\n")
  }
}
results_df <- do.call(rbind, results_list)
print(results_df)


setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Overall")
output = knitr::kable(results_df, caption = "Relative increase - Engagement", format="markdown", row.names = FALSE)
# Add explanatory comments below the table
comments <- c(
  "",
  "### Notes:",
  "- Assumption: It assumes that other variables unchanged.",
  "- Increase How much the new score exceeds the variable's mean",
  "- Score: New score needed",
  "- Predicted_score: Overall target score predicted by the model",
  "",
  ""
)
# Combine table and comments
output <- c(output, comments)
path = "./relative_total_increase(Engagement).md"
writeLines(output, path)
write.csv(results_df, file = "relative_total_increase(Engagement).csv", row.names = FALSE)

lines <- readLines("./relative_total_increase(Engagement).md")

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
  "Leadership Team Demonstrates Commitment" = "avg_score_Q33",
  "Excited about Company's Future" = "avg_score_Q34",
  "SSC Adapt Changes" = "avg_score_Q35",
  "SSC deliver commitments" = "avg_score_Q36",
  "Clear other's Responsiblity" = "avg_score_Q37",
  "Being held accountable for work" = "avg_score_Q38"
)
name_mapping <- setNames(names(new_names), unname(new_names))

df$`Related_name` <- name_mapping[df$Variable]

# Reorder columns: put "New Question No." after "Question"
nms <- names(df)
pos <- which(nms == "Variable")
df <- df[, c(nms[1:pos], "Related_name", nms[(pos+2):(length(nms)-1)])]
print(df)
df <- df[-1, ]
print(df)
output = knitr::kable(df, caption = "Relative increase - Engagement", format="markdown", row.names = FALSE)
# Add explanatory comments below the table
comments <- c(
  "",
  "### Notes:",
  "- Assumption: It assumes that other variables unchanged.",
  "- Increase How much the new score exceeds the variable's mean",
  "- Score: New score needed",
  "- Predicted_score: Overall target score predicted by the model",
  "",
  ""
)
# Combine table and comments
output <- c(output, comments)
path = "./relative_total_increase(Engagement).md"
writeLines(output, path)










generate_pd_plots <- function(model, vars, train_data, number_cols, var_labels) {
  plot_list <- lapply(vars, function(var) {
    pd <- partial(model, pred.var = var, train = train_data)
    means <- sapply(vars, function(var) mean(train_data[[var]]))
    
    # Focus on a specific range based on visual inspection
    focus_range <- subset(train_data, train_data[[var]] >= 20 & train_data[[var]] <= 85)
    
    window_size <-   # Adjusted window size for sharper slope detection
    max_slope <- -Inf
    x_start <- NA
    x_end <- NA
    
    for (i in 1:(nrow(focus_range) - window_size + 1)) {
      segment <- focus_range[i:(i + window_size - 1), ]
      formula_str <- as.formula(paste(target_string, "~", var))
      model_segment <- lm(formula_str, data = segment)
      slope <- abs(coef(model_segment)[2])
      if (slope > max_slope) {
        max_slope <- slope
        x_start <- segment[[var]][1]
        x_end <- segment[[var]][window_size]
      }
    }
    
    annotation_text <- paste("Max. Slope range:", x_start, "-", x_end)
    plot_title <- var_labels[var]
    
    ggplot(pd, aes(x = .data[[var]], y = yhat)) +
      geom_line(color = "steelblue", linewidth = 1) +
      scale_x_continuous(limits=c(50,100)) +
      scale_y_continuous(limits=c(65,68)) +
      geom_vline(xintercept = means[var], color = "blue", linetype = "dashed", size=1) +
      geom_rect(aes(xmin = x_start, xmax = x_end, ymin = -Inf, ymax = Inf),
                fill = "grey", alpha = 0.05) +
      labs(title = plot_title, x = "", y = "Partial Dependence") +
      theme_bw() +
      theme(plot.title = element_text(size = 10))
  })
  grid.arrange(grobs = plot_list, ncol = number_cols)
}

# Generate and save plots
pd_plot <- generate_pd_plots(rf_model, predictor_vars, train_data, 3, new_names)

means <- sapply(variables, function(var) mean(newdata[[var]], na.rm = TRUE))
mean<- sapply(variables, function(var) mean(data[[var]], na.rm = TRUE))

generate_pd_plots <- function(model, vars, train_data, number_cols, var_labels) {
     plot_list <- lapply(vars, function(var) {
         pd <- partial(model, pred.var = var, train = train_data)
         means <- sapply(score_variable2, function(var) mean(train_data[[var]], na.rm = TRUE))
         # Plotting
         ggplot(pd, aes(x = .data[[var]], y = yhat)) + 
             geom_line(color = "steelblue", linewidth = 1) +
             #scale_x_continuous(limits = c(50, 100)) +
             #scale_y_continuous(limits = c(0,67)) +
             geom_vline(xintercept = means[var], color = "blue", linetype = "dashed", size = 1) +
             labs(title = var_labels[var], x = "", y = "Partial Dependence") +
             theme_bw() +
             theme(plot.title = element_text(size = 10))
     })
     
     # Filter out NULL plots
     plot_list <- Filter(Negate(is.null), plot_list)
     
     grid.arrange(grobs = plot_list, ncol = number_cols)
 }



# Generate and save plots
pd_plot <- generate_pd_plots(rf_model, predictor_vars, train_data, 5, name_mapping)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/ManagerToEmployee/Without duplicate/interaction/Glint/avg_score_Q6")

ggsave("./rf_partial_plot.png", plot = pd_plot, width = 15, height = 10)