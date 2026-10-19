################################################################################
#                                                                              #
#           LaRaTh-Br - Landslide Rainfall Thresholds Brazil 1.1               #
#                                                                              #
#                Copyright (C) 2024  Rodrigo Augusto Stabile                   #
#                                                                              #
#                   E-mail: rodrigo.stabile@cemaden.gov.br                     #
#                                                                              #
#                       Phone: +55 (12) 3205-0126/0127                         #
#                                                                              #
# Adress: Centro Nacional de Monitoramento e Alertas de Desastres Naturais,    #
# Cemaden. Estrada Doutor Altino Bondesan, 500 - Distrito de Eugênio de Melo.  #
# São José dos Campos/SP, Brazil. CEP:12.247-016.                              #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
#                         SOFTWARE REGISTRATION                                #
#                                                                              #
#              Brazilian National Institute of Industrial Property             #
#                    (INPI): Registration No. BR512024001508-6                 #
#                         Registration date: 08 May 2024                       #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
#                                 LICENSE                                      #
#                                                                              #
#      This program was developed using R version 4.3.1 (2023-06-16 ucrt)      #
#                       URL https://www.r-project.org/                         #
#                                                                              #
#     This program is free software: you can redistribute it and/or modify     #
# it under the terms of the GNU Affero General Public License as published by  #
#      the Free Software Foundation, either version 3 of the License, or       #
#                   (at your option) any later version.                        #
#                                                                              #
#      This program is distributed in the hope that it will be useful,         #
#       but WITHOUT ANY WARRANTY; without even the implied warranty of         #
#        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#             GNU Affero General Public License for more details.              #
#                                                                              #
#   You should have received a copy of the GNU Affero General Public License   #
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.   #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
#                             USEFUL INFORMATION                               #
#                                                                              #
#    The code has been structured into modules and steps. Each module has an   #
# objective and can be executed separately provided that the necessary input   #
#           data is available. The steps within each module should             #
#                        be executed sequentially.                             #
#                                                                              #
#  The code has been structured for the rainfall and landslide databases from  #
#   Cemaden/Brazil, but it can be adapted for different databases as long as   #
# they contain the minimum information required for the calculation of the     #
#                                   steps.                                     #
#                                                                              #
#    It is recommended that the databases be adapted with column names and     #
#         similar information to the examples attached to this code.           #
#                                                                              #
################################################################################

#---------------------------------- MODULE 3 ----------------------------------#
#---------------------- Logistic Regression Modeling --------------------------#

##### STEP 1: INSTALL REQUIRED PACKAGES AND SETUP REPRODUCIBILITY #####

# Install required packages if they are not already installed
required_packages <- c("ggplot2", "pROC", "caTools", "car", "shiny", "scales", "rlang", "PRROC", "dplyr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load libraries
library(ggplot2)
library(pROC)
library(caTools)
library(car)
library(shiny)
library(scales)
library(rlang)
library(PRROC)
library(dplyr)

# Set warnings to be clearly visible
options(warn = 1)

# Reproducibility setup
set.seed(12345)

# Create main output directory structure
cat("\nChoose the MAIN output directory for logistic regression results.\n")
output_base <- choose.dir(caption = "Select main output directory for logistic regression results")

# Create organized folder structure
output_dir <- file.path(output_base, "logistic_regression_model")
subdirs <- c("figures", "tables", "model", "reproducibility", "predictions", "validation")

for (sub in subdirs) {
  dir.create(file.path(output_dir, sub), recursive = TRUE, showWarnings = FALSE)
}

cat("Output directory structure created at:", output_dir, "\n")

# Save session information in reproducibility folder
session_file <- file.path(output_dir, "reproducibility", "session_info.txt")
sink(session_file)
print(sessionInfo())
sink()

# Save package versions for reproducibility
package_versions <- as.data.frame(installed.packages()[, c("Package", "Version")])
package_file <- file.path(output_dir, "reproducibility", "package_versions.csv")
write.csv(package_versions, package_file, row.names = FALSE)

cat("Reproducibility information saved to:", output_dir, "/reproducibility/\n")

#------------------------------------------------------------------------------# 

##### STEP 2: IMPORT THE TABLE WITH RAIN EVENTS AND RELATED LANDSLIDES #####

cat("Choose the file .csv with rainfall events and related landslides.\n")
file_path <- file.choose()

# Import the file into a dataframe
df_events_landslides <- readr::read_csv2(file_path)

cat("The file has been successfully imported:", file_path, "\n")

# Add a column to the dataframe indicating whether landslide occurred (1) or not (0)
df_events_landslides$landslide_occurrence <- ifelse(
  df_events_landslides$num_related_landslides > 0,
  1,
  0
)

#------------------------------------------------------------------------------# 

##### STEP 3: DEFINE THE VARIABLES FOR THE LOGISTIC REGRESSION MODEL #####

# Create df_var_selection outside of Shiny
variables_to_exclude <- c(
  "count_events_x_hours_int",
  "start_event",
  "end_event",
  "station",
  "season",
  "num_related_landslides",
  "latitude",
  "longitude",
  "municipality",
  "state"
)

numeric_variables <- sapply(df_events_landslides, is.numeric)

numeric_variables <- numeric_variables[
  !names(numeric_variables) %in% variables_to_exclude
]

df_var_selection <- df_events_landslides[
  ,
  names(numeric_variables),
  drop = FALSE
]

df_var_selection$landslide_occurrence <- df_events_landslides$landslide_occurrence

# Define UI for variable selection - NO VARIABLES PRE-SELECTED
ui4 <- fluidPage(
  titlePanel("Variable Selection for Logistic Regression Model"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "variables",
        "Select variables for logistic regression:",
        choices = names(df_var_selection)[-length(df_var_selection)],
        selected = NULL
      ),
      actionButton("apply_button", "Apply"),
      actionButton("save_button", "Save Selected Variables"),
      tags$script(HTML("
        $(document).ready(function() {
          $('input:checkbox[value=\"landslide_occurrence\"]').prop('disabled', true);
        });
      "))
    ),
    mainPanel(
      h4("Instructions:"),
      p("1. Select the variables you want to include in the logistic regression model."),
      p("2. Click 'Apply' to see the list of selected variables."),
      p("3. Click 'Save Selected Variables' to proceed with the analysis."),
      textOutput("confirmation_message")
    )
  )
)

server4 <- function(input, output, session) {
  
  rv <- reactiveValues(selected = NULL)
  
  observeEvent(input$apply_button, {
    if (length(input$variables) == 0) {
      message <- "WARNING: No variables selected. Please select at least one variable."
      cat(message, "\n")
      output$confirmation_message <- renderText(message)
    } else {
      selected_vars <- c(input$variables, "landslide_occurrence")
      message <- paste("You have selected the following variables:", paste(selected_vars, collapse = ", "))
      cat(message, "\n")
      output$confirmation_message <- renderText(message)
      rv$selected <- selected_vars
    }
  })
  
  observeEvent(input$save_button, {
    if (length(input$variables) == 0) {
      message <- "ERROR: No variables selected. Please select at least one variable before saving."
      cat(message, "\n")
      output$confirmation_message <- renderText(message)
    } else {
      selected_vars <- c(input$variables, "landslide_occurrence")
      
      df_selected_variables <- df_var_selection[, c(selected_vars), drop = FALSE]
      
      assign("df_var_selection", df_selected_variables, envir = .GlobalEnv)
      
      message <- paste("Selected variables have been saved to the global environment.\n",
                       "Variables:", paste(selected_vars, collapse = ", "))
      cat(message, "\n")
      output$confirmation_message <- renderText(message)
    }
  })
}

if (interactive()) {
  shinyApp(ui = ui4, server = server4)
}

#------------------------------------------------------------------------------# 

##### STEP 4: SPLIT DATA INTO TRAINING AND TESTING SETS #####

# --- IMPORTANT: df_var_selection is still raw (no transformations) ---
set.seed(123)
split <- sample.split(df_var_selection$landslide_occurrence, SplitRatio = 0.7)
training_data_raw <- subset(df_var_selection, split == TRUE)
testing_data_raw  <- subset(df_var_selection, split == FALSE)

#------------------------------------------------------------------------------# 

##### STEP 5: APPLY WEIGHTS BASED ON CLASS PREVALENCE #####

# Calculate class distribution on raw training data
class_counts <- table(training_data_raw$landslide_occurrence)
n_majority <- max(class_counts)
n_minority <- min(class_counts)

# Calculate weights: minority class gets weight = 1, majority gets weight = minority/majority
weights <- ifelse(training_data_raw$landslide_occurrence == 1, 
                  1, 
                  n_minority / n_majority)

cat("\nClass distribution in training data (raw):\n")
print(class_counts)
cat("\nClass weights calculated:\n")
cat("  Class 0 (No Landslide): weight =", round(n_minority/n_majority, 4), "\n")
cat("  Class 1 (Landslide): weight = 1\n")
cat("\nEffective sample size after weighting:\n")
cat("  Class 0 (No Landslide):", round(n_majority * (n_minority/n_majority), 0), "\n")
cat("  Class 1 (Landslide):", n_minority, "\n")

# We will apply these weights later to the training data after preprocessing.

#------------------------------------------------------------------------------# 

##### STEP 5.1: LOG TRANSFORMATION (ONLY ON TRAINING DATA) #####

# Variables that should be log-transformed (rainfall-related variables)
log_vars <- c("duration_hrs", "average_intensity_mm_hr", 
              "total_event_rain_mm", "max_avg_intensity_60min",
              "max_avg_intensity_30min", "max_avg_intensity_90min",
              "max_avg_intensity_120min", "max_avg_intensity_180min",
              "max_avg_intensity_360min", "max_avg_intensity_720min",
              "max_avg_intensity_1440min",
              "preceding_rain_24hrs", "preceding_rain_48hrs", 
              "preceding_rain_72hrs", "preceding_rain_96hrs",
              "preceding_rain_120hrs", "preceding_rain_240hrs",
              "preceding_rain_360hrs", "preceding_rain_720hrs",
              "preceding_rain_2160hrs")

# Only those that actually exist in the selected variables
log_vars <- log_vars[log_vars %in% names(training_data_raw)]

# Small constant to avoid log(0) – we will use the same constant for all variables
LOG_SHIFT <- 0.001

# Copy raw data for preprocessing
training_data <- training_data_raw
testing_data  <- testing_data_raw

# Apply log transformation to training and testing (using same shift)
for(var in log_vars) {
  if(var %in% names(training_data)) {
    training_data[[var]] <- log(training_data[[var]] + LOG_SHIFT)
    testing_data[[var]]  <- log(testing_data[[var]]  + LOG_SHIFT)
  }
}

cat("\nLog transformation applied to rainfall-related variables.\n")
cat("Variables transformed:", paste(log_vars, collapse = ", "), "\n")

#------------------------------------------------------------------------------# 

##### STEP 6: STANDARDIZE NUMERIC VARIABLES (USING TRAINING STATISTICS) #####

# Remove infinite values generated by previous calculations/log transformations
training_data[!is.finite(as.matrix(training_data))] <- NA
testing_data[!is.finite(as.matrix(testing_data))] <- NA

# Remove rows with missing values in variables used in the model
training_data <- na.omit(training_data)
testing_data  <- na.omit(testing_data)

cat("Infinite and missing values removed before standardization.\n")

# Identify numeric variables (excluding target) after log transformation
numeric_vars <- names(training_data)[
  sapply(training_data, is.numeric) & 
    names(training_data) != "landslide_occurrence"
]

if(length(numeric_vars) > 0) {
  
  # Compute means and SDs from TRAINING data only
  train_means <- sapply(training_data[, numeric_vars, drop = FALSE], mean, na.rm = TRUE)
  train_sds   <- sapply(training_data[, numeric_vars, drop = FALSE], sd, na.rm = TRUE)
  
  # Standardize training data
  for(var in numeric_vars) {
    if(train_sds[var] > 0) {
      training_data[[var]] <- (training_data[[var]] - train_means[var]) / train_sds[var]
      testing_data[[var]]  <- (testing_data[[var]]  - train_means[var]) / train_sds[var]
    } else {
      # If zero variance, set to 0
      training_data[[var]] <- 0
      testing_data[[var]]  <- 0
      cat("  Warning: variable", var, "has zero variance in training set. Set to 0.\n")
    }
  }
  
  cat("\nVariables standardized using training set means and SDs.\n")
  
  # Save standardization parameters for later use (e.g., final model)
  std_params <- list(means = train_means, sds = train_sds, log_shift = LOG_SHIFT, log_vars = log_vars)
  model_dir <- file.path(output_dir, "model")
  if(!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)
  std_params_file <- file.path(model_dir, "standardization_params.rds")
  saveRDS(std_params, std_params_file)
  cat("Standardization parameters saved to:", std_params_file, "\n")
  
} else {
  cat("No numeric variables to standardize.\n")
  std_params <- NULL
}

# Recalculate weights after removing invalid rows
class_counts_clean <- table(training_data$landslide_occurrence)

n_majority_clean <- max(class_counts_clean)
n_minority_clean <- min(class_counts_clean)

weights_clean <- ifelse(
  training_data$landslide_occurrence == 1,
  1,
  n_minority_clean / n_majority_clean
)

# Attach weights
training_data$weights <- weights_clean

cat("\nClass weights recalculated after removing invalid values.\n")
print(class_counts_clean)

# Create a copy of training data without weights for model fitting
training_data_for_model <- training_data[, !names(training_data) %in% "weights"]

#------------------------------------------------------------------------------# 

##### STEP 7: CREATE THE LOGISTIC REGRESSION MODEL WITH WEIGHTS #####

logistic_model <- glm(
  landslide_occurrence ~ .,
  data = training_data_for_model,
  family = "binomial",
  weights = training_data$weights
)

# SAVE THE MODEL FOR REUSE
model_file <- file.path(output_dir, "model", "logistic_model.rds")
saveRDS(logistic_model, model_file)
cat("Logistic regression model saved to:", model_file, "\n")

#------------------------------------------------------------------------------# 

##### STEP 8: CHECK MULTICOLLINEARITY (VIF ANALYSIS) #####

vif_values <- tryCatch(
  vif(logistic_model),
  error = function(e) {
    cat("Warning: Could not calculate VIF due to perfect multicollinearity or singularity.\n")
    return(NA)
  }
)

if (!is.na(vif_values[1])) {
  cat("\n=== MULTICOLLINEARITY DIAGNOSTIC (VIF) ===\n")
  print(vif_values)
  
  vif_file <- file.path(output_dir, "tables", "vif_values.csv")
  write.csv(data.frame(Variable = names(vif_values), VIF = vif_values), 
            vif_file, row.names = FALSE)
  cat("VIF values saved to:", vif_file, "\n")
}

#------------------------------------------------------------------------------# 

##### STEP 9: EVALUATE MODEL PERFORMANCE #####

predictions <- predict(logistic_model, newdata = testing_data, type = "response")

# Function to calculate metrics for a given threshold
calculate_metrics_threshold <- function(observed, predicted_prob, threshold) {
  
  binary_pred <- ifelse(predicted_prob > threshold, 1, 0)
  
  cm <- table(observed = observed, predicted = binary_pred)
  
  # Initialize metrics
  accuracy <- NA
  sensitivity <- NA
  specificity <- NA
  precision <- NA
  f1_score <- NA
  
  if (nrow(cm) == 2 && ncol(cm) == 2) {
    TP <- cm[2, 2]; TN <- cm[1, 1]
    FP <- cm[1, 2]; FN <- cm[2, 1]
    
    accuracy <- (TP + TN) / sum(cm)
    
    if ((TP + FN) > 0) sensitivity <- TP / (TP + FN)
    if ((TN + FP) > 0) specificity <- TN / (TN + FP)
    if ((TP + FP) > 0) precision <- TP / (TP + FP)
    
    if (!is.na(precision) && !is.na(sensitivity) && (precision + sensitivity) > 0) {
      f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
    }
  }
  
  return(list(
    threshold = threshold,
    confusion_matrix = cm,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    f1_score = f1_score
  ))
}

# Function to calculate AUC-PR
calculate_auc_pr <- function(observed, predicted_prob) {
  
  if (length(unique(observed)) < 2) {
    return(NA)
  }
  
  # Using PRROC package
  pr_curve <- tryCatch({
    prroc_obj <- pr.curve(
      scores.class0 = predicted_prob[observed == 1],
      scores.class1 = predicted_prob[observed == 0],
      curve = TRUE
    )
    return(as.numeric(prroc_obj$auc.integral))
  }, error = function(e) {
    return(NA)
  })
  
  return(pr_curve)
}

# Calculate metrics for all thresholds
thresholds <- c(0.25, 0.5, 0.75)

# Calculate AUC-ROC
roc_obj <- roc(testing_data$landslide_occurrence, predictions, quiet = TRUE)
auc_roc <- as.numeric(auc(roc_obj))

# Calculate AUC-PR
auc_pr <- calculate_auc_pr(testing_data$landslide_occurrence, predictions)

cat("\n=== MODEL EVALUATION ON TEST SET ===\n")
cat("AUC-ROC:", round(auc_roc, 4), "\n")
cat("AUC-PR:", round(auc_pr, 4), "\n\n")

# Store results for all thresholds
results_all_thresholds <- list()

for (thresh in thresholds) {
  
  metrics <- calculate_metrics_threshold(
    testing_data$landslide_occurrence, 
    predictions, 
    thresh
  )
  
  results_all_thresholds[[paste0("threshold_", thresh)]] <- metrics
  
  cat("--- Threshold:", thresh, "---\n")
  cat("Confusion Matrix:\n")
  print(metrics$confusion_matrix)
  cat("Accuracy:", round(metrics$accuracy, 4), "\n")
  cat("Sensitivity:", round(metrics$sensitivity, 4), "\n")
  cat("Specificity:", round(metrics$specificity, 4), "\n")
  cat("Precision:", round(metrics$precision, 4), "\n")
  cat("F1-Score:", round(metrics$f1_score, 4), "\n\n")
}

#------------------------------------------------------------------------------# 

##### STEP 10: REPEATED HOLDOUT VALIDATION WITH WEIGHTS (FIXED) #####

set.seed(123)
n_iterations <- 100

# Initialize validation results with all metrics for all thresholds
validation_results <- data.frame(
  iteration = numeric(),
  auc_roc = numeric(),
  auc_pr = numeric(),
  n_pos_train = integer(),
  n_pos_test = integer(),
  
  # Threshold 0.25
  accuracy_025 = numeric(),
  sensitivity_025 = numeric(),
  specificity_025 = numeric(),
  precision_025 = numeric(),
  f1_score_025 = numeric(),
  
  # Threshold 0.50
  accuracy_050 = numeric(),
  sensitivity_050 = numeric(),
  specificity_050 = numeric(),
  precision_050 = numeric(),
  f1_score_050 = numeric(),
  
  # Threshold 0.75
  accuracy_075 = numeric(),
  sensitivity_075 = numeric(),
  specificity_075 = numeric(),
  precision_075 = numeric(),
  f1_score_075 = numeric()
)

# Lists to store coefficients
coefficients_list <- list()

for(i in 1:n_iterations) {
  
  # 1. Split from RAW data (df_var_selection is still raw)
  split_cv <- sample.split(df_var_selection$landslide_occurrence, SplitRatio = 0.7)
  train_cv_raw <- subset(df_var_selection, split_cv == TRUE)
  test_cv_raw  <- subset(df_var_selection, split_cv == FALSE)
  
  
  # 2. Apply log transformation (using the same shift as in STEP 5.1)
  train_cv <- train_cv_raw
  test_cv  <- test_cv_raw
  
  for(var in log_vars) {
    if(var %in% names(train_cv)) {
      train_cv[[var]] <- log(train_cv[[var]] + LOG_SHIFT)
      test_cv[[var]]  <- log(test_cv[[var]] + LOG_SHIFT)
    }
  }
  
  # Remove infinite values generated by transformations
  train_cv[!is.finite(as.matrix(train_cv))] <- NA
  test_cv[!is.finite(as.matrix(test_cv))] <- NA
  
  # Remove incomplete rows
  train_cv <- na.omit(train_cv)
  test_cv  <- na.omit(test_cv)
  
  
  # Count positives AFTER cleaning
  n_pos_train <- sum(train_cv$landslide_occurrence == 1)
  n_pos_test  <- sum(test_cv$landslide_occurrence == 1)
  
  
  # 3. Standardize using training set statistics of this iteration
  cv_numeric_vars <- names(train_cv)[
    sapply(train_cv, is.numeric) & 
      names(train_cv) != "landslide_occurrence"
  ]
  
  if(length(cv_numeric_vars) > 0) {
    
    cv_means <- sapply(train_cv[, cv_numeric_vars, drop = FALSE], 
                       mean, na.rm = TRUE)
    
    cv_sds <- sapply(train_cv[, cv_numeric_vars, drop = FALSE], function(x) {
      if(length(unique(x[!is.na(x)])) <= 1) {
        return(0)
      } else {
        return(sd(x, na.rm = TRUE))
      }
    })
    
    for(var in cv_numeric_vars) {
      
      if(cv_sds[var] > 0) {
        train_cv[[var]] <- (train_cv[[var]] - cv_means[var]) / cv_sds[var]
        test_cv[[var]]  <- (test_cv[[var]] - cv_means[var]) / cv_sds[var]
      } else {
        train_cv[[var]] <- 0
        test_cv[[var]]  <- 0
      }
    }
  }
  
  
  # 4. Calculate weights for this iteration's training set
  class_counts_cv <- table(train_cv$landslide_occurrence)
  
  n_minority_cv <- min(class_counts_cv)
  n_majority_cv <- max(class_counts_cv)
  
  weights_cv <- ifelse(
    train_cv$landslide_occurrence == 1, 
    1, 
    n_minority_cv / n_majority_cv
  )
  
  
  # 5. Train model
  model_cv <- glm(
    landslide_occurrence ~ ., 
    data = train_cv, 
    family = "binomial",
    weights = weights_cv
  )
  
  
  # Store coefficients
  coef_names <- names(coef(model_cv))
  coef_values <- as.numeric(coef(model_cv))
  
  for(j in seq_along(coef_names)) {
    coefficients_list[[length(coefficients_list) + 1]] <- data.frame(
      iteration = i,
      variable = coef_names[j],
      coefficient = coef_values[j]
    )
  }
  
  
  # 6. Predictions
  pred_cv <- predict(model_cv, newdata = test_cv, type = "response")
  
  
  # 7. Calculate AUC-ROC
  roc_cv <- roc(test_cv$landslide_occurrence, pred_cv, quiet = TRUE)
  auc_roc_cv <- as.numeric(auc(roc_cv))
  
  
  # 8. Calculate AUC-PR
  auc_pr_cv <- calculate_auc_pr(test_cv$landslide_occurrence, pred_cv)
  
  
  # 9. Calculate metrics for all thresholds
  metrics_025 <- calculate_metrics_threshold(test_cv$landslide_occurrence, pred_cv, 0.25)
  metrics_050 <- calculate_metrics_threshold(test_cv$landslide_occurrence, pred_cv, 0.50)
  metrics_075 <- calculate_metrics_threshold(test_cv$landslide_occurrence, pred_cv, 0.75)
  
  
  # Store all results
  validation_results <- rbind(
    validation_results,
    data.frame(
      iteration = i,
      auc_roc = auc_roc_cv,
      auc_pr = auc_pr_cv,
      n_pos_train = n_pos_train,
      n_pos_test = n_pos_test,
      
      accuracy_025 = metrics_025$accuracy,
      sensitivity_025 = metrics_025$sensitivity,
      specificity_025 = metrics_025$specificity,
      precision_025 = metrics_025$precision,
      f1_score_025 = metrics_025$f1_score,
      
      accuracy_050 = metrics_050$accuracy,
      sensitivity_050 = metrics_050$sensitivity,
      specificity_050 = metrics_050$specificity,
      precision_050 = metrics_050$precision,
      f1_score_050 = metrics_050$f1_score,
      
      accuracy_075 = metrics_075$accuracy,
      sensitivity_075 = metrics_075$sensitivity,
      specificity_075 = metrics_075$specificity,
      precision_075 = metrics_075$precision,
      f1_score_075 = metrics_075$f1_score
    )
  )
  
  
  if(i %% 10 == 0) {
    cat(
      "Iteration", i,
      "completed - AUC-ROC:", round(auc_roc_cv, 4),
      "- AUC-PR:", round(auc_pr_cv, 4),
      "- Pos Train:", n_pos_train,
      "- Pos Test:", n_pos_test,
      "\n"
    )
  }
}

# Combine all coefficients
all_coefficients <- do.call(rbind, coefficients_list)

##### RESULTS AND STATISTICS #####

cat("\n", rep("=", 60), "\n", sep="")
cat("=== REPEATED HOLDOUT VALIDATION RESULTS (100 iterations) ===\n")
cat(rep("=", 60), "\n\n", sep="")

# Performance summary function
create_performance_summary <- function(data, metric_names, suffix) {
  
  results <- data.frame(
    Metric = character(),
    Threshold = character(),
    Mean = numeric(),
    SD = numeric(),
    Min = numeric(),
    Max = numeric(),
    Q25 = numeric(),
    Median = numeric(),
    Q75 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (metric in metric_names) {
    col_name <- paste0(metric, "_", suffix)
    if (col_name %in% names(data)) {
      values <- data[[col_name]]
      results <- rbind(results, data.frame(
        Metric = metric,
        Threshold = paste0("0.", suffix),
        Mean = mean(values, na.rm = TRUE),
        SD = sd(values, na.rm = TRUE),
        Min = min(values, na.rm = TRUE),
        Max = max(values, na.rm = TRUE),
        Q25 = quantile(values, 0.25, na.rm = TRUE),
        Median = median(values, na.rm = TRUE),
        Q75 = quantile(values, 0.75, na.rm = TRUE)
      ))
    }
  }
  
  return(results)
}

# AUC summary
auc_summary <- data.frame(
  Metric = c("AUC-ROC", "AUC-PR"),
  Mean = c(mean(validation_results$auc_roc, na.rm = TRUE),
           mean(validation_results$auc_pr, na.rm = TRUE)),
  SD = c(sd(validation_results$auc_roc, na.rm = TRUE),
         sd(validation_results$auc_pr, na.rm = TRUE)),
  Min = c(min(validation_results$auc_roc, na.rm = TRUE),
          min(validation_results$auc_pr, na.rm = TRUE)),
  Max = c(max(validation_results$auc_roc, na.rm = TRUE),
          max(validation_results$auc_pr, na.rm = TRUE)),
  Q25 = c(quantile(validation_results$auc_roc, 0.25, na.rm = TRUE),
          quantile(validation_results$auc_pr, 0.25, na.rm = TRUE)),
  Median = c(median(validation_results$auc_roc, na.rm = TRUE),
             median(validation_results$auc_pr, na.rm = TRUE)),
  Q75 = c(quantile(validation_results$auc_roc, 0.75, na.rm = TRUE),
          quantile(validation_results$auc_pr, 0.75, na.rm = TRUE))
)

cat("=== AUC METRICS SUMMARY ===\n")
print(auc_summary)

# Metrics for each threshold
metric_names <- c("accuracy", "sensitivity", "specificity", "precision", "f1_score")

summary_025 <- create_performance_summary(validation_results, metric_names, "025")
summary_050 <- create_performance_summary(validation_results, metric_names, "050")
summary_075 <- create_performance_summary(validation_results, metric_names, "075")

cat("\n=== PERFORMANCE SUMMARY - THRESHOLD 0.25 ===\n")
print(summary_025)

cat("\n=== PERFORMANCE SUMMARY - THRESHOLD 0.50 ===\n")
print(summary_050)

cat("\n=== PERFORMANCE SUMMARY - THRESHOLD 0.75 ===\n")
print(summary_075)

# Format for publication
cat("\n", rep("=", 60), "\n", sep="")
cat("=== FORMAT FOR PUBLICATION (Mean ± SD) ===\n")
cat(rep("=", 60), "\n", sep="")

cat("\n--- AUC METRICS ---\n")
cat("AUC-ROC:", round(mean(validation_results$auc_roc, na.rm = TRUE), 4), "±", 
    round(sd(validation_results$auc_roc, na.rm = TRUE), 4), "\n")
cat("AUC-PR:", round(mean(validation_results$auc_pr, na.rm = TRUE), 4), "±", 
    round(sd(validation_results$auc_pr, na.rm = TRUE), 4), "\n")

for (suffix in c("025", "050", "075")) {
  
  threshold_label <- paste0("0.", substr(suffix, 2, 3))
  
  cat("\n--- THRESHOLD", threshold_label, "---\n")
  cat("Accuracy:", round(mean(validation_results[[paste0("accuracy_", suffix)]], na.rm = TRUE), 4), "±", 
      round(sd(validation_results[[paste0("accuracy_", suffix)]], na.rm = TRUE), 4), "\n")
  cat("Sensitivity:", round(mean(validation_results[[paste0("sensitivity_", suffix)]], na.rm = TRUE), 4), "±", 
      round(sd(validation_results[[paste0("sensitivity_", suffix)]], na.rm = TRUE), 4), "\n")
  cat("Specificity:", round(mean(validation_results[[paste0("specificity_", suffix)]], na.rm = TRUE), 4), "±", 
      round(sd(validation_results[[paste0("specificity_", suffix)]], na.rm = TRUE), 4), "\n")
  cat("Precision:", round(mean(validation_results[[paste0("precision_", suffix)]], na.rm = TRUE), 4), "±", 
      round(sd(validation_results[[paste0("precision_", suffix)]], na.rm = TRUE), 4), "\n")
  cat("F1-Score:", round(mean(validation_results[[paste0("f1_score_", suffix)]], na.rm = TRUE), 4), "±", 
      round(sd(validation_results[[paste0("f1_score_", suffix)]], na.rm = TRUE), 4), "\n")
}

cat("\n", rep("=", 60), "\n", sep="")
cat("=== POSITIVE CASES SUMMARY ===\n")
cat(rep("=", 60), "\n", sep="")

pos_summary <- data.frame(
  Metric = c("Train Positives", "Test Positives"),
  Mean = c(mean(validation_results$n_pos_train), mean(validation_results$n_pos_test)),
  SD = c(sd(validation_results$n_pos_train), sd(validation_results$n_pos_test)),
  Min = c(min(validation_results$n_pos_train), min(validation_results$n_pos_test)),
  Max = c(max(validation_results$n_pos_train), max(validation_results$n_pos_test))
)
print(pos_summary)

##### SAVE FILES WITH OUTPUT_DIR STRUCTURE #####

validation_output_dir <- file.path(output_dir, "validation")
if(!dir.exists(validation_output_dir)) {
  dir.create(validation_output_dir, recursive = TRUE)
}

# Save all results
write.csv(validation_results, file.path(validation_output_dir, "validation_results_100iterations.csv"), row.names = FALSE)

# Combined performance summary
all_performance_summary <- rbind(
  data.frame(Metric = auc_summary$Metric, Threshold = "N/A", auc_summary[, -1]),
  summary_025,
  summary_050,
  summary_075
)
write.csv(all_performance_summary, file.path(validation_output_dir, "performance_summary_all_thresholds.csv"), row.names = FALSE)

write.csv(auc_summary, file.path(validation_output_dir, "auc_summary.csv"), row.names = FALSE)
write.csv(summary_025, file.path(validation_output_dir, "performance_summary_threshold_025.csv"), row.names = FALSE)
write.csv(summary_050, file.path(validation_output_dir, "performance_summary_threshold_050.csv"), row.names = FALSE)
write.csv(summary_075, file.path(validation_output_dir, "performance_summary_threshold_075.csv"), row.names = FALSE)
write.csv(pos_summary, file.path(validation_output_dir, "positive_cases_summary_100iterations.csv"), row.names = FALSE)
write.csv(all_coefficients, file.path(validation_output_dir, "model_coefficients_all_100iterations.csv"), row.names = FALSE)

# Coefficient summary - MODIFIED TO INCLUDE ALL VARIABLES WITHOUT FILTERING
coefficients_summary <- aggregate(coefficient ~ variable, 
                                  data = all_coefficients, 
                                  FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                                      sd = sd(x, na.rm = TRUE),
                                                      min = min(x, na.rm = TRUE),
                                                      max = max(x, na.rm = TRUE),
                                                      q25 = quantile(x, 0.25, na.rm = TRUE),
                                                      median = median(x, na.rm = TRUE),
                                                      q75 = quantile(x, 0.75, na.rm = TRUE),
                                                      n = length(x)))
coefficients_summary <- do.call(data.frame, coefficients_summary)
names(coefficients_summary) <- c("variable", "mean", "sd", "min", "max", "q25", "median", "q75", "n_iterations")
write.csv(coefficients_summary, file.path(validation_output_dir, "coefficients_summary_100iterations.csv"), row.names = FALSE)

# ADDITIONAL: Export complete coefficient statistics with stability metrics for ALL variables
coefficients_complete <- coefficients_summary
coefficients_complete$abs_mean <- abs(coefficients_complete$mean)
coefficients_complete$cv <- coefficients_complete$sd / abs(coefficients_complete$mean) * 100
coefficients_complete$ci_lower_95 <- coefficients_complete$mean - 1.96 * (coefficients_complete$sd / sqrt(coefficients_complete$n_iterations))
coefficients_complete$ci_upper_95 <- coefficients_complete$mean + 1.96 * (coefficients_complete$sd / sqrt(coefficients_complete$n_iterations))

coefficients_complete$stable <- abs(coefficients_complete$mean) > 2 * coefficients_complete$sd
coefficients_complete$sign_consistent <- (coefficients_complete$mean > 0 & coefficients_complete$q25 > 0) | 
  (coefficients_complete$mean < 0 & coefficients_complete$q75 < 0)

write.csv(coefficients_complete, 
          file.path(validation_output_dir, "coefficients_complete_statistics.csv"), 
          row.names = FALSE)

# Article format - including AUC-PR and all thresholds
article_format <- data.frame(
  Metric = c(
    "AUC-ROC", "AUC-PR",
    "Accuracy (0.25)", "Sensitivity (0.25)", "Specificity (0.25)", "Precision (0.25)", "F1-Score (0.25)",
    "Accuracy (0.50)", "Sensitivity (0.50)", "Specificity (0.50)", "Precision (0.50)", "F1-Score (0.50)",
    "Accuracy (0.75)", "Sensitivity (0.75)", "Specificity (0.75)", "Precision (0.75)", "F1-Score (0.75)"
  ),
  Mean = c(
    mean(validation_results$auc_roc, na.rm = TRUE),
    mean(validation_results$auc_pr, na.rm = TRUE),
    mean(validation_results$accuracy_025, na.rm = TRUE),
    mean(validation_results$sensitivity_025, na.rm = TRUE),
    mean(validation_results$specificity_025, na.rm = TRUE),
    mean(validation_results$precision_025, na.rm = TRUE),
    mean(validation_results$f1_score_025, na.rm = TRUE),
    mean(validation_results$accuracy_050, na.rm = TRUE),
    mean(validation_results$sensitivity_050, na.rm = TRUE),
    mean(validation_results$specificity_050, na.rm = TRUE),
    mean(validation_results$precision_050, na.rm = TRUE),
    mean(validation_results$f1_score_050, na.rm = TRUE),
    mean(validation_results$accuracy_075, na.rm = TRUE),
    mean(validation_results$sensitivity_075, na.rm = TRUE),
    mean(validation_results$specificity_075, na.rm = TRUE),
    mean(validation_results$precision_075, na.rm = TRUE),
    mean(validation_results$f1_score_075, na.rm = TRUE)
  ),
  SD = c(
    sd(validation_results$auc_roc, na.rm = TRUE),
    sd(validation_results$auc_pr, na.rm = TRUE),
    sd(validation_results$accuracy_025, na.rm = TRUE),
    sd(validation_results$sensitivity_025, na.rm = TRUE),
    sd(validation_results$specificity_025, na.rm = TRUE),
    sd(validation_results$precision_025, na.rm = TRUE),
    sd(validation_results$f1_score_025, na.rm = TRUE),
    sd(validation_results$accuracy_050, na.rm = TRUE),
    sd(validation_results$sensitivity_050, na.rm = TRUE),
    sd(validation_results$specificity_050, na.rm = TRUE),
    sd(validation_results$precision_050, na.rm = TRUE),
    sd(validation_results$f1_score_050, na.rm = TRUE),
    sd(validation_results$accuracy_075, na.rm = TRUE),
    sd(validation_results$sensitivity_075, na.rm = TRUE),
    sd(validation_results$specificity_075, na.rm = TRUE),
    sd(validation_results$precision_075, na.rm = TRUE),
    sd(validation_results$f1_score_075, na.rm = TRUE)
  )
)

article_format$Result_Format <- paste0(
  round(article_format$Mean, 4), " ± ", round(article_format$SD, 4)
)

write.csv(article_format, file.path(validation_output_dir, "article_format_mean_sd.csv"), row.names = FALSE)

cat("\n", rep("=", 60), "\n", sep="")
cat("=== PROCESS COMPLETED SUCCESSFULLY ===\n")
cat(rep("=", 60), "\n", sep="")
cat("\nAll files saved in:", validation_output_dir, "\n")

#------------------------------------------------------------------------------# 

##### STEP 11: ANALYSIS OF COEFFICIENTS FROM REPEATED HOLDOUT VALIDATION #####

cat("\n", rep("=", 60), "\n", sep="")
cat("=== COEFFICIENT STABILITY ANALYSIS (100 iterations) ===\n")
cat(rep("=", 60), "\n\n", sep="")

coef_stability <- coefficients_complete

cat("=== VARIABLE STABILITY SUMMARY ===\n")
cat("Variables with stable coefficients (|mean| > 2*sd):\n")
stable_vars <- coef_stability$variable[coef_stability$stable & coef_stability$variable != "(Intercept)"]
if(length(stable_vars) > 0) {
  print(coef_stability[coef_stability$variable %in% stable_vars, 
                       c("variable", "mean", "sd", "sign_consistent", "stable")])
} else {
  cat("No variables with |mean| > 2*sd\n")
}

cat("\nVariables with consistent sign across iterations:\n")
consistent_vars <- coef_stability$variable[coef_stability$sign_consistent & coef_stability$variable != "(Intercept)"]
if(length(consistent_vars) > 0) {
  print(coef_stability[coef_stability$variable %in% consistent_vars, 
                       c("variable", "mean", "q25", "median", "q75", "sign_consistent")])
} else {
  cat("No variables with fully consistent sign\n")
}

write.csv(coef_stability, file.path(validation_output_dir, "coefficient_stability_analysis.csv"), row.names = FALSE)

stable_coefs <- coef_stability[coef_stability$stable & coef_stability$variable != "(Intercept)", ]
unstable_coefs <- coef_stability[!coef_stability$stable & coef_stability$variable != "(Intercept)", ]

if(nrow(stable_coefs) > 0) {
  write.csv(stable_coefs, file.path(validation_output_dir, "stable_coefficients.csv"), row.names = FALSE)
}
if(nrow(unstable_coefs) > 0) {
  write.csv(unstable_coefs, file.path(validation_output_dir, "unstable_coefficients.csv"), row.names = FALSE)
}

cat("\n✓ All coefficient statistics exported (including unstable ones)\n")
cat("✓ Files saved in:", validation_output_dir, "\n")

#------------------------------------------------------------------------------# 

##### STEP 12: CREATE PLOTS WITH ALL METRICS AND THRESHOLDS #####

cat("\n", rep("=", 60), "\n", sep="")
cat("=== PLOTS AND VISUALIZATIONS ===\n")
cat(rep("=", 60), "\n\n", sep="")

figures_dir <- file.path(output_dir, "figures")
if(!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# AUC-ROC Distribution
png(file.path(figures_dir, "auc_roc_distribution_100iterations.png"), width = 6, height = 5, units = "in", res = 300)
boxplot(validation_results$auc_roc, main = "AUC-ROC Distribution (100 Iterations)", 
        ylab = "AUC-ROC", col = "lightblue", border = "darkblue")
abline(h = mean(validation_results$auc_roc, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
dev.off()

# AUC-PR Distribution
png(file.path(figures_dir, "auc_pr_distribution_100iterations.png"), width = 6, height = 5, units = "in", res = 300)
boxplot(validation_results$auc_pr, main = "AUC-PR Distribution (100 Iterations)", 
        ylab = "AUC-PR", col = "lightgreen", border = "darkgreen")
abline(h = mean(validation_results$auc_pr, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
dev.off()

# Combined AUC comparison
auc_data <- data.frame(
  Metric = c(rep("AUC-ROC", n_iterations), rep("AUC-PR", n_iterations)),
  Value = c(validation_results$auc_roc, validation_results$auc_pr)
)

png(file.path(figures_dir, "auc_comparison_100iterations.png"), width = 8, height = 5, units = "in", res = 300)
boxplot(Value ~ Metric, data = auc_data, 
        main = "AUC-ROC vs AUC-PR Distribution (100 Iterations)",
        col = c("lightblue", "lightgreen"),
        ylab = "AUC Value")
dev.off()

# ---------------------------------------------------------------
# Metrics comparison across thresholds (CORRIGIDO)
# ---------------------------------------------------------------

metric_labels <- c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1-Score")
threshold_labels <- c("0.25", "0.50", "0.75")

# Construir matriz 5x3 (linhas = métricas, colunas = limiares)
metrics_matrix <- matrix(
  c(
    # Threshold 0.25
    mean(validation_results$accuracy_025, na.rm = TRUE),
    mean(validation_results$sensitivity_025, na.rm = TRUE),
    mean(validation_results$specificity_025, na.rm = TRUE),
    mean(validation_results$precision_025, na.rm = TRUE),
    mean(validation_results$f1_score_025, na.rm = TRUE),
    # Threshold 0.50
    mean(validation_results$accuracy_050, na.rm = TRUE),
    mean(validation_results$sensitivity_050, na.rm = TRUE),
    mean(validation_results$specificity_050, na.rm = TRUE),
    mean(validation_results$precision_050, na.rm = TRUE),
    mean(validation_results$f1_score_050, na.rm = TRUE),
    # Threshold 0.75
    mean(validation_results$accuracy_075, na.rm = TRUE),
    mean(validation_results$sensitivity_075, na.rm = TRUE),
    mean(validation_results$specificity_075, na.rm = TRUE),
    mean(validation_results$precision_075, na.rm = TRUE),
    mean(validation_results$f1_score_075, na.rm = TRUE)
  ),
  nrow = 5,
  ncol = 3,
  byrow = FALSE,
  dimnames = list(metric_labels, threshold_labels)
)

# Também criar a matriz de SDs
sd_matrix <- matrix(
  c(
    sd(validation_results$accuracy_025, na.rm = TRUE),
    sd(validation_results$sensitivity_025, na.rm = TRUE),
    sd(validation_results$specificity_025, na.rm = TRUE),
    sd(validation_results$precision_025, na.rm = TRUE),
    sd(validation_results$f1_score_025, na.rm = TRUE),
    sd(validation_results$accuracy_050, na.rm = TRUE),
    sd(validation_results$sensitivity_050, na.rm = TRUE),
    sd(validation_results$specificity_050, na.rm = TRUE),
    sd(validation_results$precision_050, na.rm = TRUE),
    sd(validation_results$f1_score_050, na.rm = TRUE),
    sd(validation_results$accuracy_075, na.rm = TRUE),
    sd(validation_results$sensitivity_075, na.rm = TRUE),
    sd(validation_results$specificity_075, na.rm = TRUE),
    sd(validation_results$precision_075, na.rm = TRUE),
    sd(validation_results$f1_score_075, na.rm = TRUE)
  ),
  nrow = 5,
  ncol = 3,
  byrow = FALSE,
  dimnames = list(metric_labels, threshold_labels)
)

# Gráfico de barras agrupadas (com SD como barras de erro)
png(file.path(figures_dir, "metrics_by_threshold_100iterations.png"), 
    width = 10, height = 6, units = "in", res = 300)

bp <- barplot(
  metrics_matrix,
  beside = TRUE,
  col = c("lightblue", "lightgreen", "lightyellow"),
  main = "Performance Metrics by Threshold (Mean ± SD)",
  ylab = "Value",
  ylim = c(0, 1.15),
  xpd = FALSE
)

# Adicionar barras de erro (SD)
arrows(
  x0 = bp,
  y0 = metrics_matrix - sd_matrix,
  x1 = bp,
  y1 = metrics_matrix + sd_matrix,
  angle = 90,
  code = 3,
  length = 0.04,
  col = "darkred",
  lwd = 1.5
)

# Adicionar legenda manualmente para os limiares
legend("topright",
       legend = threshold_labels,
       fill = c("lightblue", "lightgreen", "lightyellow"),
       title = "Threshold",
       bty = "n",
       cex = 0.9)

# Adicionar valores nas barras
text(bp, metrics_matrix + sd_matrix + 0.03,
     labels = round(metrics_matrix, 2),
     cex = 0.6,
     pos = 3)

dev.off()

# ---------------------------------------------------------------
# Gráficos individuais por métrica (opcional, mais claros)
# ---------------------------------------------------------------

for (i in seq_along(metric_labels)) {
  
  metric_name <- metric_labels[i]
  safe_name <- gsub("-", "_", tolower(metric_name))
  
  png(file.path(figures_dir, paste0(safe_name, "_by_threshold_100iterations.png")), 
      width = 6, height = 5, units = "in", res = 300)
  
  bp_i <- barplot(
    metrics_matrix[i, ],
    names.arg = threshold_labels,
    col = c("lightblue", "lightgreen", "lightyellow"),
    main = paste(metric_name, "by Threshold"),
    ylab = metric_name,
    xlab = "Threshold",
    ylim = c(0, min(1.15, max(metrics_matrix[i, ] + sd_matrix[i, ]) * 1.2))
  )
  
  arrows(
    x0 = bp_i,
    y0 = metrics_matrix[i, ] - sd_matrix[i, ],
    x1 = bp_i,
    y1 = metrics_matrix[i, ] + sd_matrix[i, ],
    angle = 90,
    code = 3,
    length = 0.05,
    col = "darkred",
    lwd = 1.5
  )
  
  text(bp_i, metrics_matrix[i, ] + sd_matrix[i, ] + 0.03,
       labels = round(metrics_matrix[i, ], 3),
       cex = 0.8,
       pos = 3)
  
  dev.off()
}

# ---------------------------------------------------------------
# Salvar tabela de métricas por limiar
# ---------------------------------------------------------------

metrics_comparison <- data.frame(
  Threshold = rep(threshold_labels, each = 5),
  Metric = rep(metric_labels, times = 3),
  Mean = as.vector(metrics_matrix),
  SD = as.vector(sd_matrix)
)

write.csv(metrics_comparison, 
          file.path(validation_output_dir, "metrics_by_threshold.csv"), 
          row.names = FALSE)

cat("\n✓ Plots saved in:", figures_dir, "\n")
cat("✓ Metrics by threshold table saved in:", validation_output_dir, "\n")

#------------------------------------------------------------------------------# 

##### STEP 13: SAVE MODEL INFORMATION FROM REPEATED VALIDATION #####

model_info_file <- file.path(output_dir, "model", "model_information.txt")
coefficients_file <- file.path(output_dir, "tables", "model_coefficients_stability.csv")

cat("LOGISTIC REGRESSION MODEL FOR LANDSLIDE PREDICTION\n", 
    "Analysis conducted on:", date(), "\n",
    paste(rep("=", 60), collapse = ""), "\n\n", 
    file = model_info_file)

cat("VALIDATION METHOD:\n", 
    "Repeated Holdout Validation with", n_iterations, "iterations\n",
    "Train/Test split ratio: 70/30\n",
    "Class balancing: Weights based on minority class prevalence\n\n",
    file = model_info_file, append = TRUE)

cat("Variables used in final model:\n", 
    paste(colnames(df_var_selection), collapse = ", "), "\n\n",
    file = model_info_file, append = TRUE)

cat("Class balancing method: Class weights based on prevalence\n", 
    file = model_info_file, append = TRUE)

# Compute final class weights on full raw data
class_counts_full <- table(df_var_selection$landslide_occurrence)
n_minority_full <- min(class_counts_full)
n_majority_full <- max(class_counts_full)
cat("  Class 0 (No Landslide) weight:", round(n_minority_full/n_majority_full, 4), "\n", 
    file = model_info_file, append = TRUE)
cat("  Class 1 (Landslide) weight: 1\n\n", 
    file = model_info_file, append = TRUE)

cat("PREPROCESSING:\n",
    "Log transformation applied to: rainfall-related variables (shift =", LOG_SHIFT, ")\n",
    "Standardization applied to all numeric variables using training set means/SDs\n",
    "Standardization parameters saved in: standardization_params.rds\n\n",
    file = model_info_file, append = TRUE)

cat("REPEATED HOLDOUT VALIDATION RESULTS (", n_iterations, " iterations):\n", sep = "",
    file = model_info_file, append = TRUE)

cat("\n--- AUC METRICS ---\n",
    file = model_info_file, append = TRUE)
cat("  Mean AUC-ROC:", round(mean(validation_results$auc_roc, na.rm = TRUE), 4), 
    "±", round(sd(validation_results$auc_roc, na.rm = TRUE), 4), "\n",
    file = model_info_file, append = TRUE)
cat("  Mean AUC-PR:", round(mean(validation_results$auc_pr, na.rm = TRUE), 4), 
    "±", round(sd(validation_results$auc_pr, na.rm = TRUE), 4), "\n",
    file = model_info_file, append = TRUE)

for (suffix in c("025", "050", "075")) {
  threshold_label <- paste0("0.", substr(suffix, 2, 3))
  cat("\n--- THRESHOLD", threshold_label, "---\n",
      file = model_info_file, append = TRUE)
  cat("  Mean Accuracy:", round(mean(validation_results[[paste0("accuracy_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("accuracy_", suffix)]], na.rm = TRUE), 4), "\n",
      file = model_info_file, append = TRUE)
  cat("  Mean Sensitivity:", round(mean(validation_results[[paste0("sensitivity_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("sensitivity_", suffix)]], na.rm = TRUE), 4), "\n",
      file = model_info_file, append = TRUE)
  cat("  Mean Specificity:", round(mean(validation_results[[paste0("specificity_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("specificity_", suffix)]], na.rm = TRUE), 4), "\n",
      file = model_info_file, append = TRUE)
  cat("  Mean Precision:", round(mean(validation_results[[paste0("precision_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("precision_", suffix)]], na.rm = TRUE), 4), "\n",
      file = model_info_file, append = TRUE)
  cat("  Mean F1-Score:", round(mean(validation_results[[paste0("f1_score_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("f1_score_", suffix)]], na.rm = TRUE), 4), "\n",
      file = model_info_file, append = TRUE)
}

# AUC 95% CI
mean_auc_roc <- mean(validation_results$auc_roc, na.rm = TRUE)
sd_auc_roc <- sd(validation_results$auc_roc, na.rm = TRUE)
se_auc_roc <- sd_auc_roc / sqrt(n_iterations)
ci_lower_auc_roc <- mean_auc_roc - 1.96 * se_auc_roc
ci_upper_auc_roc <- mean_auc_roc + 1.96 * se_auc_roc

mean_auc_pr <- mean(validation_results$auc_pr, na.rm = TRUE)
sd_auc_pr <- sd(validation_results$auc_pr, na.rm = TRUE)
se_auc_pr <- sd_auc_pr / sqrt(n_iterations)
ci_lower_auc_pr <- mean_auc_pr - 1.96 * se_auc_pr
ci_upper_auc_pr <- mean_auc_pr + 1.96 * se_auc_pr

cat("\nAUC-ROC 95% Confidence Interval (for the mean):\n",
    "  Lower bound:", round(ci_lower_auc_roc, 4), "\n",
    "  Upper bound:", round(ci_upper_auc_roc, 4), "\n",
    file = model_info_file, append = TRUE)

cat("AUC-PR 95% Confidence Interval (for the mean):\n",
    "  Lower bound:", round(ci_lower_auc_pr, 4), "\n",
    "  Upper bound:", round(ci_upper_auc_pr, 4), "\n\n",
    file = model_info_file, append = TRUE)

cat("COEFFICIENT STABILITY ACROSS ITERATIONS:\n",
    file = model_info_file, append = TRUE)

stable_vars_df <- coef_stability[coef_stability$stable & coef_stability$variable != "(Intercept)", ]
unstable_vars_df <- coef_stability[!coef_stability$stable & coef_stability$variable != "(Intercept)", ]

if(nrow(stable_vars_df) > 0) {
  cat("  Variables with stable coefficients (|mean| > 2*SD):\n",
      file = model_info_file, append = TRUE)
  for(j in 1:nrow(stable_vars_df)) {
    cat("    -", stable_vars_df$variable[j], ": mean =", round(stable_vars_df$mean[j], 4),
        ", SD =", round(stable_vars_df$sd[j], 4), 
        ", Q25 =", round(stable_vars_df$q25[j], 4),
        ", Q75 =", round(stable_vars_df$q75[j], 4), "\n",
        file = model_info_file, append = TRUE)
  }
} else {
  cat("  No variables with |mean| > 2*SD\n",
      file = model_info_file, append = TRUE)
}

if(nrow(unstable_vars_df) > 0) {
  cat("\n  Variables with UNSTABLE coefficients (|mean| <= 2*SD):\n",
      file = model_info_file, append = TRUE)
  for(j in 1:nrow(unstable_vars_df)) {
    cat("    -", unstable_vars_df$variable[j], ": mean =", round(unstable_vars_df$mean[j], 4),
        ", SD =", round(unstable_vars_df$sd[j], 4),
        ", Q25 =", round(unstable_vars_df$q25[j], 4),
        ", Q75 =", round(unstable_vars_df$q75[j], 4), "\n",
        file = model_info_file, append = TRUE)
  }
}

write.csv(coef_stability, coefficients_file, row.names = FALSE)

cat("✓ Model information saved to:", model_info_file, "\n")
cat("✓ Coefficient stability summary saved to:", coefficients_file, "\n")

#------------------------------------------------------------------------------# 

##### STEP 14: TRAIN FINAL OPERATIONAL MODEL ON FULL DATASET #####

cat("\n", rep("=", 60), "\n", sep="")
cat("=== TRAINING FINAL MODEL ON FULL DATASET ===\n")
cat(rep("=", 60), "\n", sep="")

# Use raw df_var_selection (which contains all selected variables, untouched)
df_full_raw <- df_var_selection

# 1. Apply log transformation to full dataset (using the same LOG_SHIFT)
df_full <- df_full_raw

for(var in log_vars) {
  if(var %in% names(df_full)) {
    df_full[[var]] <- log(df_full[[var]] + LOG_SHIFT)
  }
}

# Remove infinite values generated by log transformation
df_full[!is.finite(as.matrix(df_full))] <- NA

# Remove incomplete rows
df_full <- na.omit(df_full)

cat("Infinite and missing values removed before final model training.\n")


# 2. Standardize using statistics computed on the FULL dataset
numeric_vars_full <- names(df_full)[
  sapply(df_full, is.numeric) & 
    names(df_full) != "landslide_occurrence"
]

full_means <- c()
full_sds <- c()

for(var in numeric_vars_full) {
  
  mean_val <- mean(df_full[[var]], na.rm = TRUE)
  sd_val <- sd(df_full[[var]], na.rm = TRUE)
  
  if(is.na(sd_val)) {
    sd_val <- 0
  }
  
  full_means[var] <- mean_val
  full_sds[var] <- sd_val
  
  if(sd_val > 0) {
    df_full[[var]] <- (df_full[[var]] - mean_val) / sd_val
  } else {
    df_full[[var]] <- 0
    cat("  Warning:", var, "has zero variance in full dataset. Set to 0.\n")
  }
}


# Save final standardization parameters for future predictions
final_std_params <- list(
  means = full_means,
  sds = full_sds,
  log_shift = LOG_SHIFT,
  log_vars = log_vars,
  numeric_vars = numeric_vars_full,
  date_created = date()
)

final_std_file <- file.path(output_dir, "model", "final_standardization_params.rds")

saveRDS(final_std_params, final_std_file)

cat("✓ Final standardization parameters saved to:", final_std_file, "\n")


# 3. Calculate weights for full dataset
class_counts_full <- table(df_full$landslide_occurrence)

n_minority_full <- min(class_counts_full)
n_majority_full <- max(class_counts_full)

weights_full <- ifelse(
  df_full$landslide_occurrence == 1,
  1,
  n_minority_full / n_majority_full
)

cat("Class distribution in full dataset:\n")
cat("  Class 0 (No Landslide):", class_counts_full[1], "\n")
cat("  Class 1 (Landslide):", class_counts_full[2], "\n")
cat("  Weight for class 0:", round(n_minority_full/n_majority_full, 4), "\n")
cat("  Weight for class 1: 1\n")


# 4. Train final model
final_model <- glm(
  landslide_occurrence ~ ., 
  data = df_full, 
  family = "binomial",
  weights = weights_full
)


# Save final model
final_model_file <- file.path(output_dir, "model", "final_logistic_model.rds")

saveRDS(final_model, final_model_file)

cat("✓ Final model saved to:", final_model_file, "\n")


#------------------------------------------------------------------------------#
# Make predictions on events dataset using the SAME preprocessing
#------------------------------------------------------------------------------#

if(exists("df_events_landslides")) {
  
  cat("\n--- Generating predictions for events dataset ---\n")
  
  # Create a copy for preprocessing
  df_events_pred <- df_events_landslides
  
  # Apply log transformation ONLY to log_vars
  for(var in log_vars) {
    if(var %in% names(df_events_pred)) {
      df_events_pred[[var]] <- log(df_events_pred[[var]] + LOG_SHIFT)
    }
  }
  
  # Remove infinite values ONLY from columns that are numeric AND will be used
  numeric_cols <- names(df_events_pred)[sapply(df_events_pred, is.numeric)]
  for(col in numeric_cols) {
    df_events_pred[[col]][!is.finite(df_events_pred[[col]])] <- NA
  }
  
  # Apply standardization using FINAL parameters
  for(var in numeric_vars_full) {
    if(var %in% names(df_events_pred)) {
      mean_val <- full_means[var]
      sd_val <- full_sds[var]
      
      if(!is.na(sd_val) && sd_val > 0) {
        df_events_pred[[var]] <- (df_events_pred[[var]] - mean_val) / sd_val
      } else {
        df_events_pred[[var]] <- 0
      }
    }
  }
  
  # Ensure variables match final model
  vars_in_model <- names(coef(final_model))[-1]  # Remove intercept
  
  # Create subset with only model variables
  df_events_pred_subset <- df_events_pred[, vars_in_model, drop = FALSE]
  
  # Identify rows where ALL predictor variables are complete
  complete_rows <- complete.cases(df_events_pred_subset)
  
  cat("\nRows with complete predictor data:", sum(complete_rows), "out of", nrow(df_events_pred), "\n")
  cat("Rows with missing predictor data:", sum(!complete_rows), "\n")
  
  # Initialize prediction columns with NA
  df_events_landslides$landslide_probability <- NA
  df_events_landslides$landslide_prediction_025 <- NA
  df_events_landslides$landslide_prediction_050 <- NA
  df_events_landslides$landslide_prediction_075 <- NA
  
  # Make predictions only for complete rows
  if(sum(complete_rows) > 0) {
    predictions_final <- predict(
      final_model,
      newdata = df_events_pred_subset[complete_rows, , drop = FALSE],
      type = "response"
    )
    
    # Assign predictions back to original dataframe
    df_events_landslides$landslide_probability[complete_rows] <- predictions_final
    df_events_landslides$landslide_prediction_025[complete_rows] <- ifelse(predictions_final > 0.25, 1, 0)
    df_events_landslides$landslide_prediction_050[complete_rows] <- ifelse(predictions_final > 0.50, 1, 0)
    df_events_landslides$landslide_prediction_075[complete_rows] <- ifelse(predictions_final > 0.75, 1, 0)
  }
  
  # Summary (only for non-NA predictions)
  valid_predictions <- df_events_landslides$landslide_probability[!is.na(df_events_landslides$landslide_probability)]
  
  if(length(valid_predictions) > 0) {
    cat("\nPrediction summary:\n")
    cat("  Probability range:", round(min(valid_predictions), 4), "to", round(max(valid_predictions), 4), "\n")
    cat("  Mean probability:", round(mean(valid_predictions), 4), "\n")
    cat("\n  Predictions by threshold:\n")
    cat("  Threshold 0.25 - Positives:", sum(df_events_landslides$landslide_prediction_025 == 1, na.rm = TRUE), 
        "| Negatives:", sum(df_events_landslides$landslide_prediction_025 == 0, na.rm = TRUE), "\n")
    cat("  Threshold 0.50 - Positives:", sum(df_events_landslides$landslide_prediction_050 == 1, na.rm = TRUE), 
        "| Negatives:", sum(df_events_landslides$landslide_prediction_050 == 0, na.rm = TRUE), "\n")
    cat("  Threshold 0.75 - Positives:", sum(df_events_landslides$landslide_prediction_075 == 1, na.rm = TRUE), 
        "| Negatives:", sum(df_events_landslides$landslide_prediction_075 == 0, na.rm = TRUE), "\n")
  } else {
    cat("\nWARNING: No valid predictions generated!\n")
  }
  
  # Save predictions
  predictions_file <- file.path(output_dir, "predictions", "events_with_predictions.csv")
  
  write.csv2(df_events_landslides, predictions_file, row.names = FALSE)
  
  cat("\n✓ Dataset with predictions saved to:", predictions_file, "\n")
}

#------------------------------------------------------------------------------#
# Save metadata with all information
#------------------------------------------------------------------------------#

metadata <- list(
  analysis_date = date(),
  model_version = "final",
  validation_method = "Repeated Holdout Validation",
  n_iterations = n_iterations,
  train_test_split = "70/30",
  thresholds_evaluated = c(0.25, 0.50, 0.75),
  
  # Preprocessing parameters
  log_transformed_vars = log_vars,
  log_shift = LOG_SHIFT,
  standardization_parameters = list(
    means = full_means,
    sds = full_sds
  ),
  
  # Class balancing
  class_weights = list(
    class0_weight = n_minority_full/n_majority_full,
    class1_weight = 1
  ),
  
  # Performance from repeated validation
  validation_performance = list(
    auc_roc = list(mean = mean(validation_results$auc_roc, na.rm = TRUE), 
                   sd = sd(validation_results$auc_roc, na.rm = TRUE),
                   ci_lower = ci_lower_auc_roc,
                   ci_upper = ci_upper_auc_roc),
    auc_pr = list(mean = mean(validation_results$auc_pr, na.rm = TRUE), 
                  sd = sd(validation_results$auc_pr, na.rm = TRUE),
                  ci_lower = ci_lower_auc_pr,
                  ci_upper = ci_upper_auc_pr),
    threshold_025 = list(
      accuracy = list(mean = mean(validation_results$accuracy_025, na.rm = TRUE), 
                      sd = sd(validation_results$accuracy_025, na.rm = TRUE)),
      sensitivity = list(mean = mean(validation_results$sensitivity_025, na.rm = TRUE), 
                         sd = sd(validation_results$sensitivity_025, na.rm = TRUE)),
      specificity = list(mean = mean(validation_results$specificity_025, na.rm = TRUE), 
                         sd = sd(validation_results$specificity_025, na.rm = TRUE)),
      precision = list(mean = mean(validation_results$precision_025, na.rm = TRUE), 
                       sd = sd(validation_results$precision_025, na.rm = TRUE)),
      f1_score = list(mean = mean(validation_results$f1_score_025, na.rm = TRUE), 
                      sd = sd(validation_results$f1_score_025, na.rm = TRUE))
    ),
    threshold_050 = list(
      accuracy = list(mean = mean(validation_results$accuracy_050, na.rm = TRUE), 
                      sd = sd(validation_results$accuracy_050, na.rm = TRUE)),
      sensitivity = list(mean = mean(validation_results$sensitivity_050, na.rm = TRUE), 
                         sd = sd(validation_results$sensitivity_050, na.rm = TRUE)),
      specificity = list(mean = mean(validation_results$specificity_050, na.rm = TRUE), 
                         sd = sd(validation_results$specificity_050, na.rm = TRUE)),
      precision = list(mean = mean(validation_results$precision_050, na.rm = TRUE), 
                       sd = sd(validation_results$precision_050, na.rm = TRUE)),
      f1_score = list(mean = mean(validation_results$f1_score_050, na.rm = TRUE), 
                      sd = sd(validation_results$f1_score_050, na.rm = TRUE))
    ),
    threshold_075 = list(
      accuracy = list(mean = mean(validation_results$accuracy_075, na.rm = TRUE), 
                      sd = sd(validation_results$accuracy_075, na.rm = TRUE)),
      sensitivity = list(mean = mean(validation_results$sensitivity_075, na.rm = TRUE), 
                         sd = sd(validation_results$sensitivity_075, na.rm = TRUE)),
      specificity = list(mean = mean(validation_results$specificity_075, na.rm = TRUE), 
                         sd = sd(validation_results$specificity_075, na.rm = TRUE)),
      precision = list(mean = mean(validation_results$precision_075, na.rm = TRUE), 
                       sd = sd(validation_results$precision_075, na.rm = TRUE)),
      f1_score = list(mean = mean(validation_results$f1_score_075, na.rm = TRUE), 
                      sd = sd(validation_results$f1_score_075, na.rm = TRUE))
    )
  ),
  
  # Files
  files = list(
    final_model = final_model_file,
    standardization_params = final_std_file,
    predictions = if(exists("predictions_file")) predictions_file else NULL,
    validation_results = file.path(validation_output_dir, "validation_results_100iterations.csv")
  )
)

metadata_file <- file.path(output_dir, "model", "metadata.rds")
saveRDS(metadata, metadata_file)
cat("✓ Metadata saved to:", metadata_file, "\n")

cat("\n✓ Final model training and predictions completed successfully!\n")

#------------------------------------------------------------------------------# 

##### STEP 15: FINAL SUMMARY #####

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("LOGISTIC REGRESSION MODELING COMPLETED SUCCESSFULLY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nOutputs generated:\n")
cat("  ✓ Validation results (validation/validation_results_100iterations.csv)\n")
cat("  ✓ Performance summary - all thresholds (validation/performance_summary_all_thresholds.csv)\n")
cat("  ✓ AUC summary (validation/auc_summary.csv)\n")
cat("  ✓ Performance summary - threshold 0.25 (validation/performance_summary_threshold_025.csv)\n")
cat("  ✓ Performance summary - threshold 0.50 (validation/performance_summary_threshold_050.csv)\n")
cat("  ✓ Performance summary - threshold 0.75 (validation/performance_summary_threshold_075.csv)\n")
cat("  ✓ Metrics by threshold (validation/metrics_by_threshold.csv)\n")
cat("  ✓ Coefficient stability (validation/coefficients_summary_100iterations.csv)\n")
cat("  ✓ Coefficient complete statistics (validation/coefficients_complete_statistics.csv)\n")
cat("  ✓ Stable coefficients (validation/stable_coefficients.csv)\n")
cat("  ✓ Unstable coefficients (validation/unstable_coefficients.csv)\n")
cat("  ✓ Final model (model/final_logistic_model.rds)\n")
cat("  ✓ Model information (model/model_information.txt)\n")
cat("  ✓ Metadata (model/metadata.rds)\n")
cat("  ✓ Plots (figures/)\n")
cat("  ✓ Coefficient stability table (tables/model_coefficients_stability.csv)\n")
cat("  ✓ Predictions (predictions/events_with_predictions.csv)\n")

cat("\nAll outputs saved to:", output_dir, "\n")
cat(paste(rep("=", 80), collapse = ""), "\n")


#------------------------------------------------------------------------------#
# ##### STEP 16: TEMPORAL HOLDOUT VALIDATION (70/30) #########################
#------------------------------------------------------------------------------#

cat("\n", rep("=", 70), "\n", sep="")
cat("=== TEMPORAL HOLDOUT VALIDATION (70/30) ===\n")
cat(rep("=", 70), "\n\n", sep="")

##### 16.1.1: PREPARE TEMPORAL INFORMATION #####

temporal_dates <- df_events_landslides$start_event

if (!inherits(temporal_dates, "POSIXct")) {
  temporal_dates <- suppressWarnings(
    as.POSIXct(temporal_dates, tz = "GMT")
  )
}

df_temporal <- df_var_selection
df_temporal$event_date_temporal <- temporal_dates

valid_temporal <- !is.na(df_temporal$event_date_temporal)

cat("Total observations available:", nrow(df_temporal), "\n")
cat("Observations with valid event dates:", sum(valid_temporal), "\n")
cat("Observations excluded from temporal validation due to missing dates:",
    sum(!valid_temporal), "\n\n")

df_temporal <- df_temporal[valid_temporal, , drop = FALSE]

##### 16.1.2: DEFINE THE TEMPORAL CUTOFF FROM POSITIVE EVENTS #####

positive_temporal_dates <- df_temporal$event_date_temporal[
  df_temporal$landslide_occurrence == 1
]

positive_temporal_dates <- sort(positive_temporal_dates)

n_positive_total_temporal <- length(positive_temporal_dates)

if (n_positive_total_temporal < 2) {
  stop("Temporal validation cannot be performed: fewer than 2 positive events with valid dates.")
}

temporal_train_proportion <- 0.70

cutoff_index <- ceiling(
  n_positive_total_temporal * temporal_train_proportion
)

cutoff_index <- max(
  1,
  min(cutoff_index, n_positive_total_temporal - 1)
)

temporal_cutoff_date <- positive_temporal_dates[cutoff_index]

cat("Total positive events with valid dates:",
    n_positive_total_temporal, "\n")

cat("Target proportion of positive events in training:",
    temporal_train_proportion, "\n")

cat("Positive-event cutoff position:",
    cutoff_index, "of", n_positive_total_temporal, "\n")

cat("Temporal cutoff date:",
    format(temporal_cutoff_date, "%Y-%m-%d %H:%M:%S"), "\n\n")

##### 16.1.3: CREATE TEMPORAL TRAINING AND VALIDATION SETS #####

temporal_train_raw <- subset(
  df_temporal,
  event_date_temporal <= temporal_cutoff_date
)

temporal_test_raw <- subset(
  df_temporal,
  event_date_temporal > temporal_cutoff_date
)

temporal_train_raw$event_date_temporal <- NULL
temporal_test_raw$event_date_temporal <- NULL

temporal_train_class_raw <- table(
  temporal_train_raw$landslide_occurrence
)

temporal_test_class_raw <- table(
  temporal_test_raw$landslide_occurrence
)

cat("=== TEMPORAL DATASET BEFORE PREPROCESSING ===\n")
cat("Training observations:", nrow(temporal_train_raw), "\n")
cat("Validation observations:", nrow(temporal_test_raw), "\n\n")

cat("Training class distribution:\n")
print(temporal_train_class_raw)

cat("\nValidation class distribution:\n")
print(temporal_test_class_raw)

cat("\nTraining positive proportion:",
    round(
      mean(temporal_train_raw$landslide_occurrence == 1),
      4
    ),
    "\n")

cat("Validation positive proportion:",
    round(
      mean(temporal_test_raw$landslide_occurrence == 1),
      4
    ),
    "\n\n")

##### 16.1.4: LOG TRANSFORMATION #####

temporal_train <- temporal_train_raw
temporal_test  <- temporal_test_raw

for(var in log_vars) {
  
  if(var %in% names(temporal_train)) {
    
    temporal_train[[var]] <- log(
      temporal_train[[var]] + LOG_SHIFT
    )
    
    temporal_test[[var]] <- log(
      temporal_test[[var]] + LOG_SHIFT
    )
  }
}

cat("Log transformation applied using the same variables and shift as the main model.\n")

##### 16.1.5: REMOVE INFINITE AND MISSING VALUES #####

temporal_train[!is.finite(as.matrix(temporal_train))] <- NA
temporal_test[!is.finite(as.matrix(temporal_test))] <- NA

temporal_train <- na.omit(temporal_train)
temporal_test  <- na.omit(temporal_test)

cat("After removal of invalid values:\n")
cat("Training observations:", nrow(temporal_train), "\n")
cat("Validation observations:", nrow(temporal_test), "\n\n")

##### 16.1.6: STANDARDIZE USING TRAINING DATA ONLY #####

temporal_numeric_vars <- names(temporal_train)[
  sapply(temporal_train, is.numeric) &
    names(temporal_train) != "landslide_occurrence"
]

if(length(temporal_numeric_vars) > 0) {
  
  temporal_means <- sapply(
    temporal_train[, temporal_numeric_vars, drop = FALSE],
    mean,
    na.rm = TRUE
  )
  
  temporal_sds <- sapply(
    temporal_train[, temporal_numeric_vars, drop = FALSE],
    function(x) {
      
      if(length(unique(x[!is.na(x)])) <= 1) {
        return(0)
      } else {
        return(sd(x, na.rm = TRUE))
      }
    }
  )
  
  for(var in temporal_numeric_vars) {
    
    if(temporal_sds[var] > 0) {
      
      temporal_train[[var]] <- (
        temporal_train[[var]] -
          temporal_means[var]
      ) / temporal_sds[var]
      
      temporal_test[[var]] <- (
        temporal_test[[var]] -
          temporal_means[var]
      ) / temporal_sds[var]
      
    } else {
      
      temporal_train[[var]] <- 0
      temporal_test[[var]]  <- 0
      
      cat(
        "Warning: variable", var,
        "has zero variance in temporal training data. Set to 0.\n"
      )
    }
  }
}

cat("Standardization performed using temporal training statistics only.\n")

##### 16.1.7: CALCULATE CLASS WEIGHTS USING TEMPORAL TRAINING DATA #####

temporal_train_class_counts <- table(
  temporal_train$landslide_occurrence
)

if(length(temporal_train_class_counts) < 2) {
  stop(
    "Temporal training dataset contains only one response class after preprocessing. ",
    "Logistic regression cannot be fitted."
  )
}

temporal_n_majority <- max(temporal_train_class_counts)
temporal_n_minority <- min(temporal_train_class_counts)

temporal_weights <- ifelse(
  temporal_train$landslide_occurrence == 1,
  1,
  temporal_n_minority / temporal_n_majority
)

cat("\n=== TEMPORAL TRAINING CLASS WEIGHTS ===\n")
print(temporal_train_class_counts)

cat(
  "Class 0 weight:",
  round(temporal_n_minority / temporal_n_majority, 4),
  "\n"
)

cat("Class 1 weight: 1\n")

##### 16.1.8: FIT TEMPORAL LOGISTIC REGRESSION MODEL #####

temporal_model <- glm(
  landslide_occurrence ~ .,
  data = temporal_train,
  family = "binomial",
  weights = temporal_weights
)

temporal_model_file <- file.path(
  output_dir,
  "model",
  "temporal_logistic_model_70_30.rds"
)

saveRDS(
  temporal_model,
  temporal_model_file
)

cat(
  "\nTemporal logistic regression model saved to:",
  temporal_model_file,
  "\n"
)

##### 16.1.9: TEMPORAL MODEL PREDICTIONS AND METRICS FOR ALL THRESHOLDS #####

temporal_predictions <- predict(
  temporal_model,
  newdata = temporal_test,
  type = "response"
)

# Calculate AUC-ROC
temporal_auc_roc <- NA
if(length(unique(temporal_test$landslide_occurrence)) == 2) {
  temporal_roc <- roc(
    temporal_test$landslide_occurrence,
    temporal_predictions,
    quiet = TRUE
  )
  temporal_auc_roc <- as.numeric(auc(temporal_roc))
}

# Calculate AUC-PR
temporal_auc_pr <- calculate_auc_pr(
  temporal_test$landslide_occurrence, 
  temporal_predictions
)

# Calculate metrics for all thresholds
temporal_metrics_025 <- calculate_metrics_threshold(
  temporal_test$landslide_occurrence, 
  temporal_predictions, 
  0.25
)
temporal_metrics_050 <- calculate_metrics_threshold(
  temporal_test$landslide_occurrence, 
  temporal_predictions, 
  0.50
)
temporal_metrics_075 <- calculate_metrics_threshold(
  temporal_test$landslide_occurrence, 
  temporal_predictions, 
  0.75
)

cat("\n=== TEMPORAL VALIDATION METRICS ===\n")
cat("AUC-ROC:", round(temporal_auc_roc, 4), "\n")
cat("AUC-PR:", round(temporal_auc_pr, 4), "\n")

for (thresh_result in list(
  list(name = "0.25", metrics = temporal_metrics_025),
  list(name = "0.50", metrics = temporal_metrics_050),
  list(name = "0.75", metrics = temporal_metrics_075)
)) {
  cat("\n--- Threshold", thresh_result$name, "---\n")
  cat("Confusion Matrix:\n")
  print(thresh_result$metrics$confusion_matrix)
  cat("Accuracy:", round(thresh_result$metrics$accuracy, 4), "\n")
  cat("Sensitivity:", round(thresh_result$metrics$sensitivity, 4), "\n")
  cat("Specificity:", round(thresh_result$metrics$specificity, 4), "\n")
  cat("Precision:", round(thresh_result$metrics$precision, 4), "\n")
  cat("F1-Score:", round(thresh_result$metrics$f1_score, 4), "\n")
}

##### 16.1.10: TEMPORAL VALIDATION SUMMARY #####

temporal_n_pos_train <- sum(
  temporal_train$landslide_occurrence == 1
)

temporal_n_neg_train <- sum(
  temporal_train$landslide_occurrence == 0
)

temporal_n_pos_test <- sum(
  temporal_test$landslide_occurrence == 1
)

temporal_n_neg_test <- sum(
  temporal_test$landslide_occurrence == 0
)

temporal_results <- data.frame(
  validation = "Temporal 70/30",
  cutoff_date = as.character(
    format(
      temporal_cutoff_date,
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  
  train_start = as.character(
    format(
      min(
        df_temporal$event_date_temporal[
          df_temporal$event_date_temporal <= temporal_cutoff_date
        ]
      ),
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  
  train_end = as.character(
    format(
      temporal_cutoff_date,
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  
  validation_start = as.character(
    format(
      min(
        df_events_landslides$start_event[
          !is.na(df_events_landslides$start_event) &
            df_events_landslides$start_event > temporal_cutoff_date
        ]
      ),
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  
  validation_end = as.character(
    format(
      max(
        df_events_landslides$start_event[
          !is.na(df_events_landslides$start_event) &
            df_events_landslides$start_event > temporal_cutoff_date
        ]
      ),
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  
  n_train = nrow(temporal_train),
  n_test = nrow(temporal_test),
  
  n_pos_train = temporal_n_pos_train,
  n_neg_train = temporal_n_neg_train,
  
  n_pos_test = temporal_n_pos_test,
  n_neg_test = temporal_n_neg_test,
  
  positive_rate_train = temporal_n_pos_train /
    nrow(temporal_train),
  
  positive_rate_test = temporal_n_pos_test /
    nrow(temporal_test),
  
  auc_roc = temporal_auc_roc,
  auc_pr = temporal_auc_pr,
  
  # Threshold 0.25
  accuracy_025 = temporal_metrics_025$accuracy,
  sensitivity_025 = temporal_metrics_025$sensitivity,
  specificity_025 = temporal_metrics_025$specificity,
  precision_025 = temporal_metrics_025$precision,
  f1_score_025 = temporal_metrics_025$f1_score,
  
  # Threshold 0.50
  accuracy_050 = temporal_metrics_050$accuracy,
  sensitivity_050 = temporal_metrics_050$sensitivity,
  specificity_050 = temporal_metrics_050$specificity,
  precision_050 = temporal_metrics_050$precision,
  f1_score_050 = temporal_metrics_050$f1_score,
  
  # Threshold 0.75
  accuracy_075 = temporal_metrics_075$accuracy,
  sensitivity_075 = temporal_metrics_075$sensitivity,
  specificity_075 = temporal_metrics_075$specificity,
  precision_075 = temporal_metrics_075$precision,
  f1_score_075 = temporal_metrics_075$f1_score
)

cat("\n", rep("=", 70), "\n", sep="")
cat("=== TEMPORAL VALIDATION RESULTS SUMMARY ===\n")
cat(rep("=", 70), "\n\n", sep="")

cat("Temporal cutoff:", format(temporal_cutoff_date, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Training positives:", temporal_n_pos_train, "\n")
cat("Validation positives:", temporal_n_pos_test, "\n")
cat("Training negatives:", temporal_n_neg_train, "\n")
cat("Validation negatives:", temporal_n_neg_test, "\n\n")

cat("AUC-ROC:", round(temporal_auc_roc, 4), "\n")
cat("AUC-PR:", round(temporal_auc_pr, 4), "\n")

cat("\n--- THRESHOLD 0.25 ---\n")
cat("Accuracy:", round(temporal_metrics_025$accuracy, 4), "\n")
cat("Sensitivity:", round(temporal_metrics_025$sensitivity, 4), "\n")
cat("Specificity:", round(temporal_metrics_025$specificity, 4), "\n")
cat("Precision:", round(temporal_metrics_025$precision, 4), "\n")
cat("F1-Score:", round(temporal_metrics_025$f1_score, 4), "\n")

cat("\n--- THRESHOLD 0.50 ---\n")
cat("Accuracy:", round(temporal_metrics_050$accuracy, 4), "\n")
cat("Sensitivity:", round(temporal_metrics_050$sensitivity, 4), "\n")
cat("Specificity:", round(temporal_metrics_050$specificity, 4), "\n")
cat("Precision:", round(temporal_metrics_050$precision, 4), "\n")
cat("F1-Score:", round(temporal_metrics_050$f1_score, 4), "\n")

cat("\n--- THRESHOLD 0.75 ---\n")
cat("Accuracy:", round(temporal_metrics_075$accuracy, 4), "\n")
cat("Sensitivity:", round(temporal_metrics_075$sensitivity, 4), "\n")
cat("Specificity:", round(temporal_metrics_075$specificity, 4), "\n")
cat("Precision:", round(temporal_metrics_075$precision, 4), "\n")
cat("F1-Score:", round(temporal_metrics_075$f1_score, 4), "\n")

##### 16.1.11: COMPARE RANDOM AND TEMPORAL VALIDATION #####

random_mean_metrics <- data.frame(
  validation = "Repeated random holdout",
  cutoff_date = NA_character_,
  train_start = NA_character_,
  train_end = NA_character_,
  validation_start = NA_character_,
  validation_end = NA_character_,
  n_train = NA_integer_,
  n_test = NA_integer_,
  n_pos_train = mean(validation_results$n_pos_train, na.rm = TRUE),
  n_neg_train = NA_integer_,
  n_pos_test = mean(validation_results$n_pos_test, na.rm = TRUE),
  n_neg_test = NA_integer_,
  positive_rate_train = NA_real_,
  positive_rate_test = NA_real_,
  auc_roc = mean(validation_results$auc_roc, na.rm = TRUE),
  auc_pr = mean(validation_results$auc_pr, na.rm = TRUE),
  
  accuracy_025 = mean(validation_results$accuracy_025, na.rm = TRUE),
  sensitivity_025 = mean(validation_results$sensitivity_025, na.rm = TRUE),
  specificity_025 = mean(validation_results$specificity_025, na.rm = TRUE),
  precision_025 = mean(validation_results$precision_025, na.rm = TRUE),
  f1_score_025 = mean(validation_results$f1_score_025, na.rm = TRUE),
  
  accuracy_050 = mean(validation_results$accuracy_050, na.rm = TRUE),
  sensitivity_050 = mean(validation_results$sensitivity_050, na.rm = TRUE),
  specificity_050 = mean(validation_results$specificity_050, na.rm = TRUE),
  precision_050 = mean(validation_results$precision_050, na.rm = TRUE),
  f1_score_050 = mean(validation_results$f1_score_050, na.rm = TRUE),
  
  accuracy_075 = mean(validation_results$accuracy_075, na.rm = TRUE),
  sensitivity_075 = mean(validation_results$sensitivity_075, na.rm = TRUE),
  specificity_075 = mean(validation_results$specificity_075, na.rm = TRUE),
  precision_075 = mean(validation_results$precision_075, na.rm = TRUE),
  f1_score_075 = mean(validation_results$f1_score_075, na.rm = TRUE)
)

validation_comparison <- bind_rows(
  random_mean_metrics,
  temporal_results
)

##### 16.1.12: SAVE TEMPORAL VALIDATION RESULTS #####

temporal_validation_dir <- file.path(
  validation_output_dir,
  "temporal_validation"
)

if(!dir.exists(temporal_validation_dir)) {
  dir.create(
    temporal_validation_dir,
    recursive = TRUE
  )
}

# Main temporal result
write.csv2(
  temporal_results,
  file.path(
    temporal_validation_dir,
    "temporal_validation_70_30_results.csv"
  ),
  row.names = FALSE
)

# Comparison with random repeated holdout
write.csv2(
  validation_comparison,
  file.path(
    temporal_validation_dir,
    "random_vs_temporal_validation.csv"
  ),
  row.names = FALSE
)

# Temporal model coefficients
temporal_coefficients <- data.frame(
  variable = names(
    coef(temporal_model)
  ),
  coefficient = as.numeric(
    coef(temporal_model)
  )
)

write.csv2(
  temporal_coefficients,
  file.path(
    temporal_validation_dir,
    "temporal_model_coefficients_70_30.csv"
  ),
  row.names = FALSE
)

# Save preprocessing parameters used in temporal validation
temporal_standardization_params <- list(
  means = temporal_means,
  sds = temporal_sds,
  log_shift = LOG_SHIFT,
  log_vars = log_vars,
  cutoff_date = temporal_cutoff_date,
  train_proportion_positive = temporal_train_proportion
)

saveRDS(
  temporal_standardization_params,
  file.path(
    temporal_validation_dir,
    "temporal_standardization_parameters_70_30.rds"
  )
)

# Save the actual train/test datasets used in temporal validation
write.csv2(
  temporal_train,
  file.path(
    temporal_validation_dir,
    "temporal_training_dataset_70_30.csv"
  ),
  row.names = FALSE
)

write.csv2(
  temporal_test,
  file.path(
    temporal_validation_dir,
    "temporal_validation_dataset_70_30.csv"
  ),
  row.names = FALSE
)

cat("\nTemporal validation results saved to:\n")
cat(temporal_validation_dir, "\n")

#------------------------------------------------------------------------------#
# END OF TEMPORAL VALIDATION
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# FINAL COMPREHENSIVE SUMMARY
#------------------------------------------------------------------------------#

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("FINAL COMPREHENSIVE SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\n=== REPEATED RANDOM HOLDOUT VALIDATION (100 iterations) ===\n")
cat("\nAUC Metrics:\n")
cat("  AUC-ROC:", round(mean(validation_results$auc_roc, na.rm = TRUE), 4), 
    "±", round(sd(validation_results$auc_roc, na.rm = TRUE), 4),
    "(95% CI:", round(ci_lower_auc_roc, 4), "-", round(ci_upper_auc_roc, 4), ")\n")
cat("  AUC-PR:", round(mean(validation_results$auc_pr, na.rm = TRUE), 4), 
    "±", round(sd(validation_results$auc_pr, na.rm = TRUE), 4),
    "(95% CI:", round(ci_lower_auc_pr, 4), "-", round(ci_upper_auc_pr, 4), ")\n")

for (suffix in c("025", "050", "075")) {
  threshold_label <- paste0("0.", substr(suffix, 2, 3))
  cat("\nThreshold", threshold_label, ":\n")
  cat("  Accuracy:", round(mean(validation_results[[paste0("accuracy_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("accuracy_", suffix)]], na.rm = TRUE), 4), "\n")
  cat("  Sensitivity:", round(mean(validation_results[[paste0("sensitivity_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("sensitivity_", suffix)]], na.rm = TRUE), 4), "\n")
  cat("  Specificity:", round(mean(validation_results[[paste0("specificity_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("specificity_", suffix)]], na.rm = TRUE), 4), "\n")
  cat("  Precision:", round(mean(validation_results[[paste0("precision_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("precision_", suffix)]], na.rm = TRUE), 4), "\n")
  cat("  F1-Score:", round(mean(validation_results[[paste0("f1_score_", suffix)]], na.rm = TRUE), 4), 
      "±", round(sd(validation_results[[paste0("f1_score_", suffix)]], na.rm = TRUE), 4), "\n")
}

cat("\n=== TEMPORAL HOLDOUT VALIDATION (70/30) ===\n")
cat("\nAUC Metrics:\n")
cat("  AUC-ROC:", round(temporal_auc_roc, 4), "\n")
cat("  AUC-PR:", round(temporal_auc_pr, 4), "\n")

cat("\nThreshold 0.25:\n")
cat("  Accuracy:", round(temporal_metrics_025$accuracy, 4), "\n")
cat("  Sensitivity:", round(temporal_metrics_025$sensitivity, 4), "\n")
cat("  Specificity:", round(temporal_metrics_025$specificity, 4), "\n")
cat("  Precision:", round(temporal_metrics_025$precision, 4), "\n")
cat("  F1-Score:", round(temporal_metrics_025$f1_score, 4), "\n")

cat("\nThreshold 0.50:\n")
cat("  Accuracy:", round(temporal_metrics_050$accuracy, 4), "\n")
cat("  Sensitivity:", round(temporal_metrics_050$sensitivity, 4), "\n")
cat("  Specificity:", round(temporal_metrics_050$specificity, 4), "\n")
cat("  Precision:", round(temporal_metrics_050$precision, 4), "\n")
cat("  F1-Score:", round(temporal_metrics_050$f1_score, 4), "\n")

cat("\nThreshold 0.75:\n")
cat("  Accuracy:", round(temporal_metrics_075$accuracy, 4), "\n")
cat("  Sensitivity:", round(temporal_metrics_075$sensitivity, 4), "\n")
cat("  Specificity:", round(temporal_metrics_075$specificity, 4), "\n")
cat("  Precision:", round(temporal_metrics_075$precision, 4), "\n")
cat("  F1-Score:", round(temporal_metrics_075$f1_score, 4), "\n")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ALL ANALYSES COMPLETED SUCCESSFULLY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nAll outputs saved to:", output_dir, "\n")
cat("\nDirectory structure:\n")
cat("  ├── figures/          - All plots and visualizations\n")
cat("  ├── tables/           - VIF and coefficient stability tables\n")
cat("  ├── model/            - Final models, standardization params, metadata\n")
cat("  ├── predictions/      - Predictions on events dataset\n")
cat("  ├── reproducibility/  - Session info and package versions\n")
cat("  └── validation/       - All validation results\n")
cat("      ├── temporal_validation/  - Temporal validation results\n")
cat("      └── *.csv                  - Repeated holdout results\n")

# END OF CODE ------------------------------------------------------------------#

