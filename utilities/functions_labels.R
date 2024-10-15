# FUNCTIONS AND OTHER

# Study feature labels
feature_labels <- list(
  Publication.Year = "Publication Year",
  globalCitationsCount = "Number of Citations",
  location = "Location",
  SDG_theme = "SDG Theme",
  classification_type = "Classification Type",
  model_group = "Model Group",
  ancillary = "Ancillary Data",
  indices = "Indices",
  RS_device_type = "Remote Sensing Type",
  RS_device_group = "Device Group",
  no_band_group = "Number of Spectral Bands",
  RS_spatital_res_grouped = "Spatial Resolution",
  Confusion_matrix = "Confusion Matrix",
  OA_reported = "Overall Accuracy",
  number_classes = "Number of Classes",
  fraction_majority_class = "Majority-class Proportion",
  sample_size = "Sample Size"
)



# function to make results table
extract_parameters <- function(model_with_features, data) {
  
  model_without_features = rma.mv(yi, vi,
                                  data = data,
                                  tdist = TRUE,
                                  random = ~ 1 | AuthorYear / esid,
                                  method = "REML",
                                  test = "t",
                                  dfs = "contain"
  )
  
  extract_parameters <- data.frame(
    dataset = NA,
    sig_lvl2 = round(model_without_features$sigma2[2], 3), 
    sig_lvl3 = round(model_without_features$sigma2[1], 3), 
    sig_lvl2_with = round(model_with_features$sigma2[2], 3), 
    sig_lvl3_with = round(model_with_features$sigma2[1], 3), 
    QE = round(model_with_features$QE,0), 
    df_Q = model_with_features$QEdf,
    p_Q = round(model_with_features$QEp,3), 
    f_test = if (!is.null(model_with_features$formula.mods))
      round(model_with_features$QM, 0)
    else
      NA,
    df_F = if (!is.null(model_with_features$formula.mods))
      model_with_features$QMdf[1]
    else
      NA,
    p_F = if (!is.null(model_with_features$formula.mods))
      round(model_with_features$QMp, 3)
    else
      NA,
    I2_lvl2 = dmetar::var.comp(model_with_features)$results[2, 2],
    I2_lvl3 = dmetar::var.comp(model_with_features)$results[3, 2], 
    
    R2_lvl2 = (round(1 - (model_with_features$sigma2[2] / 
                            model_without_features$sigma2[2]), 3) * 100),
    
    R2_lvl3 = round(1 - (model_with_features$sigma2[1] / 
                           model_without_features$sigma2[1]), 3) * 100
  )
  return(list(parameters_table = extract_parameters, model_results = model_with_features))
}


group_small_category <- function(data) {
  # frequency of each category in the 'model' column
  model_counts <- table(data$model_group)
  
  # categories with a count less than 5 & replace small categories with 'Other'
  small_cat <- names(model_counts[model_counts < 5])
  data$model_group <- as.character(data$model_group)
  data$model_group[data$model_group %in% small_cat] <- 'Other'
  data$model_group <- factor(data$model_group)
  
  return(data)
}
