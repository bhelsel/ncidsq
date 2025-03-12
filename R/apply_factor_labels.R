apply_factor_labels <- function(data){
  load_constants(environment = environment())
  common <- c(
    "0: Never", "1: 1 time last month",
    "2: 2-3 times last month", "3: 1 time per week",
    "4: 2 times per week", "5: 3-4 times per week",
    "6: 5-6 times per week", "7: 1 time per day"
  )
  beverages <- c(common, "8: 2-3 times per day", "9: 4-5 times per day", "10: 6 or more times per day")
  foods <- c(common, "8: 2 or more times per day")
  if(!all(variable_crosswalk[, "nci", drop = TRUE] %in% colnames(data))) data <- rename_variables(data)
  for(v in 1:length(variable_crosswalk$nci)){
    ncivar <- variable_crosswalk[v, "nci", drop = TRUE]
    type <- variable_crosswalk[v, "type", drop = TRUE]
    if(length(attr(data[[ncivar]], "label")) == 1){
      customlabel <- attr(data[[ncivar]], "label")
    } else{
      customlabel <- NULL
    }
    if(type == "beverage"){
      data[[ncivar]] <- factor(data[[ncivar]], levels = 0:10, labels = gsub("\\b(10|[0-9]): ", "", beverages))
    } else if(type == "food"){
      data[[ncivar]] <- factor(data[[ncivar]], levels = 0:8, labels = gsub("[0-9]: ", "", foods))
    }
    if(length(customlabel) == 1) attr(data[[ncivar]], "label") <- customlabel
  }
  return(data)
}
