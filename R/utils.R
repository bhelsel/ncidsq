#' @title load_constants
#' @description FUNCTION_DESCRIPTION
#' @param environment PARAM_DESCRIPTION, Default: .GlobalEnv
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname load_constants
#' @export


load_constants <- function(environment = .GlobalEnv){
  load("inst/extdata/constants.RData")
  invisible(lapply(names(constants), function(x) assign(x, constants[[x]], envir = environment)))
}

#' @title remove_constants
#' @description FUNCTION_DESCRIPTION
#' @param environment PARAM_DESCRIPTION, Default: .GlobalEnv
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname remove_constants
#' @export


remove_constants <- function(environment = .GlobalEnv){
  load("inst/extdata/constants.RData")
  rm(list = names(constants), envir = environment)
}


#' @title update_r_data_from_excel
#' @description FUNCTION_DESCRIPTION
#' @param xlsx_directory PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#' @rdname update_r_data_from_excel
#' @keywords internal
#' @importFrom readxl read_xlsx
#' @importFrom stats setNames

update_r_data_from_excel <- function(xlsx_directory){
  all_file_names <- c()
  for(file in list.files(xlsx_directory, pattern = ".xlsx$", full.names = TRUE)){
    name <- gsub("[.]", "_", gsub(".xlsx$", "", basename(file)))
    all_file_names <- c(all_file_names, name)
    assign(name, readxl::read_xlsx(file))
  }
  constants <- stats::setNames(lapply(all_file_names, function(x) get(x, envir = environment())), all_file_names)
  save(constants, file = file.path(dirname(xlsx_directory), "constants.RData"))
}


#' @title rename_variables
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param reference PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname rename_variables
#' @export
#' @importFrom stats setNames

rename_variables <- function(data, reference = "kumc"){
  load_constants(environment = environment())
  rename_vector <- stats::setNames(variable_crosswalk$nci, variable_crosswalk[[reference]])
  colnames(data) <-
    ifelse(colnames(data) %in% names(rename_vector),
      rename_vector[colnames(data)], colnames(data))
  for(i in names(data)){
    attr(data[[i]], "label") <- variable_crosswalk$labels[which(variable_crosswalk$nci == i)]
  }
  attr(data, "variables") <- variable_crosswalk$nci
  attr(data, "outliers") <- variable_crosswalk$outliers
  return(data)
}

#' @title diet_frequency_table
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname diet_frequency_table
#' @keywords internal

diet_frequency_table <- function(){
  category <- c(
    "Never", "1 time last month", "2-3 times last month", "1 time per week",
    "2 times per week", "3-4 times per week", "5-6 times per week", "1 time per day",
    "2 or more times per day", "2-3 times per day", "4-5 times per day",
    "6 ore more times per day"
  )
  foods <- c(0, 0.033, 0.083, 0.143, 0.286, 0.5, 0.786, 1, 2, NA, NA, NA)
  beverages <- c(0, 0.033, 0.083, 0.143, 0.286, 0.5, 0.786, 1, NA, 2.5, 4.5, 6)
  data.frame(category, foods, beverages)
}


#' @title retrieve_norms
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname retrieve_norms
#' @keywords internal

retrieve_norms <- function(gender = c("male", "female"), type = c("nhanes", "recommendations")){
  gender <- match.arg(gender, several.ok = TRUE); type <- match.arg(type, several.ok = TRUE)
  load_constants(environment = environment())
  variables <- as.character(sapply(gender, function(x) paste0(x, "_", type)))
  return(nutrient_norms[, c("variable", "label", "unit", variables)])
}



