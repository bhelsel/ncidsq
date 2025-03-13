#' @title calculate_predicted_probabilities
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{join_by}}
#' @rdname calculate_predicted_probabilities
#' @export
#' @importFrom dplyr left_join join_by

calculate_predicted_probabilities <- function(data){
  load_constants(environment = environment())
  joined_df <- dplyr::left_join(x = data, y = calib_equation_coeff, by = dplyr::join_by(sex == gender))
  for(fm in 1:length(fm_equation_coeff$equationsfm)){
    newvar <- strsplit(fm_equation_coeff$equationsfm[fm], " = ")[[1]][1]
    currfm <- strsplit(fm_equation_coeff$equationsfm[fm], " = ")[[1]][2]
    data[[newvar]] <- eval(parse(text = currfm), envir = joined_df)
    if(grepl("_low|_high", newvar)){
      data[[newvar]] <- pmin(pmax(data[[newvar]], -100), 100)  # Apply the capping
      data[[newvar]] <- exp(data[[newvar]]) / (1 + exp(data[[newvar]]))  # Logistic transformation
    } else {
      data[[newvar]] <- ifelse(!is.na(data[[newvar]]) & data[[newvar]] < 0, 0, data[[newvar]])
    }
    attr(data[[newvar]], "label") <- fm_equation_coeff$label[fm]
  }
  return(data)
}

