#' @title portion_size_adjust
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{join_by}}
#' @rdname portion_size_adjust
#' @export
#' @importFrom dplyr left_join join_by

portion_size_adjust <- function(data){
  load_constants(environment = environment())
  joined_df <- dplyr::left_join(x = data, y = calib_portion_size, by = dplyr::join_by(sex == gender, bcage == agegrp))
  for(fm in 1:length(fm_portion_size$portionsfm)){
    newvar <- strsplit(fm_portion_size$portionsfm[fm], " = ")[[1]][1]
    currfm <- strsplit(fm_portion_size$portionsfm[fm], " = ")[[1]][2]
    data[[newvar]] <- eval(parse(text = currfm), envir = joined_df)
  }
  return(data)
}
