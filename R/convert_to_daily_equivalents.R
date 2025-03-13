#' @title convert_to_daily_equivalents
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname convert_to_daily_equivalents
#' @export

convert_to_daily_equivalents <- function(data){
  load_constants(environment = environment())
  dietfreq <- diet_frequency_table()
  for(v in 1:length(variable_crosswalk$nci)){
    ncivar <- variable_crosswalk[v, "nci", drop = TRUE]
    type <- variable_crosswalk[v, "type", drop = TRUE]
    if(ncivar %in% colnames(data)){
      if(type == "beverage"){
        data[[ncivar]] <- sapply(
          1:length(data[[ncivar]]), FUN = function(x) {
            ifelse(
              is.na(data[[ncivar]][x]), NA,
              dietfreq[which(dietfreq$category == data[[ncivar]][x]), "beverages"])
          })
      } else if(type == "food"){
        data[[ncivar]] <- sapply(
          1:length(data[[ncivar]]), FUN = function(x) {
            ifelse(
              is.na(data[[ncivar]][x]), NA,
              dietfreq[which(dietfreq$category == data[[ncivar]][x]), "foods"])
          })
      }
    }
  }
  return(data)
}
