#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data get_cereal_daily_equivalents
#' @param cereal_names PARAM_DESCRIPTION, Default: c("cereal_type1", "cereal_type2")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{join_by}}
#'  \code{\link[rlang]{sym}}
#' @rdname get_cereal_daily_equivalents
#' @export
#' @importFrom dplyr left_join join_by
#' @importFrom rlang sym

get_cereal_daily_equivalents <- function(data, cereal_names = c("cereal_type1", "cereal_type2")){

  load_constants(environment = environment())

  for(v in 1:length(cereal_names)){
    data[[cereal_names[v]]] <- gsub("\u00D5","'", iconv(data[[cereal_names[v]]], from = "ISO-8859-1", to = "UTF-8"))
    assign(sprintf("n_%s", cereal_names[v]), length(data[data[[cereal_names[v]]] != "", cereal_names[v], drop = TRUE]))
    if(v == 1){
      data <- data %>% dplyr::left_join(calib_DSQ_cereal_ntile, by = dplyr::join_by(!!rlang::sym(cereal_names[v]) == Cereal_Name))
      if(get(sprintf("n_%s", cereal_names[v])) != length(data[!is.na(data$food_code), "food_code", drop = TRUE])){
        stop("All of the first cereal choices did not match with the dietary screening questionnaire")
      }
    } else if(v == 2){
      data <- data %>% dplyr::left_join(calib_DSQ_cereal_ntile, by = dplyr::join_by(!!rlang::sym(cereal_names[v]) == Cereal_Name), suffix = c("1", "2"))
      if(get(sprintf("n_%s", cereal_names[v])) != length(data[!is.na(data$food_code2), "food_code2", drop = TRUE])){
        stop("All of the second cereal choices did not match with the dietary screening questionnaire")
      }
    }
  }

  vars <- c("whgnt", "sugnt", "calcnt", "fibnt")
  abbr <- c("wg", "as", "cm", "fb")
  for(v in 1:length(vars)){
    temp <- data[, c(which(colnames(data) == "hccerxpd"), grep(vars[v], colnames(data)))]
    temp[, paste0(abbr[v], 1:3, "f")] <- 0
    for(i in 1:nrow(temp)){
      if(!is.na(temp[[i, 2]]) & is.na(temp[[i, 3]])){
        col2 <- sprintf("%s%sf", abbr[v], temp[[i, 2]])
        temp[[i, col2]] <- temp[[i, col2]] + (temp[[i, 2]] * temp[[i, 1]])
      } else if(!is.na(temp[[i, 2]]) & !is.na(temp[[i, 3]])){
        col2 <- sprintf("%s%sf", abbr[v], temp[[i, 2]])
        temp[[i, col2]] <- temp[[i, col2]] + (temp[[i, 2]] * temp[[i, 1]] * 0.75)
        col3 <- sprintf("%s%sf", abbr[v], temp[[i, 3]])
        temp[[i, col3]] <- temp[[i, col3]] + (temp[[i, 3]] * temp[[i, 1]] * 0.25)
      }
    }
    data <- cbind(data, temp[, 4:6])
  }
  class(data) <- c("tbl_df", "tbl", "data.frame")
  return(data)
}
