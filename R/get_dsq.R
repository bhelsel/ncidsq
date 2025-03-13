#' @title get_dsq
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @param timepoint PARAM_DESCRIPTION
#' @param birthdate PARAM_DESCRIPTION
#' @param visitdate PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname get_dsq
#' @export
#' @importFrom dplyr `%>%`

get_dsq <- function(data, id = "ptid", timepoint = "redcap_event_name",
                    birthdate = "subject_birthdate", visitdate = c("visityr", "visitmo", "visitday"),
                    reference = "kumc"){
  data %>%
    calculate_age_at_visit(id, timepoint, birthdate, visitdate) %>%
    rename_variables(reference) %>%
    apply_factor_labels() %>%
    convert_to_daily_equivalents() %>%
    get_cereal_daily_equivalents() %>%
    portion_size_adjust() %>%
    calculate_predicted_probabilities()

}
