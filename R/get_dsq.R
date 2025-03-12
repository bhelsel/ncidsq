

get_dsq <- function(data){
  data %>%
    calculate_age_at_visit(
      id = "ptid", timepoint = "redcap_event_name",
      birthdate = "subject_birthdate",
      visitdate = c("visityr", "visitmo", "visitday")
    ) %>%
    rename_variables() %>%
    apply_factor_labels() %>%
    convert_to_daily_equivalents() %>%
    get_cereal_daily_equivalents() %>%
    portion_size_adjust() %>%
    calculate_predicted_probabilities()

}
