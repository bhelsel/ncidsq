calculate_age_at_visit <- function(data, id, timepoint, birthdate, visitdate){

  if(length(visitdate) == 3){
    visitdate <- sprintf("%s-%s-%s", data[[visitdate[1]]], data[[visitdate[2]]], data[[visitdate[3]]])
  } else{
    visitdate <- data[[visitdate]]
  }

  if(length(birthdate) == 3){
    birthdate <- sprintf("%s-%s-%s", data[[birthdate[1]]], data[[birthdate[2]]], data[[birthdate[3]]])
  } else{
    birthdate <- data[[birthdate]]
  }

  tryCatch({
    visitdate <- as.Date(visitdate, "%Y-%m-%d")
    if(all(is.na(visitdate))){
      stop("Date Error")
    } else{
      visit <- data.frame(data[[id]], data[[timepoint]], visitdate)
      visit <- visit[!is.na(visitdate), ]
      names(visit) <- c(id, timepoint, "visitdate")
    }
  },
  error = function(e){
    stop("Could not convert visitdate to a date.")
  })

  tryCatch({
    birthdate <- as.Date(birthdate, "%Y-%m-%d")
    if(all(is.na(birthdate))){
      stop("Date Error")
    } else{
      dob <- data.frame(data[[id]], birthdate)
      dob <- dob[!is.na(birthdate), ]
      names(dob) <- c(id, "birthdate")
    }
  },
  error = function(e){
    stop("Could not convert birthdate to a date.")
  })

  data <-
    data %>%
    merge(y = dob, by = id) %>%
    merge(y = visit, by = c(id, timepoint), all.x = TRUE) %>%
    {.[!is.na(.$visitdate), ]}

  data$age_at_visit <-
    as.Date(data$visitdate, "%Y-%m-%d") %>%
    difftime(., as.Date(data$birthdate, "%Y-%m-%d"), units = "days") %>%
    {floor(as.numeric(. / 365.25))}

  data$bcage <- as.numeric(
    cut(data$age_at_visit,
        breaks = c(seq(2, 18, 2), seq(26, 46, 10), 61, 70, 100),
        labels = 1:14, right = FALSE
    )
  )

  data$kidgrp <- ifelse(data$age_at_visit >= 2 & data$age_at_visit <= 11, 1, 0)
  data$teengrp <- ifelse(data$age_at_visit >= 12 & data$age_at_visit <= 17, 1, 0)
  data$subject_birthdate <- data$birthdate <- NULL # Remove after age is created
  cols <- c(id, timepoint, "visitdate", "age_at_visit", "bcage", "kidgrp", "teengrp")
  data <- data[, c(cols, names(data)[-which(names(data) %in% cols)])]
  class(data) <- c("tbl_df", "tbl", "data.frame")
  return(data)
}
