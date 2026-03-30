get_ref_question <- function(x) {
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}

get_value_from_uuid <- function(uuid, column, raw_data, raw_loop) {
  if (str_detect(uuid, "_")) {
    value <- raw_loop[[column]][raw_loop$uuid == uuid]
  } else {
    value <- raw_data[[column]][raw_data$uuid == uuid]
  }
  return(value)
}

get_label_from_name <- function(list.name, name, kobo_choices) {
  return(kobo_choices$label[
    kobo_choices$list_name == list.name & kobo_choices$name == name
  ])
}
