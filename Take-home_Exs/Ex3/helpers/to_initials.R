to_initials <- function(name) {
  strsplit(name, "[^[:alnum:]]")[[1]] %>%  # Split when non-alphanumeric
    substr(1, 1) %>% # Get first letter
    paste0(collapse = "")
}