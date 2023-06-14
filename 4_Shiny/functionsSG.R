classes <- dplyr::tibble(
  primary_class = "cdmSnapshot",
  column_name = c("attribute", "value"),
  column_type = c("chr", "char")
) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "summarisedResult",
    column_name = c("attribute", "value"),
    column_type = c("chr", "char")
  ))

subClasses <- dplyr::tibble(
  primary_class = "summarisedResult",
  secondary_class = "summarisedAtc",
  distinguish_content = "summariseCodelistATC",
  distinguish_variable = "generated_by"
)

result <- list(
  explore = c(
    "cdm_name", "group_name", "group_level", "strata_name", "strata_level"
  ),
  uniqueExplore = c(T, T, T, T, T),
  fun = "showResult"
)
snapshot <- list(
  fun = "showSnapshot"
)

tabs <- list(
  "cdmSnapshot" = list("Database snapshot" = snapshot),
  "summarisedAtc" = list("ATC Characterization" = result)
)

indentifyClass <- function(x) {
  cl <- colnames(x)
  classFile <- sapply(unique(classes$primary_class), function(clas) {
    columns <- clas$column_name[clas$primary_class == clas]
    if (length(cl) == length(columns)) {
      return(all(sort(cl) == sort(columns)))
    } else {
      return(FALSE)
    }
  }) %>% 
    unlist()
  return(names(classes)[classFile])
}
identifySubclass <- function(x, primaryClass) {
  possibleSubClasses <- subClasses %>%
    dplyr::filter(.data$primery_class == .env$primaryClass)
  idSubClass <- rep(FALSE, nrow(possibleSubClasses))
  for (k in 1:nrow(possibleSubClasses)) {
    variable <- possibleSubClasses$distinguish_variable[k]
    content <- possibleSubClasses$distinguish_content[k]
    idSubClass[k] <- x %>%
      dplyr::select(dplyr::all_of(variable)) %>%
      dplyr::distinct() %>%
      dplyr::filter(grepl(content, .data[[variable]])) %>%
      nrow() > 0
  }
  return(possibleSubClasses$secondary_class[idSubClass])
}
readFile<- function(fileName) {
  x <- readr::read_csv(unzip(fileName), show_col_types = FALSE, n_max = 1)
  colTypeSet <- classes %>%
    dplyr::filter(.data$primary_class == !!indentifyClass(x))
  x <- readr::read_csv(unzip(fileName), col_types = colTypes(colTypeSet))
  class(x) <- c(identifySubclass(x, primaryClass), primaryClass, class(x))
  return(x)
}
colTypes <- function(colTypeSet) {
  colType <- list()
  
}
