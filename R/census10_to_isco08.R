#' Convert Census 2010 to ISCO08
#'
#' Maps Census 2010 occupational codes to ISCO08 occupational codes using a predefined crosswalk.
#'
#' @param data A data frame containing Census 2010 codes.
#' @param census_col The column name containing Census 2010 codes.
#' @param one_to_one Logical. If TRUE, selects the closest match by title.
#' @return A data frame with the ISCO08 mappings.
#' @export

census10_to_isco08 <- function(data, census_col, one_to_one = FALSE) {
  # Required Libraries
  library(dplyr)
  library(stringdist)
  if (!exists("crosswalk_census2010_isco08", envir = environment())) {
    utils::data("crosswalk_census2010_isco08", package = "occupationsconvert")
  }
  # Ensure input column is numeric
  data[[census_col]] <- as.numeric(data[[census_col]])

  # Raise a warning if one_to_one is TRUE
  if (one_to_one) {
    warning("one_to_one is TRUE: ISCO08 is chosen based on the closest match between occ2010title and isco08_title.")
  }

  # Raise warnings for certain Census codes manually mapped to SOC2010
  manual_mappings <- c(
    "1020" = "15-1132",
    "2025" = "21-1093",
    "2550" = "25-9011",
    "3655" = "31-9091",
    "3955" = "33-9092",
    "4220" = "37-2011",
    "4460" = "39-4011",
    "6940" = "47-5011",
    "7100" = "49-2091",
    "7330" = "49-9041",
    "7630" = "49-9093",
    "9260" = "53-4011",
    "9420" = "53-6011"
  )
  warning(
    "The following occ2010 codes were manually mapped to specific SOC2010 values:\n",
    paste(names(manual_mappings), manual_mappings, sep = " -> ", collapse = "; ")
  )

  # Handle one-to-one mapping based on title similarity
  if (one_to_one) {
    # Add ISCO08 title column to the crosswalk for comparison
    crosswalk_with_titles <- crosswalk_census2010_isco08 %>%
      mutate(isco08_title = isco08)  # Replace with actual ISCO08 title column if available

    # For each Census2010 code, select the ISCO08 code with the closest title match
    crosswalk_one_to_one <- crosswalk_with_titles %>%
      group_by(occ2010) %>%
      mutate(similarity = stringdist::stringdist(occ2010title, iscolabel, method = "jw")) %>%  # Jaro-Winkler similarity
      slice_min(order_by = similarity, n = 1) %>%  # Choose the smallest string distance (highest similarity)
      ungroup()
    crosswalk_one_to_one$similarity <- NULL
  } else {
    # Keep all mappings
    crosswalk_one_to_one <- crosswalk_census2010_isco08
  }

  # Perform the merge to map Census2010 codes to ISCO08 and SOC2010
  result <- data %>%
    dplyr::left_join(crosswalk_one_to_one, by = setNames("occ2010", census_col))

  # Return the transformed dataset
  return(result)
}
