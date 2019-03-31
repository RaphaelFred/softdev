#' Reads data and creates a data frame tbl
#'
#' This function reads a csv file if existant and creates a data frame tbl.
#' @param filename Name of the file that is read into R
#' @return This function will return a data frame of format tbl_df. (For further information look at tbl_df{dplyr})
#' @importFrom readr read_csv
#' @examples#'
#' \dontrun{
#' fars_read("accident_2015.csv.bz2")
#' }
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Makes filename in C-style Formatting
#'
#' This function produces a filename out of a year input in C-style String formatting.
#' @param year Year of the document
#' @return This function will return a character-string.
#' @examples
#' make_filename(year = 2019)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  file <- sprintf("accident_%d.csv.bz2", year)
}

#' Creates a list of tibbles
#'
#' This function returns the observation "month" and "year" for a list of datasets.
#' @param years A vector of years.
#' @return A list of tibbles
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @note Invalid year values may result in an error and NULL is returned.
#' @examples
#' \dontrun{
#' fars_read_years(c("2013", "2014"))
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Shows number of observations for each month
#'
#' This function returns a tibble that summarizes the number of observations for each month and each dataset.
#' @param years A vector of years.
#' @return A tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @note Invalid year values may result in an error and NULL is returned for the specific dataset.
#' @examples
#' \dontrun{
#' fars_summarize_years(c("2013", "2014"))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' Plots the occurence of fatal injuries by state
#'
#' This function plots the occurence of fatal injuries suffered in motor vehicle traffic crashes
#' @param state.num Numeric state number
#' @param year A numeric year input
#' @return A map graphic of the selected state.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @note Error may occur if the state input is invalid or if there are no accidents to plot.
#' @examples
#' \dontrun{
#' fars_map_state(1, 2015)
#' fars_map_state(50, 2013)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
