#' Load data file
#'
#' This function reads a the USA National Highway Traffic Safety
#'  Administration NHFTS Fatality Analysis Reporting System FARS
#'  /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'  format data file and returns a tibble.
#'
#' @param filename A character string of the name of the file to be read
#'
#' @return This function returns a tibble of the data file and prints the first
#'  10 rows to the console.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  fname <- system.file("extdata", filename, package="farspackage")
  if(!file.exists(fname))
    stop("file '", fname, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(fname, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make a filename with a certain year
#'
#' This function reads in a year and creates a standard NHFTS FARS
#'  /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'  filename as #'  a character string vector.
#'
#' @param year A string of length 4 giving the year to be added to the filename.
#'
#' @return This function returns a chatacter vector.
#'
#' @example
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Creates a tibble from a year
#'
#' This function reads in a year or a list of years and creates a tibble
#'  containing month and year from a standard NHFTS FARS
#'  /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'  file containing that year in the filename.
#'
#' @param years A string of length 4 giving the year, or list of years, in the
#'  NHFTS FARS filename from which the month and year attributes will be
#'  summarised.
#'
#' @return A tibble containing the MONTH and year attribute for each input year.
#'  If a year is entered that does not have the corresponding filename a
#'  warning message will be printed to the console.
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013, 2014, 2015))
#'
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

#' Summarises the number of fatalities per month for a given year
#'
#' This function reads in a year or a list of years and creates a tibble
#'  summarising the number of fatalities per month as rows and year as
#'  columns from a standard NHFTS FARS
#'  /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'  file containing that year in the filename.
#'
#' @param years A string of length 4 giving the year, or list of years, in the
#'  NHFTS FARS filename from which the month and year attributes will be
#'  summarised.
#'
#' @return A tibble containing the number of fatalities per month for each
#'  input year.
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importForm tidyr spread
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013, 2014, 2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Generates map of fatalities per state and year
#'
#' This function produces a map of the location of fatalities per state and
#'  input year from a standard NHFTS FARS
#'  /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'  file containing that year in the filename.
#'
#' @param state.num An integer representing each US state in the NHFTS FARS
#'  format file in the STATE attribute
#' @param year A string of length 4 representing the year in the file name
#'
#'
#' @return A plot of the state boundary polygon with fatalities as points. If
#'  an invalid state number or if there are no fatalities recorded for that
#'  state for the input year a warning message will be returned.
#'
#' @importFrom dplyr filter %>%
#' @importForm maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(6, 2013)
#'
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
