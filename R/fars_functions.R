#' fars_read
#' 
#' @description
#' This function quietly loads data from a specified file. If the given 
#' file does not exist, an error will be raised. It has been designed to be used
#' in conjunction with \code{make_filename(year)}.
#'
#' @param filename String containing the input data file name.
#' 
#' @return This function returns a tibble containing the data from the specified
#'    file.
#'    
#' @importFrom dplyr, readr
#' 
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#' 
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename
#' 
#' @description
#' This function creates the appropriate filename string for the FARS data for 
#' a given year. The format of the filename is "accident_YYYY.csv.bz2". This 
#' function does not check if this year is valid.
#' 
#' @param year The year for which a filename is needed. This argument is cast
#'    as an integer inside the function, and so can be safely passed as a 
#'    character, integer, or numeric quantity without causing any errors.
#'    
#' @return This function returns a string containing the filename for FARS data
#'    for the input year.
#'    
#' @examples 
#' \dontrun{make_filename(2013)}
#' \dontrun{make_filename("2014")}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#' 
#' @description
#' This function condenses the FARS data for a list of years down to the month
#' and year of each fatal injury due to a motor vehicle collision. Note that a 
#' new "year" column is added to the table, although FARS already contains a 
#' "YEAR" column. If one of the years in the input list is invalid, NULL will
#' be returned for that year and a warning will be raised. For the available 
#' data sets, valid years are 2013, 2014, and 2015.
#' 
#' @param years A list of years for which condensed Month-Year data is desired.
#'    Each year may be specified as a character, integer, or numeric quantity
#'    without causing any errors (assuming the year itself is valid).
#' 
#' @return This function returns a list of tibbles, one for each year in the 
#' input list. Output for any invalid years will be NULL.
#' 
#' @importFrom dplyr, magrittr
#' 
#' @examples 
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
#' \dontrun{fars_read_years(2010:2020)} # Note that some years in this range
#'                                      # are invalid
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

#' fars_summarize_years
#' 
#' @description
#' This function summarizes the number of fatal injuries by month and year in 
#' the FARS data set for all of the input years. Valid years (given the data 
#' files that are currently available) are 2013, 2014, and 2015. Any invalid 
#' years will be excluded from the output summary. An error will be thrown by
#' \code{fars_read_years()} for any invalid years.
#' 
#' @param years A list of years for which monthly total fatalities will be 
#'    tabulated. Each year may be specified as a character, integer, or numeric
#'    quantity without causing any errors (assuming the year itself is valid).
#' 
#' @return This function returns a summary table of total monthly fatalities for
#'    each year in the input list. Months remain in a column and each year has
#'    its own column (as a result of \code{tidyr::spread(year, n)}). The values
#'    in the table represent total monthly fatalities for a given month-year
#'    combination.
#'    
#' @importFrom dplyr, magrittr, tidyr
#' 
#' @examples 
#' \dontrun{fars_summarize_years(c(2013, 2014, 2015))}
#' \dontrun{fars_summarize_years(2013:2015)} 
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state
#' 
#' @description 
#' This function plots the location of fatal collisions recorded by FARS for a 
#' given state and year. The collision locations are drawn over a map of the 
#' United States cropped to contain the latitude & longitude of the entries for 
#' the specified state. 
#' 
#' @details
#' If the state number given does not exist in the FARS data for the specified
#' year, an error will be raised. An invalid year will lead to an error being 
#' raised when attempting to read in the data with 
#' \code{data <- fars_read(filename)}. This function replaces unphysical latitude 
#' and longitude values with NA; note that it does not verify that the 
#' coordinates of each collision fall within the boundaries of the specified 
#' state. Any collisions with missing latitude or longitude are not plotted.
#' 
#' @param state.num A number ID associated with the state of interest. This 
#'    argument is cast as an integer inside the function, and so can be safely 
#'    passed as a character, integer, or numeric quantity without causing errors.
#' @param year The year for which the state plot is desired. This argument is 
#'    cast as an integer inside the function, and so can be safely passed as a
#'    character, integer, or numeric quantity without causing errors.
#'
#' @return This function creates a plot of the United States, zoomed in on the
#'    specified state, with the location of individual fatal collisions plotted 
#'    as points.
#'    
#' @importFrom dplyr, maps, graphics
#' 
#' @examples 
#' \dontrun{fars_map_state(1, 2013)}
#' \dontrun{fars_map_state("2", "2014")}
#' \dontrun{fars_map_state(0, 2015)} # Invalid state.num
#' \dontrun{fars_map_state(10,2010)} # Invalid year (outside of 2013-2015)
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
