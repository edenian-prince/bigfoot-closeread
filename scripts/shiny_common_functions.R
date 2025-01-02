

### round and add thousand mark commas--------------------------------------------
# stolen from SO: https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals

#' Format numbers for printing by rounding and adding commas
#' `round_add_commas` takes a number in any format that's coercible to 
#' 'numberic, converts it to a number, rounds it and then returns a formatted
#' `string` with commas separating every 1,000
#' 
#' @param x Anything that's coercible to numeric. Generally either a number or 
#' a string containing a number such as "9"
#' @return A formatted `string` containing the number rounded and seperated by commas
#' @seealso https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals
#' 
#' @export
#' 
round_add_commas <- function(x){
  
  
  format(
    round(
      as.numeric(x)), 
    big.mark=",") 
  
}

### not in function ------------------------------------------------------------

#' Determine if a value is NOT in a vector
#' 
#' `%!in%` returns a Logical value. TRUE is returned if the value is NOT contained
#' in a vector. It is the opposite of `%in%` and equivalent to `not in` in python
#' @return A logical `TRUE` or `FALSE` 
#' @seealso [%in%], https://stackoverflow.com/a/5831829/16502170
#' @export
#' @examples
#' x <- 11
#' y <- 2
#' number_vector <- 1:3
#' 
#' x %!in% number_vector
#' y %!in% number_vector
'%!in%' <- function(x, y)!('%in%'(x, y))



# Make missing values labels function ----------------------------------------
# Make labels for different charts telling the user how many missing values 
# there are for a particular variable. 

#' Make missing values labels for Shiny explanatory text
#' `make_missing_values_labels()` takes a dataset and a variable in the dataset
#' and returns a formatted `string`  that can be used as a graphics label
#' to tell the viewer how many missing values there are for that value
#' 
#' @param variable A variable present in the dataframe.
#' Missing values must be either `NA` or `== "Unknown"`
#' @param df A dataframe
#' @details
#' This function depends on the `round_add_commas()` function that is also
#' in this `functions.R` file. It also depends on the `dplyr` package
#' @seealso [round_add_commas()], see also this method for using `dplyr::filter`
#' in functions: https://stackoverflow.com/a/45263058/16502170
#' @export
make_missing_values_labels <- function(variable, df = bigfoot_points){
  
  df <- df %>% sf::st_drop_geometry()
  
  # This is the total number of records in the dataframe
  # formatted as a string, assumes one record per row
  total <-    df %>% 
    nrow() %>%
    round_add_commas()
  
  
  # This returns the total number of rows where the variable is NA
  na_values <- df %>% 
    dplyr::filter(is.na((.)[variable])) %>%
    nrow() 
  
  # This returns the total number of rows where the variable is equal to "Unkown"
  unknown <- df %>% 
    dplyr::filter( (.)[variable] == "Unknown") %>%
    nrow() 
  
  
  # This looks for which number is large NA or unknown
  # this assumes that all missing values are either NA OR "Unknown"
  # this method will not work if both NA and "Unknowns" are present
  # Returns a formatted string
  missing <- ifelse(unknown >= na_values, 
                    unknown, 
                    na_values) %>%
    round_add_commas()
  
  
  # This formats the variable name for easy use in labels
  variable_label <- ifelse(variable == "classification",
                           "a report classification",
                           ifelse(variable == "report_weekday",
                                  "the day of the week",
                                  ifelse(variable == "season",
                                         "season data",
                                         "Undefined")))
  
  
  # This is an error message that's returned if we don't know how to 
  # label a variable
  if(variable_label == "Undefined"){
    
    stop(paste0("Error: Please define an appropriate label for ",
                "\033[38;5;206;48;5;57m",
                variable,
                ")\033[0m",
                "first!"
    ))
    
  }
  
  # This is the label containing the number of missing values
  # the total number of records 
  # and the variable name
  missing_label <- paste0(missing,
                          " out of ",
                          total,
                          " sightings are missing ",
                          variable_label,
                          ".")
  
  # Return the string
  missing_label
}







