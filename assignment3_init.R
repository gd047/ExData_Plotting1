data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

data[, 11] <- as.numeric(data[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

# https://gist.github.com/skranz/9681509


#' Internal function used by s_filter, s_select etc.
eval.string.dplyr = function(.data, .fun.name, ...) {
  args = list(...)
  args = unlist(args)
  code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
  df = eval(parse(text=code,srcfile=NULL))
  df  
}

#' Modified version of dplyr's select that uses string arguments
#' @export
s_select = function(.data, ...) {
  eval.string.dplyr(.data,"select", ...)
}

#' Modified version of dplyr's filter that uses string arguments
#' @export
s_filter = function(.data, ...) {
  eval.string.dplyr(.data,"filter", ...)
}



best <- function(state, outcome){
  cols = c("Provider.Number:Phone.Number",
         switch(outcome,
           "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
           "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
           "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
           stop("Outcome is not valid"))
         )
  
  data %.% 
    s_select(cols) %>%
    s_filter(paste0("!is.na(",last(cols),")"))
}

state = "TX"
outcome = "heart attack"

