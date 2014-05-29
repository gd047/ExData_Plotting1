best <- function(state, outcome){
  
  #### load dplyr and helper functions ######################################
  require(dplyr)
  
  require(devtools)
  # https://gist.github.com/skranz/9681509 or 
  source_gist("https://gist.github.com/jknowles/10888768")
  
  #   #' Internal function used by s_filter, s_select etc.
  #   eval.string.dplyr = function(.data, .fun.name, ...) {
  #     args = list(...)
  #     args = unlist(args)
  #     code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
  #     df = eval(parse(text=code,srcfile=NULL))
  #     df  
  #   }
  #   
  #   #' Modified version of dplyr's select that uses string arguments
  #   #' @export
  #   s_select = function(.data, ...) {
  #     eval.string.dplyr(.data,"select", ...)
  #   }
  #   
  #   #' Modified version of dplyr's filter that uses string arguments
  #   #' @export
  #   s_filter = function(.data, ...) {
  #     eval.string.dplyr(.data,"filter", ...)
  #   }
  #   
  #   #' Modified version of dplyr's arrange that uses string arguments
  #   #' @export
  #   s_arrange = function(.data, ...) {
  #     eval.string.dplyr(.data,"arrange", ...)
  #   }
  ##########################################################################
  
  data <- read.csv("outcome-of-care-measures.csv",
                   na.strings =c('Not Available','NA'),
                   stringsAsFactors = FALSE) # , colClasses = "character"
  
  if (!state %in% unique(data$State)) {
    stop("invalid state")
  }
  
  #data[, 11] <- as.numeric(data[, 11])
  #data[, 17] <- as.numeric(data[, 17])
  #data[, 23] <- as.numeric(data[, 23])
  
  cols = c("Provider.Number:Phone.Number",
           switch(outcome,
                  "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                  "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                  "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                  stop("invalid outcome"))
  )
  
  res <-  data %.% 
    s_select(cols) %>%
    s_filter(paste0("!is.na(",last(cols),") & State == '",state,"'")) %>%  # exclude null
    s_arrange(paste0(last(cols),", Hospital.Name")) %>% # reorder 
    summarize(first(Hospital.Name))
  
  return(res[[1]])
}