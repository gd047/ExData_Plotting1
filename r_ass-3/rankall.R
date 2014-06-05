rankall <- function(outcome, num = "best") {
  
  #### load dplyr and helper functions ######################################
  require(dplyr)
  
  #  require(devtools)
  # https://gist.github.com/skranz/9681509 or 
  #  source_gist("https://gist.github.com/jknowles/10888768")
  
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
  
  #' Modified version of dplyr's arrange that uses string arguments
  #' @export
  s_arrange = function(.data, ...) {
    eval.string.dplyr(.data,"arrange", ...)
  }
  
  #' Modified version of dplyr's summarise that uses string arguments
  #' @export
  s_summarise = function(.data, ...) {
    eval.string.dplyr(.data,"summarise", ...)
  }
  ##########################################################################
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",
                   na.strings =c('Not Available','NA'),
                   stringsAsFactors = FALSE) # , colClasses = "character"
    
  
  ## Check that outcome is valid
  
  cols = c("Hospital.Name,State",
           switch(outcome,
                  "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                  "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                  "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                  stop("invalid outcome"))
  )
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  res <-  data %>% 
    #group_by(State) %>%
    s_select(cols) %>%
    s_filter(paste0("!is.na(",last(cols),")")) %>%  # exclude null
    s_arrange(paste0("State, ",last(cols),", Hospital.Name"))
  
  
  if (!is.numeric(num)){
    switch(num,
       "best" = res %>%
                  group_by(State) %>%
                  summarise(hospital = first(Hospital.Name)) %>%
                  select(hospital = 2, state = 1) ,
       "worst" = res %>%
                  group_by(State) %>%
                  summarise(hospital = last(Hospital.Name)) %>%
                  select(hospital = 2, state = 1),
       stop("invalid num"))
  } else {
    res %>%
      group_by(State) %>%
      s_summarise(paste0('nth(Hospital.Name,',num,')')) %>%
      select(hospital = 2, state = 1)
  }
  
}