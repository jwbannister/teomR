# teom_functions.R -- functions related to teom pairs analysis
# John Bannister
# Created 01/14/2016 -- see git for revision history
# 

#' Pair up teoms for upwind/downwind analysis
#' 
#' This function is specific to pairing teoms in TwB2 upwind/downwind analysis.
#' 
#' @import magrittr
#' @param df1 Data frame. Teoms under consideration.
#' @return Input data frame with added column listing relevant DCA for paired 
#' TEOMS. 

pair_teoms <- function(df1){
  df1$dca.group <- rep(NA, nrow(df1))
  for (i in 1:nrow(df1)){
    if (df1$deployment[i] == "T7" | df1$deployment[i] == "T2-1"){
      df1$dca.group[i] <- "south"
    }
    if (df1$deployment[i] == "T11" | df1$deployment[i] == "T16"){
      df1$dca.group[i] <- "central"
    }
    if (df1$deployment[i] == "T29-4S" | df1$deployment[i] == "T29-4N"){
      df1$dca.group[i] <- "north"
    }
  }
  df1 <- group_by(df1, dca.group) %>% 
    mutate(position = ifelse(northing.utm==max(northing.utm), "N", "S")) %>%
    arrange(desc(position)) %>%
    mutate(alpha = atan(diff(easting.utm)/diff(northing.utm))) %>%
    mutate(alpha = ifelse(alpha>0, alpha, 2*pi+alpha)) %>%
    mutate(upwind.angle = ifelse(position=="S", alpha, alpha-pi)) %>%
    select(-alpha)
  df1$upwind.angle <- ifelse(deg(df1$upwind.angle)>0, deg(df1$upwind.angle), 
                             360 + deg(df1$upwind.angle)) 
  df1$upwind.angle <- round(df1$upwind.angle, 1)
  df1 <- ungroup(df1)
  df1
}




