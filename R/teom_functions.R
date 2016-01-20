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
      df1$dca.group[i] <- "south (T2 & T3)"
    }
    if (df1$deployment[i] == "T11" | df1$deployment[i] == "T16"){
      df1$dca.group[i] <- "central (T12)"
    }
    if (df1$deployment[i] == "T29-4S" | df1$deployment[i] == "T29-4N"){
      df1$dca.group[i] <- "north (T29)"
    }
  }
  df1
}

assign_wind_angle <- function(df1){
  df1 <- group_by(df1, dca.group) %>% 
    mutate(position = ifelse(northing.utm==max(northing.utm), "N", "S")) %>%
    arrange(desc(position)) %>%
    mutate(alpha = atan(diff(easting.utm)/diff(northing.utm))) %>% 
    mutate(alpha = ifelse(alpha>0, alpha, 2*pi+alpha)) %>%
    mutate(upwind.angle = ifelse(position=="N", alpha, alpha-pi)) %>%
    mutate(upwind.angle = ifelse(upwind.angle>0, upwind.angle, 
                                 2*pi+upwind.angle)) %>%
    select(-alpha) %>%
    ungroup()
  df1$upwind.angle <- circular::deg(df1$upwind.angle) 
  df1$upwind.angle <- round(df1$upwind.angle, 1)
  df1 <- df1 %>% group_by(deployment.id) %>% 
    mutate(downwind.angle=ifelse(upwind.angle>180, upwind.angle-180,
                                 upwind.angle+180)) %>%
  ungroup()
df1
}

#' Filter hourly data by wind direction.
#' 
#' @import magrittr
#' @param df1 Data frame. Data for teoms under consideration.
#' @param locs Data frame. List of teoms with relevant upwind direction.
#' @return Data frame containing only those hours that are in an upwind 
#' direction for the associated teom. 
define_event <- function(df1, locs){
  return_df <- df1
  return_df$tag <- rep(NA, nrow(return_df))
  return_df <- return_df[numeric(0), ]
  for (i in 1:nrow(locs)){
    a <- filter_by_angle(df1, locs$deployment.id[i], locs$upwind.angle[i], 
                         "UW")
    b <- filter_by_angle(df1, locs$deployment.id[i], locs$downwind.angle[i], 
                         "DW")
    return_df <- rbind(return_df, a, b)
  }
  return_df
}

filter_by_angle <- function(df1, id, angle, tag){
  upper.angle <- angle + 11.25
  upper.angle.alt <- upper.angle-360 
  lower.angle <- angle - 11.25
  lower.angle.alt <- lower.angle+360
  if (upper.angle>360){
    if (lower.angle<0){
      events <- dplyr::filter(df1, deployment.id==id,
                              (wd>lower.angle.alt&wd<360) |
                                (wd>0&wd<upper.angle))
    }else {
      events <- dplyr::filter(df1, deployment.id==id,
                              (wd>lower.angle&wd<360) |
                                (wd>0&wd<upper.angle.alt))
    }
  }else {
    events <- dplyr::filter(df1, deployment.id==id,
                            wd>lower.angle&wd<upper.angle)
  }
  events$tag <- rep(tag, nrow(events))
  events
}
