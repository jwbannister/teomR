devtools::load_all()
devtools::load_all("../Rowens")
library(dplyr)

date1 <- "2016-02-01"
date2 <- "2016-03-01"

# pull data from portable teoms, keep wind direction and speed
teom_met <- pull_teom_wind(date1, date2)
# pull data from m-files for T7 teom, keep wind direction and speed
mfile <- pull_mfile(date1, date2)

df1 <- rbind(teom_met, mfile)

deploys <- as.character(unique(df1$deployment.id))
teom_locs <- pull_locations(deploys)

pm10_df <- pull_pm10(date1, date2, deploys)

teom_data <- inner_join(df1, select(teom_locs, deployment.id, deployment),
                  by="deployment.id") %>%
  left_join(pm10_df, by=c("datetime"="datetime_hour", "deployment")) %>%
  mutate(pm10 = ifelse(is.na(pm10.avg.x), pm10.avg.y, pm10.avg.x)) %>%
  select(-pm10.avg.x, -pm10.avg.y) 

missing_data <- find_missing(teom_data)

twb2_dcas <- list("north (T29)" = c("T29-3", "T29-4"),
                  "central (T12)" = c("T12-1"),
                  "south (T2 & T3)" = c("T3SW", "T3SE", "T2-2", "T2-3", 
                                        "T2-4", "T5-4"))

save(teom_data, teom_locs, file="./data-clean/teom_data.RData")
save(twb2_dcas, file="./data/tbw2_dcas.RData")


