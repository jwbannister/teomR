# do_control.R -- PM10 readings for  paired teoms across control areas (no DCM)
# John Bannister
# Created 01/14/2016 -- see git for revision history
# 
devtools::load_all("../Rowens")
devtools::load_all()
library(dplyr)
library(ggplot2)
load("./data-clean/teom_data.RData")

teom_locs <- pair_teoms_control(filter(teom_locs, deployment=="Olancha" | deployment=="North Beach" |
                               deployment=="T29-4N" | deployment=="T2-1"))
teom_locs <- rename(teom_locs, x=easting.utm, y=northing.utm)
teom_data <- inner_join(teom_data, select(teom_locs, deployment.id, deployment, dca.group, position),
                        by="deployment.id")
events <- group_by(teom_data, deployment.id) %>%
          filter(wd.avg < (teom_locs[teom_locs$deployment.id==unique(deployment.id), ]$upwind.angle + 11.25) &
                 wd.avg > (teom_locs[teom_locs$deployment.id==unique(deployment.id), ]$upwind.angle - 11.25)) %>%
          ungroup()

joined_events <- inner_join(events, teom_data, b=c("datetime", "dca.group")) %>%
                 filter(deployment.id.x!=deployment.id.y) %>%
                 filter(ws.avg.x!=0, ws.avg.y!=0) %>%
                 filter(wd.avg.y < wd.avg.x + 11.25 & wd.avg.y > wd.avg.x - 11.25)

clean_events <- joined_events %>% 
                mutate(avg.ws = mean(c(ws.avg.x, ws.avg.y)),
                       dir = position.x) %>%
                select(datetime, dca.group, avg.ws, dir, pm10.uw = pm10.avg.x, 
                       teom.uw = deployment.id.x, pm10.dw = pm10.avg.y, 
                       teom.dw = deployment.id.y) %>%
                mutate(pm10.delta = pm10.dw - pm10.uw)

clean_events %>% group_by(dca.group, dir) %>% summarize(mean.pm10.uw = mean(pm10.uw),
                                                   mean.pm10.dw = mean(pm10.dw),
                                                   mean.pm10.delta = mean(pm10.delta),
                                                   n = length(pm10.uw))


