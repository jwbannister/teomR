# explore.R -- Initial work on paired teoms for upwind/downwind PM10
# John Bannister
# Created 01/14/2016 -- see git for revision history
# 
devtools::load_all("../Rowens")
devtools::load_all()
library(dplyr)
library(ggplot2)
load("./data-clean/teom_data.RData")

teom_locs <- pair_teoms(teom_locs)
north_group <-c("T29-3", "T29-4")
central_group <- c("T12-1")
south_group <- c("T3SW", "T3SE", "T2-2", "T2-3", "T2-4", "T5-4")
teom_locs <- rename(teom_locs, x=easting.utm, y=northing.utm)
teom_data <- inner_join(teom_data, select(teom_locs, deployment.id, deployment, dca.group, position),
                        by="deployment.id")

p_north <- plot_owens_borders(dcas=north_group) +
           geom_point(data=filter(teom_locs, dca.group=="north"), mapping=aes(x=x, y=y)) +
           geom_label(data=filter(teom_locs, dca.group=="north"), mapping=aes(x=x, y=y, label=upwind.angle)) 
p_central <- plot_owens_borders(dcas=central_group) +
           geom_point(data=filter(teom_locs, dca.group=="central"), mapping=aes(x=x, y=y)) +
           geom_label(data=filter(teom_locs, dca.group=="central"), mapping=aes(x=x, y=y, label=upwind.angle)) 
p_south <- plot_owens_borders(dcas=south_group) +
           geom_point(data=filter(teom_locs, dca.group=="south"), mapping=aes(x=x, y=y)) +
           geom_label(data=filter(teom_locs, dca.group=="south"), mapping=aes(x=x, y=y, label=upwind.angle)) 

events <- group_by(teom_data, deployment.id) %>%
             filter(wd.avg < (teom_locs[teom_locs$deployment.id==unique(deployment.id), ]$upwind.angle + 20) &
                    wd.avg > (teom_locs[teom_locs$deployment.id==unique(deployment.id), ]$upwind.angle - 20))

joined_events <- inner_join(events, teom_data, by=c("datetime", "dca.group"))

north_times <- filter(events, dca.group=="north")$datetime
north_data <- filter(teom_data, dca.group=="north", datetime %in% north_times)
north_data <- inner_join(filter(north_data, position=="N"), filter(north_data, position=="S"), 
                         by="datetime")
teom_north <- filter(teom_data, dca.group=="N") %>%
  filter(datetime %in%

