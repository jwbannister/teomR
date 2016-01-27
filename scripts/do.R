# do.R -- Initial work on paired teoms for upwind/downwind PM11
# John Bannister
# Created 01/14/2016 -- see git for revision history
# 
devtools::load_all("../Rowens")
devtools::load_all()
library(dplyr)
library(ggplot2)

load("./data-clean/teom_data.RData")
teom_locs <- pair_teoms(teom_locs)
teom_locs <- assign_wind_angle(teom_locs)
df1 <- inner_join(teom_data, dplyr::select(teom_locs, deployment.id, 
                                          dca.group, position),
                        by="deployment.id")

events <- define_event(df1, teom_locs)

joined_events <- inner_join(filter(events, tag=="UW"),
                            filter(events, tag=="DW"), 
                            by=c("datetime", "dca.group"))

clean_events <- joined_events %>% 
                group_by(data.id.x) %>%
                dplyr::select(datetime, dca.group,
                       teom.uw=deployment.x, ws.uw=ws.x, wd.uw=wd.x, 
                       pm10.uw=pm10.avg.x, 
                       teom.dw=deployment.y, ws.dw=ws.y, wd.dw=wd.y, 
                       pm10.dw=pm10.avg.y) %>%
                mutate(day=lubridate::day(datetime),
                       ws.avg=mean(c(ws.uw, ws.dw))) %>%
                filter(ws.uw!=0,  ws.dw!=0) %>%
               ungroup() 

daily_summary <- clean_events %>% group_by(day, dca.group) %>%
  summarize(daily.pm10.uw=sum(pm10.uw)/24, daily.pm10.dw=sum(pm10.dw)/24,
# convert wind speed from mph to m/s
            ws.avg.mps=mean(c(ws.uw, ws.dw))*.44704) %>%
  mutate(pm10.delta=daily.pm10.dw - daily.pm10.uw) %>%
  ungroup()

write.csv(daily_summary, 
          file="./output/twb2_wind_events_daily_summary_dec2015.csv",
          row.names=F)
write.csv(clean_events, 
          file="./output/twb2_wind_events_hourly_events_dec2015.csv",
          row.names=F)

# plots of relevant DCA groups with associated teoms and upwind angles
north_group <-c("T29-3", "T29-4")
central_group <- c("T12-1")
south_group <- c("T3SW", "T3SE", "T2-2", "T2-3", "T2-4", "T5-4")
basemap <- brick("~/dropbox/gis/owens/owens_20151111_30prcnt.tif")
maprgb <- RStoolbox::ggRGB(basemap)
p_north <- plot_owens_borders(dcas=north_group) +
           geom_point(data=filter(teom_locs, dca.group=="north (T29)"), 
                      mapping=aes(x=x, y=y)) +
           geom_label(data=filter(teom_locs, dca.group=="north (T29)"), 
                      mapping=aes(x=x, y=y, label=deployment),
                      nudge_x=125) 

north_blank <- p_north + theme(axis.line=element_blank(),
                               axis.text.x=element_blank(),
                               axis.text.y=element_blank(),
                               axis.ticks=element_blank(),
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank(),
                               legend.position="none",
                               panel.background=element_blank(),
                               panel.border=element_blank(),
                               panel.grid.major=element_blank(),
                               panel.grid.minor=element_blank(),
                               plot.background=element_blank())

