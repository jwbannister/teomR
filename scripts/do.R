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
df1 <- inner_join(teom_data, select(teom_locs, deployment.id, 
                                          dca.group, position),
                        by="deployment.id")

events <- define_event(df1, teom_locs)

joined_events <- inner_join(filter(events, tag=="UW"),
                            filter(events, tag=="DW"), 
                            by=c("datetime", "dca.group"))

clean_events <- joined_events %>% 
                group_by(data.id.x) %>%
                select(datetime, dca.group,
                       teom.uw=deployment.x, ws.uw=ws.x, wd.uw=wd.x, 
                       pm10.uw=pm10.avg.x, 
                       teom.dw=deployment.y, ws.dw=ws.y, wd.dw=wd.y, 
                       pm10.dw=pm10.avg.y) %>%
                mutate(day=lubridate::day(datetime)) %>%
                filter(ws.uw!=0,  ws.dw!=0) %>%
               ungroup() 

print(
ggplot(filter(clean_events, dca.group=="north (T29)"), aes(wd.uw)) +
  geom_histogram()
)

print(
ggplot(filter(clean_events, dca.group=="central (T12)"), aes(wd.uw)) +
  geom_histogram()
)

print(
ggplot(filter(clean_events, dca.group=="south (T2 & T3)"), aes(wd.uw)) +
  geom_histogram()
)

daily_summary <- clean_events %>% group_by(day, dca.group) %>%
  summarize(daily.pm10.uw=sum(pm10.uw)/24, daily.pm10.dw=sum(pm10.dw)/24) %>%
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
p_north <- plot_owens_borders(dcas=north_group) +
           geom_point(data=filter(teom_locs, dca.group=="north (T29)"), 
                      mapping=aes(x=x, y=y)) +
           geom_label(data=filter(teom_locs, dca.group=="north (T29)"), 
                      mapping=aes(x=x, y=y, label=deployment)) 
p_central <- plot_owens_borders(dcas=central_group) +
           geom_point(data=filter(teom_locs, dca.group=="central (T12)"), 
                      mapping=aes(x=x, y=y)) +
           geom_label(data=filter(teom_locs, dca.group=="central (T12)"), 
                      mapping=aes(x=x, y=y, label=deployment)) 
p_south <- plot_owens_borders(dcas=south_group) +
           geom_point(data=filter(teom_locs, dca.group=="south (T2/T3)"), 
                      mapping=aes(x=x, y=y)) +
           geom_label(data=filter(teom_locs, dca.group=="south (T2/T3)"), 
                      mapping=aes(x=x, y=y, label=deployment)) 

write.csv(clean_events,
          file="./output/twb2_paired_teoms_DEC_events_data.csv")

test <- filter(teom_data, deployment.id==1849)
