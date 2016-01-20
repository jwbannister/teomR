# explore.R -- Initial work on paired teoms for upwind/downwind PM11
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
teom_locs <- rename(teom_locs, x=easting.utm, y=northing.utm)
df1 <- inner_join(teom_data, select(teom_locs, deployment.id, deployment, 
                                          dca.group, position),
                        by="deployment.id")

events <- define_event(df1, teom_locs)

joined_events <- inner_join(events, df1, b=c("datetime", "dca.group")) %>%
                 filter(deployment.id.x!=deployment.id.y) %>%
                 filter(ws.x!=0 & ws.y!=0) %>%
                 filter(wd.y < wd.x + 11.25 & wd.y > wd.x - 11.25)

clean_events <- joined_events %>% 
                group_by(data.id.x) %>%
                select(datetime, dca.group,
                       ws.uw=ws.x, wd.uw=wd.x, pm10.uw=pm10.avg.x, 
                       ws.dw=ws.y, wd.dw=wd.y, pm10.dw=pm10.avg.y) %>%
                mutate(day=lubridate::day(datetime), 
                       ws.avg=mean(c(ws.uw, ws.dw)), 
                       wd.avg=mean(c(wd.uw, wd.dw))) %>%
               ungroup() 

print(
ggplot(filter(clean_events, dca.group=="north (T29)"), aes(wd.avg)) +
  geom_histogram()
)

print(
ggplot(filter(clean_events, dca.group=="central (T12)"), aes(wd.avg)) +
  geom_histogram()
)

print(
ggplot(filter(clean_events, dca.group=="south (T2 & T3)"), aes(wd.avg)) +
  geom_histogram()
)

daily_summary <- clean_events %>% group_by(day, dca.group) %>%
  summarize(daily.pm10.uw=sum(pm10.uw)/24, daily.pm10.dw=sum(pm10.dw)/24) %>%
  mutate(pm10.delta=daily.pm10.dw - daily.pm10.uw) %>%
  ungroup()

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
