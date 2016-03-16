# do_pairs.R -- Initial work on paired teoms for upwind/downwind PM11
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
# df1 == all available hourly data from teoms
df1 <- inner_join(teom_data, dplyr::select(teom_locs, deployment.id, 
                                           dca.group, position),
                  by="deployment.id")
# filter by direction - keep only hours for which wind is blowing within +/- 
# 11.25 deg of line between teom pairs.
events <- define_event(df1, teom_locs)
joined_events <- inner_join(filter(events, tag=="UW"),
                            filter(events, tag=="DW"), 
                            by=c("datetime", "dca.group"))
clean_events <- joined_events %>% 
  group_by(data.id.x) %>%
  dplyr::select(datetime, dca.group,
                teom.uw=deployment.x, ws.uw=ws.x, wd.uw=wd.x, 
                pm10.uw=pm10.x, 
                teom.dw=deployment.y, ws.dw=ws.y, wd.dw=wd.y, 
                pm10.dw=pm10.y) %>%
  mutate(day=lubridate::day(datetime),
            # convert wind speed from mph to m/s
         ws.avg.mps=mean(c(ws.uw, ws.dw))*.44704) %>%
  filter(ws.uw!=0,  ws.dw!=0) %>% arrange(datetime) %>%
  ungroup() 

# summarize pm10 differnce between matched TEOM pairs by day.
daily_summary <- clean_events %>% group_by(day, dca.group) %>%
  summarize(daily.pm10.uw=sum(pm10.uw)/24, daily.pm10.dw=sum(pm10.dw)/24,
            # convert wind speed from mph to m/s
            ws.avg.mps=mean(c(ws.uw, ws.dw))*.44704) %>%
mutate(pm10.delta=daily.pm10.dw - daily.pm10.uw) %>%
ungroup()

# display days with PM10 differences greater than 10 
filter(daily_summary, pm10.delta > 10)

sub_locs <- filter(teom_locs, dca.group==names(twb2_dcas)[2])
p1 <- teom_pair_plots(teom_locs=sub_locs, 
                      df1=df1[complete.cases(df1), ], 
                      dcas=twb2_dcas[names(twb2_dcas)==
                                     sub_locs$dca.group[1]][[1]])

df_uw <- df1 %>% select(datetime, dca.group, 
                                 pm10=pm10.uw, ws.avg)
df_uw$loc <- rep("Upwind", nrow(df_uw))
df_dw <- df1 %>% select(datetime, dca.group, 
                                 pm10=pm10.dw, ws.avg)
df_dw$loc <- rep("Downwind", nrow(df_dw))
df2 <- rbind(df_uw, df_dw)
for (i in unique(df2$dca.group)){
      p4 <- df2 %>% filter(dca.group==i) %>% arrange(datetime) %>%
        ggplot(aes(x=datetime, y=pm10)) +
        geom_path(aes(color=loc)) +
        ylab("PM10 Daily Average") + xlab("Date") +
       theme(legend.title=element_blank()) 
      png(filename=paste0("./output/", i, " Tracking"), 
          height=6, width=6, units="in", res=300)
      print(p4)
      dev.off()
}

for (i in unique(df1$dca.group)){
      p4 <- df1 %>% filter(dca.group==i) %>% arrange(datetime) %>%
        ggplot(aes(x=datetime, y=pm10.avg)) +
        geom_path(aes(color=deployment)) +
        ylab("PM10 Hourly Average") + xlab("Date") +
       theme(legend.title=element_blank()) 
      png(filename=paste0("./output/", i, " tracking - ", 
                          tolower(month_char), " ", report_year, ".png"),
          height=6, width=6, units="in", res=300)
      print(p4)
      dev.off()
}
