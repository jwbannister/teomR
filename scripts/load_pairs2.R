# teom_pairs_load2.R -- load data for twb2 paired teom analysis
# John Bannister
# 
load_all("../Rowens")
library(dplyr)

report_month <- 1 # SET THIS FOR MONTH TO SUMMARIZE
report_year <- 2016 # SET THIS FOR YEAR TO SUMMARIZE
month2 <- sprintf("%02d", report_month)
month2plus1 <- sprintf("%02d", report_month+1)
start.time <- paste0("'", report_year, "-", month2, "-01 00:00:00'")
end.time <- paste0("'", report_year, "-", month2plus1, "-01 00:00:00'")
month_char <- lubridate::month(lubridate::ymd(substr(start.time, 2, 11)), 
                                              label=TRUE, abbr=FALSE)

# pull wind data from portable teoms
# teom.teom_summary_data table contains wind data and analog pm10 data from teom 
# stations (transferred via radio network). This pm10 data is a good indication 
# of pm10 levels, but it not official, compliance-quality data. (see below)
print("pulling wind data from teom_summary...")
teom_wind <- query_owenslake(paste0("SELECT * FROM teoms.teom_summary_data 
                                   WHERE datetime > timestamp ", start.time, 
                                  "AND datetime < timestamp ", end.time))
if (sum(teom_wind$qaqc_level_id!=0)>0) print("QA/QC failures in data!")
teom_wind <- teom_wind[teom_wind$qaqc_level_id==0, ]
teom_wind <- select(teom_wind, data.id = teom_summary_data_id, datetime, 
                  deployment.id = deployment_id, wd, ws)
teom_wind <- filter(teom_wind, deployment.id!=8)
teom_wind$pm10.avg <- rep(NA, nrow(teom_wind))

# pull wind data and pm10 data from m-files for T7 teom
# for teom stations maintained by the district, digital pm10 data is not 
# immediately available. Typical turnaround for district maintained pm10 data is 
# 4 months. For immediate reporting, use analog pm10 data reported in 
# archive.mfile_data. For historical reports which require compliance-quality 
# data, use district 1-minute digital pm10 data.
print("pulling wind data from m-file...")
mfile <- query_owenslake(paste0("SELECT * FROM archive.mfile_data 
                                 WHERE datetime > timestamp ", start.time, 
                                "AND datetime < timestamp ", end.time, 
                                "AND site = 'T7'"))
mfile <- mfile[is.na(mfile$qaqc_level_id), ]
mfile <- select(mfile, data.id = did, datetime, deployment.id = deployment_id, 
                wd = dir, ws = aspd, pm10.avg=teom)

# combine portable and district teom station data
teom_data <- rbind(teom_wind, mfile)

# get deployment.id -> deployment and location reference index
deploys <- as.character(unique(teom_data$deployment.id))
deploys <- paste0("(", paste(deploys, collapse=", "), ")")
teom_locs <- query_owenslake(paste0("SELECT deployment_id, deployment, 
                                     northing_utm, easting_utm, description 
                                     FROM instruments.deployments 
                                     WHERE deployment_id IN ", deploys))
colnames(teom_locs) <- gsub("_", ".", colnames(teom_locs))
teom_locs <- select(teom_locs, -description)
teom_locs <- rename(teom_locs, x=easting.utm, y=northing.utm)

# assign deployment name to lines of teom_data by deployment.id
teom_data <- inner_join(teom_data, 
                        select(teom_locs, deployment.id, deployment),
                        by="deployment.id") 

# pull compliance-quality pm10 data 
# pm10 data in teom.deployment_data is 1-minute digital data, manually 
# downloadecd from teom stations. This 1-minute digital should be used for 
# reporting if available. (see below) Note that this query automatically 
# produces 1-hour pm10 averages.
print("pulling PM10 data...")
digital_pm10 <- 
  query_owenslake(paste0("SELECT d.deployment, 
                          file_uploads.date_trunc_hour(datetime) 
                          AS datetime_hour, AVG(dd.teomamc) 
                          FROM teoms.deployment_data dd 
                          JOIN instruments.deployments d 
                          ON dd.deployment_id=d.deployment_id 
                          WHERE datetime > timestamp ", start.time, 
                         "AND datetime < timestamp ", end.time, 
                         "AND dd.deployment_id 
                          IN ", deploys, 
                         "GROUP BY d.deployment, 
                         file_uploads.date_trunc_hour(datetime) 
                         ORDER BY d.deployment, datetime_hour"))
digital_pm10 <- rename(digital_pm10, pm10.avg=avg)
print(paste0("removing ", nrow(filter(digital_pm10, pm10.avg < -35)),
             " hours with pm10.avg < -35"))
digital_pm10 <- filter(digital_pm10, pm10.avg > -35)

# check to make sure that data for each teom station spans entire date range
print(
teom_data %>% group_by(deployment) %>%
  summarize(start.date = min(datetime), end.date = max(datetime))
)

# join compliance-quality pm10 data onto portable teom data points
teom_data <- left_join(teom_data, digital_pm10, by=c("datetime"="datetime_hour",
                                                 "deployment"))
combine_pm10 <- reshape2::melt(data.frame(data.id=teom_data$data.id, x=teom_data$pm10.avg.x, 
                                  y=teom_data$pm10.avg.y), id.var="data.id",
                               value.name="pm10.avg", na.rm=TRUE) %>%
                select(-variable)
teom_data <- teom_data %>% select(-pm10.avg.x, -pm10.avg.y) %>%
             inner_join(combine_pm10, by="data.id")

save(teom_data, teom_locs, 
     file=paste0("./data-clean/twb2_paired_teom_data_", tolower(month_char), 
                 report_year, ".RData"))

