devtools::load_all("../Rowens")
library(dplyr)

# pull data from portable teoms, keep wind direction and speed
print("pulling teom_summary...")
teom_df <- query_owenslake("SELECT * FROM teoms.teom_summary_data WHERE datetime > timestamp '2015-12-01 00:00:00' AND datetime < timestamp '2016-01-01 00:00:00'")
if (sum(teom_df$qaqc_level_id!=0)>0) print("QA/QC failures in data!")
teom_df <- teom_df[teom_df$qaqc_level_id==0, ]
teom_df <- select(teom_df, data.id = teom_summary_data_id, datetime, 
                  deployment.id = deployment_id, wd, ws)
teom_df <- filter(teom_df, deployment.id!=8)

# pull data from m-files for T7 teom, keep wind direction and speed
print("pulling m-file...")
mfile <- query_owenslake("SELECT * FROM archive.mfile_data WHERE datetime > timestamp '2015-12-01 00:00:00' AND datetime < timestamp '2016-01-01 00:00:00' AND site = 'T7'")
mfile <- mfile[is.na(mfile$qaqc_level_id), ]
mfile <- select(mfile, data.id = did, datetime, deployment.id = deployment_id, 
                wd = dir, ws = aspd)

wind_data <- rbind(teom_df, mfile)

deploys <- as.character(unique(wind_data$deployment.id))
deploys <- paste0("(", paste(deploys, collapse=", "), ")")
teom_locs <- query_owenslake(paste0("SELECT deployment_id, deployment, northing_utm, easting_utm, description FROM instruments.deployments WHERE deployment_id IN ", deploys))
colnames(teom_locs) <- gsub("_", ".", colnames(teom_locs))
teom_locs <- select(teom_locs, -description)
teom_locs <- rename(teom_locs, x=easting.utm, y=northing.utm)

wind_data <- inner_join(wind_data, 
                        select(teom_locs, deployment.id, deployment),
                        by="deployment.id") 

print("pulling PM10 data...")
compliance_df <- query_owenslake(paste0("SELECT d.deployment, file_uploads.date_trunc_hour(datetime) AS datetime_hour, AVG(dd.teomamc) FROM teoms.deployment_data dd JOIN instruments.deployments d ON dd.deployment_id=d.deployment_id WHERE datetime > timestamp '2015-12-01 00:00:00' AND datetime < timestamp '2016-01-01 00:00:00' AND dd.deployment_id IN ", deploys, "GROUP BY d.deployment, file_uploads.date_trunc_hour(datetime) ORDER BY d.deployment, datetime_hour"))
compliance_df <- rename(compliance_df, pm10.avg=avg)
print(paste0("removing ", nrow(filter(compliance_df, pm10.avg < -35)),
             " hours with pm10.avg < -35"))
compliance_df <- filter(compliance_df, pm10.avg > -35)

teom_data <- left_join(wind_data, compliance_df, by=c("datetime"="datetime_hour",
                                                 "deployment"))

save(teom_data, teom_locs, file="./data-clean/teom_data.RData")

