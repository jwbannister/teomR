devtools::load_all("../Rowens")
library(dplyr)

teom_df <- query_owenslake("SELECT * FROM teoms.teom_summary_data WHERE datetime > timestamp '2015-12-01 00:00:00' AND datetime < timestamp '2016-01-01 00:00:00'")
if (sum(teom_df$qaqc_level_id!=0)>0) print("QA/QC failures in data!")
teom_df <- teom_df[teom_df$qaqc_level_id==0, ]
# select out relevant data columns from TEOM data. Select scalar wind speed and 
# scalar wind direction averages (not vector) 
teom_df <- select(teom_df, data.id = teom_summary_data_id, datetime, deployment.id = deployment_id,
                  pm10.avg = pm10_avg, wd.avg = wd, ws.avg = ws)

mfile <- query_owenslake("SELECT * FROM archive.mfile_data WHERE datetime > timestamp '2015-12-01 00:00:00' AND datetime < timestamp '2016-01-01 00:00:00' AND (site = 'T7' OR site = 'Olancha' OR site = 'NorthBch')")
mfile <- mfile[is.na(mfile$qaqc_level_id), ]
# select out relevant columns (scalar speed and direction averages).
mfile <- select(mfile, data.id = did, datetime, deployment.id = deployment_id, pm10.avg = teom, 
                wd.avg = dir, ws.avg = aspd)
teom_data <- rbind(teom_df, mfile)
teom_data <- filter(teom_data, deployment.id!=8)

deploys <- as.character(unique(teom_data$deployment.id))
deploys <- paste(deploys, collapse=", ")
deploys <- paste0("(", deploys, ")")
teom_locs <- query_owenslake(paste0("SELECT deployment_id, deployment, northing_utm, easting_utm, description FROM instruments.deployments WHERE deployment_id IN ", deploys))
colnames(teom_locs) <- gsub("_", ".", colnames(teom_locs))

save(teom_data, teom_locs, file="./data-clean/teom_data.RData")
