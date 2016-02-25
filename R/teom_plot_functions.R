# teom_plot_functions.R -- functions for plotting TwB2 paired TEOM results
# John Bannister
# 

#' Plot paired TEOMS with wind roses.
#' 
#' Created multiple 3 PNG files (1 for each TwB2 TEOM pair, showing TwB2 areas, 
#' locations of TEOMS, and wind roses for time period. Function is specific to 
#' data as generated in teom_pairs_do.R file.
#' 
#' @param teom_locs Data frame. TEOMS with pairs grouping in *dca.group* column.
#' @param df1 Data frame. Hourly PM10 data.
#' @return 3 PNG files of plots in **output** subdirectory of working directory.
teom_pair_png_plots <- function(teom_locs, df1){
  twb2_dcas <- list("north (T29)" = c("T29-3", "T29-4"),
                    "central (T12)" = c("T12-1"),
                    "south (T2 & T3)" = c("T3SW", "T3SE", "T2-2", "T2-3", 
                                          "T2-4", "T5-4"))
  for (i in 1:3){
    sub_locs <- filter(teom_locs, dca.group==names(twb2_dcas)[i])
    a <- list(grobs=c(), centers=c())
    maxpm10 <- max(filter(df1, dca.group==sub_locs$dca.group[1])$pm10.avg)
    valueseq <- c(10, 50, 150, 500)
    legend.plot <- df1 %>% filter(dca.group==sub_locs$dca.group[1]) %>%
      plot_rose(., value='pm10.avg', dir='wd', valueseq=valueseq,
                legend.title="PM10")
    legnd <- g_legend(legend.plot)
    for (j in 1:2){
      p <- filter(df1, deployment==sub_locs$deployment[j]) %>% 
        plot_rose_image_only(., value='pm10.avg', dir='wd', valueseq=valueseq)
      png(filename="./output/p.png", bg="transparent")
      print(p)
      dev.off()
      img <- png::readPNG("./output/p.png")
      ras <- grid::rasterGrob(img, interpolate=TRUE)
      a$grobs[[j]] <- ras
      a$centers[[j]] <- c(sub_locs$x[j], sub_locs$y[j])
    }
    p2 <- plot_owens_borders(dcas=twb2_dcas[[i]]) + 
      theme(axis.line=element_blank(),
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
    info <- ggplot_build(p2)
    dca_xrange <- info[[2]]$ranges[[1]]$x.range
    plot_xrange <- c(min(dca_xrange, a$centers[[1]][1], a$centers[[2]][1]),
                     max(dca_xrange, a$centers[[1]][1], a$centers[[2]][1]))
    dca_yrange <- info[[2]]$ranges[[1]]$y.range
    plot_yrange <- c(min(dca_yrange, a$centers[[1]][2], a$centers[[2]][2]),
                     max(dca_yrange, a$centers[[1]][2], a$centers[[2]][2]))
    maxspan <- max(c(plot_xrange[2] - plot_xrange[1], 
                     plot_yrange[2] - plot_yrange[1]))
    midpoint <- c(mean(plot_xrange), mean(plot_yrange))
    buffer <- maxspan / 10
    xrange <- c(midpoint[1] - (maxspan/2) - buffer, 
                midpoint[1] + (maxspan/2) + buffer)
    yrange <- c(midpoint[2] - (maxspan/2) - buffer, 
                midpoint[2] + (maxspan/2) + buffer)
    p3 <- p2 + xlim(xrange[1], xrange[2]) + ylim(yrange[1], yrange[2]) +
      annotation_custom(a$grobs[[1]], xmin=a$centers[[1]][1] - 2*buffer,
                        xmax=a$centers[[1]][1] + 2*buffer, 
                        ymin=a$centers[[1]][2] - 2*buffer,
                        ymax=a$centers[[1]][2] + 2*buffer) +
annotation_custom(a$grobs[[2]], xmin=a$centers[[2]][1] - 2*buffer,
                  xmax=a$centers[[2]][1] + 2*buffer, 
                  ymin=a$centers[[2]][2] - 2*buffer,
                  ymax=a$centers[[2]][2] + 2*buffer) +
annotation_custom(legnd, xmin=xrange[2] - buffer, xmax=xrange[2],
                  ymin = yrange[1] + buffer, ymax=yrange[1] + buffer) +
geom_label(data=sub_locs, mapping=aes(x=x, y=y, label=deployment), 
           nudge_x=1.5*buffer)
png(filename=paste0("./output/", names(twb2_dcas)[i], " - ", month_char, " ",
                    report_year, ".png"), 
    width=6, height=6, units="in", res=300)
print(p3)
file.remove("./output/p.png")
dev.off()
  }
}

#' Create stripped down image of directional rose ggplot object.
#' 
#' Useful for greating image to be overlaid on map as grob.
#' 
#' @import ggplot2
#' @param data Data frame.  
#' @param value Text string. Column name of value variable.
#' @param dir Text string. Column name of direction variable.
#' @param dirres Numeric. Rose paddle angle width.
#' @param valuemin Numeric. Lower value limit. All rows with value < valuemin 
#' are removed from data.
#' @param valueseq Numberic. Bin cutoffs for values.
#' @param palette Text string. Name of ColorBrewer palette to use for value 
#' fill.
#' @param plot.label Text string. Will be placed at center of rose.
#' @return Returns the plot as a ggplot object. Paddles lengths (total and fill)
#' are proportional to percentage of total.
#' @examples
#' plot_rose(datafile, value='sand_flux', dir='resultant_wd_10m',
#'           valuemin=low.cutoff)
plot_rose_image_only <- function(data,
                                 value,
                                 dir,
                                 dirres = 22.5,
                                 valuemin = 0,
                                 valueseq,
                                 palette = "YlOrRd",
                                 plot.label){

  data <- data[data[[value]]>valuemin, ] 
  ifelse(missing(valueseq),
         valueseq <- round(seq(valuemin, max(data[[value]]), 
                               (max(data[[value]]) - valuemin) / 5), 0),
         valueseq <- c(valuemin, valueseq))
  # get some information about the number of bins, etc.
  n.value.seq <- length(valueseq)
  n.colors.in.range <- n.value.seq 
  # create the color map
  value.colors <- 
    colorRampPalette(RColorBrewer::brewer.pal(min(max(3,
                                                      n.colors.in.range),
                                                  min(9,
                                                      n.colors.in.range)),                                                 palette))(n.colors.in.range)
  value.breaks <- c(valueseq, max(data[[value]]) + 1)
  value.labels <- c(paste(c(valueseq[1:n.value.seq-1]), '-',
                          c(valueseq[2:n.value.seq])), 
                    paste0(valueseq[n.value.seq], "+"))
  data$value.binned <- cut(x = data[[value]],
                           breaks = value.breaks,
                           labels = value.labels,
                           ordered_result = TRUE)
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  # summarize data
  data_sum <- data %>% dplyr::group_by(dir.binned, value.binned) %>%
    summarize(value.prcnt = 100 * length(value.binned) / nrow(data))
  prcnt_sums <- data_sum %>% group_by(dir.binned) %>%
    summarize(total.prcnt = sum(value.prcnt))
  # y-axis labels data frame
  b <- data.frame(label = c('5%', "10%", "15%", "20%", "25%", "30%", "35%", 
                            "40%", "45%", "50%", "55%", "60%", "65%", "70%",
                            "75%", "80%", "85%", "90%", "95%"), 
                  value.prcnt = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55,
                                  60, 65, 70, 75, 80, 85, 90, 95),
                  dir.binned = rep('NW', 19))
  ind <- which(b$value.prcnt==mround(max(prcnt_sums$total.prcnt), 5))
  b <- b[1:ind, ]
  # create the plot ----
  p.rose <- ggplot(data = data_sum,
                   aes(x = dir.binned, y = value.prcnt,
                       fill = value.binned)) +
geom_bar(stat='identity', color="black") + 
scale_x_discrete(drop = FALSE,
                 labels = waiver()) +
scale_y_continuous(breaks=b$value.prcnt, labels = waiver()) +
coord_polar(start = -((dirres/2)/360) * 2*pi) +
scale_fill_manual(values = value.colors,
                  drop = FALSE) +
theme(axis.line=element_blank(),
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
if (missing(plot.label)){
  p.rose <- p.rose
} else{
  p.rose <- p.rose + geom_label(mapping=aes(y=0, label=plot.label, fill=NULL))
}
return(p.rose)
}

#' strip legend from ggplot object
#' 
#' @param a.gplot ggplot object.
#' @return A grob of the plot legend
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} 
