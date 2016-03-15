#' Create directional rose ggplot object
#' 
#' @import ggplot2
#' @import RColorBrewer
#' @param data Data frame.  
#' @param value Text string. Column name of value variable.
#' @param dir Text string. Column name of direction variable.
#' @param valuemin Numeric. Lower value limit. All rows with value < valuemin 
#' are removed from data.
#' @param valueseq Numberic. Bin cutoffs for values.
#' @param palette Text string. Name of ColorBrewer palette to use for value 
#' fill.
#' @param legend.title Text string.
#' @param plot.title Text string.
#' @return Returns the plot as a ggplot object. Paddles lengths (total and fill)
#' are proportional to percentage of total.
#' @examples
#' plot_rose(datafile, value='sand_flux', dir='resultant_wd_10m',
#'           valuemin=low.cutoff, legend.title="Sand Flux",
#'           plot.title="Sand Roses")
plot_rose <- function(data,
                      value,
                      dir,
                      dirres = 22.5,
                      valuemin = 0,
                      valueseq,
                      palette = "YlOrRd",
                      legend.title = "",
                      plot.title = "",
                      ylabel.loc="NW"){

  data <- data[data[[value]]>valuemin, ] 
  ifelse(missing(valueseq),
    valueseq <- round(seq(valuemin, max(data[[value]]), 
                    (max(data[[value]]) - valuemin) / 5), 0)[1:5],
    valueseq <- c(valuemin, valueseq))
  # get some information about the number of bins, etc.
  n.value.seq <- length(valueseq)
  n.colors.in.range <- n.value.seq 
  # create the color map
  value.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                      n.colors.in.range),
                                                  min(9,
                                                      n.colors.in.range)),                                               
                                              palette))(n.colors.in.range)
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
                  dir.binned = rep(ylabel.loc, 19))
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
scale_fill_manual(name = legend.title, 
                  values = value.colors,
                  drop = FALSE) +
theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
ggtitle(plot.title) +
geom_text(data=b, aes(label=label, fill=NULL), size=3)

return(p.rose)
}

