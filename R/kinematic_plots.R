#' A Standard kinematic plot
#'
#' Changes between radians and degrees
#' @param df Data frame.
#' @param df.phase Data frame containing information of touchdown/take-off times.
#' @param y Parameter to be plotted. Specified as string with quotes.
#' @param x Typically time (default=t).
#' @export
#'

plot_kinematic <- function(df, y='value', x='t', group='segment',
                           df2 = df.phase){
  temp.data <- data.frame(df)
  temp.phase <- rbind(df2$td, df2$to)
  temp.phase2 <- c(rbind(df2$td, df2$to))
  p <-ggplot2::ggplot() +
    # geom_vline(xintercept = df2$td,
    #            linetype = "dashed",
    #            alpha = .2) +
    # geom_vline(xintercept = df2$to,
    #            linetype = "dashed",
    #            alpha = .2) +
    geom_rect(aes(xmin = df2$td,
                xmax = df2$to,
                ymin = ifelse(min(temp.data$value) > 100, 100, min(temp.data$value)-abs(min(temp.data$value)/20)),
                ymax = max(temp.data$value)+(max(temp.data$value)/20),
                fill = df2$foot
                ),
            alpha = .5,
            show.legend = FALSE) +
    scale_fill_viridis_d(guide=FALSE,
                         direction = -1)

  if (min(temp.data$value) < 0){
  p <- p + geom_hline(yintercept = 0,
             alpha = .3,
             linetype = 5)
  }


  p <- p +
    geom_line(aes(x = temp.data$t,
                  y = temp.data$value,
                  colour = temp.data$segment),
              #colour = "orange",
              show.legend = TRUE) +
    geom_point(aes(x = temp.data$t,
                  y = temp.data$value,
                  color = temp.data$segment),
               show.legend = FALSE
               ) +
    labs(x = "",
         y = "") +
    scale_x_continuous(breaks = temp.phase2,
                       labels = paste0(specify_decimal(temp.phase2, 3), ' s')) +
    scale_color_viridis_d() +
    # scale_fill_brewer(palette = "Spectral") +
    scale_y_continuous(limits = c(min(df[[y]]), max(df[[y]]))) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          legend.key = element_rect(colour = "black"),
          legend.justification = c(1,0),
          legend.position = c(.27,.84),
          axis.line.x.bottom = element_line(colour = "black", size = .7),
          axis.line.y.left = element_line(colour = "black", size = .7),
          text = element_text(family = "serif", size = 10)
    )
   p <- plotly::ggplotly(p, tooltip=c("y")) %>%
     plotly::layout(
       hovermode = "x unified",
       # hoverinfo="y",
       # hoverlabel = list(
       #   yshift=-100
       # ),
       showlegend=FALSE,
       spikedistance=-1,
       xaxis = list(
         # type="linear",
         # dtick=1,
         showspikes=TRUE,
         # showline=TRUE,
         spikemode="toaxis+across+marker",
         spikesnap="cursor+data",
         spikedash="solid",
         spikethickness=2,
         tickangle=-45,
         ticksuffix=" s"
         # showticksuffix="all",
         # tickprefix="hey ",
         # showticklabels=TRUE
         )
       )

  return(p)
}

#' A Plotly-based kinematic plot
#'
#' Makes a standard plotly figure.
#' @param df Data frame.
#' @param df.phase Data frame containing information of touchdown/take-off times.
#' @param y Parameter to be plotted. Specified as string with quotes.
#' @param x Typically time (default=t).
#' @export
#'

plot_kinematics.plotly <- function(df, y='value', x='t', group='segment',
                                   df2 = df.phase){
  #temp.data <- data.frame(df)
  temp.phase <- rbind(df2$td, df2$to)
  temp.phase2 <- c(rbind(df2$td, df2$to))

  p <- plotly::plot_ly(data=df,
                  x= ~t,
                  y=~value,
                  color=~segment,
                  colors = viridis(nlevels(df$segment),
                                   alpha = 1,
                                   begin = 0,
                                   end = 1,
                                   direction = -1),
                  type='scatter',
                  mode='lines+markers',
                  line=list(shape='spline')
  )
  return(p)
}
