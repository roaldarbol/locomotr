library(dplyr)
library(ggplot2)
library(plotly)
kin_animate <- function(df){
# df <- res[[3]]
df$t <- specify_decimal(df$t, 3)

p <- ggplot(df, aes(frame=t)) +
  geom_point(aes(xHip, yHip)) +
  geom_point(aes(xKnee, yKnee)) +
  geom_point(aes(xAnkle, yAnkle)) +
  geom_segment(aes(x=xHip, y=yHip, xend=xKnee, yend=yKnee)) +
  geom_segment(aes(x=xKnee,y=yKnee, xend=xAnkle, yend=yAnkle)) +
  labs(x="", y="")

  # theme(panel.background = element_blank(),
  #       panel.grid = element_blank(),
  #       legend.key = element_rect(colour = "black"),
  #       legend.justification = c(1,0),
  #       legend.position = c(.27,.84),
  #       axis.line.x.bottom = element_line(colour = "black", size = .7),
  #       axis.line.y.left = element_line(colour = "black", size = .7),
  #       text = element_text(family = "serif", size = 10)
  # )

ggplotly(p, width=1200, height=300) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d")) %>%
  animation_opts(frame = 150,
                 easing = "linear",
                 redraw = FALSE) %>%
  animation_slider(currentvalue = list(visible=FALSE),
                   label=list(visible=FALSE))
}
