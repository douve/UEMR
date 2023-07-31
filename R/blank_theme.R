

blank_theme <- ggplot2::theme_minimal()+
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid=ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    plot.title=ggplot2::element_text(size=14, face="bold")
  )
