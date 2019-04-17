

interactivePlot <-
ggplot2::ggplot(bets) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = bets, xend = bets,
      y = modPrice, yend = mktPrice
      )
  ) +
  ggiraph::geom_point_interactive( 
    ggplot2::aes(
      x = bets,
      y = modPrice,
      tooltip = paste0("Model Price:", round(mktPrice, 2)), 
      data_id = paste0("Model Price:", round(mktPrice, 2))
    ), 
    colour = "grey",
    size = 3
  ) +
  ggiraph::geom_point_interactive( 
    ggplot2::aes(
      x = bets,
      y = mktPrice,
      tooltip = paste0("Market Price:", round(mktPrice, 2)), 
      data_id = paste0("Market Price:", round(mktPrice, 2))
      ), 
    colour = "blue",
    size = 3
  ) +
  ggplot2::labs(
    y = "Fractional Odds",
    title = "Market Odds Value"
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.y = element_blank(),
    axis.text = element_text(size = 9)
  )

ggiraph::girafe(code = print(interactivePlot) )

