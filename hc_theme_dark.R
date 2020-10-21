hc_dark_theme <- function(...) {
  theme <- hc_theme(
    chart = list(
      backgroundColor = "#34495e"
    ),
    xAxis = list(
      labels = list(
        style = list(
          color = "#FFFFFF"
        )
      ),
      gridLineColor = "#46627f",
      #tickColor = "#46627f",
      lineColor = "#FFFFFF",
      title = list(
        style = list(
          color = "#FFFFFF"
        )
      )
    ),
    yAxis = list(
      labels = list(
        style = list(
          color = "#FFFFFF"
        )
      ),
      gridLineColor = "#46627f",
      lineColor = "#FFFFFF",
      tickColor = "#FFFFFF",
      title = list(
        style = list(
          color = "#FFFFFF"
        )
      )
    ),
    legend = list(
      itemStyle = list(
        color = "#FFFFFF"
      ),
      itemHoverStyle = list(
        color = "cyan"
      ),
      itemHiddenStyle = list(
        color = "gray"
      )
    )
  )
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

hc_light_theme <- function(...) {
  theme <- hc_theme(
    chart = list(
      backgroundColor = "none"
    ),
    xAxis = list(
      lineColor = "black"
    ),
    yAxis = list(
      lineColor = "black"
    )
  )
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}


