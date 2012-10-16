\name{pie}
\alias{pie}
\title{Creates a pie chart using ggplot2.}
\usage{
  pie(df, col, label = col)
}
\arguments{
  \item{df}{the data frame.}

  \item{col}{the name of the column to generate the pie
  chart for.}

  \item{label}{the label for the legend.}
}
\description{
  Use pie charts with care. See
  http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=00018S
  on Edward Tufte's website for good arguments against the
  use of pie charts. For a contrary point-of-view, see
  Spence's article, No Humble Pie: The Origins and Usage of
  a Statistical Chart
  (http://www.psych.utoronto.ca/users/spence/Spence%202005.pdf).
}

