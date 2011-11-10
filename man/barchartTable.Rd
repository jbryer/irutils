\name{barchartTable}
\alias{barchartTable}
\title{Creates a bar chart using percentages ideal for likert items.}
\usage{
  barchartTable(items, nlevels =
  length(which(!is.na(unique(items[, i])))), low.color =
  "blue", high.color = "red", neutral.color = "white")
}
\arguments{
  \item{items}{the items to summarize.}
}
\description{
  Creates a bar chart using percentages ideal for likert
  items.
}

