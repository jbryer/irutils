\name{plotBarchartTable}
\alias{plotBarchartTable}
\title{Creates a bar chart using percentages ideal for likert items.}
\usage{
  plotBarchartTable(items, grouping = NULL, nlevels =
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

