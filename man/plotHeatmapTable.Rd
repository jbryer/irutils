\name{plotHeatmapTable}
\alias{plotHeatmapTable}
\title{Function creates and returns a tabular heatmap ideal for summarizing likert
survey items.}
\usage{
  plotHeatmapTable(items, nlevels =
  length(which(!is.na(unique(items[, i])))), low.color =
  "white", high.color = "blue")
}
\arguments{
  \item{items}{the items to summarize.}
}
\description{
  Function creates and returns a tabular heatmap ideal for
  summarizing likert survey items.
}

