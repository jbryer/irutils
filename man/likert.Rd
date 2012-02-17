\name{likert}
\alias{likert}
\title{Constructor function to create a likert class.}
\usage{
  likert(items, grouping = NULL,
    nlevels = length(levels(items[, 1])))
}
\arguments{
  \item{items}{data frame containing the likert based
  items. The variables in the data frame should be
  factors.}

  \item{grouping}{(optional) should the results be
  summarized by the given grouping variable.}

  \item{nlevels}{number of possible levels. Only necessary
  if there are missing levels.}
}
\description{
  TODO: Need more documentation
}

