\name{cacheQuery}
\alias{cacheQuery}
\title{This will first look in the given directory for a CSV version of the file, if
it exists, that will be read and returned. Otherwise it will execute the query
and then saves a CSV file.}
\usage{
  cacheQuery(query = NULL, dir = getwd(), ...)
}
\description{
  This will first look in the given directory for a CSV
  version of the file, if it exists, that will be read and
  returned. Otherwise it will execute the query and then
  saves a CSV file.
}
\author{
  Jason Bryer <jbryer@excelsior.edu>
}

