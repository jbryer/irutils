\name{execQuery}
\alias{execQuery}
\title{Executes the specified query and returns a data frame. This function currently
supports RODBC, RSQLite, and RMySQL. For other databases, use getQuery() and
execute the SQL statement using the appropriate database connection.}
\usage{
  execQuery(query = NULL, connection = NULL, ...)
}
\description{
  Executes the specified query and returns a data frame.
  This function currently supports RODBC, RSQLite, and
  RMySQL. For other databases, use getQuery() and execute
  the SQL statement using the appropriate database
  connection.
}
\author{
  Jason Bryer <jbryer@excelsior.edu>
}

