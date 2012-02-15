\name{saveIPEDStoDB}
\alias{saveIPEDStoDB}
\title{This function will add all the IPEDS surveys to the given database (vis-a-vis the
connection object). Typically this will be a SQLite database. Note that this
function will make efforts to conform differing table structres across years but
it is up to the user to verify the data integrity. Typically variables present in
one year and not others will simply be identified as missing (i.e. NA). Since
IPEDS periodically adds, removes, and changes variables problems may occur.}
\usage{
  saveIPEDStoDB(conn, surv = surveys$SurveyID,
    years = 2010:2006)
}
\description{
  This function will add all the IPEDS surveys to the given
  database (vis-a-vis the connection object). Typically
  this will be a SQLite database. Note that this function
  will make efforts to conform differing table structres
  across years but it is up to the user to verify the data
  integrity. Typically variables present in one year and
  not others will simply be identified as missing (i.e.
  NA). Since IPEDS periodically adds, removes, and changes
  variables problems may occur.
}

