#' Returns the current directory containing SQL files.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getSQLRepos <- function() {
	return(irutils:::sqlrepos)
}

#' Sets the current directory containing SQL files.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
setSQLRepos <- function(repos) {
	assignInNamespace("sqlrepos", repos, "irutils")
}

#' Returns a list of available queries in the current repository.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getQueries <- function() {
	files = list.files(path=getSQLRepos(), pattern="*.sql")
 	return( substr(files, 0, nchar(files)-4) )
}

#' Executes the specified query and returns a data frame. This function currently
#' supports RODBC, RSQLite, and RMySQL. For other databases, use getQuery() and
#' execute the SQL statement using the appropriate database connection.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
execQuery <- function(query=NULL, connection=NULL, ...) {
	sql = getQuery(query=query, ...)
	df <- NULL
	if(class(connection) == 'RODBC' ) {
		df <- sqlQuery(connection, sql)
	} else if(class(connection) == 'RSQLite') {
		df <- dbSendQuery(connection, sql)
	} else if(class(connection) == 'RMySQL') {
		df <- dbSendQuery(connection, sql)
	} else {
		stop('Unsupported database connection.')
	}
	return(df)
}

#' Returns the query as a string.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getQuery <- function(query=NULL, ...) {
	sql = scan(paste(getSQLRepos(), "/", query, ".sql", sep=''), what="character", 
			   sep=';', multi.line=FALSE, comment.char=c("#"), quiet=TRUE, quote=NULL)
	sql = paste(sql, collapse=" ")
 	parmvals = unlist(list(...))
 	if(length(parmvals)>0) {
	 	for(i in 1:length(parmvals)) {
			sql = gsub(paste(":", names(parmvals)[i], ":", sep=''), parmvals[i], sql)
	 	}
 	}
 	return(sql)
}

#' Returns the query as a string. For internal use only.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getQueryDesc <- function(query=NULL, ...) {
	desc = ''
	sql = scan(paste(getSQLRepos(), "/", query, ".sql", sep=''), what="character", 
			   sep=';', multi.line=FALSE, comment.char=c(""), quiet=TRUE, quote=NULL)
 	for(i in 1:length(sql)) {
 		tmp = strsplit(sql[i], '#')
   		if(length(tmp[[1]]) > 1) {
   			desc = paste(desc, tmp[[1]][length(tmp[[1]])], sep=' ')
   		}
 	}
 	return(desc)
}

#' This will first look in the given directory for a CSV version of the file, if
#' it exists, that will be read and returned. Otherwise it will execute the query
#' and then saves a CSV file.
#' @export
cacheQuery <- function(query=NULL, dir=getwd(), filename=NULL, ...) {
	parms = getParameters(query)
 	parmvals = unlist(list(...))
	if(is.null(filename)) {
		filename = paste(dir, '/', query, sep='')
		if(length(parms) > 0) {
		 	for(i in 1:length(parms)) {
		 		filename = paste(filename, parms[i], parmvals[parms[i]], sep='.')
		 	}
		}
	 	filename = paste(filename, 'csv', sep='.')
	}
 	message(paste("Cached query file:", filename))
	if(file.exists(filename)) {
		df = read.csv(filename)
	} else {
		df = execQuery(query=query, ...)
		write.csv(df, filename, row.names=FALSE)
	}
	return(df)
}



#' Returns the parameters that must be set for the given query.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getParameters <- function(query) {
	sql = getQuery(query)
	pos = gregexpr(":", sql)
 	results = c()
 	for(i in seq(1, length(pos[[1]]), by=2)) {
		results = c(results, (substr(sql, pos[[1]][i]+1, pos[[1]][i+1]-1)) )
 	}
 	return(unique(results))
}

