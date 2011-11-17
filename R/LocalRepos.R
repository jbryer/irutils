#' This package will download all packages for the given type to \code{local.repos}
#' from the given repository.
#' @seealso \code{\link{download.packages}} which this function wraps
#' @export
createLocalRepos <- function(local.repos, repos='http://cran.r-project.org', type='source') {
	rversion = paste(R.version$major, strsplit(R.version$minor, '.', fixed=TRUE)[[1]][1], sep='.')
	pkgs = as.data.frame(available.packages(contriburl=contrib.url(repos, type=type)))
	dir = '/src/contrib'
	if(type == 'mac.binary.leopard' | type == 'mac.binary') {
		dir = paste('/bin/macosx/leopard/contrib/', rversion, '/', sep='')
	} else if(type == 'win.binary') {
		dir = paste('/bin/windows/contrib/', rversion, sep='')
	}
	dir.create(paste(local.repos, dir, sep=''), recursive=TRUE, showWarnings=FALSE)
	download.packages(pkgs$Package, paste(local.repos, dir, sep=''), repos=repos, type=type)
	write_PACKAGES(dir=local.repos, subdirs=TRUE, type=type)
	write_PACKAGES(dir=paste(local.repos, dir, sep=''), subdirs=TRUE, type=type)
}

#' This function will update the local repository with new packages and/or newer versions.
#' @export
updateLocalRepos <- function(local.repos, repos='http://cran.r-project.org', type='source') {
	rversion = paste(R.version$major, strsplit(R.version$minor, '.', fixed=TRUE)[[1]][1], sep='.')
	remote.pkgs = as.data.frame(available.packages(contriburl=contrib.url(repos, type=type)))
	local.contriburl = contrib.url(paste('file://', local.repos, sep=''), type=type)
	local.pkgs = available.packages(local.contriburl)
	remote.pkgs = as.data.frame(remote.pkgs)
	local.pkgs = as.data.frame(local.pkgs)
	new.pkgs = remote.pkgs[-which( paste(remote.pkgs$Package,remote.pkgs$Version) %in% paste(local.pkgs$Package,local.pkgs$Version)), ]
	if(nrow(new.pkgs) > 0) {
		dir = '/src/contrib'
		if(type == 'mac.binary.leopard' | type == 'mac.binary') {
			dir = paste('/bin/macosx/leopard/contrib/', rversion, '/', sep='')
		} else if(type == 'win.binary') {
			dir = paste('/bin/windows/contrib/', rversion, sep='')
		}
		download.packages(new.pkgs$Package, paste(local.repos, dir, sep=''), repos=repos, type=type)
		write_PACKAGES(dir=local.repos, subdirs=TRUE, type=type)
		write_PACKAGES(dir=paste(local.repos, dir, sep=''), subdirs=TRUE, type=type)		
	}
}

#' Returns a list of available packages in the local repository.
#' @seealso \code{\link{available.packages}} which this function wraps
#' @export
local.available.packages <- function(local.repos, type='source') {
	rversion = paste(R.version$major, strsplit(R.version$minor, '.', fixed=TRUE)[[1]][1], sep='.')
	local.contriburl = contrib.url(paste('file://', local.repos, sep=''), type=type)
	return(available.packages(local.contriburl))
}

#' Installs packages from the local repository.
#' @seealso \code{\link{install.packages}} which this function wraps
#' @export
local.install.packages <- function(pkgs, local.repos, type='source') {
	rversion = paste(R.version$major, strsplit(R.version$minor, '.', fixed=TRUE)[[1]][1], sep='.')
	local.contriburl = contrib.url(paste('file://', local.repos, sep=''), type=type)
	local.pkgs = available.packages(local.contriburl)

	dir = '/src/contrib/'
	extension = '.tar.gz'
	if(type == 'mac.binary.leopard' | type == 'mac.binary') {
		dir = paste('/bin/macosx/leopard/contrib/', rversion, '/', sep='')
		extension = '.tgz'
	} else if(type == 'win.binary') {
		dir = paste('/bin/windows/contrib/', rversion, '/', sep='')
		extension = '.zip'
	}
	installed = installed.packages()
	#package.dependencies(local.pkgs[local.pkgs[,'Package'] %in% pkgs,])
	
	d = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
	toinstall = utils:::getDependencies(pkgs, d, local.pkgs)
	ininstall2 = utils:::getDependencies(toinstall, d, local.pkgs)
	while(length(toinstall) != length(ininstall2)) {
		toinstall = ininstall2
		ininstall2 = utils:::getDependencies(toinstall, d, local.pkgs)		
	}
	
	for(i in 1:length(toinstall)) {
		toinstall[i] = paste(local.repos, dir,
				 toinstall[i], '_', 
				 local.pkgs[local.pkgs[,'Package'] == toinstall[i],'Version'], 
				 extension,
				 sep='')
	}
	print('Install the following packages:')
	print(toinstall)
	install.packages(toinstall, repos=NULL, contrib.url=local.contriburl, available=local.pkgs, type=type)
}

# local.install <- function(pkgs, local.repos, ...) {
# 	irutils.httpd.handler <- function(path, query, ...) {
# 		path = gsub("^/custom/irutils/", "", path)
# 		f = sprintf("%s%s%s",
# 					 local.repos,
# 					 .Platform$file.sep,
# 					 path) 
# 		list(file=f,
# 			 "content-type"="text/html",
# 			 "status code"=200L)
# 	}
# 
# 	if(!tools:::httpdPort > 0L) {
# 		tools:::startDynamicHelp()
# 	}
# 	env = get(".httpd.handlers.env", asNamespace("tools"))
# 	env[["irutils"]] = irutils.httpd.handler
# 	root.dir = local.repos
# 	file = file.path(local.repos)
# 	.url = sprintf("http://127.0.0.1:%s/custom/irutils", 
# 				tools:::httpdPort)
# 	#browseURL(.url)
# 	install.packages(pkgs, repos=.url, ...)
# }
# 
# local.available <- function(local.repos, ...) {
# 	irutils.httpd.handler <- function(path, query, ...) {
# 		path = gsub("^/custom/irutils/", "", path)
# 		f = sprintf("%s%s%s",
# 					local.repos,
# 					.Platform$file.sep,
# 					path) 
# 		list(file=f,
# 			 "content-type"="text/html",
# 			 "status code"=200L)
# 	}
# 	
# 	if(!tools:::httpdPort > 0L) {
# 		tools:::startDynamicHelp()
# 	}
# 	env = get(".httpd.handlers.env", asNamespace("tools"))
# 	env[["irutils"]] = irutils.httpd.handler
# 	root.dir = local.repos
# 	file = file.path(local.repos)
# 	.url = sprintf("http://127.0.0.1:%s/custom/irutils", 
# 				   tools:::httpdPort)
# 	available.packages(contriburl=contrib.url(.url, ...), ...)
# }
# 
# local.available('E:/Rrepos', type='win.binary')
# local.install('abind', 'E:/Rrepos/', type='source')
# available.packages(contrib.url('http://127.0.0.1:18778/custom/irutils', type='win.source'), type='source')
