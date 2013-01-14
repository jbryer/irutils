## irutils

irutils is an R package providing common utility functions convenient for institutional researchers. To get started, execute the following commands in R:

	install.packages(c('devtools', 'roxygen2'), repos='http://cran.r-project.org')
	install_github('irutils', 'jbryer')

If you are an instituional researcher, it is likely you will want to install the `ipeds` package as well. That package is hosted by Github and can be installed with the following command:

	install_github('ipes','jbryer')

Note that many of the functions that use to be in `irutils` have been moved to their own package, including [`retention`](http://jason.bryer.org/retention), [`likert`](http://jason.bryer.org/likert), and [`sqlutils`](http://jason.bryer.org/sqlutils).

More information at [http://jason.bryer.org/irutils](http://jason.bryer.org/irutils)
