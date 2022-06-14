# test ontology
require(psych)
require(devtools)
require(usethis)

factor_facets = list(A = letters[seq(1,3)], B=letters[seq(4, 6)], C=letters[seq(7,9)], D=letters[seq(10,12)])
facet_items = list(a = paste0('a', seq(1,5)),
				  b = paste0('b', seq(1,5)),
				  c = paste0('c', seq(1,5)),
				  d = paste0('d', seq(1,5)),
				  e = paste0('e', seq(1,5)),
				  f = paste0('f', seq(1,5)),
				  g = paste0('g', seq(1,5)),
				  h = paste0('h', seq(1,5)),
				  i = paste0('i', seq(1,5)),
				  j = paste0('j', seq(1,5)),
				  k = paste0('k', seq(1,5)),
				  l = paste0('l', seq(1,5)))


facet_dfs = lapply(facet_items, function(faceti, n = 500, low=-3, high=3, d=NULL, c=0, z=1, cat=5, mod='logistic', mu=0, sd=1){
	obj=sim.poly(nvar = length(faceti),n = n, low=low,high=high,d=d, cat=cat, mod=mod, c=c, z=z,mu=mu,sd=sd)
	df = obj$items
	colnames(df) = faceti
	list(obj=obj, df=df)
})

d = Reduce(cbind, extract(facet_dfs, df))

# use correlations from sim.poly
fload=fa(d, nfact=12, fm='pa')$loadings

# introduce correlations between factors
set.seed(1983)
gload = matrix(c(runif(3,0.5,0.99), rep(0,9),
				 rep(0,3), runif(3,0.5,0.99), rep(0,6),
				 rep(0,6), runif(3,0.5,0.99), rep(0,3),
				 rep(0,9), runif(3,0.5,0.99)), ncol=4)

d2 = sim.hierarchical(gload,fload, raw=TRUE, n=500)

ddf <- data.frame(apply(d2$observed, 2, function(x) as.numeric(unlist(cut(x,5, labels=1:5)))))
colnames(ddf) = sort(unlist(facet_items))
# str(ddf)
#
# corPlot(ddf)
# efa_60_12 = fa(ddf, nfact=12, fm='pa')
# efa_60_12_sco = factor.scores(ddf, efa_60_12)$scores
# efa_12_4 = fa(efa_60_12_sco, nfact=4, fm='pa', rotate='promax')

usethis::use_data(ddf, overwrite=T)
usethis::use_data(facet_items, overwrite=T)
usethis::use_data(factor_facets, overwrite=T)

#' Dummy Data Set
#'
#' @name ddf
#' @description A data.frame representing 4 global factors and 12 facets.
#' @docType data
#' @author Nikolaos Giallousis \email{ngiallousis@@psyget.gr}
#' #@references \url{data_blah.com}
#' @keywords data
NULL

#' Test Ontology
#'
#' @name facet_items
#' @description A list relating items to facets.
#' @docType data
#' @author Nikolaos Giallousis \email{ngiallousis@@psyget.gr}
#' #@references \url{data_blah.com}
#' @keywords data
NULL

#' Test Ontology
#'
#' @name factor_facets
#' @description A list relating facets to global factors.
#' @docType data
#' @author Nikolaos Giallousis \email{ngiallousis@@psyget.gr}
#' #@references \url{data_blah.com}
#' @keywords data
NULL
