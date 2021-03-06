% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelib_functions.R
\name{analyze_items}
\alias{analyze_items}
\title{Quick CTT and Rasch Item Analysis}
\usage{
analyze_items(faceti, d)
}
\examples{
data(ddf)
item_analysis0 = lapply(facet_items, analyze_items, d=ddf)
ctt0 = extract(item_analysis0, ctt)
alphas0 = func2matx(rbind, ctt0, extract, total)
item_stats0 = extract(ctt0, item.stats)

rasch0 = extract(item_analysis0, rasch)
ifit0 = extract(item_analysis0, ifit)
pfit0 = extract(item_analysis0, pfit)
}
