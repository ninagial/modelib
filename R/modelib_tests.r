require(testthat)

context("Test Ontology is Constructed Correctly")

test_that("Test Ontology Object have consistent lengths and values", {
		expect_that(length(factor_facets), is_equivalent_to(4))
		expect_that(sapply(factor_facets, length), is_equivalent_to(rep(3,4)))
		expect_that(names(factor_facets), is_equivalent_to(LETTERS[1:4]))
		expect_true(all(unlist(factor_facets) == names(facet_items)))
		expect_that(names(table(unlist(facet_items))), is_equivalent_to(paste0(rep(letters[1:12], each=5), seq(1:5))))
})



