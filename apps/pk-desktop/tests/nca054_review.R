source('apps/pk-desktop/tests/nca053_review.R')

test_that('the initially hidden period hint stays eligible to receive Shiny updates', {
 code<-paste(readLines('apps/pk-desktop/R/nca.R',warn=FALSE),collapse='\n')
 expect_match(code,'shiny::outputOptions(output, "period_order_run_hint", suspendWhenHidden = FALSE)',fixed=TRUE)
})
