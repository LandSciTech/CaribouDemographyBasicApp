

test_that("bboutools data works", {
  surv_data <- rbind(bboudata::bbousurv_a, bboudata::bbousurv_b) %>%
    filter(Year %in% c(2012:2016))
  recruit_data <- rbind(bboudata::bbourecruit_a, bboudata::bbourecruit_b) %>%
    filter(Year %in% c(2012:2016))
  N0 <- 500
  expect_s3_class(bbouMakeSummaryTable(surv_data, recruit_data, N0), "data.frame")

})
