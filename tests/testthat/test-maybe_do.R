test_that("multiplication works", {
  expect_equal(maybe_do(c(2, 2,100), TRUE, function(x){x*2}), c(4,4,200))
})
