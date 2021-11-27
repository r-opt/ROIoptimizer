test_that("you can extract a ROI OP object from a ROI optimizer", {
  solver <- ROI_optimizer("glpk")
  x <- moi_add_variables(solver, 2)
  res <- ROI::as.OP(solver)
  expect_s3_class(res, "OP")
})
