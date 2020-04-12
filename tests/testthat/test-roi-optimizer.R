library(ROI.plugin.glpk)
test_that("simple MILP integration test", {
  model <- ROI_optimizer("glpk")
  x <- moi_add_variables(model, 2)
  moi_set(
    model,
    objective_function,
    scalar_affine_function(list(scalar_affine_term(3, x[[1]]), scalar_affine_term(2, x[[2]])), 0)
  )
  moi_set(model, objective_sense, MAX_SENSE)
  moi_add_constraint(
    model,
    scalar_affine_function(list(scalar_affine_term(1, x[[1]]), scalar_affine_term(1, x[[2]])), 0.0),
    less_than_set(5.0)
  )
  moi_add_constraint(model, single_variable(x[[1]]), greater_than_set(0.0))
  moi_add_constraint(model, single_variable(x[[2]]), greater_than_set(-1.0))
  moi_optimize(model)
  result <- lapply(x, function(var) moi_get(model, variable_primal, var))
  expect_equal(result[[1]], 6)
  expect_equal(result[[2]], -1)
})

test_that("knapsack MIP integration test", {
  # based on the MOI.jl docs
  # http://www.juliaopt.org/MathOptInterface.jl/v0.8/apimanual/#Solving-and-retrieving-the-results-1
  b <- c(1.1, 2.0, 3.0)
  w <- c(0.3, 0.5, 1.0)
  C <- 1.2

  num_variables <- length(b)

  optimizer <- ROI_optimizer("glpk")

  x <- moi_add_variables(optimizer, num_variables)

  objective_function <- scalar_affine_function(
    lapply(seq_len(num_variables), function(i) {
      scalar_affine_term(b[[i]], x[[i]])
    }), 0.0
  )
  moi_set(optimizer, MOI::objective_function, objective_function)
  moi_set(optimizer, MOI::objective_sense, MOI::MAX_SENSE)

  knapsack_function <- scalar_affine_function(lapply(seq_len(num_variables), function(i) {
    scalar_affine_term(w[[i]], x[[i]])
  }), 0.0)
  moi_add_constraint(optimizer, knapsack_function, less_than_set(C))

  for (i in 1:num_variables) {
    moi_add_constraint(optimizer, single_variable(x[[i]]), zero_one_set)
  }

  moi_optimize(optimizer)

  termination_status <- moi_get(optimizer, termination_status)
  obj_value <- moi_get(optimizer, objective_value())
  expect_equal(termination_status, MOI::SUCCESS)

  expect_true(moi_get(optimizer, result_count) > 0)
  expect_equal(moi_get(optimizer, primal_status()), MOI::FEASIBLE_POINT)
  primal_variable_result <- moi_get(optimizer, variable_primal, x)
  expect_equal(primal_variable_result, c(1, 1, 0))
  expect_equal(obj_value, 3.1)
})

test_that("Binary variables", {
  model <- ROI_optimizer("glpk")
  x <- moi_add_constrained_variable(model, zero_one_set)
  y <- moi_add_constrained_variable(model, integer_set)
  z <- moi_add_constrained_variable(model, interval_set(10, 20))
  z1 <- moi_add_constrained_variable(model, less_than_set(10))
  moi_optimize(model)
})
