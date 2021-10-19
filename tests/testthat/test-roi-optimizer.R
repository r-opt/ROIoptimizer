library(ROI.plugin.glpk)
test_that("simple MILP integration test", {
  model <- ROI_optimizer("glpk")
  x <- moi_add_variables(model, 2)
  moi_set(
    model,
    moi_objective_function,
    moi_scalar_affine_function(list(moi_scalar_affine_term(3, x[[1]]), moi_scalar_affine_term(2, x[[2]])), 0)
  )
  moi_set(model, moi_objective_sense, MOI_MAX_SENSE)
  moi_add_constraint(
    model,
    moi_scalar_affine_function(list(moi_scalar_affine_term(1, x[[1]]), moi_scalar_affine_term(1, x[[2]])), 0.0),
    moi_less_than_set(5.0)
  )
  moi_add_constraint(model, moi_single_variable(x[[1]]), moi_greater_than_set(0.0))
  moi_add_constraint(model, moi_single_variable(x[[2]]), moi_greater_than_set(-1.0))
  moi_optimize(model)
  result <- lapply(x, function(var) moi_get(model, moi_variable_primal, var))
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

  objective_function <- moi_scalar_affine_function(
    lapply(seq_len(num_variables), function(i) {
      moi_scalar_affine_term(b[[i]], x[[i]])
    }), 0.0
  )
  moi_set(optimizer, moi_objective_function, objective_function)
  moi_set(optimizer, moi_objective_sense, MOI_MAX_SENSE)

  knapsack_function <- moi_scalar_affine_function(lapply(seq_len(num_variables), function(i) {
    moi_scalar_affine_term(w[[i]], x[[i]])
  }), 0.0)
  moi_add_constraint(optimizer, knapsack_function, moi_less_than_set(C))

  for (i in 1:num_variables) {
    moi_add_constraint(optimizer, moi_single_variable(x[[i]]), moi_zero_one_set)
  }

  moi_optimize(optimizer)

  termination_status <- moi_get(optimizer, moi_termination_status)
  obj_value <- moi_get(optimizer, moi_objective_value())
  expect_equal(termination_status, MOI_SUCCESS)

  expect_true(moi_get(optimizer, moi_result_count) > 0)
  expect_equal(moi_get(optimizer, moi_primal_status()), MOI_FEASIBLE_POINT)
  primal_variable_result <- moi_get(optimizer, moi_variable_primal, x)
  expect_equal(primal_variable_result, c(1, 1, 0))
  expect_equal(obj_value, 3.1)
})

test_that("Binary variables", {
  expect_silent({
    model <- ROI_optimizer("glpk")
    x <- moi_add_constrained_variable(model, moi_zero_one_set)
    y <- moi_add_constrained_variable(model, moi_integer_set)
    z <- moi_add_constrained_variable(model, moi_interval_set(10, 20))
    z1 <- moi_add_constrained_variable(model, moi_less_than_set(10))
    moi_optimize(model)
  })
})

test_that("Seting a single affine term as objective function", {
  model <- ROI_optimizer("glpk")
  x <- moi_add_variable(model)
  expect_silent(
    moi_set(model, moi_objective_function, moi_scalar_affine_term(42, x))
  )
})
