#' In-Memory backend that simply stores everything in memory and matrix form
#'
#' @keywords internal
#' @import MOI
#' @export
setClass("ROI_optimizer", contains = "MOI_abstract_optimizer", slot = c(ptr = "ANY"))

#' A ROI Optimizer
#'
#' @param solver a ROI solver name
#' @param control a list of controls
#'
#' @include roi-optimizer-r6class.R
#' @export
ROI_optimizer <- function(solver, control = list()) {
  new("ROI_optimizer", ptr = ROIOptimizerClassR6$new(solver, control))
}

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_variable", signature("ROI_optimizer"), function(model) {
  new("MOI_variable_index", value = model@ptr$add_variable("continious"))
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_scalar_affine_function", "MOI_less_than_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- model@ptr$add_linear_constraint(func, "<=", set@upper - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_scalar_affine_function", "MOI_equal_to_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- model@ptr$add_linear_constraint(func, "==", set@value - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_scalar_quadratic_function", "MOI_less_than_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- model@ptr$add_quadratic_constraint(func, "<=", set@upper - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_scalar_quadratic_function", "MOI_equal_to_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- model@ptr$add_quadratic_constraint(func, "==", set@value - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_scalar_affine_function", "MOI_greater_than_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- model@ptr$add_linear_constraint(func, ">=", set@lower - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_scalar_quadratic_function", "MOI_greater_than_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- model@ptr$add_quadratic_constraint(func, ">=", set@lower - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_single_variable", "MOI_greater_than_set"), function(model, func, set) {
  model@ptr$set_variable_lb(func@variable@value, set@lower)
  new("MOI_constraint_index", value = -1) #TODO
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_single_variable", "MOI_zero_one_set"), function(model, func, set) {
  model@ptr$set_variable_type(func@variable@value, "integer")
  model@ptr$set_variable_lb(func@variable@value, 0)
  model@ptr$set_variable_ub(func@variable@value, 1)
  new("MOI_constraint_index", value = -1) #TODO
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_single_variable", "MOI_interval_set"), function(model, func, set) {
  model@ptr$set_variable_lb(func@variable@value, set@lower)
  model@ptr$set_variable_ub(func@variable@value, set@upper)
  new("MOI_constraint_index", value = -1) #TODO
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_single_variable", "MOI_less_than_set"), function(model, func, set) {
  model@ptr$set_variable_ub(func@variable@value, set@upper)
  new("MOI_constraint_index", value = -1) #TODO
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_add_constraint", signature("ROI_optimizer", "MOI_single_variable", "MOI_integer_set"), function(model, func, set) {
  model@ptr$set_variable_type(func@variable@value, "integer")
  new("MOI_constraint_index", value = -1) #TODO
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_set", signature("ROI_optimizer", "MOI_objective_function", "MOI_scalar_quadratic_function", "missing"), function(model, type, index, value) {
  index <- canonicalize(index)
  model@ptr$set_quadratic_objective(index)
  model
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_set", signature("ROI_optimizer", "MOI_objective_function", "MOI_scalar_affine_function", "missing"), function(model, type, index, value) {
  index <- canonicalize(index)
  model@ptr$set_linear_objective(index)
  model
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_set", signature("ROI_optimizer", "MOI_objective_function", "MOI_scalar_affine_term", "missing"), function(model, type, index, value) {
  index <- scalar_affine_function(list(index), 0)
  model@ptr$set_linear_objective(index)
  model
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_set", signature("ROI_optimizer", "MOI_objective_function", "numeric", "missing"), function(model, type, index, value) {
  model@ptr$set_linear_objective(
    MOI::scalar_affine_function(list(), index)
  )
  model
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_set", signature("ROI_optimizer", "MOI_objective_sense", "MOI_optimization_sense", "missing"), function(model, type, index, value) {
  sense <- if (inherits(index, "MOI_optimization_sense_max")) "max" else "min"
  model@ptr$set_objective_sense(sense)
  model
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_optimize", signature("ROI_optimizer"), function(model) {
  model@ptr$optimize()
  model
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_variable_primal_attribute", "MOI_variable_index"), function(model, type, index) {
  model@ptr$get_variable_value(index@value)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_variable_dual_attribute", "MOI_variable_index"), function(model, type, index) {
  model@ptr$get_variable_dual(index@value)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_constraint_dual", "MOI_constraint_index"), function(model, type, index) {
  model@ptr$get_row_dual(index@value)
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_termination_status", "missing"), function(model, type) {
  model@ptr$get_termination_status()
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_number_of_variables", "missing"), function(model, type) {
  model@ptr$nvars()
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_number_of_constraints", "missing"), function(model, type) {
  model@ptr$nconstraints()
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_termination_solver_message_attribute", "missing"), function(model, type) {
  model@ptr$get_termination_message()
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_objective_value", "missing"), function(model, type) {
  model@ptr$get_objective_value()
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_primal_status", "missing"), function(model, type) {
  if (all.equal(model@ptr$get_termination_status(), MOI::SUCCESS)) {
    MOI::FEASIBLE_POINT
  } else {
    MOI::NO_SOLUTION
  }
})

#' @export
#' @rdname ROI_optimizer-class
setMethod("moi_get", signature("ROI_optimizer", "MOI_result_count", "missing"), function(model, type) {
  if (all.equal(model@ptr$get_termination_status(), MOI::SUCCESS)) {
    1L
  } else {
    0L
  }
})
