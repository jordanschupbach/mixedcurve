# -*- origami-fold-style: triple-braces -*-

# {{{ License
# Copyright (C) <2025>  <Jordan Schupbach>
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.
# }}} License

# TODO: add function to parse lpk formulas

# {{{ lpk
#' Local polynomial Kernel regression
#'
#' @param formula A formula object specifying the model to be fitted of the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate the fitted values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
lpk <- function(formula, 
                queries,
                data,
                degree = 0,
                kernel = mixedcurve::gauss_kern,
                h,
                parallel = FALSE) { }
# }}} lpk

# {{{ glpk
#' Generalized Local polynomial Kernel regression
#'
#' @param formula A formula object specifying the model to be fitted of the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate the fitted values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
glpk <- function(formula, degree = 0,
                queries,
                data,
                kernel = mixedcurve::gauss_kern,
                h,
                family = "gaussian",
                parallel = FALSE) {

}
# }}} lpk

# {{{ lpkme
#' Local polynomial Kernel Mixed-Effect regression
#'
#' @param formula A formula object specifying the model to be fitted of the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate the fitted values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
lpkme <- function(formula,
                queries,
                data,
                h, 
                degree = 0,
                kernel = mixedcurve::gauss_kern,
                parallel = FALSE) { }
# }}} lpkme

# {{{ glpkme
#' Local polynomial Kernel Mixed-Effect regression
#'
#' @param formula A formula object specifying the model to be fitted of the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate the fitted values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
glpkme <- function(formula,
                queries,
                data,
                h,
                degree = 0,
                kernel = mixedcurve::gauss_kern,
                family = "poisson",
                parallel = FALSE) { }
# }}} lpkme

# Local Variables:
# eval: (origami-mode t)
# End:
