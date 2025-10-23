
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

# {{{ classify terms

#' Classify terms in a formula
#'
#' This function takes a formula and classifies each term in the formula as
#' "response", "fixed effect", "random effect", or "kernel fixed effect".
#'
#' @param form A formula object.
#'
#' @return A data frame with two columns: "term" and "type". The "term" column
#' contains the terms from the formula, and the "type" column contains the
#' corresponding classifications.
#'
#' @export
#'
#' @examples
#' form <- y ~ K_h(x | a ) + c * d + (K_h(x) | ind / rep) + (x | rep)
#' classify_terms(form)
classify_terms <- function(form) {
  if (!inherits(form, "formula")) {
    stop("Input must be a formula")
  }
  if (length(form) != 3) {
    stop("Formula must have a left-hand side and a right-hand side")
  }
  length(form)
  response_char <- as.character(form[2])
  predict_char <- as.character(form[3])
  terms <- strsplit(predict_char, "\\s*\\+\\s*")[[1]]
  classify_term <- function(term) {
    # Check for kernel fixed effect
    if (grepl("^K_h\\(.+\\)$", term)) {
      return("kernel fixed effect")
    }
    # Check for kernel random effect
    if (grepl("^\\(K_h\\(.+\\)\\s*\\|\\s*.+\\)$", term) ||
        grepl("^\\(K_h\\(.+\\)\\|.+( / .+)?\\)$", term)) {
      return("kernel random effect")
    }
    # Check for standard random effect
    if (grepl("^\\(.+\\|.+( / .+)?\\)$", term)) {
      return("random effect")
    }
    # Check for fixed effect
    if (grepl("^.+(\\s\\*\\s.+)?$", term)) {
      return("fixed effect")
    }
    return("unknown")
  }
  classified_terms <- sapply(terms, classify_term)
  classified_terms
  ret <- data.frame("term" = response_char,
                    type = "response",
                    stringsAsFactors = FALSE)
  for (i in seq_along(classified_terms)) {
    ret <- rbind(ret,
                 data.frame("term" = terms[i],
                            type = classified_terms[i],
                            stringsAsFactors = FALSE))
  }
  rownames(ret) <- seq_len(nrow(ret))
  ret
}

# }}} classify terms


parse_kernel_term <- function(term) {
  matches <- regmatches(
    term,
    regexec("K_h\\(([^|]+?)(?:\\s*\\|\\s*([^\\)]+))?\\)", term)
  )
  lhs <- trimws(matches[[1]][2])  
  rhs <- if (length(matches[[1]]) > 2 && !is.na(matches[[1]][3]) && matches[[1]][3] != "") {
    trimws(matches[[1]][3])  
  } else {
    NA  
  }
  list(lhs = lhs, rhs = rhs)
}



# {{{ parse terms
parse_terms <- function(form) {
  classified <- classify_terms(form)
  parsed_terms <- data.frame(term = character(), type = character(),
                             lhs = character(), rhs = character(),
                             stringsAsFactors = FALSE)
  for (i in seq_len(nrow(classified))) {
    term <- classified$term[i]
    term_type <- classified$type[i]
    
    if (grepl("^K_h\\(", term)) {
      if (term_type == "kernel fixed effect") {
        lhs_rhs <- parse_kernel_term(term)
        lhs <- lhs_rhs$lhs
        rhs <- lhs_rhs$rhs
        parsed_terms <- rbind(parsed_terms, 
                              data.frame(term = term, type = term_type,
                                         lhs = lhs, rhs = rhs,
                                         stringsAsFactors = FALSE))
      }
    } else if (term_type == "kernel random effect") {
      matches <- regmatches(term, regexec("\\((.+?)\\s*\\|\\s*(.+?)\\)", term))
      if (length(matches[[1]]) > 1) {
        lhs <- trimws(matches[[1]][2])
        rhs <- trimws(matches[[1]][3])
        parsed_terms <- rbind(parsed_terms,
                              data.frame(term = term, type = term_type,
                                         lhs = lhs, rhs = rhs,
                                         stringsAsFactors = FALSE))
      }
    } else if (term_type == "random effect") {
      matches <- regmatches(term, regexec("\\((.+?)\\|(.+?)\\)", term))
      if (length(matches[[1]]) > 1) {
        lhs <- trimws(matches[[1]][2])
        rhs <- trimws(matches[[1]][3])
        parsed_terms <- rbind(parsed_terms,
                              data.frame(term = term, type = term_type,
                                         lhs = lhs, rhs = rhs,
                                         stringsAsFactors = FALSE))
      }
    } else if (term_type == "fixed effect" || term_type == "response") {
      parsed_terms <- rbind(parsed_terms,
                            data.frame(term = term, type = term_type,
                                       lhs = term, rhs = NA,
                                       stringsAsFactors = FALSE))
    }
  }
  
  parsed_terms[parsed_terms == "<NA>"] <- NA
  parsed_terms[parsed_terms == "NA"] <- NA
  rownames(parsed_terms) <- seq_len(nrow(parsed_terms))
  parsed_terms
}

# }}} parse terms


kernel_to_lm_formula <- function(formula) {
  #' formula <- y ~ K_h(x)
  pf <- mixedcurve::parse_terms(formula)
  #' pf
  if (sum(pf$type == "kernel fixed effect") > 1) {
    stop("Multiple fixed effect kernel terms found in the formula.
Please include only one fixed effect kernel term or a
kernel term across a categorical term (e.g., K_h(x * y)
or K_h(x * y | cat)).")
  }
  if (sum(pf$type == "kernel random effect") >= 1) {
    stop("Random effect kernel terms found in the formula.
Please use lpkme or glpkme methods for mixed-effects models.")
  }
  if (length(pf$term[which(pf$type == "kernel fixed effect")]) == 0) {
    stop("No fixed-effect kernel term found in the formula.")
  }
  response_term <- pf$term[which(pf$type == "response")]
  kfe_term <- pf$term[which(pf$type == "kernel fixed effect")]
  kfe_term_lhs_rhs <- mixedcurve::parse_kernel_term(kfe_term)
  #' kfe_term_lhs <- kfe_term_lhs_rhs$lhs
  kfe_term_rhs <- kfe_term_lhs_rhs$rhs
  kre_rhs_term <- pf$rhs[which(pf$type == "kernel random effect")]
  kre_lhs_term <- pf$lhs[which(pf$type == "kernel random effect")]
  #' parsed_kre_lhs_term <- mixedcurve::parse_kernel_term(kre_lhs_term)
  #' mixedcurve::parse_kernel_term(kre_lhs_term)
  formula_str <- paste0(
    response_term, " ~ ",
    if (!is.na(kfe_term_rhs)) {
      paste0(kfe_term_rhs)
    } else {
      "1"
    },
    if (length(pf$term[which(pf$type == "fixed effect")]) > 0) {
      paste0(" + ", paste(pf$term[which(pf$type == "fixed effect")],
                          collapse = " + "
                          ))
    } else {
      ""
    }
  )
  as.formula(formula_str)
}






# {{{ kernel_to_lme4_formula

kernel_to_lme4_formula <- function(formula) {
  pf <- mixedcurve::parse_terms(formula)
  if (sum(pf$type == "kernel fixed effect") > 1) {
    stop("Multiple fixed effect kernel terms found in the formula.
Please include only one fixed effect kernel term or a
kernel term across a categorical term (e.g., K_h(x * y)
or K_h(x * y | cat)).")
  }
  if (sum(pf$type == "kernel random effect") > 1) {
    stop("Multiple random effect kernel terms found in the formula.
Please include only one random effect kernel term or a
kernel term across a categorical term (e.g., (K_h(x * y) | grp)
or (K_h(x * y) | grp / grp2)).")
  }
  if (length(pf$term[which(pf$type == "kernel fixed effect")]) == 0) {
    stop("No fixed-effect kernel term found in the formula.")
  }
  if (length(pf$term[which(pf$type == "kernel random effect")]) == 0) {
    stop("No random-effect kernel term found in the formula.")
  }
  response_term <- pf$term[which(pf$type == "response")]
  kfe_term <- pf$term[which(pf$type == "kernel fixed effect")]
  kfe_term_lhs_rhs <- mixedcurve::parse_kernel_term(kfe_term)
  #' kfe_term_lhs <- kfe_term_lhs_rhs$lhs
  kfe_term_rhs <- kfe_term_lhs_rhs$rhs
  kre_rhs_term <- pf$rhs[which(pf$type == "kernel random effect")]
  kre_lhs_term <- pf$lhs[which(pf$type == "kernel random effect")]
  parsed_kre_lhs_term <- mixedcurve::parse_kernel_term(kre_lhs_term)
  mixedcurve::parse_kernel_term(kre_lhs_term)
  formula_str <- paste0(
    response_term, " ~ ",
    if (!is.na(kfe_term_rhs)) {
      paste0(kfe_term_rhs)
    } else {
      "1"
    },
    if (length(pf$term[which(pf$type == "random effect")]) > 0) {
      paste0(" + ", paste(pf$term[which(pf$type == "random effect")],
        collapse = " + "
      ))
    } else {
      ""
    },
    if (!is.na(parsed_kre_lhs_term$lhs)) {
      paste0(" + (1", " | ", kre_rhs_term, ")")
    } else {
      ""
    }
  )
  as.formula(formula_str)
}


# }}} kernel_to_lme4_formula
