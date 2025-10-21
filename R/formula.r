
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
  response_char <- as.character(form[2])
  predict_char <- as.character(form[3])
  terms <- strsplit(predict_char, "\\s*\\+\\s*")[[1]]
  classify_term <- function(term) {
    if (grepl("^K_h\\(.+\\|.+\\)$", term)) {
      "kernel fixed effect"
    } else if (grepl("^\\(.+\\|.+ / .+\\)$", term) ||
                 grepl("^K_h\\(.+\\|.+ / .+\\)$", term) ||
                 grepl("^\\(.+\\|.+\\)$", term)) {
      "random effect"
    } else if (grepl("^.+(\\s\\*\\s.+)?$", term)) {
      "fixed effect"
    } else {
      "unknown"
    }
  }
  classified_terms <- sapply(terms, classify_term)
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
      term_type <- "kernel fixed effect"  # Explicitly set the correct type
    }
    if (term_type == "kernel fixed effect") {
      matches <- regmatches(
        term,
        regexec("K_h\\(([^|]+?)(?:\\s*\\|\\s*([^\\)]+))?\\)", term)
      )
      lhs <- trimws(matches[[1]][2])  # Extract lhs
      rhs <- if (length(matches[[1]]) > 2 &&
                   !is.na(matches[[1]][3]) && matches[[1]][3] != "") {
        trimws(matches[[1]][3])  # Extract rhs if present and not empty
      } else {
        NA  # Ensure rhs is NA if not present
      }
      parsed_terms <- rbind(parsed_terms,
                            data.frame(term = term, type = term_type,
                                       lhs = lhs, rhs = rhs,
                                       stringsAsFactors = FALSE))
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
                                       lhs = term, rhs = NA,  # Set rhs as NA
                                       stringsAsFactors = FALSE))
    }
  }
  parsed_terms[parsed_terms == "<NA>"] <- NA
  parsed_terms[parsed_terms == "NA"] <- NA
  rownames(parsed_terms) <- seq_len(nrow(parsed_terms))
  parsed_terms
}

# }}} parse terms
