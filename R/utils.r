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

# {{{ create_dir

#' This function creates a directory if it does not already exist.
#'
#' @param dir_path A string representing the path of the directory
#'                 to be created.
#' @return None. The function is called for its side effects of
#'               creating a directory.
#'
#' @export
#'
#' @examples
#' mixedcurve::create_dir("new_directory")
create_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Directory created:", dir_path, "\n")
  } else {
    cat("Directory already exists:", dir_path, "\n")
  }
}

# }}} create_dir

# {{{ dark_mode

#' Function to set dark mode theme for R plots
#'
#' This function customizes the graphical parameters in R to
#' create a dark mode theme for plots. It sets the background
#' color to a dark gray and adjusts text and line colors to be
#' light for better visibility. It also defines a custom color
#' palette suitable for dark mode.
#'
#' @return None. The function is called for its side effects of
#'               changing graphical parameters.
#'
#' @export
#'
#' @examples
#' mixedcurve::dark_mode()
#' plot(1:10, 1:10, main = "Dark Mode Plot", xlab = "X-axis", ylab = "Y-axis")
dark_mode <- function() {
  par(
    bg = "gray30",
    fg = "white",
    col = "white",
    col.axis = "white",
    col.lab = "white",
    col.main = "white",
    col.sub = "white"
  )
  custom_palette <- c(
    "white", "#DF536B", "#61D04F", "#2297E6",
    "#28E2E5", "#CD0BBC", "#F5C710", "gray80"
  )
  palette(custom_palette)
}

# }}} dark_mode

# {{{ light_mode

#' Function to set light mode theme for R plots
#'
#' This function customizes the graphical parameters in R to
#' create a dark mode theme for plots. It sets the background
#' color to white and adjusts text and line colors to be
#' dark for better visibility. It also defines a custom color
#' palette suitable for light mode.
#'
#' @return None. The function is called for its side effects of
#'               changing graphical parameters.
#'
#' @examples
#' mixedcurve::light_mode()
#' plot(1:10, 1:10, main = "Light Mode Plot", xlab = "X-axis", ylab = "Y-axis")
#' @export
light_mode <- function() {
  par(
    bg = "white",
    fg = "black",
    col = "black",
    col.axis = "black",
    col.lab = "black",
    col.main = "black",
    col.sub = "black"
  )
  custom_palette <- c(
    "black", "#DF536B", "#61D04F", "#2297E6",
    "#28E2E5", "#CD0BBC", "#F5C710", "gray40"
  )
  palette(custom_palette)
}
# }}} light_mode

# {{{ use_package

#' Ensure a package is installed and loaded
#'
#' This function checks if a specified R package is installed.
#' If not, it installs the package from CRAN and then loads
#' it into the R session.
#'
#' @param package_name A string representing the name of the package to be used.
#' @param repo A string specifying the CRAN repository URL to use
#'             for installation. Default is "http://cran.us.r-project.org".
#'
#' @return None. The function is called for its side effects of installing
#'               and loading the package.
#'
#' @export
#' @examples
#' mixedcurve::use_package("ggplot2")
use_package <- function(package_name, repo = "http://cran.us.r-project.org") {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, repos = repo, quiet = TRUE, ask = FALSE)
    library(package_name, character.only = TRUE)
  }
}
# }}} use_package

# {{{ parse_args

#' Parse Command Line Arguments
#'
#' This function parses command line arguments passed to an R script.
#' It extracts arguments specified in the format `--arg_name value` and
#' returns them as a named list. The function automatically determines
#' the type of each argument (logical, numeric, or character).
#'
#' @param silent Logical. If TRUE, suppress messages regarding the absence
#'                        of arguments.
#'
#' @return A named list containing the parsed arguments.
#' Each argument will be stored in the list with its name as the key,
#' and converted to the appropriate type (logical, numeric, or character).
#'
#' @export
#'
#' @examples
#' # Run the following in the R console with command line arguments:
#' # Rscript my_script.R --darkmode TRUE --nsim 100
#' args <- mixedcurve::parse_args()
#' print(args)
parse_args <- function(silent = FALSE) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    if (!silent) {
      warning("No command line arguments were passed.")
    }
    return(list()) # Return an empty list if no args are provided
  }

  named_args <- list()

  for (i in seq(1, length(args), by = 2)) {
    if (i + 1 <= length(args)) {
      arg_name <- sub("^--", "", args[i])
      arg_value <- args[i + 1]

      # Determine type and convert accordingly
      if (arg_value %in% c("TRUE", "FALSE")) {
        named_args[[arg_name]] <- as.logical(arg_value)
      } else if (grepl("^[0-9]+$", arg_value)) {
        named_args[[arg_name]] <- as.numeric(arg_value)
      } else {
        named_args[[arg_name]] <- as.character(arg_value)
      }
    }
  }

  return(named_args)
}

# }}} parse_args


#' List Available Vignettes
#'
#' This function lists the names of available vignettes
#' in the 'mixedcurve' package by scanning the built_vignettes
#' directory for HTML files.
#'
#' @return A character vector containing the names of available vignettes.
#'
#' @export
#'
#' @examples
#' vignettes <- mixedcurve::list_vignettes()
#' print(vignettes)
list_vignettes <- function() {
  vig_dir <- paste0(system.file("", package = "mixedcurve"), "built_vignettes/")
  vig_files <- list.files(vig_dir, pattern = "\\.html$")
  vig_names <- sub("\\.html$", "", vig_files)
  return(vig_names)
}


#' Open a Vignette in the Default Web Browser
#'
#' This function opens a specified vignette from the 'mixedcurve'
#' package in the default web browser. The vignette is identified
#' by its name.
#'
#' @param vname A string representing the name of the vignette to be opened.
#' @return None. The function is called for its side effects of
#'               opening a web browser.
#' @export
#' @examples
#' mixedcurve::open_vignette(list_vignettes()[1])
#'
open_vignette <- function(vname) {
  vig_path <- paste0(system.file("", package = "mixedcurve"), "built_vignettes/", vname, ".html")
  # TODO: handle other OS types
  system(paste("xdg-open", shQuote(vig_path)))
}



# Local Variables:
# eval: (origami-mode t)
# End:
