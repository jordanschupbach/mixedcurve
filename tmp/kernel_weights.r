



gauss_kern <- function(x) {
  exp((-1 / 2) * ((x)^2))
}

k_h <- function(x, h) {
  gauss_kern(x / h) / h
}


lm_kernel_weights <- function(formula, data, bwidth, query) {
  formula_str <- as.character(formula)
  response_var <- strsplit(formula_str[2], " ~ ")[[1]][1]
  kernel_part <- strsplit(formula_str[2], " ~ ")[[1]][2]
  kernel_part <- gsub(" ", "", kernel_part)
  if (grepl("\\|", kernel_part)) {
    domain_grp <- unlist(strsplit(kernel_part, "\\|"))
    domain_var <- sub("K_h\\((.*)\\)", "\\1", domain_grp[1])  # extract x
    grp_var <- domain_grp[2]                                   # extract grp
  } else {
    stop("The kernel function must include a grouping variable.")
  }
  design_matrix <- model.matrix(as.formula(paste0(response_var, " ~ ", grp_var)), data)
  weights <- sqrt(k_h(data[[sub("\\)", "", domain_var)]], query, bwidth))
  weighted_design_matrix <- sweep(design_matrix, 1, weights, FUN = "*")
  colnames(weighted_design_matrix) <- paste("w_", colnames(design_matrix), sep = "")
  as.data.frame(weighted_design_matrix)
}

formula <- y ~ K_h(x | grp)
bwidth <- 0.2
query <- 0.2
df <- data.frame(y = rnorm(100), x = runif(100), grp = factor(sample(1:3, 100, replace = TRUE)))
lhs <- formula[[2]]
rhs <- formula[[3]]
formula_str <- as.character(formula)
response_var <- strsplit(formula_str[2], " ~ ")[[1]][1]
response_var
kernel_part <- strsplit(formula_str[2], " ~ ")[[1]][2]
kernel_part
kernel_part <- gsub(" ", "", kernel_part)
if (grepl("\\|", kernel_part)) {
  domain_grp <- unlist(strsplit(kernel_part, "\\|"))
  domain_var <- sub("K_h\\((.*)\\)", "\\1", domain_grp[1])  # extract x
  grp_var <- domain_grp[2]                                   # extract grp
} else {
  stop("The kernel function must include a grouping variable.")
}
design_matrix <- model.matrix(as.formula(paste0(response_var, " ~ ", grp_var)), data)
weights <- sqrt(k_h(data[[sub("\\)", "", domain_var)]], query, bwidth))
weighted_design_matrix <- sweep(design_matrix, 1, weights, FUN = "*")
colnames(weighted_design_matrix) <- paste("w_", colnames(design_matrix), sep = "")
as.data.frame(weighted_design_matrix)





w <- lm_kernel_weights(df, bwidth = 0.1, query = 0.5, formula = y ~ K_h(x | grp))
head(w)  # View the first few rows of the weighted design matrix
