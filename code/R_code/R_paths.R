# Author: Filippo Monti
# R paths

stopifnot(requireNamespace("rprojroot", quietly = TRUE))
stopifnot(requireNamespace("fs", quietly = TRUE))

# Find project root by common anchors
root <- rprojroot::find_root(
  rprojroot::has_file(".git") |
    rprojroot::has_file("_quarto.yml") |
    rprojroot::has_dir("code")
)

main_path <- root

get_path <- function(env, default) {
  val <- Sys.getenv(env, unset = NA_character_)
  fs::path_norm(ifelse(is.na(val) || val == "", default, val))
}

input_path  <- get_path("NPCTMC_LOGS", fs::path(main_path, "output"))
output_figures_path <- get_path("NPCTMC_FIGS", fs::path(main_path, "figures"))

code_path <- fs::path(main_path, "code")
R_code_path <- fs::path(main_path, "code", "R_code")
R_functions_path <- fs::path(R_code_path, "R_functions")
R_classes_path   <- fs::path(R_code_path, "R_classes")

fs::dir_create(c(input_path, output_figures_path))

if (interactive()) {
  msg <- paste(
    "Paths:",
    paste("  main:    ", main_path),
    paste("  input:   ", input_path,  "   # logs here"),
    paste("  output:  ", output_figures_path, "   # figures here"),
    paste("  code:    ", code_path),
    paste("  Rcode:    ", R_code_path),
    paste("  R_fns:   ", R_functions_path),
    paste("  R_cls:   ", R_classes_path),
    sep = "\n"
  )
  message(msg)
}
