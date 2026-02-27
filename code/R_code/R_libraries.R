# author: Filippo Monti (revised)
# Robust installer/loader for CRAN + Bioconductor packages
install_load_packages <- function(packages, attach = TRUE) {
  stopifnot(is.character(packages), length(packages) > 0)

  # Ensure a CRAN mirror
  if (is.null(getOption("repos")) || getOption("repos")["CRAN"] %in% c("@CRAN@", NULL, "")) {
    options(repos = c(CRAN = "https://cran.rstudio.com/"))
  }

  # Known Bioconductor packages in your list (extend if needed)
  bioc_known <- c("ggtree", "treeio")

  # Helper: is a package installed (without attaching)?
  is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

  # Split into CRAN vs Bioc (based on known list)
  pkgs <- unique(packages)
  bioc_pkgs <- intersect(pkgs, bioc_known)
  cran_pkgs <- setdiff(pkgs, bioc_pkgs)

  # Determine what needs installation
  need_bioc <- bioc_pkgs[!vapply(bioc_pkgs, is_installed, logical(1))]
  need_cran <- cran_pkgs[!vapply(cran_pkgs, is_installed, logical(1))]

  # Install CRAN packages (if any)
  if (length(need_cran)) {
    install.packages(need_cran, dependencies = TRUE)
  }

  # Install Bioconductor packages (if any)
  if (length(need_bioc)) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    BiocManager::install(need_bioc, ask = FALSE, update = FALSE)
  }

  # Optionally attach (only once; avoid the require+library double load)
  if (isTRUE(attach)) {
    for (pkg in pkgs) {
      # This gives a clear error if something failed to install
      library(pkg, character.only = TRUE)
    }
  }

  invisible(TRUE)
}

# Your package groups
package_lists <- list(
  plotting = c("bayesplot", "ggplot2", "gridExtra", "GGally", "cowplot", "camcorder", "purrr", "pammtools"),
  colors = c("RColorBrewer", "viridis", "ggsci", "ggthemes", "ggprism"),
  mcmc = c("coda", "MCMCpack", "HDInterval"),
  phylogenetics = c("treeio", "ape", "phangorn", "ggtree"),
  maps = c("mapproj", "rnaturalearth"),
  dates = c("lubridate"),
  strings = c("stringr"),
  stat = c("mgcv"),
  fonts = c("sysfonts", "showtext"),
  general = c("data.table", "tidyverse", "xml2"),
  files_management = c("fs")
)

# Install & (by default) attach each group
invisible(lapply(package_lists, install_load_packages))
