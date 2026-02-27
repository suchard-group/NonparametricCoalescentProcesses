
loadCmuSerif <- function() {
  # Load packages
  stopifnot(requireNamespace("sysfonts", quietly = TRUE))
  stopifnot(requireNamespace("showtext", quietly = TRUE))
  
  # Add CMU Serif font, trying common locations or bundled fallback
  cmu_paths <- c(
    fs::path_home("Library", "Fonts", "cmunrm.otf"),           # macOS user fonts
    "/usr/share/fonts/opentype/cmu/cmunrm.otf",                # typical Linux install
    "/usr/share/fonts/truetype/cmu/cmunrm.otf",                # alternate Linux path
    fs::path_home("AppData", "Local", "Microsoft", "Windows", "Fonts", "cmunrm.otf") # Windows
  )
  
  # Pick the first existing path
  font_path <- cmu_paths[file.exists(cmu_paths)][1]
  
  if (!is.na(font_path)) {
    sysfonts::font_add("CMU Serif", regular = font_path)
  } else {
    warning("CMU Serif not found — using default system serif font instead.")
  }
  
  showtext::showtext_auto(enable = TRUE)
}


loadCmuFonts <- function() {
  # Required packages
  stopifnot(requireNamespace("sysfonts", quietly = TRUE))
  stopifnot(requireNamespace("showtext", quietly = TRUE))
  stopifnot(requireNamespace("fs", quietly = TRUE))
  
  # Potential OS font paths
  cmu_candidates <- list(
    regular = c(
      fs::path_home("Library", "Fonts", "cmunrm.otf"),  # macOS
      "/usr/share/fonts/opentype/cmu/cmunrm.otf",       # Linux
      "/usr/share/fonts/truetype/cmu/cmunrm.otf",       # Linux
      fs::path_home("AppData", "Local", "Microsoft", "Windows", "Fonts", "cmunrm.otf")
    ),
    italic = c(
      fs::path_home("Library", "Fonts", "cmunti.otf"),
      "/usr/share/fonts/opentype/cmu/cmunti.otf",
      "/usr/share/fonts/truetype/cmu/cmunti.otf",
      fs::path_home("AppData", "Local", "Microsoft", "Windows", "Fonts", "cmunti.otf")
    ),
    bold = c(
      fs::path_home("Library", "Fonts", "cmunbx.otf"),
      "/usr/share/fonts/opentype/cmu/cmunbx.otf",
      "/usr/share/fonts/truetype/cmu/cmunbx.otf",
      fs::path_home("AppData", "Local", "Microsoft", "Windows", "Fonts", "cmunbx.otf")
    ),
    bolditalic = c(
      fs::path_home("Library", "Fonts", "cmunbi.otf"),
      "/usr/share/fonts/opentype/cmu/cmunbi.otf",
      "/usr/share/fonts/truetype/cmu/cmunbi.otf",
      fs::path_home("AppData", "Local", "Microsoft", "Windows", "Fonts", "cmunbi.otf")
    )
  )
  
  # Select the first existing font file for each variant
  find_font <- function(paths) {
    existing <- paths[file.exists(paths)]
    if (length(existing) > 0) existing[1] else NA
  }
  
  cmu_paths <- lapply(cmu_candidates, find_font)
  
  # Add CMU Serif if at least regular is found
  if (!is.na(cmu_paths$regular)) {
    sysfonts::font_add(
      "CMU Serif",
      regular     = cmu_paths$regular,
      italic      = cmu_paths$italic,
      bold        = cmu_paths$bold,
      bolditalic  = cmu_paths$bolditalic
    )
  } else {
    warning("CMU Serif fonts not found — using default system serif.")
  }
  
  # Add math font (Latin Modern Math)
  lm_math_paths <- c(
    "/Library/TeX/Root/texmf-dist/fonts/opentype/public/lm-math/latinmodern-math.otf",
    "/usr/share/fonts/opentype/public/lm-math/latinmodern-math.otf"
  )
  lm_math <- find_font(lm_math_paths)
  
  if (!is.na(lm_math)) {
    sysfonts::font_add("lmmath", lm_math)
  } else {
    message("Latin Modern Math not found — math may not match CMU style.")
  }
  
  # Enable showtext globally
  showtext::showtext_auto(enable = TRUE)
  
  invisible(TRUE)
}


