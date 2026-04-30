# global.R
# Runs once when the app starts on shinyapps.io.
# Ensures TinyTeX and all required LaTeX packages are present before
# any user triggers a PDF render.

library(tinytex)

# Install TinyTeX if not already present (first deployment only)
if (!tinytex::is_tinytex()) {
  message("TinyTeX not found — installing...")
  tinytex::install_tinytex()
}

# Pre-install every LaTeX package the report template uses.
# This prevents mid-render downloads that can cause timeouts on shinyapps.io.
# tlmgr_install() is a no-op for packages already present, so safe to re-run.
required_latex_pkgs <- c(
  "xetex",        # latex_engine: xelatex
  "fontspec",     # xelatex font loading
  "geometry",     # margin=2cm
  "booktabs",     # kable(..., booktabs = TRUE)
  "xcolor",       # colour support
  "fancyhdr",     # headers/footers
  "mdwtools",     # misc dependencies
  "parskip",      # paragraph spacing
  "setspace",     # line spacing
  "hyperref",     # PDF links/metadata
  "caption",      # figure/table captions
  "float"         # figure placement
)

installed <- tinytex::tl_pkgs()
to_install <- setdiff(required_latex_pkgs, installed)

if (length(to_install) > 0) {
  message("Installing LaTeX packages: ", paste(to_install, collapse = ", "))
  tinytex::tlmgr_install(to_install)
}

message("LaTeX environment ready.")
