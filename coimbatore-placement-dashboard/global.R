# =============================================================================
# global.R - libraries, constants, simulated data, helpers
# Coimbatore Higher Education Placement Insights Dashboard
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(dplyr)
})

# ---- Constants --------------------------------------------------------------
RS      <- "\u20b9"                                   # Indian rupee sign
STREAMS <- c("IT/CS", "Core Engineering", "Commerce")
TYPES   <- c("Tier-1", "Tier-2")
YEARS   <- 2023:2026

STREAM_COLORS  <- c("IT/CS" = "#6366f1", "Core Engineering" = "#f59e0b", "Commerce" = "#10b981")
TIER_COLORS    <- c("Tier-1" = "#4f46e5", "Tier-2" = "#06b6d4")
COMPARE_COLORS <- c("#4f46e5", "#f59e0b", "#10b981")

DATA_FILE <- "simulated_data.csv"

# ---- College master table (simulated attributes, not official figures) ------
COLLEGES <- data.frame(
  college = c(
    "PSG College of Technology",
    "Coimbatore Institute of Technology",
    "Amrita Vishwa Vidyapeetham, Coimbatore",
    "Kumaraguru College of Technology",
    "Government College of Technology",
    "Karunya Institute of Technology and Sciences",
    "Sri Krishna College of Engineering and Technology",
    "Sri Ramakrishna Engineering College",
    "Hindusthan College of Engineering and Technology",
    "Dr. N.G.P. Institute of Technology",
    "Sri Eshwar College of Engineering",
    "PSG College of Arts & Science",
    "PSGR Krishnammal College for Women",
    "Dr. G.R. Damodaran College of Science"
  ),
  short = c("PSG Tech", "CIT", "Amrita", "KCT", "GCT", "Karunya", "SKCET",
            "SREC", "HiCET", "NGPIT", "SECE", "PSG CAS", "PSGR KCW", "GRD"),
  institution_type = c("Tier-1", "Tier-1", "Tier-1", "Tier-1", "Tier-1", "Tier-1",
                       "Tier-2", "Tier-2", "Tier-2", "Tier-2", "Tier-2",
                       "Tier-2", "Tier-2", "Tier-2"),
  fee = c(1.60, 1.45, 3.20, 1.60, 0.55, 2.10, 1.35, 1.40, 1.20, 1.30, 1.15, 0.60, 0.50, 0.45),
  quality = c(1.35, 1.25, 1.45, 1.22, 1.15, 1.10, 1.00, 1.02, 0.88, 0.90, 0.86, 1.05, 0.98, 0.92),
  placement_base = c(0.90, 0.88, 0.91, 0.87, 0.84, 0.82, 0.80, 0.81, 0.74, 0.76, 0.75, 0.83, 0.80, 0.77),
  streams = c(
    "IT/CS;Core Engineering", "IT/CS;Core Engineering", "IT/CS;Core Engineering;Commerce",
    "IT/CS;Core Engineering", "IT/CS;Core Engineering", "IT/CS;Core Engineering;Commerce",
    "IT/CS;Core Engineering", "IT/CS;Core Engineering", "IT/CS;Core Engineering",
    "IT/CS;Core Engineering", "IT/CS;Core Engineering",
    "IT/CS;Commerce", "IT/CS;Commerce", "IT/CS;Commerce"
  ),
  stringsAsFactors = FALSE
)

# ---- Data generation (deterministic) ----------------------------------------
generate_placement_data <- function(seed = 2026) {
  set.seed(seed)

  grid <- do.call(rbind, lapply(seq_len(nrow(COLLEGES)), function(i) {
    expand.grid(
      college = COLLEGES$college[i],
      year    = YEARS,
      stream  = strsplit(COLLEGES$streams[i], ";")[[1]],
      stringsAsFactors = FALSE
    )
  }))

  d <- left_join(grid, COLLEGES %>% select(-streams), by = "college")
  n <- nrow(d)

  stream_pkg  <- c("IT/CS" = 6.2, "Core Engineering" = 4.6, "Commerce" = 3.6)
  stream_rate <- c("IT/CS" = 0.03, "Core Engineering" = -0.02, "Commerce" = -0.04)
  stream_size <- c("IT/CS" = 190, "Core Engineering" = 270, "Commerce" = 130)

  d$avg_package <- round(
    unname(stream_pkg[d$stream]) * d$quality * (1 + 0.055 * (d$year - 2023)) *
      exp(rnorm(n, 0, 0.04)), 2)
  d$highest_package <- round(d$avg_package * runif(n, 2.2, 3.8), 2)

  rate <- d$placement_base + unname(stream_rate[d$stream]) +
    0.008 * (d$year - 2023) + rnorm(n, 0, 0.02)
  rate <- pmin(0.985, pmax(0.5, rate))

  d$students       <- round(unname(stream_size[d$stream]) * runif(n, 0.8, 1.2))
  d$placed         <- round(d$students * rate)
  d$placement_rate <- round(d$placed / d$students * 100, 1)
  d$record_id      <- seq_len(n)

  d %>% transmute(
    record_id, college, short_name = short, institution_type,
    annual_fee_lakh = fee, year, stream, students, placed,
    placement_rate, avg_package, highest_package
  )
}

load_placement_data <- function() {
  required <- c("record_id", "college", "short_name", "institution_type",
                "annual_fee_lakh", "year", "stream", "students", "placed",
                "placement_rate", "avg_package", "highest_package")
  if (file.exists(DATA_FILE)) {
    d <- tryCatch(read.csv(DATA_FILE, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(d) && nrow(d) > 0 && all(required %in% names(d))) return(d)
  }
  d <- generate_placement_data()
  try(write.csv(d, DATA_FILE, row.names = FALSE), silent = TRUE)
  d
}

# Individual (simulated) placement offers, used for boxplots and density curves
build_offer_data <- function(df, seed = 11) {
  set.seed(seed)
  n_each <- pmin(df$placed, 45)
  idx    <- rep(seq_len(nrow(df)), n_each)
  base   <- df[idx, c("record_id", "college", "short_name", "institution_type", "year", "stream")]
  sigma  <- 0.38
  pk <- rlnorm(length(idx), meanlog = log(df$avg_package[idx]) - sigma^2 / 2, sdlog = sigma)
  pk <- pmin(pk, df$highest_package[idx])
  pk <- pmax(pk, 1.8)
  base$package <- round(pk, 2)
  rownames(base) <- NULL
  base
}

placement <- load_placement_data()
placement$stream <- factor(placement$stream, levels = STREAMS)
offers <- build_offer_data(placement)

ALL_COLLEGES     <- sort(unique(placement$college))
DEFAULT_COMPARE  <- intersect(c("PSG College of Technology",
                                "Coimbatore Institute of Technology",
                                "Amrita Vishwa Vidyapeetham, Coimbatore"), ALL_COLLEGES)

# ---- Helpers ----------------------------------------------------------------
wavg <- function(x, w) sum(x * w) / sum(w)

alpha_col <- function(hex, a = 0.25) {
  r <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", r[1], r[2], r[3], a)
}

style_plot <- function(p) {
  p %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font   = list(family = "Inter, Segoe UI, system-ui, sans-serif", color = "#334155"),
      margin = list(l = 55, r = 20, t = 30, b = 55),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.22),
      hoverlabel = list(font = list(family = "Inter, Segoe UI, system-ui, sans-serif"))
    ) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}