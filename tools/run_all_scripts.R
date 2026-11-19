# ==============================================================================
# Smoke test: jalankan semua script tutorial dan laporkan yang gagal
# ------------------------------------------------------------------------------
# Jalankan dari root repo:
#   Rscript tools/run_all_scripts.R
# Setiap script dijalankan di proses R terpisah (bersih), plot diarahkan ke
# device kosong. Aplikasi Shiny diuji dengan shiny::testServer().
# Keluar dengan status 1 jika ada script yang gagal.
# ==============================================================================

if (!file.exists("R-Studio-Tutorial.Rproj")) {
  stop("Jalankan script ini dari root repo (folder yang berisi R-Studio-Tutorial.Rproj).")
}

rscript <- file.path(R.home("bin"), "Rscript")

# File yang hanya berisi definisi fungsi / bukan tutorial yang berdiri sendiri
dikecualikan <- c(
  "^tools/", "^data/", "^Shiny_GUI/", "/helpers/", "^install_packages\\.R$",
  "Regresi_Kernel/GCV\\.R$", "Regresi_Kernel/CV\\.R$",
  "Regresi_Spline/GCV\\.R$", "Regresi_Polinomial_Lokal/GCV_Polinomial\\.R$"
)
scripts <- list.files(".", pattern = "\\.R$", recursive = TRUE)
scripts <- scripts[!Reduce(`|`, lapply(dikecualikan, grepl, x = scripts))]

jalankan <- function(kode) {
  log <- tempfile(fileext = ".log")
  mulai <- Sys.time()
  status <- system2(rscript, c("--no-save", "--no-restore", "-e", shQuote(kode)),
                    stdout = log, stderr = log)
  list(ok = status == 0,
       detik = round(as.numeric(difftime(Sys.time(), mulai, units = "secs")), 1),
       log = readLines(log, warn = FALSE))
}

hasil <- list()
for (f in scripts) {
  r <- jalankan(sprintf('pdf(NULL); source("%s", echo = FALSE)', f))
  cat(sprintf("[%s] %-70s %5.1f s\n", if (r$ok) "OK  " else "GAGAL", f, r$detik))
  if (!r$ok) cat(paste("    ", tail(r$log, 8)), sep = "\n")
  hasil[[f]] <- r$ok
}

# ------------------------------------------------------------------------------
# Aplikasi Shiny
# ------------------------------------------------------------------------------
uji_shiny <- c(
  "Shiny_GUI/Gui_Clustering" = '
    shiny::testServer(shiny::shinyAppDir(app), {
      session$setInputs(xcol = "Sepal.Length", ycol = "Petal.Length", clusters = 3)
      stopifnot(length(unique(clusters()$cluster)) == 3)
    })',
  "Shiny_GUI/Gui_Regresi" = '
    csv <- normalizePath("data/mall_customers.csv")
    shiny::testServer(shiny::shinyAppDir(app), {
      session$setInputs(file1 = list(datapath = csv), header = TRUE, sep = ",",
                        quote = "\\"", disp = "head")
      stopifnot(nrow(data_upload()) == 200)
      session$setInputs(y_var = "Spending_Score", x_var = c("Age", "Annual_Income"))
      stopifnot(inherits(model(), "lm"))
    })',
  "Shiny_GUI/Shiny_Dashboard" = '
    shiny::testServer(shiny::shinyAppDir(app), {
      session$setInputs(bins = 20)
    })'
)
for (app in names(uji_shiny)) {
  kode <- sprintf('pdf(NULL); app <- "%s"; ui <- source(file.path(app, "ui.R"))$value;
                   stopifnot(inherits(ui, c("shiny.tag", "shiny.tag.list")));
                   %s', app, uji_shiny[[app]])
  r <- jalankan(kode)
  cat(sprintf("[%s] %-70s %5.1f s\n", if (r$ok) "OK  " else "GAGAL", app, r$detik))
  if (!r$ok) cat(paste("    ", tail(r$log, 8)), sep = "\n")
  hasil[[app]] <- r$ok
}

gagal <- names(hasil)[!unlist(hasil)]
cat("\n", sum(unlist(hasil)), "dari", length(hasil), "berhasil.\n")
if (length(gagal)) {
  cat("Gagal:\n", paste(" -", gagal, collapse = "\n"), "\n")
  quit(status = 1)
}
