port <- as.integer(Sys.getenv("PORT", "3838"))
host <- Sys.getenv("HOST", "0.0.0.0")
app_dir <- if (dir.exists("/app/src")) {
  "/app/src"
} else if (dir.exists("src")) {
  "src"
} else {
  getwd()
}
shiny::runApp(app_dir, host = host, port = port, launch.browser = FALSE)
