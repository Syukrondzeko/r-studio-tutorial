# ==============================================================================
# Shiny GUI: Upload Data CSV dan Regresi Linier (Server)
# ==============================================================================

function(input, output, session) {

  # Data dibaca SATU kali di sini, lalu dipakai oleh semua output.
  data_upload <- reactive({
    req(input$file1)
    tryCatch(
      read.csv(input$file1$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote),
      # Jika separator salah (mis. file pakai ";" tapi dipilih ","), tampilkan
      # pesan error yang aman di aplikasi
      error = function(e) stop(safeError(e))
    )
  })

  # Isi pilihan variabel dengan kolom numerik dari data yang diupload
  observeEvent(data_upload(), {
    num_vars <- names(Filter(is.numeric, data_upload()))
    updateSelectInput(session, "y_var", choices = num_vars,
                      selected = num_vars[1])
    updateSelectInput(session, "x_var", choices = num_vars,
                      selected = num_vars[min(2, length(num_vars))])
  })

  output$contents <- renderTable({
    df <- data_upload()
    if (input$disp == "head") head(df) else df
  })

  output$summ <- renderPrint({
    summary(data_upload())
  })

  model <- reactive({
    req(input$y_var, input$x_var)
    x_var <- setdiff(input$x_var, input$y_var)
    validate(need(length(x_var) > 0, "Pilih minimal satu prediktor yang berbeda dari Y."))
    rumus <- reformulate(sprintf("`%s`", x_var), response = sprintf("`%s`", input$y_var))
    lm(rumus, data = data_upload())
  })

  output$model <- renderPrint({
    summary(model())
  })

  output$diagnostik <- renderPlot({
    par(mfrow = c(2, 2))
    plot(model())
  })
}
