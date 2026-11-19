# ==============================================================================
# Shiny Dashboard Sederhana (Server)
# ==============================================================================

function(input, output, session) {

  output$histogram <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins, col = "steelblue",
         border = "white", main = "", xlab = "Lama erupsi (menit)")
  })

  output$ringkasan <- renderPrint({
    summary(faithful)
  })

  output$scatter <- renderPlot({
    plot(faithful$eruptions, faithful$waiting, pch = 19, col = "steelblue",
         xlab = "Lama erupsi (menit)", ylab = "Waktu tunggu (menit)")
    abline(lm(waiting ~ eruptions, data = faithful), col = "red", lwd = 2)
  })

  output$raw <- renderTable({
    faithful
  })
}
