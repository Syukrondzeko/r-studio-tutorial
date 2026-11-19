# ==============================================================================
# Shiny GUI: K-Means Clustering Data Iris (Server)
# ==============================================================================

function(input, output, session) {

  # Gabungkan variabel yang dipilih menjadi data frame baru
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    req(input$clusters >= 1)
    set.seed(123)
    kmeans(selectedData(), centers = input$clusters, nstart = 10)
  })

  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}
