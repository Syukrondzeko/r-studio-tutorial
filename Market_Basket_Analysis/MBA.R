# ==============================================================================
# Market Basket Analysis (Algoritma Apriori)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : arules, arulesViz
# Data    : Groceries (dataset dari paket arules)
# ==============================================================================

library(arules)
library(arulesViz)
data("Groceries")
summary(Groceries)
inspect(head(Groceries, 5))   # 5 transaksi pertama (total ada 9835 transaksi)
# Dokumentasi fungsi: ?apriori

# Apriori dengan parameter default (support = 0.1, confidence = 0.8)
gr_rules <- apriori(Groceries)

# Support 0.01 terlalu besar -> tidak ada aturan yang terbentuk
gr_rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.8))

# Turunkan support menjadi 0.001
gr_rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
gr_rules
inspect(gr_rules[1:10])

# Urutkan berdasarkan support
gr_rules <- sort(gr_rules, by = "support", decreasing = TRUE)
inspect(gr_rules[1:10])

# Aturan dengan "bottled beer" di sisi kiri (lhs)
gr_rules_beer <- apriori(Groceries,
                         parameter = list(supp = 0.001, conf = 0.008),
                         appearance = list(default = "rhs", lhs = "bottled beer"))
inspect(head(gr_rules_beer, 5))

# Visualisasi graph 20 aturan dengan lift tertinggi
plot(head(gr_rules, 20, by = "lift"), method = "graph")

# Versi interaktif (dibuka di Viewer RStudio / browser)
plot(head(gr_rules, 20, by = "lift"), method = "graph", engine = "htmlwidget")
