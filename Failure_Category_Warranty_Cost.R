# 1. Load data
df <- read_excel(file.choose())


options(scipen = 999)   # avoid scientific notation on the axis

# 2. Total warranty cost for each failure type by supplier
suppliers <- sort(unique(df$coil_supplier))

cost_by_type <- function(type) {
  sub <- df[df$failure_type == type, ]
  out <- tapply(sub$warranty_cost, factor(sub$coil_supplier, levels = suppliers), sum)
  out[is.na(out)] <- 0
  out
}

leak_cost     <- cost_by_type("Leak")
wiring_cost   <- cost_by_type("Wiring")
controls_cost <- cost_by_type("Controls")

# 3. Combine into a matrix (rows = bars, columns = suppliers)
cost_matrix <- rbind("Leak Warranty Cost"     = leak_cost,
                     "Wiring Warranty Cost"   = wiring_cost,
                     "Controls Warranty Cost" = controls_cost)

# Print the exact numbers to the console to verify
cost_matrix

# 4. Create the side-by-side grouped bar chart
bp <- barplot(cost_matrix,
              beside = TRUE,
              main = "Warranty Cost of Leak, Wiring, and Controls Failures by Coil Supplier",
              xlab = "Coil Supplier",
              ylab = "Total Warranty Cost ($)",
              col = c("steelblue", "tomato", "seagreen"),
              ylim = c(0, max(cost_matrix) * 1.15),
              legend.text = TRUE,
              args.legend = list(x = "topright", bty = "n"))

# 5. Add exact dollar labels on top of each bar
text(x = bp,
     y = cost_matrix,
     labels = paste0("$", format(cost_matrix, big.mark = ",")),
     pos = 3,
     cex = 0.75)
