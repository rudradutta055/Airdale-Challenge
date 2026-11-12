# 1. Load data
df <- read_excel(file.choose())


# 2. Count each failure type for each supplier
suppliers <- sort(unique(df$coil_supplier))

leak_failures     <- table(factor(df$coil_supplier[df$failure_type == "Leak"],
                                  levels = suppliers))
wiring_failures   <- table(factor(df$coil_supplier[df$failure_type == "Wiring"],
                                  levels = suppliers))
controls_failures <- table(factor(df$coil_supplier[df$failure_type == "Controls"],
                                  levels = suppliers))

# 3. Combine the counts into a matrix (rows = bars, columns = suppliers)
counts_matrix <- rbind("Leak Failures"     = leak_failures,
                       "Wiring Failures"   = wiring_failures,
                       "Controls Failures" = controls_failures)

# Print the exact numbers to the console to verify
counts_matrix

# 4. Create the side-by-side grouped bar chart
bp <- barplot(counts_matrix,
              beside = TRUE,
              main = "Leak, Wiring, and Controls Failures by Coil Supplier",
              xlab = "Coil Supplier",
              ylab = "Number of Failures",
              col = c("steelblue", "tomato", "seagreen"),
              ylim = c(0, max(counts_matrix) * 1.15),
              legend.text = TRUE,
              args.legend = list(x = "topright", bty = "n"))

# 5. Add exact count labels on top of each bar
text(x = bp,
     y = counts_matrix,
     labels = counts_matrix,
     pos = 3,
     cex = 0.9)
+
