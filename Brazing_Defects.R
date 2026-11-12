library(readxl)

# 1. Load your Excel file from Documents via pop-up window
df <- read_excel(file.choose())

# Filter for Brazing defects and plot
##brazing_df <- subset(df, rework_defect_category == "Brazing")

##boxplot(warranty_cost ~ coil_supplier,
        #data = brazing_df,
        #main = "Warranty Cost of Brazing Defects by Coil Supplier",
        #xlab = "Coil Supplier",
        #ylab = "Warranty Cost ($)",
        #col = c("lightblue", "salmon", "lightgreen"))

# 2. Calculate Total Defects and Brazing Defects for each supplier
total_defects   <- table(df$coil_supplier)
brazing_defects <- table(factor(df$coil_supplier[df$rework_defect_category == "Brazing"], 
                                levels = names(total_defects)))

# 3. Combine both counts into a matrix (rows = bars, columns = suppliers)
counts_matrix <- rbind("Total Defects" = total_defects, 
                       "Brazing Defects" = brazing_defects)

# Print the exact numbers to the console to verify
counts_matrix

# 4. Create the side-by-side grouped bar chart
bp <- barplot(counts_matrix,
              beside = TRUE,
              main = "Total Defects vs. Brazing Defects by Coil Supplier",
              xlab = "Coil Supplier",
              ylab = "Number of Defects",
              col = c("steelblue", "tomato"),
              ylim = c(0, max(counts_matrix) * 1.15),
              legend.text = TRUE,
              args.legend = list(x = "topright", bty = "n"))

# 5. Add exact count labels on top of each bar
text(x = bp, 
     y = counts_matrix, 
     labels = counts_matrix, 
     pos = 3, 
     cex = 0.9)
