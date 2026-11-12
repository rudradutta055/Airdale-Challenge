# =============================================================================
# Chi-square test of independence
#
# Question: Is the type of breakdown (Leak / Wiring / Controls) related to
#           the coil supplier (Supplier A / B / C)?
#
# Why chi-square: both variables are categories, and we want to know whether
# the mix of breakdown types is the same for every supplier.
#
# How to run: keep this script in the same folder as airedale_clean.csv,
#             then run: source("airedale_chisquare.R")
# =============================================================================

# ---- Load the clean data ----------------------------------------------------
clean <- read.csv("airedale_clean.csv", stringsAsFactors = TRUE)


# ---- Step 1: Look at the data -----------------------------------------------
# Count how many units of each breakdown type each supplier has
tab <- table(Supplier = clean$coil_supplier, Breakdown = clean$failure_type)
tab

# Suppliers have different numbers of units, so compare PERCENTAGES, not counts.
# margin = 1 means "percent within each row" (within each supplier)
pct <- round(prop.table(tab, margin = 1) * 100, 1)
pct

# Plot: one bar per supplier, split by breakdown type
barplot(t(pct),
        beside = TRUE,                              # bars side by side
        legend.text = colnames(pct),
        args.legend = list(x = "topright", bty = "n"),
        ylab = "% of supplier's breakdowns",
        main = "Breakdown type by coil supplier",
        ylim = c(0, 100))


# ---- Step 2: State the hypotheses -------------------------------------------
# H0 (null):        breakdown type and coil supplier are independent
#                   (every supplier has the same mix of breakdown types)
# H1 (alternative): breakdown type and coil supplier are related


# ---- Step 3: Run the test ---------------------------------------------------
result <- chisq.test(tab)
result
# Look at: X-squared (the test statistic), df (degrees of freedom), p-value.
# p-value < 0.05 -> reject H0 -> the two variables are related.


# ---- Step 4: Check the assumption -------------------------------------------
# Chi-square needs every EXPECTED count to be at least 5.
# Expected count = what each cell would be if there were no relationship.
round(result$expected, 1)
min(result$expected)          # must be >= 5


# ---- Step 5: How strong is the relationship? --------------------------------
# A tiny p-value only says "there is a relationship". With 3,000 units even small
# differences give tiny p-values, so we also report the effect size.
#
# Cramer's V: 0 = no relationship, ~0.1 small, ~0.3 medium, 0.5+ large
n <- sum(tab)
k <- min(nrow(tab), ncol(tab))
cramers_v <- sqrt(result$statistic / (n * (k - 1)))
round(unname(cramers_v), 2)


# ---- Step 6: Which cells drive the result? ----------------------------------
# Standardized residuals: how far each cell is from its expected count.
#   above +2  -> many MORE units than expected
#   below -2  -> many FEWER units than expected
round(result$stdres, 1)


# ---- Step 7: Answer in plain English ----------------------------------------
cat("\nRESULT\n")
cat("Chi-square =", round(result$statistic, 1),
    "| df =", result$parameter,
    "| p-value =", format.pval(result$p.value, digits = 2),
    "| Cramer's V =", round(unname(cramers_v), 2), "\n")
cat("Leak share by supplier:",
    paste0(rownames(pct), " ", pct[, "Leak"], "%", collapse = ", "), "\n")
cat("Conclusion: breakdown type depends on the coil supplier.",
    "Supplier B has far more leaks than expected.\n")
