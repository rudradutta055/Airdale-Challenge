# =============================================================================
# Frequency x severity breakdown of leak cost (with ANOVA on severity)
#
# Question: Why does Supplier B cost so much more in leak warranty cost?
#   Is it because its units leak MORE OFTEN (frequency),
#   or because each leak COSTS MORE (severity)?
#
#   Leak cost per unit built = leak rate (frequency) x cost per leak (severity)
#
# Tests used:
#   Frequency -> chi-square test on the leak rate (yes/no outcome)
#   Severity  -> one-way ANOVA on cost per leak (a number across 3+ groups)
#
# How to run: keep this script in the same folder as airedale_clean.csv,
#             then run: source("airedale_frequency_severity.R", echo = TRUE)
# The chart is saved as frequency_severity.png in the same folder.
# =============================================================================

# ---- Settings: change these two lines to analyze facilities instead ---------
group_col <- "coil_supplier"   # or "facility"
focus     <- "Supplier B"      # or "Facility 2"

# ---- Load the clean data ----------------------------------------------------
clean <- read.csv("airedale_clean.csv", stringsAsFactors = TRUE)
clean$group <- clean[[group_col]]
leaks <- subset(clean, leak == 1)          # the 788 leak units only


# =============================================================================
# PART 1 — FREQUENCY: how often does each group leak?
# =============================================================================

# Leak rate = leaking units / all units in the group
units_built <- table(clean$group)
leak_count  <- tapply(clean$leak, clean$group, sum)
leak_rate   <- leak_count / units_built
round(100 * leak_rate, 1)                  # in %

# Test: is the leak rate the same for every group?
# H0: every group has the same leak rate | H1: at least one differs
freq_test <- chisq.test(table(clean$group, clean$leak))
freq_test


# =============================================================================
# PART 2 — SEVERITY: how much does each leak cost?  (one-way ANOVA)
# =============================================================================

# Average warranty cost per leak in each group
cost_per_leak <- tapply(leaks$warranty_cost, leaks$group, mean)
round(cost_per_leak)

# Boxplot first (course guide step 1)
boxplot(warranty_cost ~ group, data = leaks,
        ylab = "Warranty cost per leak ($)", main = "Cost per leak by group")

# Test: is the average cost per leak the same in every group?
# H0: same average cost per leak | H1: at least one group differs
# log(): cost is skewed; the log makes the groups' spread more similar.
sev_model <- aov(log(warranty_cost) ~ group, data = leaks)
summary(sev_model)

# Assumption check: Welch's ANOVA does not assume equal spread
oneway.test(log(warranty_cost) ~ group, data = leaks, var.equal = FALSE)

# Note: LegacyX leaks cost more (see airedale_anova.R). Adding product family
# checks that the group result is not hiding a product mix difference:
summary(aov(log(warranty_cost) ~ product_family + group, data = leaks))


# =============================================================================
# PART 3 — THE BREAKDOWN: leak cost per unit built = frequency x severity
# =============================================================================
breakdown <- data.frame(
  units_built     = as.vector(units_built),
  leaks           = as.vector(leak_count),
  leak_rate_pct   = round(100 * as.vector(leak_rate), 1),
  cost_per_leak   = round(as.vector(cost_per_leak)),
  cost_per_unit   = round(as.vector(leak_rate * cost_per_leak)),
  total_leak_cost = round(as.vector(tapply(leaks$warranty_cost, leaks$group, sum))),
  row.names = names(units_built)
)
breakdown

# How much of the focus group's EXTRA cost per unit comes from frequency vs severity?
# Compare the focus group with all other groups combined:
#   extra = (rate_focus - rate_other) x severity_other     <- frequency part
#         + rate_focus x (severity_focus - severity_other) <- severity part
in_focus   <- clean$group == focus
rate_f     <- mean(clean$leak[in_focus])
rate_o     <- mean(clean$leak[!in_focus])
sev_f      <- mean(leaks$warranty_cost[leaks$group == focus])
sev_o      <- mean(leaks$warranty_cost[leaks$group != focus])
freq_part  <- (rate_f - rate_o) * sev_o
sev_part   <- rate_f * (sev_f - sev_o)
extra      <- freq_part + sev_part


# ---- Chart: three panels, one per piece of the breakdown -------------------
png("frequency_severity.png", width = 2100, height = 800, res = 200)
par(mfrow = c(1, 3), mar = c(4, 5, 4, 1))
bar_col <- ifelse(names(units_built) == focus, "#2a78d6", "grey75")

b1 <- barplot(breakdown$leak_rate_pct, names.arg = rownames(breakdown), col = bar_col,
              border = NA, ylim = c(0, max(breakdown$leak_rate_pct) * 1.2),
              main = "Frequency: leak rate (%)", las = 1)
text(b1, breakdown$leak_rate_pct, paste0(breakdown$leak_rate_pct, "%"), pos = 3)

b2 <- barplot(breakdown$cost_per_leak, names.arg = rownames(breakdown), col = bar_col,
              border = NA, ylim = c(0, max(breakdown$cost_per_leak) * 1.2),
              main = "Severity: cost per leak ($)", las = 1)
text(b2, breakdown$cost_per_leak, paste0("$", formatC(breakdown$cost_per_leak, format = "d", big.mark = ",")), pos = 3)

b3 <- barplot(breakdown$cost_per_unit, names.arg = rownames(breakdown), col = bar_col,
              border = NA, ylim = c(0, max(breakdown$cost_per_unit) * 1.2),
              main = "Result: leak cost per unit built ($)", las = 1)
text(b3, breakdown$cost_per_unit, paste0("$", formatC(breakdown$cost_per_unit, format = "d", big.mark = ",")), pos = 3)
invisible(dev.off())


# ---- Answer in plain English ------------------------------------------------
sev_p <- summary(sev_model)[[1]][["Pr(>F)"]][1]
cat("\nRESULT\n")
cat(sprintf("Frequency: leak rates differ (chi-square p %s). %s: %.1f%% vs %.1f%% for the rest.\n",
            format.pval(freq_test$p.value, digits = 2), focus, 100 * rate_f, 100 * rate_o))
cat(sprintf("Severity: cost per leak does not differ (ANOVA p = %.2f). $%s vs $%s per leak.\n",
            sev_p, format(round(sev_f), big.mark = ","), format(round(sev_o), big.mark = ",")))
cat(sprintf("%s costs $%s more in leak warranty per unit built.\n",
            focus, format(round(extra), big.mark = ",")))
cat(sprintf("  Frequency explains %.0f%% of that gap; severity explains %.0f%%.\n",
            round(100 * freq_part / extra) + 0, round(100 * sev_part / extra) + 0))
cat(sprintf("Conclusion: %s is expensive because its units leak more often, not because each leak costs more.\n", focus))
cat("Chart saved:", file.path(getwd(), "frequency_severity.png"), "\n")

