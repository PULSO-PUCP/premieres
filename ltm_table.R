# Load summary
summaryLTM <- readRDS("summaryLTM.rds")


# Extract model fit information dynamically
loglik <- round(summaryLTM$logLik)
aic    <- round(summaryLTM$AIC, 2)
bic    <- round(summaryLTM$BIC, 2)
grad   <- signif(summaryLTM$max.sc, 5)
conv   <- ifelse(summaryLTM$conv == 0, "Yes", "No")
opt    <- summaryLTM$control$method
quad   <- summaryLTM$control$GHk
# 


# First line: fit metrics
fit_note1 <- paste0(
    "\\multicolumn{5}{l}{\\footnotesize\\textit{Model fit: log-likelihood = ", loglik,
    "; AIC = ", aic, "; BIC = ", bic, ".}} \\\\"
)

# Second line: estimation details
fit_note2 <- paste0(
    "\\multicolumn{5}{l}{\\footnotesize\\textit{Estimated via ", opt,
    " optimization.}} \\\\"
)
fit_note3 <- paste0(
    "\\multicolumn{5}{l}{\\footnotesize\\textit{Convergence: ", conv,
    " (grad = ", grad, ").}} \\\\"
)

# Second line: estimation details
fit_note4 <- paste0(
    "\\multicolumn{5}{l}{\\footnotesize\\textit{Integration: Gauss-Hermite with ", quad, " points.}} \\\\"
)


# Extract and format coefficients
ltm_coef <- as.data.frame(summaryLTM$coefficients)
ltm_coef$Parameter <- rownames(ltm_coef)

# Compute p-values and stars
ltm_coef$p.value <- 2 * (1 - pnorm(abs(ltm_coef$z.vals)))
ltm_coef$stars <- cut(ltm_coef$p.value,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", ""))

# Rename and reorder columns
colnames(ltm_coef)[1:3] <- c("Estimate", "Std. Error", "z-value")
ltm_coef <- ltm_coef[, c("Parameter", "Estimate", "Std. Error", "z-value", "stars")]

# Relabel for clarity
rename_parameter <- function(param) {
    param <- gsub("Dscrmn.NeverElectoralParticipation", "Discrimination: No electoral participation", param)
    param <- gsub("Dscrmn.GraduateDegreeInForeignCountry", "Discrimination: Foreign degree", param)
    param <- gsub("Dscrmn.RelevantActivityPrivateSector", "Discrimination: Private sector experience", param)
    param <- gsub("Dffclt.NeverElectoralParticipation", "Difficulty: No electoral participation", param)
    param <- gsub("Dffclt.GraduateDegreeInForeignCountry", "Difficulty: Foreign degree", param)
    param <- gsub("Dffclt.RelevantActivityPrivateSector", "Difficulty: Private sector experience", param)
    return(param)
}
ltm_coef$Parameter <- rename_parameter(ltm_coef$Parameter)

# Split by type
discrim <- ltm_coef[grep("Discrimination", ltm_coef$Parameter), ]
diff <- ltm_coef[grep("Difficulty", ltm_coef$Parameter), ]

# Create LaTeX section headers
section_row <- function(text, template) {
    df <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(template)))
    colnames(df) <- colnames(template)
    df[1, 1] <- paste0("\\addlinespace\n\\multicolumn{4}{l}{\\textit{", text, "}} \\\\[-15pt]")
    return(df)
}

library(xtable)

# Simplify parameter labels
discrim$Parameter <- c("No electoral participation", "Foreign degree", "Private sector experience")
diff$Parameter <- c("No electoral participation", "Foreign degree", "Private sector experience")


# Build the combined table again
combined_table <- rbind(
    section_row("Discrimination Parameters", ltm_coef),
    discrim,
    section_row("Difficulty Parameters", ltm_coef),
    diff
)


# Label columns cleanly
colnames(combined_table)[1:5] <- c("Parameter", "Estimate", "Std. Error", "z-value", "")


# Create LaTeX table using xtable
xtab <- xtable(combined_table,
               caption = "Latent Trait Model: Discrimination and Difficulty Parameters",
               label = "tab:ltm_parameters",
               align = c("l", "l", "r", "r", "r", "c"))

# Print to LaTeX with custom settings
print(xtab,
      include.rownames = FALSE,
      sanitize.text.function = identity,
      caption.placement = "top",
      label = "tab:ltm_parameters",
      hline.after = c(-1, 0, nrow(combined_table)),
      add.to.row = list(
          pos = list(nrow(combined_table)),
          command = c(
              paste0("\\addlinespace\n\\addlinespace\n",fit_note1,
                     fit_note2,fit_note3,fit_note4,"\\hline \n \\multicolumn{5}{l}{\\textit{Note}: $^{*} p<0.05$, $^{.} p<0.10$} \\\\ \n")
          ) 
      ),
      file = "ltm_parameters_table.tex"
)