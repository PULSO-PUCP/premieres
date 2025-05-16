# cfa ---------------------------------------------------------------------



# Load library
library(lavaan)

# Load data
rm(list = ls())
linkGit='https://github.com/PULSO-PUCP/pcm_history/raw/refs/heads/main/spellsModel.xlsx'
df <- rio::import(linkGit)

model <- '
  Technocrat =~ NeverElectoralParticipation +
                 GraduateDegreeInForeignCountry +
                 RelevantActivityPrivateSector
'

fit <- cfa(model, data = df,
           ordered = c("NeverElectoralParticipation",
                       "GraduateDegreeInForeignCountry",
                       "RelevantActivityPrivateSector",
                       "RelevantActivityAcademia",
                       "DifferentThanLawyer"),
           estimator = "WLSMV")


summary(fit, standardized = TRUE, fit.measures = TRUE)


####
rm(list = ls())
df <- read_excel("perfil_technoAll.xlsx")



# otherLatent -------------------------------------------------------------


library(psych)

# Subset binary indicators
efa_data <- df[, c("NeverElectoralParticipation",
                   "GraduateDegreeInForeignCountry",
                   "RelevantActivityPrivateSector",
                   "RelevantActivityAcademia",
                   "DifferentThanLawyer")]

# Run EFA with fit stats
efa_fit <- fa(efa_data, nfactors = 1, fm = "ml", rotate = "none")

# View factor loadings and fit
print(efa_fit)

# View RMSEA and other fit measures
efa_fit$RMSEA  # RMSEA and confidence interval
efa_fit$TLI    # Tucker-Lewis Index
efa_fit$BIC    # Bayesian information criterion

###

# Load the dataset
df <- read_excel("perfil_technoAll.xlsx")

# Select the five binary indicators
techno_vars <- df[, c("NeverElectoralParticipation",
                      "GraduateDegreeInForeignCountry",
                      "RelevantActivityPrivateSector",
                      "RelevantActivityAcademia",
                      "DifferentThanLawyer")]

# Standardize variables (important even if binary)
techno_scaled <- scale(techno_vars)

# Run PCA
pca_result <- prcomp(techno_scaled, center = TRUE, scale. = TRUE)

# Summary: variance explained
summary(pca_result)

# Print component loadings (rotation matrix)
pca_result$rotation

# Scree plot (optional)
plot(pca_result, type = "lines", main = "Scree Plot of Technocrat PCA")

# Extract PC1 as index
df$technocrat_index_pca <- pca_result$x[,1]

# View the index
head(df[, c("PCM", "technocrat_index_pca")])