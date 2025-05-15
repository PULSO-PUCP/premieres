# install.packages("ltm")

# Load data
rm(list = ls())
library(ltm)

linkToDrive='https://docs.google.com/spreadsheets/d/e/2PACX-1vQ-5w7bF_alC0gQxiD3eklMlHgGQlwh_PHmibfi79Iqs2jFOuSTtyJ4nHzwcn330PbdpscMaZh99Ipn/pub?gid=0&single=true&output=csv'
df <- read.csv(linkToDrive)


# keep the relevant 3 binary indicators
ltm_data <- df[, c("NeverElectoralParticipation",
                   "GraduateDegreeInForeignCountry",
                   "RelevantActivityPrivateSector")]

ltm_model <- ltm(ltm_data ~ z1)
saveRDS(ltm_model,file = 'LTMmodel.rds')
as.data.frame(ltm_model$coefficients)

# View item parameters (discrimination and difficulty)
saveRDS(summary(ltm_model),file = 'summaryLTM.rds')

as.data.frame(coef(summary(ltm_model)))

# Plot Item Characteristic Curves
plot(ltm_model)

# Extract latent trait scores

# Step 1: Create a response pattern key for each row in your data
df$resp_pattern <- apply(ltm_data, 1, paste0, collapse = "")

# Step 2: Create the same pattern key for the LTM score table
ltm_score_table <- ltm::factor.scores(ltm_model, method = "EAP")$score.dat
ltm_score_table$resp_pattern <- apply(ltm_score_table[, 1:3], 1, paste0, collapse = "")

# Step 3: Match z1 scores to each individual based on their response pattern
df$technocrat_trait_ltm <- ltm_score_table$z1[match(df$resp_pattern, ltm_score_table$resp_pattern)]

rio::export(df,'df_ltm.xlsx',row.names = F)


# plotICC -----------------------------------------------------------------


library(dplyr)
library(tidyr)
library(ggplot2)

# Extract coefficients from ltm model
coefs <- coef(ltm_model)

# Discrimination (a) and Difficulty (b)
params <- data.frame(
    item = rownames(coefs),
    a = coefs[, "Dscrmn"],
    b = coefs[, "Dffclt"]
)

# Define ability values
theta_vals <- seq(-4, 4, length.out = 500)

# Create a function to compute the ICC using the 2PL formula
icc_fn <- function(a, b, theta) {
    1 / (1 + exp(-a * (theta - b)))
}

# Generate ICC data
icc_data <- expand.grid(theta = theta_vals, item = params$item) %>%
    left_join(params, by = "item") %>%
    mutate(prob = icc_fn(a, b, theta))

# Relabel for display
icc_data$item <- factor(icc_data$item,
                        levels = c("NeverElectoralParticipation",
                                   "GraduateDegreeInForeignCountry",
                                   "RelevantActivityPrivateSector"),
                        labels = c("No electoral experience",
                                   "Foreign graduate degree",
                                   "Private sector experience"))

iccplot=ggplot(icc_data, aes(x = theta, y = prob, color = item, linetype = item)) +
    geom_line(size = 1) +
    labs(
        title = "Item Characteristic Curves",
        x = "Technocratic Trait (θ)",
        y = "Probability of trait being present",
        color = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = c(0.9, 0.15),  # ↪ bottom-right inside the plot
        legend.justification = c(1, 0),   # ↪ anchor legend box to bottom-right corner
        legend.background = element_rect(fill = alpha("white", 0.8), color = "gray80"),
        legend.title = element_blank()
    )


ggsave("iccplot.pdf", iccplot,width = 8,height = 5)

