# Load data
rm(list = ls())
df <- read_excel("spellsModel.xlsx")

# Create survival object
surv_obj <- Surv(time = df$spell_duration, event = df$status)
# Fit Cox model with regimeFujimori and technocratic trait
cox_model <- coxph(surv_obj ~ regimeFujimori + technocrat_trait_ltm, data = df)


stargazer::stargazer(cox_model)
# View results
saveRDS(cox_model,file = 'cox_model_invariants.rds')
