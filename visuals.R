###
library(ggplot2)
library(dplyr)
library(rio)
linkGit='https://github.com/PULSO-PUCP/pcm_history/raw/refs/heads/main/spellsModel.xlsx'

df <- rio::import(linkGit) %>%
    arrange(start) %>%
    mutate(
        internal_label = make.unique(as.character(PCM)),
        spell_label = factor(internal_label, levels = internal_label),
        display_label = PCM
    )

q1 <- quantile(df$spell_duration, 0.25)
med <- median(df$spell_duration)
q3 <- quantile(df$spell_duration, 0.75)
iqr <- IQR(df$spell_duration)
max_whisker <- q3 + 1.5 * iqr #(upper bound for typical values)



# Map y-axis positions
df$y_pos <- as.numeric(df$spell_label)

# Identify president segments (start and end y positions)
pres_blocks <- df %>%
    mutate(block = cumsum(PresidentOfExecutive != lag(PresidentOfExecutive, default = first(PresidentOfExecutive)))) %>%
    group_by(block, PresidentOfExecutive) %>%
    summarise(ymin = min(y_pos) - 0.5, ymax = max(y_pos) + 0.5, .groups = "drop")

pres_blocks <- pres_blocks %>%
    mutate(
        ymid = (ymin + ymax) / 2  # vertical midpoint for label
    )

# Begin plot
durationLolli=ggplot(df, aes(x = spell_duration, y = spell_label)) +
    
    geom_rect(data = pres_blocks, aes(xmin = 0, xmax = max(df$spell_duration) + 100,
                                      ymin = ymin, ymax = ymax,
                                      fill = as.factor(block)),
              color = NA, alpha = 1, inherit.aes = FALSE) +
    geom_text(
        data = pres_blocks,
        aes(x = max_whisker+150, y = ymid, label = PresidentOfExecutive),  # x=10 keeps labels on the left
        inherit.aes = FALSE,
        hjust = 0.5,  # left-align
        size = 2.5,  # adjust for readability
        fontface = "italic",color='grey50')+
    
    scale_fill_manual(values = rep(c("white", "grey95"), length.out = nrow(pres_blocks))) +
    
    guides(fill = "none")+
    # Lollipop stems and heads
    geom_segment(aes(x = 0, xend = spell_duration, yend = spell_label), color = "black") +
    geom_point(color = "black") +
    scale_y_discrete(labels = df$display_label) +
    # Use only those as x-axis ticks
    scale_x_continuous(
        expand = c(0, 0),
        breaks = round(c(q1, med, q3, max_whisker)),
        labels = round(c(q1, med, q3, max_whisker))
    )+
    geom_vline(xintercept = c(q1, med, q3), linetype = "dotted", color = "grey40") +
    geom_vline(xintercept = max_whisker, linetype = "dotted", color = "red")+
    labs(
        title = "Variability of Prime Ministers duration by Government",
        subtitle = "Peru, 1980–2025",
        x = "Days in Office (showing quartiles, and max whisker)",
        y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(margin = margin(r = 0)) 
    )

ggsave("durationLolli.pdf", durationLolli)



# kaplanDuration ----------------------------------------------------------


# Load data
rm(list = ls())

df <- rio::import(linkGit)

# Create survival object
surv_obj <- Surv(time = df$spell_duration, event = df$status)

# Fit Kaplan-Meier model
km_fit <- survfit(surv_obj ~ 1)  # no stratification, just baseline survival


# Define time points where you want annotations
time_points <- c(0, 180, 365, 730, 1000)  # Adjust as needed

# Extract survival values and number at risk at those times
summary_km <- summary(km_fit, times = time_points)
risk_df <- data.frame(
    time   = summary_km$time,
    n_risk = summary_km$n.risk,
    surv   = summary_km$surv
)

# Create the survival plot
kaplan_plot <- ggsurvplot(
    km_fit,
    data = df,
    conf.int = TRUE,
    risk.table = FALSE,
    legend = "none",
    xlab = "Days in Office",
    ylab = "Survival Probability",
    title = "Survival of Peruvian Prime Ministers, 1980–2025",
    palette = "black",
    censor.shape = "|",
    censor.size = 2
)

# Add the number-at-risk annotations at curve height
kaplanDuration=kaplan_plot$plot +
    geom_label(
        data = risk_df,
        aes(x = time, y = surv, label = paste0("n=", n_risk)),
        size = 3,
        vjust = -0.5,
        color = "black"
    )

ggsave("kaplanDuration.pdf", kaplanDuration)