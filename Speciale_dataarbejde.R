####Libraries####
library(modelsummary)
library(fixest)   
library(haven)
library(lmtest)
library(broom)
library(gridExtra)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggdist)
library(ggrepel)
library(readxl)
library(stargazer)
library(kableExtra)
library(tidyr)
library(zoo)
library(lubridate)
library(areaplot)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(grid)



##################### Uploader ######################################


#Set the path:
setwd("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse")

#Uploader data
d9 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2009_with_Predictions.xlsx")
d10 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2010_with_Predictions.xlsx")
d11 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2011_with_Predictions.xlsx")
d12 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2012_with_Predictions.xlsx")
d13 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2013_with_Predictions.xlsx")
d14 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2014_with_Predictions.xlsx")
d15 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2015_with_Predictions.xlsx")
d16 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2016_with_Predictions.xlsx")
d17 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2017_with_Predictions.xlsx")
d18 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2018_with_Predictions.xlsx")
d19 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2019_with_Predictions.xlsx")
d20 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2020_with_Predictions.xlsx")
d21 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2021_with_Predictions.xlsx")
d22 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2022_with_Predictions.xlsx")
d23 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2023_with_Predictions.xlsx")
d24 <- read_excel("/Users/karolinematildekruse/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Speciale/Data_analyse/2024_with_Predictions.xlsx")

# Merging datasets
D <- rbind(d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24)

# Opret variablen 'year' ud fra 'parid'
D$year <- as.numeric(substr(D$parid, 1, 4))

# Omdøber variabel:
D <- D %>% rename(ip = Predicted_Label)

##################### CLEANING: Statsminister og partier ######################

# Fjerner partier uden mandater
D <- D %>% filter(!group %in% c("CD", "Pause", "UP", "MødeSlut", "KRF", "FRI", "NY", "FP"))

# Samler nordatlantiske mandater til ét parti:
D <- D %>%
  mutate(group = case_when(
    group %in% c("NQ", "IA", "JF", "SIU", "SP", "T", "FF") ~ "Nordatlantiske Mandater",
    TRUE ~ group  # Beholder andre partier uændrede
  ))

# Opretter numerisk variabel for år, og indskrænker data til at gå fra 2009 til og med 2024. Derudover, filtrer data for observationer fra 2009 til 2024
D <- D %>%
  mutate(parid_date = as.Date(substr(parid, 1, 10), format = "%Y-%m-%d"))


# Koder Statsministeren ud
D <- D %>%
  mutate(
    group = case_when(
      # Direkte match uden tidsafhængighed
      speaker == "Claus Hjort Frederiksen" ~ "V",
      speaker == "Rasmus Jarlov" ~ "KF",
      speaker == "Søren Pape Poulsen" ~ "KF",
      speaker == "Mai Mercado" ~ "KF",
      speaker == "Lars Christian Lilleholt" ~ "V",
      speaker == "Ellen Trane Nørby" ~ "V",
      speaker == "Jakob Ellemann-Jensen" ~ "V",
      speaker == "Tommy Ahlers" ~ "V",
      speaker == "Karsten Lauritzen" ~ "V",
      speaker == "Troels Lund Poulsen" ~ "V",
      speaker == "Kristian Jensen" ~ "V",
      speaker == "Ole Birk Olesen" ~ "LA",
      speaker == "Ulla Tørnæs" ~ "V",
      speaker == "Eva Kjer Hansen" ~ "V",
      speaker == "Thyra Frank" ~ "LA",
      speaker == "Mette Bock" ~ "LA",
      speaker == "Mette Frederiksen" ~ "S",
      speaker == "Trine Bramsen" ~ "S",
      speaker == "Mogens Jensen" ~ "S",
      speaker == "Kaare Dybvad Bek" ~ "S",
      speaker == "Kaare Dybvad" ~ "S",
      speaker == "Nicolai Wammen" ~ "S",
      speaker == "Magnus Heunicke" ~ "S",
      speaker == "Nick Hækkerup" ~ "S",
      speaker == "Dan Jørgensen" ~ "S",
      speaker == "Pernille Rosenkrantz-Theil" ~ "S",
      speaker == "Simon Kollerup" ~ "S",
      speaker == "Mattias Tesfaye" ~ "S",
      speaker == "Joy Mogensen" ~ "S",
      speaker == "Ane Halsboe-Jørgensen" ~ "S",
      speaker == "Benny Engelbrecht" ~ "S",
      speaker == "Morten Bødskov" ~ "S",
      speaker == "Lea Wermelin" ~ "S",
      speaker == "Peter Hummelgaard" ~ "S",
      speaker == "Peter Hummelgaard Thomsen" ~ "S",
      speaker == "Rasmus Prehn" ~ "S",
      speaker == "Jeppe Kofod" ~ "S",
      speaker == "Flemming Møller Mortensen" ~ "S",
      speaker == "Jesper Petersen" ~ "S",
      speaker == "Jeppe Bruus" ~ "S",
      speaker == "Christian Rabjerg Madsen" ~ "S",
      speaker == "Louise Schack Elholm" ~ "V",
      speaker == "Thomas Danielsen" ~ "V",
      speaker == "Lars Aagaard" ~ "M",
      speaker == "Sophie Løhde" ~ "V",
      speaker == "Jacob Jensen" ~ "V",
      speaker == "Marie Bjerre" ~ "V",
      speaker == "Mette Kierkgaard" ~ "M",
      speaker == "Morten Dahlin" ~ "V",
      speaker == "Mia Wagner" ~ "V",
      speaker == "Stephanie Lose" ~ "V",
      speaker == "Anders Samuelsen" ~ "LA",
      # Tidsafhængige regler
      speaker == "Merete Riisager" & parid_date <= as.Date("2010-09-23") ~ "RV",
      speaker == "Merete Riisager" & parid_date > as.Date("2010-09-23") ~ "LA",
      speaker == "Lars Løkke Rasmussen" & parid_date <= as.Date("2021-01-01") ~ "V",
      speaker == "Lars Løkke Rasmussen" & parid_date >= as.Date("2021-06-05") ~ "M",
      speaker == "Inger Støjberg" & parid_date <= as.Date("2021-02-04") ~ "V",
      speaker == "Inger Støjberg" & parid_date >= as.Date("2022-06-23") ~ "DD",
      speaker == "Simon Emil Ammitzbøll-Bille" & parid_date <= as.Date("2008-10-13") ~ "RV",
      speaker == "Simon Emil Ammitzbøll-Bille" & parid_date <= as.Date("2009-06-16") ~ "Borgerligt Centrum",
      speaker == "Simon Emil Ammitzbøll-Bille" & parid_date <= as.Date("2019-10-22") ~ "LA",
      speaker == "Simon Emil Ammitzbøll-Bille" & parid_date <= as.Date("2020-10-08") ~ "Fremad",
      speaker == "Astrid Krag" & parid_date <= as.Date("2014-01-30") ~ "SF",
      speaker == "Astrid Krag" & parid_date > as.Date("2014-01-30") ~ "S",
      speaker == "Jakob Engel-Schmidt" & parid_date <= as.Date("2020-12-31") ~ "V",
      speaker == "Jakob Engel-Schmidt" & parid_date > as.Date("2020-12-31") ~ "M",
      speaker == "Christina Egelund" & parid_date <= as.Date("2010-12-31") ~ "KF",
      speaker == "Christina Egelund" & parid_date <= as.Date("2019-10-20") ~ "LA",
      speaker == "Christina Egelund" & parid_date <= as.Date("2020-10-08") ~ "Fremad",
      speaker == "Christina Egelund" & parid_date > as.Date("2022-12-15") ~ "M",
      TRUE ~ group # Hvis ingen regler matcher, behold den eksisterende værdi
    )
  )

# Create Folketingsår variable based on the month in `parid_date`
D <- D %>%
  mutate(
    folketingsaar = ifelse(
      lubridate::month(parid_date) >= 10,  # October or later
      paste0(lubridate::year(parid_date), "/", lubridate::year(parid_date) + 1),  # Current/next year
      paste0(lubridate::year(parid_date) - 1, "/", lubridate::year(parid_date))   # Previous/current year
    )
  )



##################### DATA PREP ####
# Add variables to the original dataframe
D <- D %>%
  mutate(
    speech_id = sub("-[0-9]+$", "", parid),
    year = as.numeric(substr(parid_date, 1, 4)),
    ip_category = case_when(
      ip == 1 ~ "Identitetspolitik",
      TRUE ~ "Andre"
    ),
    opposition = case_when(
      group %in% c("S", "EL", "ALT", "FG", "RV", "SF") ~ "Rød",
      group %in% c("DF", "V", "KF", "NB", "LA", "DD", "M") ~ "Blå",
      TRUE ~ NA_character_
    ),
    floej = case_when(
      group %in% c("DF", "NB", "DD") ~ "Ydre højrefløj",
      group %in% c("EL", "FG", "ALT") ~ "Ydre venstrefløj",
      group %in% c("RV", "SF", "S", "V", "K", "LA", "M") ~ "Centrum",
      TRUE ~ NA_character_
    ),
    parid_date = as.Date(parid_date) # Sikrer korrekt datoformat
  ) %>%
  filter(!is.na(opposition)) %>%
  filter(parid_date < as.Date("2024-07-01") & parid_date >= as.Date("2009-10-01"))


# Speech-level summary
speech_data <- D %>%
  group_by(speech_id, folketingsaar) %>%
  summarise(
    total_paragraphs = n(),
    ip_paragraphs = sum(ip == 1, na.rm = TRUE),
    proportion_ip = ip_paragraphs / total_paragraphs,
    speaker = first(speaker),
    group = first(group),
    year = first(year),
    chair = first(chair),
    parid_date = first(parid_date),
    parawordcount = sum(parawordcount, na.rm = TRUE)
  ) %>%
  ungroup()


################################################################################
##############################             #####################################
##############################. Deskriptiv #####################################
##############################             #####################################
###############################################################################

######################### Plot over antal taler pr. år ######################### 
# Define election years
election_years <- c("2011/2012", "2015/2016", "2019/2020", "2022/2023")

# Calculate the total number of speeches per folketingsår
speech_agg_folketingsaar <- speech_data %>%
  mutate(text_count = 1) %>%  # Create a count variable
  group_by(folketingsaar) %>%  # Group by folketingsår
  summarise(total_texts_per_folketingsaar = sum(text_count), .groups = "drop") %>%
  ungroup() %>%
  mutate(is_election_year = folketingsaar %in% election_years)  # Flag election years

# Calculate the mean number of speeches per folketingsår
mean_total_texts_per_folketingsaar <- mean(speech_agg_folketingsaar$total_texts_per_folketingsaar)

# PLOT
Figure_1 <- ggplot(speech_agg_folketingsaar, aes(x = folketingsaar, y = total_texts_per_folketingsaar / 1000)) +
  # Grey lines for lollipop sticks
  geom_segment(
    aes(x = folketingsaar, xend = folketingsaar, y = 0, yend = total_texts_per_folketingsaar / 1000), 
    color = "#808080"
  ) +
  # Conditional points: black for election years, grey otherwise
  geom_point(
    aes(color = is_election_year),
    size = 3, shape = 21, stroke = 0.5, fill = "darkgrey"
  ) +
  # Add dashed horizontal line for the mean
  geom_hline(
    yintercept = mean_total_texts_per_folketingsaar / 1000, linetype = "dashed", color = "darkgrey"
  ) +
  # Annotate the mean line
  annotate(
    "text", 
    x = max(speech_agg_folketingsaar$folketingsaar), 
    y = mean_total_texts_per_folketingsaar / 1000 + 2, 
    label = "Gennemsnit", color = "black", hjust = 1
  ) +
  # Add labels and scales
  labs(
    title = "",
    x = "",
    y = "Folketingstaler (1000)"
  ) +
  scale_x_discrete() +  # Use discrete scale for folketingsår
  scale_y_continuous(
    limits = c(0, 40), breaks = seq(0, 80, by = 20)
  ) +
  # Adjust colors for election years and improve legend
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "darkgrey"),
    labels = c("TRUE" = "Valgår", "FALSE" = "Ikke valgår")
  ) +
  # Minimal theme adjustments
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
      axis.line.x = element_line(color = "grey50", linewidth = 0.6),
      axis.line.y = element_line(color = "grey50", linewidth = 0.6),
      panel.border = element_blank(),
      legend.position = "none",
      plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),
      axis.text.y = element_text(size = 16, color = "black"),
      axis.title.x = element_text(size = 18, color = "black", margin = margin(t = 15)),
      axis.title.y = element_text(size = 18, color = "black", margin = margin(r = 15)),
      panel.grid.major.y = element_line(color = "grey90", linetype = "dotted"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
  )

# Print the graph
print(Figure_1)

# Save the plot as an image
ggsave("f1.png", plot = Figure_1, width = 12, height = 7, dpi = 300)


######################## Partier absolutte andele over tid ############################ 
# Define custom colors for parties
party_colors <- c(
  "S" = "#C62828", "V" = "#42A5F5", "KF" = "#339966", "RV" = "#800080",
  "SF" = "#FFB6C1", "DF" = "#FFD700", "EL" = "#FF1493", "LA" = "#000000",
  "M" = "#4B0082", "ALT" = "#90EE90", "FG" = "#228B22", "KD" = "#006400",
  "NB" = "#1E90FF", "DD" = "#365A8C"
)

# Define election years
election_years <- c("2011/2012", "2015/2016", "2019/2020", "2022/2023")

# Group data by folketingsaar and group to count speeches
speech_counts <- speech_data %>%
  group_by(folketingsaar, group) %>%
  summarise(speech_count = n(), .groups = "drop") %>%
  # Remove FG in 2022/2023
  filter(!(group == "FG" & folketingsaar == "2022/2023"))

# Identify the first year each party appears
first_appearance <- speech_counts %>%
  group_by(group) %>%
  filter(folketingsaar == min(folketingsaar)) %>%
  ungroup()

# Plot the data
ggplot(speech_counts, aes(x = folketingsaar, y = speech_count, color = group, group = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # Add vertical lines for election years
  geom_vline(
    xintercept = which(unique(speech_counts$folketingsaar) %in% election_years),
    linetype = "dashed", color = "black", size = 0.7
  ) +
  # Add party labels at their first appearance
  geom_text_repel(
    data = speech_counts %>% group_by(group) %>% filter(folketingsaar == min(folketingsaar)),
    aes(label = group),
    nudge_x = 0.5,              # Slight nudge to move labels
    nudge_y = 0.5,              # Adjust vertical position to avoid overlap
    direction = "y",            # Spread labels in the vertical direction
    size = 6,                   # Font size
    fontface = "bold",          # Bold text
    box.padding = 0.5,          # Padding around labels
    point.padding = 0.2,        # Space around points
    segment.color = "grey50",   # Line color to point
    show.legend = FALSE         # Hide legend for text labels
  ) +
  scale_color_manual(values = party_colors) +
  labs(
    x = NULL,
    y = "Antal Folketingstaler",
    color = "Party"
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Gem plot som PNG
ggsave("figure_3.png", width = 20, height = 8, dpi = 300)





##############  Plot over partier, der taler mest på tværs af år #####################
# Calculate the total number of speeches and average speeches per year
speech_agg <- speech_data %>%
  mutate(text_count = 1) %>%  # Create a count variable
  group_by(group, year) %>%   # Group by group and year
  summarise(yearly_texts = sum(text_count), .groups = "drop") %>%
  group_by(group) %>%
  summarise(
    total_texts_across_years = sum(yearly_texts),            # Total speeches across years
    years_with_data = n(),                                   # Count only years with data
    avg_speeches_per_year = total_texts_across_years / years_with_data,  # Average
    .groups = "drop"
  ) %>%
  ungroup()

# PLOT
# PLOT
ggplot(
  speech_agg %>% filter(!group %in% c("Fremad", "Nordatlantiske Mandater", "UFG")), 
  aes(x = reorder(group, avg_speeches_per_year), y = avg_speeches_per_year)
) +
  # Add grey lines for lollipop sticks
  geom_segment(
    aes(x = reorder(group, avg_speeches_per_year), 
        xend = reorder(group, avg_speeches_per_year), 
        y = 0, yend = avg_speeches_per_year), 
    color = "#808080"
  ) +
  # Add points with colors based on `group`
  geom_point(aes(color = group), size = 3) +
  labs(
    title = "",
    x = "",
    y = "Gennemsnitlige folketingstaler pr. år",  # Updated without (1000)
    color = NULL
  ) +
  # Set y-axis scale dynamically
  scale_y_continuous(
    breaks = seq(0, max(speech_agg$avg_speeches_per_year, na.rm = TRUE), by = 1000),
    limits = c(0, NA)
  ) +
  # Define custom colors for parties
  scale_color_manual(values = party_colors) +
  # Minimal theme adjustments
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    axis.line.x = element_line(color = "grey50", size = 0.7), # Grey x-axis line
    axis.line.y = element_line(color = "grey50", size = 0.7),
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

ggsave("figur3.png", width = 12, height = 7, dpi = 300)

################################################################################
##############################             #####################################
##############################.    H1      #####################################
##############################             #####################################
###############################################################################


######################### ABSOLUTTE UDVIKLING ##################################
# Beregn total IP-paragraffer pr. folketingsår
total_ip_paragraphs <- D %>%
  filter(ip == 1) %>%
  group_by(folketingsaar) %>%
  summarise(total_paragraphs = n(), .groups = "drop")

# Beregn total IP-taler pr. folketingsår (taler med > 75% IP-indhold)
total_ip_speeches <- speech_data %>%
  filter(proportion_ip > 0.75) %>%
  mutate(
    folketingsaar = ifelse(
      lubridate::month(parid_date) >= 10, 
      paste0(lubridate::year(parid_date), "/", lubridate::year(parid_date) + 1),
      paste0(lubridate::year(parid_date) - 1, "/", lubridate::year(parid_date))
    )
  ) %>%
  group_by(folketingsaar) %>%
  summarise(total_speeches = n(), .groups = "drop")

# Plot total IP-paragraffer og IP-taler med Loess-kurver
absolute <- ggplot() +
  # Loess-kurve for IP-paragraffer
  geom_smooth(
    data = total_ip_paragraphs,
    aes(x = as.numeric(factor(folketingsaar, levels = unique(folketingsaar))),
        y = total_paragraphs),
    method = "loess",
    se = FALSE,
    color = "black",
    linetype = "dotted"
  ) +
  # Loess-kurve for IP-taler
  geom_smooth(
    data = total_ip_speeches,
    aes(x = as.numeric(factor(folketingsaar, levels = unique(folketingsaar))),
        y = total_speeches),
    method = "loess",
    se = FALSE,
    color = "black",
    linetype = "solid"
  ) +
  # Label for IP-paragraffer
  annotate(
    "text",
    x = max(as.numeric(factor(total_ip_paragraphs$folketingsaar))) - 0.2,
    y = max(total_ip_paragraphs$total_paragraphs, na.rm = TRUE) - 2250,  # Flyt 500 enheder ned
    label = "Afsnit",
    hjust = 1, vjust = 0.5, size = 6, fontface = "bold"
  ) +
  # Label for IP-taler
  annotate(
    "text",
    x = max(as.numeric(factor(total_ip_speeches$folketingsaar))) - 0.2,
    y = max(total_ip_speeches$total_speeches, na.rm = TRUE),
    label = "Taler",
    hjust = 1, vjust = 0.5, size = 6, fontface = "bold"
  ) +
  # Tilpas x-aksen med årstal
  scale_x_continuous(
    breaks = 1:length(unique(total_ip_paragraphs$folketingsaar)),
    labels = unique(total_ip_paragraphs$folketingsaar),
    expand = expansion(add = c(0.5, 1))
  ) +
  # Tilpas y-aksen
  scale_y_continuous(
    limits = c(0, 9000),
    breaks = seq(0, max(total_ip_paragraphs$total_paragraphs, na.rm = TRUE), by = 2500)
  ) +
  # Labels til akser
  labs(
    x = NULL,
    y = "Identitetspolitisk aktivitet (Absolut)"
  ) +
  # Tilpas tema
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    axis.line.x = element_line(color = "grey50", size = 0.7),
    axis.line.y = element_line(color = "grey50", size = 0.7),
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Gem plottet
ggsave("total_ip_speeches_paragraphs.png", width = 12, height = 7, dpi = 300)


######################## LOESS CURVES AFSNIT OG TALER############################
# Paragraphs: calculate the proportion of IP paragraphs by folketingsår
paragraphs_loess <- D %>%
  mutate(is_ip = ip == 1) %>%
  group_by(folketingsaar) %>%
  summarise(proportion_ip = mean(is_ip, na.rm = TRUE)) %>%
  ungroup()

# Speeches: calculate the proportion of IP speeches (> 75% IP paragraphs) by folketingsår
speeches_loess <- speech_data %>%
  mutate(
    folketingsaar = ifelse(
      lubridate::month(parid_date) >= 10, 
      paste0(lubridate::year(parid_date), "/", lubridate::year(parid_date) + 1),
      paste0(lubridate::year(parid_date) - 1, "/", lubridate::year(parid_date))
    )
  ) %>%
  group_by(folketingsaar) %>%
  summarise(proportion_ip_speeches = mean(proportion_ip > 0.75, na.rm = TRUE)) %>%
  ungroup()

# Plot the Loess curves
Loess <- ggplot() +
  # Add smoothed line for paragraphs (Afsnit)
  geom_smooth(
    data = paragraphs_loess,
    aes(x = as.numeric(factor(folketingsaar, levels = unique(folketingsaar))),
        y = proportion_ip * 100),
    method = "loess",
    se = FALSE,
    color = "black",
    linetype = "dotted"
  ) +
  # Add smoothed line for speeches (Taler)
  geom_smooth(
    data = speeches_loess,
    aes(x = as.numeric(factor(folketingsaar, levels = unique(folketingsaar))),
        y = proportion_ip_speeches * 100),
    method = "loess",
    se = FALSE,
    color = "black",
    linetype = "solid"
  ) +
  # Add text label for Afsnit (Dotted Line)
  annotate(
    "text",
    x = max(as.numeric(factor(paragraphs_loess$folketingsaar))) - 0.2, # Move slightly inward
    y = max(paragraphs_loess$proportion_ip * 100),
    label = "Afsnit",
    hjust = 1, vjust = 0.5, size = 6, fontface = "bold"
  ) +
  # Add text label for Taler (Solid Line)
  annotate(
    "text",
    x = max(as.numeric(factor(speeches_loess$folketingsaar))) - 0.2, # Move slightly inward
    y = max(speeches_loess$proportion_ip_speeches * 100),
    label = "Taler",
    hjust = 1, vjust = 0.5, size = 6, fontface = "bold"
  ) +
  # Customize x-axis with Folketingsår labels
  scale_x_continuous(
    breaks = 1:length(unique(paragraphs_loess$folketingsaar)),
    labels = unique(paragraphs_loess$folketingsaar),
    expand = expansion(add = c(0.5, 1))
  ) +
  # Customize y-axis
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 5),
    labels = function(x) x
  ) +
  # Labels for axes
  labs(
    x = NULL,
    y = "Andel identitetspolitik (%)"
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    axis.line.x = element_line(color = "grey50", size = 0.7), # Grey x-axis line
    axis.line.y = element_line(color = "grey50", size = 0.7), # Grey y-axis line
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_blank()      # Remove border
  )

# Save the combined plot
ggsave("h1loess.png", , width = 12, height = 7, dpi = 300)

#################### TWO LOESS NEXT TO EACHOTHER ##############
# Kombiner dine plots
combined <- grid.arrange(Loess, absolute, ncol = 2)

# Gem det kombinerede plot med ggsave
ggsave("combined.png", plot = combined, width = 18, height = 7, dpi = 300)

#### EKSPORTER ALLE TAL TIL TABEL:
# Merge paragraphs and speeches data into a single table
merged_data <- paragraphs_loess %>%
  full_join(speeches_loess, by = "folketingsaar") %>%
  rename(
    `Folketingsår` = folketingsaar,
    `Paragraphs (IP Proportion)` = proportion_ip,
    `Speeches (IP > 75% Proportion)` = proportion_ip_speeches
  )

# Convert to a dataframe with rounded values
export_data <- merged_data %>%
  mutate(
    `Paragraphs (IP Proportion)` = round(`Paragraphs (IP Proportion)`, 3),
    `Speeches (IP > 75% Proportion)` = round(`Speeches (IP > 75% Proportion)`, 3)
  )

# Generate LaTeX table with stargazer
stargazer(
  export_data,
  type = "latex",
  summary = FALSE,
  title = "Proportions of IP Paragraphs and Speeches by Folketingsår",
  out = "proportions_table.tex",
  digits = 3,
  rownames = FALSE
)

######## Tal til Loess############

# Paragraphs: Calculate absolute numbers of IP paragraphs per Folketingsår
paragraphs_absolute <- D %>%
  mutate(is_ip = ip == 1) %>%
  group_by(folketingsaar) %>%
  summarise(
    total_paragraphs = n(),
    ip_paragraphs = sum(is_ip, na.rm = TRUE)
  ) %>%
  ungroup()

# Speeches: Calculate absolute numbers of IP speeches per Folketingsår
speeches_absolute <- speech_data %>%
  mutate(
    folketingsaar = ifelse(
      lubridate::month(parid_date) >= 10, 
      paste0(lubridate::year(parid_date), "/", lubridate::year(parid_date) + 1),
      paste0(lubridate::year(parid_date) - 1, "/", lubridate::year(parid_date))
    )
  ) %>%
  group_by(folketingsaar) %>%
  summarise(
    total_speeches = n(),
    ip_speeches = sum(proportion_ip > 0.75, na.rm = TRUE)
  ) %>%
  ungroup()

# Combine the absolute numbers into one table
absolute_data <- paragraphs_absolute %>%
  full_join(speeches_absolute, by = "folketingsaar") %>%
  rename(
    `Folketingsår` = folketingsaar,
    `Total Paragraphs` = total_paragraphs,
    `IP Paragraphs` = ip_paragraphs,
    `Total Speeches` = total_speeches,
    `IP Speeches` = ip_speeches
  )

# Export the table as LaTeX with absolute numbers
stargazer(
  absolute_data,
  type = "latex",
  summary = FALSE,
  title = "Absolute Counts of IP Paragraphs and Speeches by Folketingsår",
  out = "absolute_counts_table.tex",
  digits = 0,
  rownames = FALSE
)


######### KATEGORIER AF TALER #################
# Kategorisering og sikring af alle kategorier baseret på taleniveau
speech_distribution <- speech_data %>%
  mutate(
    category = case_when(                              # Kategoriser taler baseret på proportion_ip
      proportion_ip > 0.75 ~ "Høj identitetspolitik",
      proportion_ip > 0.25 ~ "Moderat identitetspolitik",
      proportion_ip > 0 ~ "Lav identitetspolitik",
      TRUE ~ "Ingen identitetspolitik"
    )
  ) %>%
  group_by(folketingsaar, category) %>%
  summarise(speech_count = n(), .groups = "drop") %>%  # Tæl taler pr. kategori
  complete(folketingsaar, category, fill = list(speech_count = 0)) %>%  # Sørg for, at alle kategorier vises
  group_by(folketingsaar) %>%
  mutate(
    total_speeches = sum(speech_count),               # Beregn total for alle kategorier
    proportion = speech_count / total_speeches * 100  # Proportion i procent
  ) %>%
  ungroup() %>%
  filter(category != "Ingen identitetspolitik")       # Fjern "Ingen identitetspolitik"

# Visualisering

# Ensure the category variable is a factor with the desired order
speech_distribution$category <- factor(
  speech_distribution$category,
  levels = c("Høj identitetspolitik", "Moderat identitetspolitik", "Lav identitetspolitik")
)


# PLOT:
Figure_H1_2 <- ggplot(speech_distribution, aes(x = folketingsaar, y = proportion, color = category, group = category)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    y = "Folketingstaler med identitetspolitik (%)",
    x = "",
    color = NULL  # Remove legend title
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    axis.line.x = element_line(color = "grey50", size = 0.7), # Grey x-axis line
    axis.line.y = element_line(color = "grey50", size = 0.7), # Grey y-axis line
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_blank()      # Remove border
  )+
  scale_y_continuous(limits = c(0, 20)) +
  scale_color_manual(
    values = c(
      "Høj identitetspolitik" = "#023047",  # Dark blue
      "Moderat identitetspolitik" = "#219EBC",  # Medium blue
      "Lav identitetspolitik" = "#8ECAE6"  # Light blue
    ),
    labels = c(
      "Høj identitetspolitik" = "Høj grad identitetspolitik",
      "Moderat identitetspolitik" = "Moderat grad identitetspolitik",
      "Lav identitetspolitik" = "Lav grad identitetspolitik"
    )
  )

# Gem plot som fil
ggsave("H1_2.png", plot = Figure_H1_2, width = 12, height = 7, dpi = 300)

#### TAL FRA GRAFEN: 
# Categorize speeches based on `proportion_ip`
speech_distribution <- speech_data %>%
  mutate(
    category = case_when(
      proportion_ip > 0.75 ~ "Høj identitetspolitik",
      proportion_ip > 0.25 ~ "Moderat identitetspolitik",
      proportion_ip > 0 ~ "Lav identitetspolitik",
      TRUE ~ "Ingen identitetspolitik"
    )
  ) %>%
  group_by(folketingsaar, category) %>%
  summarise(speech_count = n(), .groups = "drop") %>%  # Count speeches per category
  complete(folketingsaar, category, fill = list(speech_count = 0)) %>%  # Ensure all categories are present
  group_by(folketingsaar) %>%
  mutate(
    total_speeches = sum(speech_count),                # Total speeches for each folketingsaar
    proportion = round(speech_count / total_speeches * 100, 2)  # Proportion of speeches in percentage (rounded)
  ) %>%
  ungroup()

# Wide format for export
export_data <- speech_distribution %>%
  select(folketingsaar, category, speech_count, proportion) %>%
  pivot_wider(
    names_from = category,
    values_from = c(speech_count, proportion),
    names_sep = "_"
  )

# Export the table using stargazer
library(stargazer)
stargazer(
  export_data,
  type = "latex",
  summary = FALSE,
  title = "Distribution of Speeches by Identity Politics Category and Folketingsår",
  out = "speech_distribution_table.tex",
  digits = 2,
  rownames = FALSE
)




############ ÅRLIG GNS. ####
# Udregn gennemsnitlig og samlet absolut og relativ ændring
change_summary <- speech_distribution %>%
  group_by(category) %>%
  summarise(
    `Gns. absolut ændring` = mean(abs(diff(proportion)), na.rm = TRUE),
    `Samlet absolut ændring` = last(proportion) - first(proportion),
    `Gns. relativ ændring (%)` = mean(abs(diff(proportion / first(proportion))), na.rm = TRUE) * 100,
    `Samlet relativ ændring (%)` = (last(proportion) - first(proportion)) / first(proportion) * 100
  )

# Konverter change_summary til data.frame for kompatibilitet med stargazer
change_summary_df <- as.data.frame(change_summary)

# Lav en pæn tabel med stargazer
stargazer(change_summary_df,
          type = "latex",                         # Generer LaTeX-kode
          title = "Ændringer i Identitetspolitik pr. Kategori",
          summary = FALSE,                        # Fjern automatisk opsummering
          rownames = FALSE,                      # Fjern række-navne
          digits = 2,                            # Begræns til 2 decimaler
          column.sep.width = "10pt",             # Juster kolonneafstand
          label = "tab:change_summary",          # Etiket til krydsreferencer i LaTeX
          out = "change_summary_table.tex")      # Gem tabellen som en .tex-fil













###############################################################################
##############################. Robusthed #####################################
###############################################################################

## PARTIERNES ABSOLUTTE UDVIKLING (BILAG) ######
# Beregn total IP-taler (i stedet for andele) per parti og folketingsår
speech_distribution <- speech_data %>%
  filter(!is.na(folketingsaar) & !is.na(group)) %>%
  mutate(
    is_high_ip_speech = ifelse(proportion_ip > 0.75, 1, 0) # Markér høj-IP-taler
  ) %>%
  group_by(folketingsaar, group) %>%
  summarise(
    total_speeches = n(),                          # Samlet antal taler
    high_ip_speeches = sum(is_high_ip_speech, na.rm = TRUE) # Antal høj-IP-taler
  ) %>%
  ungroup() %>%
  filter(!(group == "NB" & folketingsaar == "2018/2019"), 
         !(group == "FG" & folketingsaar == "2022/2023")) %>%
  arrange(group, folketingsaar)

# Tilføj labels ved første år og visualiseringseksra
speech_distribution <- speech_distribution %>%
  group_by(group) %>%
  mutate(
    first_label_position = ifelse(folketingsaar == min(folketingsaar), high_ip_speeches, NA),
    last_label_position = ifelse(folketingsaar == max(folketingsaar), high_ip_speeches, NA)
  ) %>%
  ungroup()

# Liste over folketingsvalg (til lodrette linjer)
valg_aar <- c("2011/2012", "2015/2016", "2019/2020", "2022/2023")

# Visualisering af samlet antal IP-taler
ggplot(speech_distribution, aes(x = folketingsaar, y = high_ip_speeches, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Lodrette linjer for folketingsvalg
  geom_vline(
    xintercept = which(unique(speech_distribution$folketingsaar) %in% valg_aar),
    linetype = "dashed", color = "black", linewidth = 0.7
  ) +
  # Labels ved første år
  geom_text_repel(
    data = speech_distribution %>% filter(!is.na(first_label_position)),
    aes(label = group),
    nudge_x = 0.5,
    nudge_y = 50,              # Juster nudge_y for bedre placering
    direction = "y",
    size = 6,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.2,
    segment.color = "grey50",
    show.legend = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, max(speech_distribution$high_ip_speeches) + 50),
    breaks = seq(0, max(speech_distribution$high_ip_speeches, na.rm = TRUE), by = 50),
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = c(0.01, 0)) +
  labs(
    x = NULL,
    y = "Antal identitetspolitiske taler (> 75%)",
    color = "Parti"
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  scale_color_manual(values = party_colors)

# Gem plottet
ggsave("total_ip_speeches_by_party.png", width = 20, height = 8, dpi = 300)


## GENERELLE DEBAT: ANTAL TALER: PARTINIVEUA #######
# Filtrer kun officielle partier og beregn andelen af identitetspolitiske taler
speech_distribution <- speech_data %>%
  filter(!is.na(folketingsaar) & !is.na(group)) %>%
  mutate(
    is_high_ip_speech = ifelse(proportion_ip > 0.75, 1, 0) # Markér høj-IP-taler
  ) %>%
  group_by(folketingsaar, group) %>%
  summarise(
    total_speeches = n(),
    high_ip_speeches = sum(is_high_ip_speech, na.rm = TRUE),
    proportion_high_ip = (high_ip_speeches / total_speeches) * 100
  ) %>%
  ungroup() %>%
  filter(!(group == "NB" & folketingsaar == "2018/2019"), 
         !(group == "FG" & folketingsaar == "2022/2023")) %>%
  arrange(group, folketingsaar)

# Tilføj labels ved første år og visualiseringseksra
speech_distribution <- speech_distribution %>%
  group_by(group) %>%
  mutate(
    first_label_position = ifelse(folketingsaar == min(folketingsaar), proportion_high_ip, NA),
    last_label_position = ifelse(folketingsaar == max(folketingsaar), proportion_high_ip, NA)
  ) %>%
  ungroup()

# Liste over folketingsvalg (til lodrette linjer)
valg_aar <- c("2011/2012", "2015/2016", "2019/2020", "2022/2023")

# Updated ggplot code
ggplot(speech_distribution, aes(x = folketingsaar, y = proportion_high_ip, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Add election year markers
  geom_vline(
    xintercept = which(unique(speech_distribution$folketingsaar) %in% valg_aar),
    linetype = "dashed", color = "black", linewidth = 0.7
  ) +
  geom_text_repel(
    data = speech_distribution %>% filter(!is.na(first_label_position)),
    aes(label = group),
    nudge_x = 0.5,              # Slight nudge to move labels
    nudge_y = 0.5,              # Adjust vertical position to avoid overlap
    direction = "y",            # Spread labels in the vertical direction
    size = 6,                   # Font size
    fontface = "bold",          # Bold text
    box.padding = 0.5,          # Padding around labels
    point.padding = 0.2,        # Space around points
    segment.color = "grey50",   # Line color to point
    show.legend = FALSE         # Hide legend for text labels
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 5),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = c(0.01, 0)) +
  labs(
    x = NULL,
    y = "Andel identitetspolitik (%)",
    color = "Parti"
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  scale_color_manual(values = party_colors)

# Save the updated plot
ggsave("robust_1.png", width = 20, height = 8, dpi = 300)


#################### TABEL TIL GRAF (BILAG) #########################

# Create a summary table from `speech_distribution`
summary_table <- speech_distribution %>%
  select(folketingsaar, group, total_speeches, high_ip_speeches, proportion_high_ip) %>% # Match variable names
  arrange(group, folketingsaar) %>%
  mutate(
    proportion_high_ip = round(proportion_high_ip, 2),  # Round proportions to two decimals
    total_speeches = as.integer(total_speeches),        # Ensure integers for counts
    high_ip_speeches = as.integer(high_ip_speeches)     # Ensure integers for counts
  ) %>%
  rename(
    "Year" = folketingsaar,                             # Rename columns for LaTeX readability
    "Party" = group,
    "Total Speeches" = total_speeches,
    "High-IP Speeches" = high_ip_speeches,
    "Proportion High-IP (%)" = proportion_high_ip
  )

# Generate the LaTeX table
stargazer(
  summary_table,
  type = "latex",
  summary = FALSE,
  title = "Distribution of Identity Politics Speeches by Party and Year",
  out = "speech_distribution_table.tex",
  digits = 2,
  rownames = FALSE
)



## IDENTITETSPOLITISKE DEBAT: ANTAL TALER: PARTINIVEAU ######
# Beregn partiers andel af høj-IP taler (ip_proportion > 75%) pr. år
party_ip_share_speeches <- speech_data %>%
  filter(proportion_ip > 0.75) %>%  # Kun taler med ip_proportion > 75%
  group_by(folketingsaar, group) %>%
  summarise(
    party_high_ip_speeches = n(),                   # Antal høj-IP taler pr. parti
    .groups = "drop"
  ) %>%
  group_by(folketingsaar) %>%
  mutate(
    total_high_ip_speeches = sum(party_high_ip_speeches), # Total antal høj-IP taler pr. år
    party_share = (party_high_ip_speeches / total_high_ip_speeches) * 100 # Partiets andel i procent
  ) %>%
  ungroup()

# Add labels for Folketingsvalg
valg_aar <- c("2011/2012", "2015/2016", "2019/2020", "2022/2023")

# Add labels for the first data point for each group
party_ip_share_speeches <- party_ip_share_speeches %>%
  group_by(group) %>%
  mutate(
    first_label_position = ifelse(folketingsaar == min(folketingsaar), party_share, NA)
  ) %>%
  ungroup()

# Updated ggplot code
plot_speeches_IP <- ggplot(party_ip_share_speeches, aes(x = folketingsaar, y = party_share, color = group, group = group)) +
  # Add line and points for the data
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Add labels at the first appearance
  geom_text(
    data = party_ip_share_speeches %>% filter(!is.na(first_label_position)),
    aes(label = group),
    hjust = -0.1, vjust = -0.5, size = 6, fontface = "bold", show.legend = FALSE
  ) +
  # Add election year markers
  geom_vline(
    xintercept = which(unique(party_ip_share_speeches$folketingsaar) %in% valg_aar),
    linetype = "dashed", color = "black", linewidth = 0.7
  ) +
  # Y-axis scaling and labels
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  # X-axis scaling and labels
  scale_x_discrete(expand = c(0.01, 0)) +
  # Add plot labels and titles
  labs(
    x = NULL,
    y = "Andel af den identitetspolitik debat (%)",
    color = "Parti"
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(size = 22, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  scale_color_manual(values = party_colors)

# Save the updated plot
ggsave("robust_2.png", width = 20, height = 8, dpi = 300)


# Create a summary table from `party_ip_share_speeches`
summary_table <- party_ip_share_speeches %>%
  select(folketingsaar, group, party_high_ip_speeches, total_high_ip_speeches, party_share) %>% # Match variable names
  arrange(group, folketingsaar) %>%
  mutate(
    party_share = round(party_share, 2),               # Round party share to two decimals
    party_high_ip_speeches = as.integer(party_high_ip_speeches),  # Ensure integers for counts
    total_high_ip_speeches = as.integer(total_high_ip_speeches)   # Ensure integers for counts
  ) %>%
  rename(
    "Year" = folketingsaar,                            # Rename columns for LaTeX readability
    "Party" = group,
    "High-IP Speeches (Party)" = party_high_ip_speeches,
    "Total High-IP Speeches (Year)" = total_high_ip_speeches,
    "Party Share (%)" = party_share
  )

# Generate the LaTeX table
stargazer(
  summary_table,
  type = "latex",
  summary = FALSE,
  title = "Party Contribution to High-Identity Politics Speeches by Year",
  out = "party_ip_share_table.tex",
  digits = 2,
  rownames = FALSE
)






########## S OG V #########
# Beregn total IP-afsnit og taler per år til normalisering
total_ip_paragraphs_per_year <- D %>%
  filter(ip == 1) %>%
  group_by(folketingsaar) %>%
  summarise(total_ip_paragraphs = n(), .groups = "drop")

total_ip_speeches_per_year <- speech_data %>%
  filter(proportion_ip > 0.75) %>%
  group_by(folketingsaar) %>%
  summarise(total_ip_speeches = n(), .groups = "drop")

# Beregn andel af IP-afsnit for S og V
sv_ip_paragraph_share <- D %>%
  filter(ip == 1 & group %in% c("S", "V")) %>%
  group_by(folketingsaar, group) %>%
  summarise(group_ip_paragraphs = n(), .groups = "drop") %>%
  left_join(total_ip_paragraphs_per_year, by = "folketingsaar") %>%
  mutate(
    proportion = (group_ip_paragraphs / total_ip_paragraphs) * 100,
    type = "Afsnit"
  )

# Beregn andel af IP-taler for S og V
sv_ip_speech_share <- speech_data %>%
  filter(proportion_ip > 0.75 & group %in% c("S", "V")) %>%
  group_by(folketingsaar, group) %>%
  summarise(group_ip_speeches = n(), .groups = "drop") %>%
  left_join(total_ip_speeches_per_year, by = "folketingsaar") %>%
  mutate(
    proportion = (group_ip_speeches / total_ip_speeches) * 100,
    type = "Taler"
  )

# Kombinér datasæt
combined_data <- bind_rows(sv_ip_paragraph_share, sv_ip_speech_share) %>%
  mutate(folketingsaar = factor(folketingsaar, levels = unique(folketingsaar)))

# Liste over valgår (til lodrette linjer)
election_years <- data.frame(
  folketingsaar = c("2011/2012", "2015/2016", "2019/2020", "2022/2023"),
  label_position = 100
) %>%
  mutate(folketingsaar = factor(folketingsaar, levels = levels(combined_data$folketingsaar)))

## IDENTITETSPOLITISKE DEBAT: ANTAL PARAGRAFFER: PARTINIVEAU  ####

party_ip_share <- D %>%
  filter(ip == 1) %>%  # Kun paragraffer om identitetspolitik
  group_by(folketingsaar, group) %>%
  summarise(
    party_ip_paragraphs = n(),                    # Antal identitetspolitiske paragraffer pr. parti
    .groups = "drop"
  ) %>%
  group_by(folketingsaar) %>%
  mutate(
    total_ip_paragraphs = sum(party_ip_paragraphs), # Total antal IP-paragraffer pr. år
    party_share = (party_ip_paragraphs / total_ip_paragraphs) * 100 # Partiets andel i procent
  ) %>%
  ungroup()

# Create the updated visualization without election years
plot_paragraphs_IP <- ggplot(party_ip_share, aes(x = folketingsaar, y = party_share, color = group, group = group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.5) +
  # Add labels at the first year where the party has data
  geom_text_repel(
    data = party_ip_share %>% group_by(group) %>% filter(folketingsaar == min(folketingsaar)),
    aes(label = group),
    nudge_x = 0.5,              # Lidt horisontal nudge
    nudge_y = 0.5,              # Vertikal justering
    direction = "y",            # Spred labels i vertikal retning
    size = 6,                   # Skriftstørrelse
    fontface = "bold",          # Fed skrift
    box.padding = 0.5,          # Ekstra plads omkring label
    point.padding = 0.2,        # Plads omkring punkter
    segment.color = "grey50",   # Linjefarve til punkt
    show.legend = FALSE         # Skjul label-legend
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = c(0.01, 0)) +
  labs(
    x = NULL,
    y = "Andel identitetspolitik (%)",
    color = "Parti"
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  scale_color_manual(values = party_colors)

# Save the updated plot without the legend
ggsave("bilag_partier.png", plot = plot_paragraphs_IP, width = 20, height = 8, dpi = 300)

## GENERELLE DEBAT: ANTAL PARAGRAFFER: PARTINIVEAU #######

# Filtrer kun officielle partier og fjern alle år med proportion = 0
speech_distribution <- D %>%
  group_by(folketingsaar, group) %>%
  summarise(
    total_paragraphs = n(),                         # Total antal observationer for partiet
    ip_paragraphs = sum(ip == 1, na.rm = TRUE),     # Antal observationer med identitetspolitik
    proportion = ip_paragraphs / total_paragraphs * 100 # Andel i procent
  ) %>%
  filter(proportion > 0) %>%                        # Fjern alle år med proportion = 0
  arrange(group, folketingsaar) %>%                 # Sortér korrekt
  group_by(group) %>%
  mutate(
    lag_year = lag(as.integer(substr(folketingsaar, 1, 4))),  # Forrige år
    current_year = as.integer(substr(folketingsaar, 1, 4)),
    continuous = (current_year - lag_year) == 1 | is.na(lag_year), # Kun kontinuerlige år
    connection_group = cumsum(!continuous)          # Ny gruppe for hver brud
  ) %>%
  ungroup()

# Liste over folketingsvalg (til lodrette linjer)
valg_aar <- c("2011/2012", "2015/2016", "2019/2020", "2022/2023")

# Tilføj partinavne ved slutningen af linjerne
speech_distribution <- speech_distribution %>%
  group_by(group) %>%
  mutate(label_position = ifelse(folketingsaar == max(folketingsaar), proportion, NA))

# Tilføj partinavne ved første datapunkt for hvert parti
plot_paragraphs <- ggplot(speech_distribution, aes(x = folketingsaar, y = proportion, color = group)) +
  geom_line(aes(group = interaction(group, connection_group)), size = 1.5) +
  geom_point(size = 3) +
  
  # Tilføj labels ved første datapunkt
  geom_text_repel(
    data = speech_distribution %>%
      group_by(group) %>%
      filter(folketingsaar == min(folketingsaar[!is.na(proportion)])),
    aes(label = group),
    nudge_x = -0.5,
    nudge_y = 1,
    size = 6,
    fontface = "bold",
    color = "black",
    segment.color = "grey50",
    show.legend = FALSE
  ) +
  
  # Lodrette linjer for folketingsvalg
  geom_vline(xintercept = valg_aar, linetype = "dashed", color = "grey40", size = 0.8) +
  
  # Justering af y-aksen og farveskala
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5)
  ) +
  scale_x_discrete(expand = c(0.01, 0)) +
  
  # Farveskala for partier
  scale_color_manual(values = c(
    "S" = "#C62828",      # Strong red for S
    "V" = "#42A5F5",      # Lighter blue for V
    "KF" = "#339966",
    "RV" = "#800080",
    "SF" = "#FFB6C1",     # Light pink for SF
    "DF" = "#FFD700",
    "EL" = "#FF1493",     # Pink for EL
    "LA" = "#000000",
    "M" = "#4B0082",
    "ALT" = "#90EE90",
    "FG" = "#228B22",
    "KD" = "#006400",
    "NB" = "#1E90FF",
    "DD" = "#365A8C"
  )) +
  
  # Labels og titler
  labs(
    x = "",
    y = "Andel identitetspolitik (%)",
    color = "Parti"
  ) +
  
  # Æstetik som tidligere
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 20, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, color = "black", margin = margin(r = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


# Gem grafen igen med passende størrelse
ggsave("Partier_ud_i_tid.png", width = 18, height = 8, dpi = 300)

#### TAL FRA FIGRUEN ####
# Calculate party's share of identity politics paragraphs
party_ip_share <- D %>%
  filter(ip == 1) %>%  # Only paragraphs about identity politics
  group_by(folketingsaar, group) %>%
  summarise(
    party_ip_paragraphs = n(),                     # Number of identity politics paragraphs per party
    .groups = "drop"
  ) %>%
  group_by(folketingsaar) %>%
  mutate(
    total_ip_paragraphs = sum(party_ip_paragraphs), # Total identity politics paragraphs per year
    party_share = round((party_ip_paragraphs / total_ip_paragraphs) * 100, 2) # Party's share in percentage, rounded
  ) %>%
  ungroup()

# Prepare data for export
export_table <- party_ip_share %>%
  select(folketingsaar, group, party_ip_paragraphs, total_ip_paragraphs, party_share) %>%
  arrange(group, folketingsaar)  # Sort by group and year

# Export the table using stargazer
stargazer(
  export_table,
  type = "latex",
  summary = FALSE,
  title = "Party Share of Identity Politics Paragraphs by Folketingsår",
  out = "party_ip_share_table.tex",
  digits = 2,
  rownames = FALSE
)



################################################################################
##############################             #####################################
##############################      H2     #####################################
##############################             #####################################
################################################################################

################## FORSKEL PÅ YDREFLØJ OG CENTRUM #####################

################## DEN GENERELLE DEBAT KODE ################################
# Calculate total paragraphs for each category and year
total_paragraphs_per_category <- D %>%
  group_by(folketingsaar, kategori = case_when(
    group %in% c("EL", "ALT", "FG", "NB", "DD", "DF") ~ "Nichepartier",
    group %in% c("RV", "V", "S", "SF", "LA", "KF", "M") ~ "Mainstreampartier",
    TRUE ~ NA_character_
  )) %>%
  summarise(total_paragraphs = n(), .groups = "drop") %>%
  filter(!is.na(kategori))  # Remove invalid categories

# Calculate IP paragraphs for each category and year
kategori_ip_paragraph_share <- D %>%
  filter(ip == 1) %>%  # Only IP paragraphs
  group_by(folketingsaar, kategori = case_when(
    group %in% c("EL", "ALT", "FG", "NB", "DD", "DF") ~ "Nichepartier",
    group %in% c("RV", "V", "S", "SF", "LA", "KF", "M") ~ "Mainstreampartier",
    TRUE ~ NA_character_
  )) %>%
  summarise(kategori_ip_paragraphs = n(), .groups = "drop") %>%
  left_join(total_paragraphs_per_category, by = c("folketingsaar", "kategori")) %>%
  mutate(
    proportion = (kategori_ip_paragraphs / total_paragraphs) * 100  # Divide by total paragraphs in the category
  )

# Calculate total speeches for each category and year
total_speeches_per_category <- speech_data %>%
  group_by(folketingsaar, kategori = case_when(
    group %in% c("EL", "ALT", "FG", "NB", "DD", "DF") ~ "Nichepartier",
    group %in% c("RV", "V", "S", "SF", "LA", "KF", "M") ~ "Mainstreampartier",
    TRUE ~ NA_character_
  )) %>%
  summarise(total_speeches = n(), .groups = "drop") %>%
  filter(!is.na(kategori))  # Remove invalid categories

# Calculate IP speeches for each category and year
kategori_ip_speech_share <- speech_data %>%
  filter(proportion_ip > 0.75) %>%  # Filter for speeches where IP content > 75%
  group_by(folketingsaar, kategori = case_when(
    group %in% c("EL", "ALT", "FG", "NB", "DD", "DF") ~ "Nichepartier",
    group %in% c("RV", "V", "S", "SF", "LA", "KF", "M") ~ "Mainstreampartier",
    TRUE ~ NA_character_
  )) %>%
  summarise(kategori_ip_speeches = n(), .groups = "drop") %>%
  left_join(total_speeches_per_category, by = c("folketingsaar", "kategori")) %>%
  mutate(
    proportion = (kategori_ip_speeches / total_speeches) * 100  # Divide by total speeches in the category
  )

# Combine datasets
combined_data <- bind_rows(
  kategori_ip_paragraph_share %>% mutate(type = "Afsnit"),
  kategori_ip_speech_share %>% mutate(type = "Taler")
)

# Ensure folketingsaar is a factor
combined_data <- combined_data %>%
  mutate(folketingsaar = factor(folketingsaar, levels = unique(folketingsaar)))

election_years <- data.frame(
  folketingsaar = c("2011/2012", "2015/2016", "2019/2020", "2022/2023"),
  label_position = 18,  # Inden for y-aksens limits
  type = "Afsnit"
) %>%
  mutate(folketingsaar = factor(folketingsaar, levels = levels(combined_data$folketingsaar)))

################## DEN GENERELLE DEBAT plot ################################
# Updated plot with specified layout
plot_combined <- ggplot(combined_data, aes(x = folketingsaar, y = proportion, color = kategori, linetype = type)) +
  geom_line(aes(group = interaction(kategori, type)), size = 1.5) +
  geom_vline(data = election_years, aes(xintercept = folketingsaar), 
             linetype = "dashed", color = "black", size = 0.8) + 
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 5),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  scale_x_discrete() +  # x-axis stays as it is
  labs(
    x = NULL,
    y = "Andel identitetspolitik (%)",
    color = "",
    linetype = ""
  ) +
  scale_color_manual(values = c(
    "Nichepartier" = "darkorange",  # Red
    "Mainstreampartier" = "darkblue"  # Blue
  )) +
  scale_linetype_manual(values = c(
    "Afsnit" = "dotted",  # Dotted line for Afsnit
    "Taler" = "solid"     # Solid line for Taler
  )) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(size = 22, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(t = 40)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

print(plot_combined)

# Save the plot
ggsave("H2_figur1.png", plot_combined, width = 16, height = 8, dpi = 300)

##### TAL FRA TABELLEN ####
# Extract relevant data into a clean table
table_data <- combined_data %>%
  select(folketingsaar, kategori, type, proportion) %>%
  arrange(folketingsaar, kategori, type) %>%
  mutate(proportion = round(proportion, 2))  # Round proportions to 2 decimals

# Preview the table
print(table_data)

# Create a LaTeX table
stargazer(
  table_data,
  summary = FALSE,
  rownames = FALSE,
  title = "Proportion of Identity Politics by Category and Type",
  label = "tab:identity_politics",
  out = "identity_politics_table.tex"
)

################## DEN IDENTITETSPOLITISKE DEBAT KODE ##################
# Calculate total IP paragraphs and speeches per year for normalization
total_ip_paragraphs_per_year <- D %>%
  filter(ip == 1) %>%
  group_by(folketingsaar) %>%
  summarise(total_ip_paragraphs = n(), .groups = "drop")

total_ip_speeches_per_year <- speech_data %>%
  filter(proportion_ip > 0.75) %>%
  group_by(folketingsaar) %>%
  summarise(total_ip_speeches = n(), .groups = "drop")

# Calculate proportion of IP paragraphs by category and year
kategori_ip_paragraph_share <- D %>%
  filter(ip == 1) %>%
  group_by(folketingsaar, kategori = case_when(
    group %in% c("EL", "ALT", "FG", "NB", "DD", "DF") ~ "Nichepartier",
    group %in% c("RV", "V", "S", "SF", "LA", "KF", "M") ~ "Mainstreampartier",
    TRUE ~ NA_character_
  )) %>%
  summarise(kategori_ip_paragraphs = n(), .groups = "drop") %>%
  left_join(total_ip_paragraphs_per_year, by = "folketingsaar") %>%
  mutate(
    proportion = (kategori_ip_paragraphs / total_ip_paragraphs) * 100  # Calculate proportion
  )

# Calculate proportion of IP speeches by category and year
kategori_ip_speech_share <- speech_data %>%
  filter(proportion_ip > 0.75) %>%
  group_by(folketingsaar, kategori = case_when(
    group %in% c("EL", "ALT", "FG", "NB", "DD", "DF") ~ "Nichepartier",
    group %in% c("RV", "V", "S", "SF", "LA", "KF", "M") ~ "Mainstreampartier",
    TRUE ~ NA_character_
  )) %>%
  summarise(kategori_ip_speeches = n(), .groups = "drop") %>%
  left_join(total_ip_speeches_per_year, by = "folketingsaar") %>%
  mutate(
    proportion = (kategori_ip_speeches / total_ip_speeches) * 100  # Calculate proportion
  )

# Combine datasets
combined_data <- bind_rows(
  kategori_ip_paragraph_share %>% mutate(type = "Afsnit"),
  kategori_ip_speech_share %>% mutate(type = "Taler")
)

# Ensure folketingsaar is a factor
combined_data <- combined_data %>%
  mutate(folketingsaar = factor(folketingsaar, levels = unique(folketingsaar)))

# Create election years dataset
election_years <- data.frame(
  folketingsaar = c("2011/2012", "2015/2016", "2019/2020", "2022/2023"),
  label_position = 100  # Adjust label position for election markers
) %>%
  mutate(folketingsaar = factor(folketingsaar, levels = levels(combined_data$folketingsaar)))

################## DEN IDENTITETSPOLITISKE DEBAT PLOT ##################
# Updated plot with different colors and line types
plot_combined <- ggplot(combined_data, aes(x = folketingsaar, y = proportion, color = kategori, linetype = type)) +
  geom_line(aes(group = interaction(kategori, type)), size = 1.5) +
  geom_vline(data = election_years, aes(xintercept = as.numeric(folketingsaar)),
             linetype = "dashed", color = "black", size = 0.8) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = scales::comma,
    expand = c(0, 0)
  )  +
  scale_color_manual(values = c(
    "Nichepartier" = "orange",  # Red for Nichepartier
    "Mainstreampartier" = "darkblue"  # Blue for Mainstreampartier
  )) +
  scale_linetype_manual(values = c(
    "Afsnit" = "dotted",  # Dotted line for Afsnit
    "Taler" = "solid"     # Solid line for Taler
  )) +
  labs(
    x = NULL,
    y = "Andel af den identitetspolitiske debat (%)",
    color = "",
    linetype = ""
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(size = 22, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(t = 20)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


# Save the plot
ggsave("H2_figur2.png", plot_combined, width = 16, height = 8, dpi = 300)


######### TAL FRA FIGUREN #####
# Filter the data for paragraphs (Afsnit)
paragraph_data <- combined_data %>%
  filter(type == "Afsnit") %>%
  select(folketingsaar, kategori, proportion)

# Filter the data for speeches (Taler)
speech_data <- combined_data %>%
  filter(type == "Taler") %>%
  select(folketingsaar, kategori, proportion)

# Convert proportions to decimal format for LaTeX table (optional)
paragraph_data <- paragraph_data %>%
  mutate(proportion = proportion / 100)

speech_data <- speech_data %>%
  mutate(proportion = proportion / 100)

# Create LaTeX table for paragraphs
stargazer(
  as.data.frame(paragraph_data),
  summary = FALSE,
  rownames = FALSE,
  title = "Proportion of IP Content in Paragraphs by Year and Category",
  label = "tab:paragraphs",
  out = "paragraph_data.tex"
)

# Create LaTeX table for speeches
stargazer(
  as.data.frame(speech_data),
  summary = FALSE,
  rownames = FALSE,
  title = "Proportion of IP Content in Speeches by Year and Category",
  label = "tab:speeches",
  out = "speech_data.tex"
)




################################################################################
################################################################################
##############################             #####################################
##############################     H3      #####################################
##############################             #####################################
################################################################################
################################################################################

# Definér partier for højre- og venstrefløjen
right_parties <- c("DF", "NB", "DD")
left_parties <- c("EL", "FG", "ALT") 

############# GENERELLE DEBAT: ANTAL PARAGRAFFER ########
# Beregn IP-andel pr. fløj og år baseret på paragrafniveau i D-datatabellen
fløj_ip_data <- D %>%
  mutate(
    # Kategoriser partier i fløje (højre, venstre)
    fløj = case_when(
      group %in% right_parties ~ "Yder højrefløj",   # Højre fløj
      group %in% left_parties ~ "Yder venstrefløj",  # Venstre fløj
      TRUE ~ NA_character_  # Hvis partiet ikke er med, tildel NA
    )
  ) %>%
  filter(!is.na(fløj)) %>%  # Filtrer kun de partier, der er i fløje
  group_by(folketingsaar, fløj) %>%  # Gruppér efter år og fløj
  summarize(
    total_ip_obs = sum(ip == 1, na.rm = TRUE),  # Summér Afsnit (ip == 1 betyder IP)
    total_obs = n(),  # Totale observationer (alle paragraffer)
    ip_share = total_ip_obs / total_obs  # Beregn andel Afsnit
  ) %>%
  ungroup()  # Fjern gruppering

# Tjek data for at se resultaterne
print(fløj_ip_data)

# T-test på IP-andel (gennemsnit over alle år) mellem højre- og venstrefløj
t_test_result <- t.test(
  ip_share ~ fløj,  # Sammenlign IP-andelen mellem højre- og venstrefløjen
  data = fløj_ip_data,
  alternative = "two.sided",  # Test for forskel mellem grupper
  var.equal = FALSE  # Brug Welch's t-test, hvis variansen ikke er ens
)

# Print resultaterne af t-testen
print(t_test_result)

############ GENERELLE DEBAT: ANTAL TALER #####

# Beregn andel af "High IP" taler pr. folketingsår og fløj
fløj_high_ip_per_folketingsaar <- speech_data %>%
  mutate(
    # Kategoriser fløje
    fløj = case_when(
      group %in% right_parties ~ "Yder højrefløj",   # Højre fløj
      group %in% left_parties ~ "Yder venstrefløj",  # Venstre fløj
      TRUE ~ NA_character_  # Hvis partiet ikke er med, tildel NA
    ),
    # Marker "High Identity Politics" taler
    high_ip = ifelse(proportion_ip > 0.75, 1, 0)
  ) %>%
  filter(!is.na(fløj)) %>%  # Fjern rækker uden fløj
  group_by(folketingsaar, fløj) %>%  # Gruppér efter folketingsår og fløj
  summarize(
    total_taler = n(),  # Total antal taler for fløjen det folketingsår
    high_ip_taler = sum(high_ip, na.rm = TRUE),  # Antal "High IP" taler
    high_ip_share = (high_ip_taler / total_taler) * 100  # Andel "High IP" taler i procent
  ) %>%
  ungroup()

# Tjek resultaterne
print(fløj_high_ip_per_folketingsaar)

# T-test: Sammenlign andel af "High IP" taler mellem fløje
t_test_result <- t.test(
  high_ip_share ~ fløj,  # Sammenlign andelen mellem højre- og venstrefløjen
  data = fløj_high_ip_per_folketingsaar,
  alternative = "two.sided"  # To-sidet test
)

# Print resultatet af t-testen
print(t_test_result)


####### GENERELLE DEBAT: TO TALER I EN GRAF ####
# Add a new column to identify the data source
fløj_ip_data <- fløj_ip_data %>%
  mutate(type = "Afsnit", value = ip_share * 100)

fløj_high_ip_per_folketingsaar <- fløj_high_ip_per_folketingsaar %>%
  mutate(type = "Taler", value = high_ip_share)

# Combine the two datasets
combined_data <- bind_rows(fløj_ip_data, fløj_high_ip_per_folketingsaar)

# Plot the combined data
combined_plot <- ggplot(combined_data, aes(x = folketingsaar, y = value, color = fløj, linetype = type, group = interaction(fløj, type))) +
  geom_line(size = 1.5) +
  geom_vline(xintercept = c("2011/2012", "2015/2016", "2019/2020", "2022/2023"),
             linetype = "dashed", color = "black", size = 0.5) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 100, by = 5),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Andel identitetspolitik (%)",
    color = "",
    linetype = ""
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(size = 22, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(t = 20)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  scale_color_manual(values = c(
    "Yder højrefløj" = "lightblue",
    "Yder venstrefløj" = "lightpink"
  ))


# Save the plot
ggsave("H2.1_ipdebat.png", plot = combined_plot, width = 16, height = 8, dpi = 300)

#### TAL FRA FIGUREN #####
# Split the data into subsets based on type
ip_paragraphs_data <- combined_data %>%
  filter(type == "Afsnit") %>%
  select(folketingsaar, fløj, value) %>%
  mutate(value = round(value, 2)) %>% # Round to two decimals
  rename(
    `Folketingsår` = folketingsaar,
    `Fløj` = fløj,
    `Procentandel (%)` = value
  )

high_ip_taler_data <- combined_data %>%
  filter(type == "Taler") %>%
  select(folketingsaar, fløj, value) %>%
  mutate(value = round(value, 2)) %>% # Round to two decimals
  rename(
    `Folketingsår` = folketingsaar,
    `Fløj` = fløj,
    `Procentandel (%)` = value
  )

# Export tables to LaTeX
stargazer(
  ip_paragraphs_data,
  type = "latex",
  summary = FALSE,
  title = "Procentandel for Afsnit",
  out = "ip_paragraphs_percentage_table.tex",
  digits = 2,
  rownames = FALSE
)

stargazer(
  high_ip_taler_data,
  type = "latex",
  summary = FALSE,
  title = "Procentandel for Taler",
  out = "high_ip_taler_percentage_table.tex",
  digits = 2,
  rownames = FALSE
)





#####

########### IDENTITETSPOLITISKE DEBAT: ANTAL TALER##############
# Beregn fløjenes andel af High IP-taler pr. folketingsår
fløj_high_ip_taler_per_år <- speech_data %>%
  mutate(
    # Kategoriser fløje og Mainstreampartier
    fløj = case_when(
      group %in% right_parties ~ "Yder højrefløj",
      group %in% left_parties ~ "Yder venstrefløj",
      TRUE ~ "midten"  # Alle andre partier betragtes som midten
    )
  ) %>%
  filter(proportion_ip > 0.75) %>%  # Filtrer kun taler med proportion_ip > 75%
  group_by(folketingsaar, fløj) %>%
  summarise(
    high_ip_taler = n(),  # Antal High IP-taler pr. folketingsår pr. kategori
    .groups = "drop"
  ) %>%
  group_by(folketingsaar) %>%
  mutate(
    total_high_ip_taler = sum(high_ip_taler, na.rm = TRUE),  # Totale High IP-taler pr. år
    fløj_high_ip_share = (high_ip_taler / total_high_ip_taler) * 100  # Andel af High IP-taler pr. kategori
  ) %>%
  ungroup()  # Fjern gruppering

# Print den nye dataframe
print(fløj_high_ip_taler_per_år)

# Filtrer data for kun at inkludere højre og venstre fløj
fløj_high_ip_taler_per_år_ttest <- fløj_high_ip_taler_per_år %>%
  filter(fløj %in% c("Yder højrefløj", "Yder venstrefløj"))

# T-test for forskellen i andelen af High IP-taler mellem fløjene
t_test_result <- t.test(
  fløj_high_ip_share ~ fløj,  # Sammenlign High IP-andel mellem højre og venstre fløj
  data = fløj_high_ip_taler_per_år_ttest,
  alternative = "two.sided",  # Test for forskel mellem grupper
  var.equal = FALSE  # Brug Welch's t-test, hvis variansen ikke er ens
)

# Print resultatet af t-testen
print(t_test_result)

# Fjern midten fra data for visualisering
fløj_high_ip_taler_per_år_filtered <- fløj_high_ip_taler_per_år %>%
  filter(fløj %in% c("Yder højrefløj", "Yder venstrefløj"))  # Kun højre og venstre fløj

########## IDENTITETSPOLITISKE DEBAT: ANTAL PARAGRAFFER: YDERFLØJE  ##############

# Beregn andele af IP-debatten for hver fløj
fløj_ip_debat <- D %>%
  mutate(
    # Kategoriser partier i fløje
    fløj = case_when(
      group %in% right_parties ~ "Yder højrefløj",   # Højre fløj
      group %in% left_parties ~ "Yder venstrefløj", # Venstre fløj
      TRUE ~ "midten"                      # Alle andre partier betragtes som "midten"
    )
  ) %>%
  group_by(folketingsaar, fløj) %>%  # Gruppér efter år og fløj
  summarize(
    ip_paragraphs = sum(ip == 1, na.rm = TRUE),  # Antal Afsnit for fløjen
    total_ip_paragraphs = sum(ip == 1, na.rm = TRUE),  # Totale Afsnit
    .groups = "drop"
  ) %>%
  group_by(folketingsaar) %>%  # Gruppér efter år for at beregne samlet andel
  mutate(
    fløj_ip_share = (ip_paragraphs / sum(ip_paragraphs, na.rm = TRUE)) * 100  # Andel af samlet IP-debat
  ) %>%
  ungroup()

# Tjek data
print(fløj_ip_debat)

# Filtrer data for kun at inkludere højre og venstre fløj
fløj_ip_debat_fløje <- fløj_ip_debat %>%
  filter(fløj %in% c("Yder højrefløj", "Yder venstrefløj"))

# T-test for forskellen i andelen af samlet IP-debat mellem fløjene
t_test_result <- t.test(
  fløj_ip_share ~ fløj,  # Sammenlign IP-debattens andel mellem højre og venstre fløj
  data = fløj_ip_debat_fløje,
  alternative = "two.sided"  # Test for forskel mellem grupper
)

# Print resultatet af t-testen
print(t_test_result)


########## IDENTITETSPOLITISKE DEBAT: TO GRAFER I EN #####
# Add a `type` column to differentiate datasets
fløj_high_ip_taler_per_år_filtered <- fløj_high_ip_taler_per_år_filtered %>%
  mutate(type = "Taler", value = fløj_high_ip_share)

fløj_ip_debat_fløje <- fløj_ip_debat_fløje %>%
  mutate(type = "Paragraffer", value = fløj_ip_share)

# Combine the datasets
combined_data <- bind_rows(fløj_high_ip_taler_per_år_filtered, fløj_ip_debat_fløje)

# Create the combined plot
combined_plot <- ggplot(combined_data, aes(x = folketingsaar, y = value, color = fløj, linetype = type, group = interaction(fløj, type))) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_vline(xintercept = c("2011/2012", "2015/2016", "2019/2020", "2022/2023"),
             linetype = "dashed", color = "grey", size = 0.5) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 100, by = 10),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Andel af den identitetspolitiske debat (%)",
    color = "",
    linetype = ""
  ) +
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(size = 22, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, color = "black", margin = margin(t = 20)),
    axis.title.y = element_text(size = 24, color = "black", margin = margin(t = 20)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  scale_color_manual(values = c(
    "Yder højrefløj" = "lightblue",
    "Yder venstrefløj" = "lightpink"
  ))

# Save the plot
ggsave("H2.1.IPdebatsamlet.png", plot = combined_plot, width = 16, height = 8, dpi = 300)


#### TAL FRA FIGUREN #####
# Data for Taler
high_ip_taler_table <- fløj_high_ip_taler_per_år_filtered %>%
  select(folketingsaar, fløj, fløj_high_ip_share) %>%
  mutate(fløj_high_ip_share = round(fløj_high_ip_share, 2)) %>% # Afrunding til 2 decimaler
  rename(
    `Folketingsår` = folketingsaar,
    `Fløj` = fløj,
    `Procentandel (%)` = fløj_high_ip_share
  )

# Data for IP-debat andel
ip_debat_table <- fløj_ip_debat_fløje %>%
  select(folketingsaar, fløj, fløj_ip_share) %>%
  mutate(fløj_ip_share = round(fløj_ip_share, 2)) %>% # Afrunding til 2 decimaler
  rename(
    `Folketingsår` = folketingsaar,
    `Fløj` = fløj,
    `Procentandel (%)` = fløj_ip_share
  )

# Eksportér Taler som LaTeX-tabel
stargazer(
  high_ip_taler_table,
  type = "latex",
  summary = FALSE,
  title = "Procentandel for Taler per Fløj",
  out = "high_ip_taler_table.tex",
  digits = 2,
  rownames = FALSE
)

# Eksportér IP-debat andel som LaTeX-tabel
stargazer(
  ip_debat_table,
  type = "latex",
  summary = FALSE,
  title = "Procentandel for IP-Debat per Fløj",
  out = "ip_debat_table.tex",
  digits = 2,
  rownames = FALSE
)












