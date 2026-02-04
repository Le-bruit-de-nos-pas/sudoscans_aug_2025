

library(readxl)
library(tidyverse)
library(data.table)

# Summaries clinical -------------------------------------------------------------------------------

Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

names(Sudoscan_df)

length(unique(Sudoscan_df$Anonymisation)) # 175

Sudoscan_df %>% group_by(Anonymisation) %>% count() %>%
  ungroup() %>%
  summarise(mean=mean(n), sd=sd(n), median=median(n), q1=quantile(n, 0.25), q3=quantile(n, 0.75))

# mean    sd median    q1    q3
# 2.77  2.10      2     1   3.5

Sudoscan_df %>% filter(!is.na(`date Sudoscan`)) %>% group_by(Anonymisation) %>% count() %>%
  ungroup() %>%
  summarise(mean=mean(n), sd=sd(n), median=median(n), q1=quantile(n, 0.25), q3=quantile(n, 0.75))

# mean    sd median    q1    q3
#  2.03  1.42      1     1     3

Sudoscan_df %>% select(Anonymisation, Genre) %>% distinct() %>%
  group_by(Genre) %>% count() %>% mutate(perc=n/175)

# Genre     n  perc
# 1 F        99 0.566
# 2 M        76 0.434


Sudoscan_df %>% group_by(Anonymisation) %>%
  filter(`date Sudoscan`==max(`date Sudoscan`, na.rm=T)) %>%
  select(Anonymisation, `Diagnostic AMS`) %>% distinct() %>%
  group_by(`Diagnostic AMS`) %>% count() %>% mutate(perc=n/175)

# `Diagnostic AMS`         n  perc
# 1 "AMS-C \"Possible\""     7 0.04 
# 2 "AMS-C \"Probable\""    40 0.229
# 3 "AMS-P \"Possible\""    24 0.137
# 4 "AMS-P \"Probable\""   104 0.594


# 10 July 2025 end of follow-up ?

min(Sudoscan_df$`date 1ère consultation`, na.rm=T) # "2008-07-07 UTC"
max(Sudoscan_df$`date 1ère consultation`, na.rm=T) # "2022-03-28 UTC"

min(Sudoscan_df$`Date décès`, na.rm=T) # "2015-03-27 UTC"
max(Sudoscan_df$`Date décès`, na.rm=T) # "2025-04-13 UTC"

min(Sudoscan_df$`date Sudoscan`, na.rm=T) # "2012-07-31 UTC"
max(Sudoscan_df$`date Sudoscan`, na.rm=T) # "2022-05-16 UTC"


Sudoscan_df %>% filter(!is.na(`Cause fin de suivi`)) %>%
  select(Anonymisation) %>% distinct() %>% count() # 149 death


Sudoscan_df %>% group_by(Anonymisation) %>%
  summarise(first_date=min(`date Sudoscan`, na.rm=T), last_date=max(`date Sudoscan`, na.rm=T)) %>%
  mutate(diff=as.numeric(
    difftime(last_date, 
             first_date, 
             units = "days") / 30.44)
  ) %>%
  summarise(mean=mean(diff), sd=sd(diff), median=median(diff), q1=quantile(diff, 0.25), q3=quantile(diff, 0.75))

# mean    sd median    q1    q3
# 1  15.0  21.0      0     0  27.4

Sudoscan_df %>% group_by(Anonymisation) %>%
  mutate(first_date=min(`date Sudoscan`, na.rm=T)) %>%
  select(Anonymisation, first_date, `Date décès`) %>%
  mutate(end_of_followup=as.Date("2025-07-10")) %>%
  filter(!is.na(`Date décès`)) %>%
  mutate(diff=as.numeric(
    difftime( `Date décès`,  first_date,
              units = "days") / 30.44)
  ) %>% ungroup() %>%
  summarise(mean=mean(diff), sd=sd(diff), median=median(diff), q1=quantile(diff, 0.25), q3=quantile(diff, 0.75))

# mean    sd median    q1    q3
#   1  42.1  26.1   37.2  23.4  56.0

Sudoscan_df %>% group_by(Anonymisation) %>%
  mutate(first_date=min(`date Sudoscan`, na.rm=T)) %>%
  select(Anonymisation, first_date, `Date décès`) %>%
  mutate(end_of_followup=as.Date("2025-07-10")) %>% ungroup() %>%
  anti_join(Sudoscan_df %>% filter(!is.na(`Date décès`)) %>% select(Anonymisation ) %>% distinct()) %>%
  distinct() %>%
  mutate(diff=as.numeric(
    difftime(end_of_followup, 
             first_date, 
             units = "days") / 30.44)
  ) %>% ungroup() %>%
  summarise(mean=mean(diff), sd=sd(diff), median=median(diff), q1=quantile(diff, 0.25), q3=quantile(diff, 0.75))

# mean    sd median    q1    q3
#   1  69.7  26.4   64.9  45.6  94.2


Sudoscan_df %>% mutate(`date 1ère consultation`=as.character(`date 1ère consultation`)) %>%
  mutate(`date 1ère consultation`=as.numeric(str_sub(`date 1ère consultation`, 1, 4))) %>%
  mutate(diseaese_dur=`date 1ère consultation`-`année 1ers symptômes`) %>%
  summarise(mean=mean(diseaese_dur), sd=sd(diseaese_dur), median=median(diseaese_dur), q1=quantile(diseaese_dur, 0.25), q3=quantile(diseaese_dur, 0.75))

# mean    sd median    q1    q3
#   1  4.21  2.09      4     3     6

Sudoscan_df %>% filter(!is.na(`ESC Pieds Moyennes  (µS)`)) %>%
  group_by(Anonymisation) %>% slice(1) %>% ungroup() %>%
  select(`ESC Pieds Moyennes  (µS)`) %>% drop_na() %>%
  summarise(mean=mean(`ESC Pieds Moyennes  (µS)`), sd=sd(`ESC Pieds Moyennes  (µS)`), 
            median=median(`ESC Pieds Moyennes  (µS)`), q1=quantile(`ESC Pieds Moyennes  (µS)`, 0.25), q3=quantile(`ESC Pieds Moyennes  (µS)`, 0.75))
  
# mean    sd median    q1    q3
#   1  57.1  22.1     62  43.5  73.5


Sudoscan_df %>% filter(!is.na(`Assymetrie Moyenne Pieds ( %)`)) %>%
  group_by(Anonymisation) %>% slice(1) %>% ungroup() %>%
  select(`Assymetrie Moyenne Pieds ( %)`) %>% drop_na() %>%
  summarise(mean=mean(`Assymetrie Moyenne Pieds ( %)`), sd=sd(`Assymetrie Moyenne Pieds ( %)`), 
            median=median(`Assymetrie Moyenne Pieds ( %)`), q1=quantile(`Assymetrie Moyenne Pieds ( %)`, 0.25), q3=quantile(`Assymetrie Moyenne Pieds ( %)`, 0.75))

# mean    sd median    q1    q3
#   1  10.7  11.8      7   2.5  15.5

Sudoscan_df %>% filter(!is.na(`ESC Mains Moyennes (µS)`)) %>%
  group_by(Anonymisation) %>% slice(1) %>% ungroup() %>%
  select(`ESC Mains Moyennes (µS)`) %>% drop_na() %>%
  summarise(mean=mean(`ESC Mains Moyennes (µS)`), sd=sd(`ESC Mains Moyennes (µS)`), 
            median=median(`ESC Mains Moyennes (µS)`), q1=quantile(`ESC Mains Moyennes (µS)`, 0.25), q3=quantile(`ESC Mains Moyennes (µS)`, 0.75))

# mean    sd median    q1    q3
#   1  49.0  19.9     50  33.5    64

Sudoscan_df %>% filter(!is.na(`Assymetrie Moyenne des Mains (%)`)) %>%
  group_by(Anonymisation) %>% slice(1) %>% ungroup() %>%
  select(`Assymetrie Moyenne des Mains (%)`) %>% drop_na() %>%
  summarise(mean=mean(`Assymetrie Moyenne des Mains (%)`), sd=sd(`Assymetrie Moyenne des Mains (%)`), 
            median=median(`Assymetrie Moyenne des Mains (%)`), q1=quantile(`Assymetrie Moyenne des Mains (%)`, 0.25), q3=quantile(`Assymetrie Moyenne des Mains (%)`, 0.75))

# mean    sd median    q1    q3
#   1  13.4  13.5     10     4    18



Sudoscan_df %>% mutate(`SCORE COMPASS`=as.numeric(`SCORE COMPASS`)) %>%
  filter(!is.na(`SCORE COMPASS`)) %>%
  group_by(Anonymisation) %>% slice(1) %>% ungroup() %>%
  select(`SCORE COMPASS`) %>% drop_na() %>%
  summarise(mean=mean(`SCORE COMPASS`), sd=sd(`SCORE COMPASS`), 
            median=median(`SCORE COMPASS`), q1=quantile(`SCORE COMPASS`, 0.25), q3=quantile(`SCORE COMPASS`, 0.75))

# mean    sd median    q1    q3
#   1  32.7  16.5   31.8  18.5  47.2

vars <- c(
  "SCORE SCOPA",
  "COMPASS score orthostatisme",
  "COMPASS score vasomoteur",
  "COMPASS score secretomoteur",
  "COMPASS score gastro-intestinal",
  "COMPASS score vessie",
  "COMPASS score pupillomoteur",
  "SCORE COMPASS",
  "UMSARS1 Orthostatique",
  "UMSARS1 TB urinaire",
  "UMSARS1 TB sexuels",
  "UMSARS1 transit",
  "Score UMSARS1",
  "Score UMSARS 2",
  "Score UMSARS 4"
)

summarise_var <- function(var) {
  var_sym <- sym(var)
  
  Sudoscan_df %>%
    mutate(!!var_sym := as.numeric(!!var_sym)) %>%
    filter(!is.na(!!var_sym)) %>%
    group_by(Anonymisation) %>%
    slice(1) %>%
    ungroup() %>%
    select(!!var_sym) %>%
    drop_na() %>%
    summarise(
      variable = var,
      mean = mean(!!var_sym),
      sd = sd(!!var_sym),
      median = median(!!var_sym),
      q1 = quantile(!!var_sym, 0.25),
      q3 = quantile(!!var_sym, 0.75)
    )
}

results <- map_dfr(vars, summarise_var)


# ----------------

# Percent drops PAD SBP HR ------------------

 


plot <- drops_df %>%
  mutate(perc=100*perc) %>%
  mutate(type=ifelse(type=="FC", "Heart Rate",
                     ifelse(type=="PAD", "Diastolic Blood Pressure", "Systolic Blood Pressure"))) %>%
  ggplot(aes(minute, perc, colour=type, fill=type)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Minutes Since Supine") +
  ylab("Percent % Changes \n") +
  scale_colour_manual(values=c( "#539EB0", "#C43B1F","#124A87")) +
  scale_fill_manual(values=c( "#539EB0","#C43B1F", "#124A87")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black")

ggsave(file="drops.svg", plot=plot, width=5, height=5)


drops_df %>% group_by(Anonymisation) %>% 
  filter(`date Sudoscan`==min(`date Sudoscan`)) %>%
  group_by(Anonymisation, `date Sudoscan`, type) %>% filter(perc==min(perc)) %>%
  select(-minute, -value, -first) %>%
  distinct() %>%
  ungroup() %>%
  group_by(type) %>%
  summarise(mean=mean(perc), sd=sd(perc), 
            median=median(perc), q1=quantile(perc, 0.25), q3=quantile(perc, 0.75))



# -------------


# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `Score UMSARS 2` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `Score UMSARS 2`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`Score UMSARS 2`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)

 

test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank Score UMSARS 2` <- rank(test$`Score UMSARS 2`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank Score UMSARS 2`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                  (1  | Anonymisation),
                                data = test)
summary(model_rank_scaled)


res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `Score UMSARS 2`, dataset = test)
res


# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `Score UMSARS1` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `Score UMSARS1`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`Score UMSARS1`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank Score UMSARS1` <- rank(test$`Score UMSARS1`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank Score UMSARS1`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `Score UMSARS1`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `UMSARS1 Orthostatique` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `UMSARS1 Orthostatique`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`UMSARS1 Orthostatique`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank UMSARS1 Orthostatique` <- rank(test$`UMSARS1 Orthostatique`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank UMSARS1 Orthostatique`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `UMSARS1 Orthostatique`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `UMSARS1 Orthostatique` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `UMSARS1 Orthostatique`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`UMSARS1 Orthostatique`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank UMSARS1 Orthostatique` <- rank(test$`UMSARS1 Orthostatique`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank UMSARS1 Orthostatique`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `UMSARS1 Orthostatique`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `UMSARS1 TB urinaire` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `UMSARS1 TB urinaire`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`UMSARS1 TB urinaire`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank UMSARS1 TB urinaire` <- rank(test$`UMSARS1 TB urinaire`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank UMSARS1 TB urinaire`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `UMSARS1 TB urinaire`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `UMSARS1 TB urinaire` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `UMSARS1 TB urinaire`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`UMSARS1 TB urinaire`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank UMSARS1 TB urinaire` <- rank(test$`UMSARS1 TB urinaire`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank UMSARS1 TB urinaire`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `UMSARS1 TB urinaire`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `UMSARS1 TB sexuels` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `UMSARS1 TB sexuels`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`UMSARS1 TB sexuels`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank UMSARS1 TB sexuels` <- rank(test$`UMSARS1 TB sexuels`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank UMSARS1 TB sexuels`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `UMSARS1 TB sexuels`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `UMSARS1 TB sexuels` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `UMSARS1 TB sexuels`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`UMSARS1 TB sexuels`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank UMSARS1 TB sexuels` <- rank(test$`UMSARS1 TB sexuels`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank UMSARS1 TB sexuels`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `UMSARS1 TB sexuels`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `UMSARS1 transit` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `UMSARS1 transit`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`UMSARS1 transit`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank UMSARS1 transit` <- rank(test$`UMSARS1 transit`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank UMSARS1 transit`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `UMSARS1 transit`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `SCORE SCOPA` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `SCORE SCOPA`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`SCORE SCOPA`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank SCORE SCOPA` <- rank(test$`SCORE SCOPA`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank SCORE SCOPA`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `SCORE SCOPA`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `SCORE SCOPA` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `SCORE SCOPA`) %>% drop_na()

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`SCORE SCOPA`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank SCORE SCOPA` <- rank(test$`SCORE SCOPA`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank SCORE SCOPA`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `SCORE SCOPA`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `SCORE COMPASS` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `SCORE COMPASS`) %>% drop_na()
test$`SCORE COMPASS` <- as.numeric(test$`SCORE COMPASS`)

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`SCORE COMPASS`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank SCORE COMPASS` <- rank(test$`SCORE COMPASS`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank SCORE COMPASS`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `SCORE COMPASS`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `SCORE COMPASS` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `SCORE COMPASS`) %>% drop_na()

test$`SCORE COMPASS` <- as.numeric(test$`SCORE COMPASS`)

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`SCORE COMPASS`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank SCORE COMPASS` <- rank(test$`SCORE COMPASS`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank SCORE COMPASS`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `SCORE COMPASS`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `COMPASS score vasomoteur` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `COMPASS score vasomoteur`) %>% drop_na()
test$`COMPASS score vasomoteur` <- as.numeric(test$`COMPASS score vasomoteur`)

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score vasomoteur`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank COMPASS score vasomoteur` <- rank(test$`COMPASS score vasomoteur`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score vasomoteur`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `COMPASS score vasomoteur`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `COMPASS score vasomoteur` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `COMPASS score vasomoteur`) %>% drop_na()

test$`COMPASS score vasomoteur` <- as.numeric(test$`COMPASS score vasomoteur`)

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score vasomoteur`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank COMPASS score vasomoteur` <- rank(test$`COMPASS score vasomoteur`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score vasomoteur`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `COMPASS score vasomoteur`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `COMPASS score gastro-intestinal` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `COMPASS score gastro-intestinal`) %>% drop_na()
test$`COMPASS score gastro-intestinal` <- as.numeric(test$`COMPASS score gastro-intestinal`)

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score gastro-intestinal`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank COMPASS score gastro-intestinal` <- rank(test$`COMPASS score gastro-intestinal`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score gastro-intestinal`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `COMPASS score gastro-intestinal`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `COMPASS score gastro-intestinal` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `COMPASS score gastro-intestinal`) %>% drop_na()

test$`COMPASS score gastro-intestinal` <- as.numeric(test$`COMPASS score gastro-intestinal`)

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score gastro-intestinal`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank COMPASS score gastro-intestinal` <- rank(test$`COMPASS score gastro-intestinal`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score gastro-intestinal`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `COMPASS score gastro-intestinal`, dataset = test)
res

# ------------


# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `COMPASS score secretomoteur` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `COMPASS score secretomoteur`) %>% drop_na()
test$`COMPASS score secretomoteur` <- as.numeric(test$`COMPASS score secretomoteur`)

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score secretomoteur`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank COMPASS score secretomoteur` <- rank(test$`COMPASS score secretomoteur`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score secretomoteur`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `COMPASS score secretomoteur`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `COMPASS score secretomoteur` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `COMPASS score secretomoteur`) %>% drop_na()

test$`COMPASS score secretomoteur` <- as.numeric(test$`COMPASS score secretomoteur`)

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score secretomoteur`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank COMPASS score secretomoteur` <- rank(test$`COMPASS score secretomoteur`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score secretomoteur`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `COMPASS score secretomoteur`, dataset = test)
res

# ------------


# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `COMPASS score vessie` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `COMPASS score vessie`) %>% drop_na()
test$`COMPASS score vessie` <- as.numeric(test$`COMPASS score vessie`)

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score vessie`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank COMPASS score vessie` <- rank(test$`COMPASS score vessie`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score vessie`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `COMPASS score vessie`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `COMPASS score vessie` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `COMPASS score vessie`) %>% drop_na()

test$`COMPASS score vessie` <- as.numeric(test$`COMPASS score vessie`)

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score vessie`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank COMPASS score vessie` <- rank(test$`COMPASS score vessie`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score vessie`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `COMPASS score vessie`, dataset = test)
res

# ------------


# Correlations with SUDOSACAN: `ESC Pieds Moyennes  (µS)`, `COMPASS score pupillomoteur` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `COMPASS score pupillomoteur`) %>% drop_na()
test$`COMPASS score pupillomoteur` <- as.numeric(test$`COMPASS score pupillomoteur`)

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score pupillomoteur`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank COMPASS score pupillomoteur` <- rank(test$`COMPASS score pupillomoteur`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score pupillomoteur`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = `COMPASS score pupillomoteur`, dataset = test)
res

# ------------

# Correlations with SUDOSACAN: `ESC Mains Moyennes (µS)`, `COMPASS score pupillomoteur` -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)
names(Sudoscan_df)
test <- Sudoscan_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, `COMPASS score pupillomoteur`) %>% drop_na()

test$`COMPASS score pupillomoteur` <- as.numeric(test$`COMPASS score pupillomoteur`)

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$`COMPASS score pupillomoteur`, center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank COMPASS score pupillomoteur` <- rank(test$`COMPASS score pupillomoteur`)

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank COMPASS score pupillomoteur`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = `COMPASS score pupillomoteur`, dataset = test)
res

# ------------


# Correlations with SUDOSACAN: Percent drops PAD SBP HR Correlations with ESC ------------------


Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)


names(Sudoscan_df) 

drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 'PAS couche':FCminute10, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`)

drops_df <- drops_df %>%
  pivot_longer(
    cols = `PAS couche`:FCminute10,
    names_to = "measure",
    values_to = "value"
  ) %>%
  # Separate type of measure (PAS, PAD, FC) from time
  mutate(
    minute = case_when(
      grepl("couche", measure) ~ 0,
      TRUE ~ readr::parse_number(measure)
    ),
    type = case_when(
      grepl("^PAS", measure) ~ "PAS",
      grepl("^PAD", measure) ~ "PAD",
      grepl("^FC", measure)  ~ "FC"
    )
  ) %>%
  select(Anonymisation, `date Sudoscan`, type, minute, value, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`)



drops_df <- drops_df %>% drop_na()

drops_df <- drops_df %>%
  left_join(drops_df %>% filter(minute==0) %>% rename("first"="value") %>% select(-minute))

drops_df <- drops_df %>%
  mutate(perc=(value-first)/first)


Supine_PAS <- drops_df %>% filter(minute==0) %>%
  select(Anonymisation, type, value, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`) %>%
  filter(type=="PAS")



test <- Supine_PAS %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, value ) %>% drop_na()
test$value  <- as.numeric(test$value )

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$value , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank value` <- rank(test$value )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = value , dataset = test)
res


test <- Supine_PAS %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, value ) %>% drop_na()

test$value  <- as.numeric(test$value )

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$value , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank value` <- rank(test$value )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = value , dataset = test)
res








Supine_PAD <- drops_df %>% filter(minute==0) %>%
  select(Anonymisation, type, value, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`) %>%
  filter(type=="PAD")



test <- Supine_PAD %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, value ) %>% drop_na()
test$value  <- as.numeric(test$value )

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$value , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank value` <- rank(test$value )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = value , dataset = test)
res






test <- Supine_PAD %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, value ) %>% drop_na()

test$value  <- as.numeric(test$value )

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$value , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank value` <- rank(test$value )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = value , dataset = test)
res



Supine_FC <- drops_df %>% filter(minute==0) %>%
  select(Anonymisation, type, value, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`) %>%
  filter(type=="FC")



test <- Supine_FC %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, value ) %>% drop_na()
test$value  <- as.numeric(test$value )

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$value , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank value` <- rank(test$value )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = value , dataset = test)
res






test <- Supine_FC %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, value ) %>% drop_na()

test$value  <- as.numeric(test$value )

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$value , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank value` <- rank(test$value )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = value , dataset = test)
res



Min_delta_PAS <- drops_df %>% filter(minute!=0) %>%
  group_by(Anonymisation, `date Sudoscan`, type) %>%
  filter(perc==min(perc)) %>% select(-minute) %>%
  distinct() %>%
  filter(type=="PAS")



test <- Min_delta_PAS %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, perc ) %>% drop_na()
test$perc  <- as.numeric(test$perc )

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$perc , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank value` <- rank(test$perc )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = perc , dataset = test)
res






test <- Min_delta_PAS %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, perc ) %>% drop_na()

test$perc  <- as.numeric(test$perc )

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$perc , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank value` <- rank(test$perc )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = perc , dataset = test)
res


Min_delta_PAD <- drops_df %>% filter(minute!=0) %>%
  group_by(Anonymisation, `date Sudoscan`, type) %>%
  filter(perc==min(perc)) %>% select(-minute) %>%
  distinct() %>%
  filter(type=="PAD")



test <- Min_delta_PAD %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, perc ) %>% drop_na()
test$perc  <- as.numeric(test$perc )

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$perc , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank value` <- rank(test$perc )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = perc , dataset = test)
res






test <- Min_delta_PAD %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, perc ) %>% drop_na()

test$perc  <- as.numeric(test$perc )

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$perc , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank value` <- rank(test$perc )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = perc , dataset = test)
res


Min_delta_FC <- drops_df %>% filter(minute!=0) %>%
  group_by(Anonymisation, `date Sudoscan`, type) %>%
  filter(perc==max(perc)) %>% select(-minute) %>%
  distinct() %>%
  filter(type=="FC")



test <- Min_delta_FC %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, perc ) %>% drop_na()
test$perc  <- as.numeric(test$perc )

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$perc , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank value` <- rank(test$perc )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = perc , dataset = test)
res






test <- Min_delta_FC %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, perc ) %>% drop_na()

test$perc  <- as.numeric(test$perc )

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$UMSARS_scaled <- scale(test$perc , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(UMSARS_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank value` <- rank(test$perc )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank value`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = perc , dataset = test)
res





# -------------


# Correlations with SUDOSACAN: Disease/Dx duration --------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)


disease_dur_df <- Sudoscan_df %>% mutate(`date 1ère consultation`=as.character(`date 1ère consultation`)) %>%
  mutate(`date 1ère consultation`=as.numeric(str_sub(`date 1ère consultation`, 1, 4))) %>%
  mutate(diseaese_dur=`date 1ère consultation`-`année 1ers symptômes`) %>%
  select(Anonymisation, diseaese_dur, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`)




test <- disease_dur_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, diseaese_dur ) %>% drop_na()
test$diseaese_dur  <- as.numeric(test$diseaese_dur )

test$ESC_scaled <- scale(test$`ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$diseaese_dur_scaled <- scale(test$diseaese_dur , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(diseaese_dur_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)




test$`Rank ESC Pieds Moyennes  (µS)` <- rank(test$`ESC Pieds Moyennes  (µS)`)
test$`Rank diseaese_dur` <- rank(test$diseaese_dur )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Pieds Moyennes  (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank diseaese_dur`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = diseaese_dur , dataset = test)
res


test <- disease_dur_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, diseaese_dur ) %>% drop_na()

test$diseaese_dur  <- as.numeric(test$diseaese_dur )

test$ESC_scaled <- scale(test$`ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$diseaese_dur_scaled <- scale(test$diseaese_dur , center = TRUE, scale = TRUE)

model_raw_scaled <- lmerTest::lmer(diseaese_dur_scaled ~ ESC_scaled + (1 | Anonymisation), data = test)
summary(model_raw_scaled)


test$`Rank ESC Mains Moyennes (µS)` <- rank(test$`ESC Mains Moyennes (µS)`)
test$`Rank diseaese_dur` <- rank(test$diseaese_dur )

test$Rank_ESC_scaled <- scale(test$`Rank ESC Mains Moyennes (µS)`, center = TRUE, scale = TRUE)
test$Rank_Score_scaled <- scale(test$`Rank diseaese_dur`, center = TRUE, scale = TRUE)

model_rank_scaled <- lmerTest::lmer(Rank_Score_scaled ~ Rank_ESC_scaled + 
                                      (1  | Anonymisation),
                                    data = test)
summary(model_rank_scaled)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = diseaese_dur , dataset = test)
res
# --------

# Drops over time ---------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)


ignore <- Sudoscan_df %>% filter(`date Sudoscan`<`date 1ère consultation`)

Sudoscan_df <- Sudoscan_df %>% 
  mutate(`date Sudoscan`=as.Date(`date Sudoscan`), `date 1ère consultation`=as.Date(`date 1ère consultation`)) %>%
  mutate(elapsed=`date Sudoscan`-`date 1ère consultation`)

drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 
                                   'PAS couche':FCminute10, elapsed)

drops_df <- drops_df %>%
  pivot_longer(
    cols = `PAS couche`:FCminute10,
    names_to = "measure",
    values_to = "value"
  ) %>%
  # Separate type of measure (PAS, PAD, FC) from time
  mutate(
    minute = case_when(
      grepl("couche", measure) ~ 0,
      TRUE ~ readr::parse_number(measure)
    ),
    type = case_when(
      grepl("^PAS", measure) ~ "PAS",
      grepl("^PAD", measure) ~ "PAD",
      grepl("^FC", measure)  ~ "FC"
    )
  ) %>%
  select(Anonymisation, `date Sudoscan`, type, minute, value, elapsed)


drops_df <- drops_df %>% drop_na()

drops_df <- drops_df %>%
  left_join(drops_df %>% filter(minute==0) %>% rename("first"="value") %>% select(-minute))

drops_df <- drops_df %>%
  mutate(perc=(value-first)/first)

drops_df$elapsed <- as.numeric(drops_df$elapsed) / 30.44

plot <- drops_df %>% group_by(Anonymisation, `date Sudoscan`, elapsed, type) %>%
  filter(
    perc == if_else(type != "FC", min(perc), max(perc))
  ) %>%
  select(-minute, -first) %>%
  distinct() %>%
  mutate(perc=100*perc) %>%
  mutate(type=ifelse(type=="PAD", "Diastolic Blood Pressure", 
                     ifelse(type=="FC", "Heart Rate",
                            "Systolic Blood Pressure"))) %>%
  ggplot(aes(elapsed, perc, colour=type, fill=type)) +
  geom_smooth() +
  scale_colour_manual(values=c( "#539EB0", "#C43B1F","#124A87")) +
  scale_fill_manual(values=c( "#539EB0","#C43B1F", "#124A87")) +
  coord_cartesian(ylim=c(-50,50)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number elapsed months since 1st MSA consulation") +
  ylab("% Blood Pressure Drop \n") +
  scale_x_continuous(breaks = seq(-20, 150, by = 20)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black")

ggsave(file="dropsovertime.svg", plot=plot, width=6, height=6)


drops_df <- drops_df %>% group_by(Anonymisation, `date Sudoscan`, elapsed, type) %>%
  filter(
    perc == if_else(type != "FC", min(perc), max(perc))
  ) %>%
  select(-minute, -first) %>%
  distinct() %>%
  mutate(perc=100*perc) 


test <- drops_df %>% ungroup() %>% filter(type=="PAS") %>% select(Anonymisation, perc, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(perc ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)
res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = perc, measure2 = elapsed, dataset = test)
res

test <- drops_df %>% ungroup() %>% filter(type=="PAD") %>% select(Anonymisation, perc, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(perc ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = perc, measure2 = elapsed, dataset = test)
res

test <- drops_df %>% ungroup() %>% filter(type=="FC") %>% select(Anonymisation, perc, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(perc ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)


res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = perc, measure2 = elapsed, dataset = test)
res

# ------------
# ESC Measurements Over Time -----------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

Sudoscan_df <- Sudoscan_df %>% 
  mutate(`date Sudoscan`=as.Date(`date Sudoscan`), `date 1ère consultation`=as.Date(`date 1ère consultation`)) %>%
  mutate(elapsed=`date Sudoscan`-`date 1ère consultation`)

drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 
                                   `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`, elapsed)

drops_df <- drops_df %>% drop_na()

drops_df$elapsed <- as.numeric(drops_df$elapsed) / 30.44

plot <- drops_df %>%
  select(-`date Sudoscan`) %>%
  gather(key, value,`ESC Pieds Moyennes  (µS)`:`ESC Mains Moyennes (µS)`) %>%
  mutate(key=ifelse(key=="ESC Pieds Moyennes  (µS)", "ESC Feet (µS)", "ESC Hands (µS)")) %>%
  ggplot(aes(elapsed, value, colour=key, fill=key)) +
  geom_smooth() +
  scale_colour_manual(values=c(  "#C43B1F","#124A87")) +
  scale_fill_manual(values=c( "#C43B1F", "#124A87")) +
  # coord_cartesian(ylim=c(-50,50)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number elapsed months since 1st MSA consulation") +
  ylab("ESC (µS) \n") +
  scale_x_continuous(breaks = seq(-20, 150, by = 20)) 

ggsave(file="ESCovertime.svg", plot=plot, width=5, height=5)




test <- drops_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(`ESC Pieds Moyennes  (µS)` ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Pieds Moyennes  (µS)`, measure2 = elapsed, dataset = test)
res

test <- drops_df %>% select(Anonymisation, `ESC Mains Moyennes (µS)`, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(`ESC Mains Moyennes (µS)` ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `ESC Mains Moyennes (µS)`, measure2 = elapsed, dataset = test)
res
# -------------
# UMSARS Measurements Over Time -----------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

Sudoscan_df <- Sudoscan_df %>% 
  mutate(`date Sudoscan`=as.Date(`date Sudoscan`), `date 1ère consultation`=as.Date(`date 1ère consultation`)) %>%
  mutate(elapsed=`date Sudoscan`-`date 1ère consultation`)

drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 
                                   `Score UMSARS1`, `Score UMSARS 2`, elapsed)

drops_df <- drops_df %>% drop_na()

drops_df$elapsed <- as.numeric(drops_df$elapsed) / 30.44

plot <- drops_df %>%
  select(-`date Sudoscan`) %>%
  gather(key, value,`Score UMSARS1`:`Score UMSARS 2`) %>%
  mutate(key=ifelse(key=="Score UMSARS1", "UMSARS 1", "UMSARS 2")) %>%
  ggplot(aes(elapsed, value, colour=key, fill=key)) +
  geom_smooth() +
  scale_colour_manual(values=c(  "#C43B1F","#124A87")) +
  scale_fill_manual(values=c( "#C43B1F", "#124A87")) +
 coord_cartesian(ylim=c(0,50)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number elapsed months since 1st MSA consulation") +
  ylab("UMSARS Scores\n") +
  scale_x_continuous(breaks = seq(-20, 150, by = 20)) 

ggsave(file="umsarsovertime.svg", plot=plot, width=5, height=5)





test <- drops_df %>% select(Anonymisation, `Score UMSARS1`, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(`Score UMSARS1` ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `Score UMSARS1`, measure2 = elapsed, dataset = test)
res

test <- drops_df %>% select(Anonymisation, `Score UMSARS 2`, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(`Score UMSARS 2` ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `Score UMSARS 2`, measure2 = elapsed, dataset = test)
res



# --------------
# COMPASS / SCOPA Measurements Over Time -----------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

Sudoscan_df <- Sudoscan_df %>% 
  mutate(`date Sudoscan`=as.Date(`date Sudoscan`), `date 1ère consultation`=as.Date(`date 1ère consultation`)) %>%
  mutate(elapsed=`date Sudoscan`-`date 1ère consultation`)

drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 
                                   `SCORE SCOPA`, `SCORE COMPASS`, elapsed)

drops_df <- drops_df %>% drop_na()

drops_df$elapsed <- as.numeric(drops_df$elapsed) / 30.44

plot <- drops_df %>%
  select(-`date Sudoscan`) %>%
  mutate(`SCORE SCOPA`=as.numeric(`SCORE SCOPA`)) %>%
  mutate(`SCORE COMPASS`=as.numeric(`SCORE COMPASS`)) %>%
  gather(key, value,`SCORE SCOPA`:`SCORE COMPASS`) %>%
  mutate(key=ifelse(key=="SCORE SCOPA", "SCOPA", "COMPASS")) %>%
  ggplot(aes(elapsed, value, colour=key, fill=key)) +
  geom_smooth() +
  scale_colour_manual(values=c(  "#C43B1F","#124A87")) +
  scale_fill_manual(values=c( "#C43B1F", "#124A87")) +
  coord_cartesian(ylim=c(0,70)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number elapsed months since 1st MSA consulation") +
  ylab("COMPASS / SCOPA Scores\n") +
  scale_x_continuous(breaks = seq(-20, 150, by = 20)) 

ggsave(file="compassovertime.svg", plot=plot, width=5, height=5)



drops_df <- drops_df %>%
  mutate(`SCORE SCOPA`=as.numeric(`SCORE SCOPA`)) %>%
  mutate(`SCORE COMPASS`=as.numeric(`SCORE COMPASS`)) 

test <- drops_df %>% select(Anonymisation, `SCORE SCOPA`, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(`SCORE SCOPA` ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `SCORE SCOPA`, measure2 = elapsed, dataset = test)
res

test <- drops_df %>% select(Anonymisation, `SCORE COMPASS`, elapsed) %>% drop_na()
test <- test %>% mutate(elapsed=elapsed/12)
model_raw <- lmerTest::lmer(`SCORE COMPASS` ~ elapsed + (1 | Anonymisation), data = test)
summary(model_raw)

res <- rmcorr::rmcorr(participant = "Anonymisation", measure1 = `SCORE COMPASS`, measure2 = elapsed, dataset = test)
res



# --------------
# ESC vs Drops predicting UMSARS 1 ---------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)


drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 
                                   'PAS couche':FCminute10, `Score UMSARS1`, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`)

drops_df <- drops_df %>%
  pivot_longer(
    cols = `PAS couche`:FCminute10,
    names_to = "measure",
    values_to = "value"
  ) %>%
  # Separate type of measure (PAS, PAD, FC) from time
  mutate(
    minute = case_when(
      grepl("couche", measure) ~ 0,
      TRUE ~ readr::parse_number(measure)
    ),
    type = case_when(
      grepl("^PAS", measure) ~ "PAS",
      grepl("^PAD", measure) ~ "PAD",
      grepl("^FC", measure)  ~ "FC"
    )
  ) %>%
  select(Anonymisation, `date Sudoscan`, type, minute, value,  `Score UMSARS1`, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`)


drops_df <- drops_df %>% drop_na()

drops_df <- drops_df %>%
  left_join(drops_df %>% filter(minute==0) %>% rename("first"="value") %>% select(-minute))

drops_df <- drops_df %>%
  mutate(perc=(value-first)/first)


test_drops_umsars_1 <- drops_df %>% group_by(Anonymisation, `date Sudoscan`, type) %>%
  filter(
    perc == if_else(type != "FC", min(perc), max(perc))
  ) %>%
  select(-minute, -first) %>%
  distinct() %>%
  mutate(perc=100*perc) %>% ungroup() %>%
  select( -value) %>% distinct() %>%
  filter(type!="FC") %>%
  spread(key=type, value=perc)


library(lme4)
library(lmerTest)
library(MuMIn)

test_drops_umsars_1 <- test_drops_umsars_1 %>% drop_na()


# Model 1: ESC Hands + ESC Feet
model_ESC <- lmer(`Score UMSARS1` ~ `ESC Pieds Moyennes  (µS)` + `ESC Mains Moyennes (µS)` +
                    (1 | Anonymisation),
                  data = test_drops_umsars_1)

# Model 2: PAS + PAD
model_BP <- lmer(`Score UMSARS1` ~ PAS + PAD +
                   (1 | Anonymisation),
                 data = test_drops_umsars_1)

# Get marginal (fixed effects) and conditional (fixed + random) R²
r2_ESC <- r.squaredGLMM(model_ESC)
r2_BP  <- r.squaredGLMM(model_BP)

r2_ESC
r2_BP

# Optional: Compare models side-by-side
summary(model_ESC)
summary(model_BP)


# Combined model
model_combined <- lmer(`Score UMSARS1` ~ `ESC Pieds Moyennes  (µS)` + `ESC Mains Moyennes (µS)` +
                         PAS + PAD +
                         (1 | Anonymisation),
                       data = test_drops_umsars_1)

anova(model_BP, model_combined)  # does adding ESC improve BP?
anova(model_ESC, model_combined) # does adding BP improve ESC?



# Generate predictions for each model
test_drops_umsars_1 <- test_drops_umsars_1 %>%
  mutate(pred_ESC = predict(model_ESC),
         pred_BP  = predict(model_BP))

# Reshape for plotting
plot_data <- test_drops_umsars_1 %>%
  select(`Score UMSARS1`, pred_ESC, pred_BP) %>%
  pivot_longer(cols = starts_with("pred_"),
               names_to = "Model",
               values_to = "Predicted") %>%
  mutate(Model = recode(Model,
                        pred_ESC = "ESC (Hands + Feet)",
                        pred_BP  = "Blood Pressure (SBP + DBP)"))


plot <- ggplot(plot_data, aes(x = Predicted, y = `Score UMSARS1`, colour=Model, fill=Model)) +
  geom_jitter(alpha = 0.6,  shape=1, stroke=1) +
  geom_smooth(method = "lm", se = TRUE ) +
  scale_fill_manual(values=c("#124A87","#C43B1F" )) +
  scale_colour_manual(values=c("#124A87","#C43B1F" )) +
  labs(x = "\n Predicted UMSARS 1 Score",
       y = "Observed UMSARS 1 Score \n") +
  coord_cartesian(xlim=c(0,50), ylim =c(10,50)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))



ggsave(file="dropsvsesc_umsars1.svg", plot=plot, width=8, height=5)


library(caret)  # for createFolds, train/test partition

set.seed(123)

# Prepare data
data_cv <- test_drops_umsars_1

# Get unique patients for grouped CV
unique_ids <- unique(data_cv$Anonymisation)

# Create 10 folds of patient IDs
folds <- createFolds(unique_ids, k = 10, list = TRUE, returnTrain = FALSE)

# Function to run CV for a given formula
run_cv <- function(formula, data, folds) {
  results <- data.frame(RMSE = numeric(), R2 = numeric())
  
  for (i in seq_along(folds)) {
    test_ids <- unique_ids[folds[[i]]]
    
    train_data <- data %>% filter(!Anonymisation %in% test_ids)
    test_data  <- data %>% filter(Anonymisation %in% test_ids)
    
    model <- lmer(formula, data = train_data)
    
    preds <- predict(model, newdata = test_data, allow.new.levels = TRUE)
    obs   <- test_data$`Score UMSARS1`
    
    rmse <- sqrt(mean((preds - obs)^2))
    r2   <- cor(preds, obs)^2
    
    results <- rbind(results, data.frame(RMSE = rmse, R2 = r2))
  }
  
  return(results)
}

# Run CV for each model
cv_ESC <- run_cv(`Score UMSARS1` ~ `ESC Pieds Moyennes  (µS)` + `ESC Mains Moyennes (µS)` + (1 | Anonymisation),
                 data_cv, folds)

cv_BP <- run_cv(`Score UMSARS1` ~ PAS + PAD + (1 | Anonymisation),
                data_cv, folds)

cv_combined <- run_cv(`Score UMSARS1` ~ `ESC Pieds Moyennes  (µS)` + `ESC Mains Moyennes (µS)` +
                        PAS + PAD + (1 | Anonymisation),
                      data_cv, folds)

# Summarize results
summary_cv <- tibble(
  Model = c("ESC", "BP", "Combined"),
  Mean_RMSE = c(mean(cv_ESC$RMSE), mean(cv_BP$RMSE), mean(cv_combined$RMSE)),
  Mean_R2   = c(mean(cv_ESC$R2),   mean(cv_BP$R2),   mean(cv_combined$R2))
)

print(summary_cv)


# ------------
# ESC vs Drops predicting UMSARS 2 ---------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)


drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 
                                   'PAS couche':FCminute10, `Score UMSARS 2`, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`)

drops_df <- drops_df %>%
  pivot_longer(
    cols = `PAS couche`:FCminute10,
    names_to = "measure",
    values_to = "value"
  ) %>%
  # Separate type of measure (PAS, PAD, FC) from time
  mutate(
    minute = case_when(
      grepl("couche", measure) ~ 0,
      TRUE ~ readr::parse_number(measure)
    ),
    type = case_when(
      grepl("^PAS", measure) ~ "PAS",
      grepl("^PAD", measure) ~ "PAD",
      grepl("^FC", measure)  ~ "FC"
    )
  ) %>%
  select(Anonymisation, `date Sudoscan`, type, minute, value,  `Score UMSARS 2`, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`)


drops_df <- drops_df %>% drop_na()

drops_df <- drops_df %>%
  left_join(drops_df %>% filter(minute==0) %>% rename("first"="value") %>% select(-minute))

drops_df <- drops_df %>%
  mutate(perc=(value-first)/first)


test_drops_umsars_2 <- drops_df %>% group_by(Anonymisation, `date Sudoscan`, type) %>%
  filter(
    perc == if_else(type != "FC", min(perc), max(perc))
  ) %>%
  select(-minute, -first) %>%
  distinct() %>%
  mutate(perc=100*perc) %>% ungroup() %>%
  select( -value) %>% distinct() %>%
  filter(type!="FC") %>%
  spread(key=type, value=perc)


library(lme4)
library(lmerTest)
library(MuMIn)

test_drops_umsars_2 <- test_drops_umsars_2 %>% drop_na()


# Model 1: ESC Hands + ESC Feet
model_ESC <- lmer(`Score UMSARS 2` ~ `ESC Pieds Moyennes  (µS)` + `ESC Mains Moyennes (µS)` +
                    (1 | Anonymisation),
                  data = test_drops_umsars_2)

# Model 2: PAS + PAD
model_BP <- lmer(`Score UMSARS 2` ~ PAS + PAD +
                   (1 | Anonymisation),
                 data = test_drops_umsars_2)

# Get marginal (fixed effects) and conditional (fixed + random) R²
r2_ESC <- r.squaredGLMM(model_ESC)
r2_BP  <- r.squaredGLMM(model_BP)

r2_ESC
r2_BP

# Optional: Compare models side-by-side
summary(model_ESC)
summary(model_BP)


# Combined model
model_combined <- lmer(`Score UMSARS 2` ~ `ESC Pieds Moyennes  (µS)` + `ESC Mains Moyennes (µS)` +
                         PAS + PAD +
                         (1 | Anonymisation),
                       data = test_drops_umsars_2)

anova(model_BP, model_combined)  # does adding ESC improve BP?
anova(model_ESC, model_combined) # does adding BP improve ESC?



# Generate predictions for each model
test_drops_umsars_2 <- test_drops_umsars_2 %>%
  mutate(pred_ESC = predict(model_ESC),
         pred_BP  = predict(model_BP))

# Reshape for plotting
plot_data <- test_drops_umsars_2 %>%
  select(`Score UMSARS 2`, pred_ESC, pred_BP) %>%
  pivot_longer(cols = starts_with("pred_"),
               names_to = "Model",
               values_to = "Predicted") %>%
  mutate(Model = recode(Model,
                        pred_ESC = "ESC (Hands + Feet)",
                        pred_BP  = "Blood Pressure (SBP + DBP)"))


plot <- ggplot(plot_data, aes(x = Predicted, y = `Score UMSARS 2`, colour=Model, fill=Model)) +
  geom_jitter(alpha = 0.6,  shape=1, stroke=1) +
  geom_smooth(method = "lm", se = TRUE ) +
  scale_fill_manual(values=c("#124A87","#C43B1F" )) +
  scale_colour_manual(values=c("#124A87","#C43B1F" )) +
  labs(x = "\n Predicted UMSARS 2 Score",
       y = "Observed UMSARS 2 Score \n") +
  coord_cartesian(xlim=c(10,50), ylim =c(10,50)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))



ggsave(file="dropsvsesc_umsars2.svg", plot=plot, width=8, height=5)




library(caret)  # for createFolds, train/test partition

set.seed(123)

# Prepare data
data_cv <- test_drops_umsars_2

# Get unique patients for grouped CV
unique_ids <- unique(data_cv$Anonymisation)

# Create 10 folds of patient IDs
folds <- createFolds(unique_ids, k = 10, list = TRUE, returnTrain = FALSE)

# Function to run CV for a given formula
run_cv <- function(formula, data, folds) {
  results <- data.frame(RMSE = numeric(), R2 = numeric())
  
  for (i in seq_along(folds)) {
    test_ids <- unique_ids[folds[[i]]]
    
    train_data <- data %>% filter(!Anonymisation %in% test_ids)
    test_data  <- data %>% filter(Anonymisation %in% test_ids)
    
    model <- lmer(formula, data = train_data)
    
    preds <- predict(model, newdata = test_data, allow.new.levels = TRUE)
    obs   <- test_data$`Score UMSARS 2`
    
    rmse <- sqrt(mean((preds - obs)^2))
    r2   <- cor(preds, obs)^2
    
    results <- rbind(results, data.frame(RMSE = rmse, R2 = r2))
  }
  
  return(results)
}

# Run CV for each model
cv_ESC <- run_cv(`Score UMSARS 2` ~ `ESC Pieds Moyennes  (µS)` + `ESC Mains Moyennes (µS)` + (1 | Anonymisation),
                 data_cv, folds)

cv_BP <- run_cv(`Score UMSARS 2` ~ PAS + PAD + (1 | Anonymisation),
                data_cv, folds)

cv_combined <- run_cv(`Score UMSARS 2` ~ `ESC Pieds Moyennes  (µS)` + `ESC Mains Moyennes (µS)` +
                        PAS + PAD + (1 | Anonymisation),
                      data_cv, folds)

# Summarize results
summary_cv <- tibble(
  Model = c("ESC", "BP", "Combined"),
  Mean_RMSE = c(mean(cv_ESC$RMSE), mean(cv_BP$RMSE), mean(cv_combined$RMSE)),
  Mean_R2   = c(mean(cv_ESC$R2),   mean(cv_BP$R2),   mean(cv_combined$R2))
)

print(summary_cv)
# ------------
# Plot ESC vs  UMSARS  -------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`, `Score UMSARS 2`, `Score UMSARS1`) %>% drop_na()


plot <- test %>%
  ggplot(aes(`Score UMSARS 2`, `ESC Pieds Moyennes  (µS)`)) +
  geom_jitter(colour="#C43B1F", fill="#C43B1F", shape=1, stroke=1) +
  geom_smooth(colour="#124A87", fill="#124A87") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n UMSARS 2") +
  ylab("ESC Feet (µS)\n") 

ggsave(file="ESCfeetumsars2.svg", plot=plot, width=5, height=5)





plot <- test %>%
  ggplot(aes(`Score UMSARS1`, `ESC Pieds Moyennes  (µS)`)) +
  geom_jitter(colour="#C43B1F", fill="#C43B1F", shape=1, stroke=1) +
  geom_smooth(colour="#124A87", fill="#124A87") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n UMSARS 1") +
  ylab("ESC Feet (µS)\n") 

ggsave(file="ESCfeetumsars1.svg", plot=plot, width=5, height=5)





plot <- test %>%
  ggplot(aes(`Score UMSARS 2`, `ESC Mains Moyennes (µS)`)) +
  geom_jitter(colour="#C43B1F", fill="#C43B1F", shape=1, stroke=1) +
  geom_smooth(colour="#124A87", fill="#124A87") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n UMSARS 2") +
  ylab("ESC Hands (µS)\n") 

ggsave(file="ESChandsumsars2.svg", plot=plot, width=5, height=5)





plot <- test %>%
  ggplot(aes(`Score UMSARS1`, `ESC Mains Moyennes (µS)`)) +
  geom_jitter(colour="#C43B1F", fill="#C43B1F", shape=1, stroke=1) +
  geom_smooth(colour="#124A87", fill="#124A87") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n UMSARS 1") +
  ylab("ESC Hands (µS)\n") 

ggsave(file="ESChandsumsars1.svg", plot=plot, width=5, height=5)
# ------------

# ESC vs Drops predicting UMSARS  ---------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)


drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 
                                   'PAS couche':FCminute10, `Score UMSARS1`, `Score UMSARS 2`)

drops_df <- drops_df %>%
  pivot_longer(
    cols = `PAS couche`:FCminute10,
    names_to = "measure",
    values_to = "value"
  ) %>%
  # Separate type of measure (PAS, PAD, FC) from time
  mutate(
    minute = case_when(
      grepl("couche", measure) ~ 0,
      TRUE ~ readr::parse_number(measure)
    ),
    type = case_when(
      grepl("^PAS", measure) ~ "PAS",
      grepl("^PAD", measure) ~ "PAD",
      grepl("^FC", measure)  ~ "FC"
    )
  ) %>%
  select(Anonymisation, `date Sudoscan`, type, minute, value,  `Score UMSARS1`, `Score UMSARS 2`)


drops_df <- drops_df %>% drop_na()

drops_df <- drops_df %>%
  left_join(drops_df %>% filter(minute==0) %>% rename("first"="value") %>% select(-minute))

drops_df <- drops_df %>%
  mutate(perc=(value-first)/first)


test_drops_umsars <- drops_df %>% group_by(Anonymisation, `date Sudoscan`, type) %>%
  filter(
    perc == if_else(type != "FC", min(perc), max(perc))
  ) %>%
  select(-minute, -first) %>%
  distinct() %>%
  mutate(perc=100*perc) %>% ungroup() %>%
  select( -value) %>% distinct() %>%
  filter(type!="FC") %>%
  spread(key=type, value=perc)


plot <- test_drops_umsars %>%
  ggplot(aes(`Score UMSARS 2`, PAD)) +
  geom_jitter(colour="#C43B1F", fill="#C43B1F", shape=1, stroke=1) +
  geom_smooth(colour="#124A87", fill="#124A87") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n UMSARS 2") +
  ylab("DBP % Drop\n") 

ggsave(file="DBPumsars2.svg", plot=plot, width=5, height=5)



plot <- test_drops_umsars %>%
  ggplot(aes(`Score UMSARS1`, PAD)) +
  geom_jitter(colour="#C43B1F", fill="#C43B1F", shape=1, stroke=1) +
  geom_smooth(colour="#124A87", fill="#124A87") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n UMSARS 1") +
  ylab("DBP % Drop\n") 

ggsave(file="DBPumsars1.svg", plot=plot, width=5, height=5)






plot <- test_drops_umsars %>%
  ggplot(aes(`Score UMSARS 2`, PAS)) +
  geom_jitter(colour="#C43B1F", fill="#C43B1F", shape=1, stroke=1) +
  geom_smooth(colour="#124A87", fill="#124A87") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n UMSARS 2") +
  ylab("SBP % Drop\n") 

ggsave(file="SBPumsars2.svg", plot=plot, width=5, height=5)



plot <- test_drops_umsars %>%
  ggplot(aes(`Score UMSARS1`, PAS)) +
  geom_jitter(colour="#C43B1F", fill="#C43B1F", shape=1, stroke=1) +
  geom_smooth(colour="#124A87", fill="#124A87") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n UMSARS 1") +
  ylab("SBP % Drop\n") 

ggsave(file="SBPumsars1.svg", plot=plot, width=5, height=5)


# ------------
# Survival analysis ESC --------------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

mortality_df <- Sudoscan_df %>% select(Anonymisation, `date 1ère consultation`, `date Sudoscan`, `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`,
                                       `Cause fin de suivi`, `Date décès` ) 

mortality_df$`date 1ère consultation` <- as.Date(mortality_df$`date 1ère consultation`)
mortality_df$`date Sudoscan` <- as.Date(mortality_df$`date Sudoscan`)
mortality_df$`Date décès` <- as.Date(mortality_df$`Date décès`)

survival_curve <- mortality_df %>% group_by(Anonymisation) %>%
select(Anonymisation, `date 1ère consultation`)  %>%
  distinct()  %>% filter(`date 1ère consultation`==min(`date 1ère consultation`)) %>%
  distinct() %>%
  left_join(
    mortality_df %>% group_by(Anonymisation) %>%
      select(Anonymisation, `Date décès`)  %>%
      distinct()  %>% filter(`Date décès`==min(`Date décès`, na.rm=T)) %>%
      distinct()
  ) %>%
  mutate(died=ifelse(is.na(`Date décès`),0,1)) %>%
  mutate(final=ifelse(is.na(`Date décès`), as.Date("2025-07-10"), as.Date(`Date décès`) )) %>%
  mutate(first=as.numeric(`date 1ère consultation`)) %>%
  mutate(elapsed=final-first) %>%
  select(Anonymisation, died, elapsed)


survival_curve$elapsed <- survival_curve$elapsed/30.44

library(survival)
library(survminer)

# Create the survival object
surv_obj <- Surv(time = survival_curve$elapsed,
                 event = survival_curve$died)

# Fit KM curve
km_fit <- survfit(surv_obj ~ 1)


km_fit


#        n events median 0.95LCL 0.95UCL
# [1,] 175    149   46.2    40.5    52.2


# Plot
surv_plot <- ggsurvplot(km_fit,
           data = survival_curve,
           conf.int = TRUE,
           palette = "#124A87",
           surv.median.line="hv",
           test.for.trend = TRUE,
           cumevents = TRUE,
           cumcensor = TRUE,
           xlab = "\n Months from 1st consultation",
           ylab = "Survival probability \n",
           risk.table = TRUE,
           tables.height = 0.15,
           ggtheme = theme_minimal())



# Make sure all date columns are Date objects
mortality_clean <- mortality_df %>%
  mutate(across(
    c(`date 1ère consultation`, `date Sudoscan`, `Date décès`),
    as.Date
  ))

# Get death date per patient
mortality_clean <- mortality_clean %>%
  group_by(Anonymisation) %>%
  mutate(death_date = suppressWarnings(min(`Date décès`[`Cause fin de suivi` == "Décès"], na.rm = TRUE))) %>%
  mutate(death_date = ifelse(is.infinite(death_date), NA, death_date)) %>%
  ungroup()

# Convert back to Date (ifelse above can make it lose Date class)
mortality_clean <- mortality_clean %>%
  mutate(death_date = as.Date(death_date, origin = "1970-01-01"))

esc_visits <- mortality_clean %>%
  filter(!is.na(`date Sudoscan`)) %>%
  select(Anonymisation, 
         date_first = `date 1ère consultation`,
         date_sudoscan = `date Sudoscan`,
         esc_feet = `ESC Pieds Moyennes  (µS)`,
         esc_hands = `ESC Mains Moyennes (µS)`,
         death_date)


censor_date <- as.Date("2025-07-10")

esc_visits <- esc_visits %>%
  mutate(end_followup = ifelse(!is.na(death_date), death_date, censor_date),
         end_followup = as.Date(end_followup, origin = "1970-01-01"))



tv_data <- esc_visits %>%
  arrange(Anonymisation, date_sudoscan) %>%
  group_by(Anonymisation) %>%
  mutate(start_time = as.numeric(date_sudoscan - min(date_first)),
         stop_time  = lead(start_time, default = as.numeric(end_followup[1] - min(date_first))),
         died       = ifelse(!is.na(death_date) & 
                             stop_time == as.numeric(death_date - min(date_first)), 1, 0)) %>%
  ungroup()



tv_data <- tv_data %>%
  mutate(esc_feet_10  = esc_feet / 10,
         esc_hands_10 = esc_hands / 10)

cox_tv_10 <- coxph(Surv(start_time, stop_time, died) ~ esc_feet_10  + 
                     cluster(Anonymisation),
                   data = tv_data)

summary(cox_tv_10)

cox_tv_10 <- coxph(Surv(start_time, stop_time, died) ~ esc_hands_10  + 
                     cluster(Anonymisation),
                   data = tv_data)

summary(cox_tv_10)




cox_feet_nocluster <- coxph(Surv(start_time, stop_time, died) ~ esc_feet_10,
                            data = tv_data)

ggforest(cox_feet_nocluster, data = tv_data,
         main = "Hazard Ratios for ESC Feet (per 10-point increase)",
         fontsize = 1)



cox_hands_nocluster <- coxph(Surv(start_time, stop_time, died) ~ esc_hands_10,
                            data = tv_data)

ggforest(cox_hands_nocluster, data = tv_data,
         main = "Hazard Ratios for ESC Hands (per 10-point increase)",
         fontsize = 1)











# --------------------
# Survival analysis UMSARS --------------------------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

mortality_df <- Sudoscan_df %>% select(Anonymisation, `date 1ère consultation`, `date Sudoscan`, `Score UMSARS1`, `Score UMSARS 2`,
                                       `Cause fin de suivi`, `Date décès` ) 

mortality_df$`date 1ère consultation` <- as.Date(mortality_df$`date 1ère consultation`)
mortality_df$`date Sudoscan` <- as.Date(mortality_df$`date Sudoscan`)
mortality_df$`Date décès` <- as.Date(mortality_df$`Date décès`)

survival_curve <- mortality_df %>% group_by(Anonymisation) %>%
select(Anonymisation, `date 1ère consultation`)  %>%
  distinct()  %>% filter(`date 1ère consultation`==min(`date 1ère consultation`)) %>%
  distinct() %>%
  left_join(
    mortality_df %>% group_by(Anonymisation) %>%
      select(Anonymisation, `Date décès`)  %>%
      distinct()  %>% filter(`Date décès`==min(`Date décès`, na.rm=T)) %>%
      distinct()
  ) %>%
  mutate(died=ifelse(is.na(`Date décès`),0,1)) %>%
  mutate(final=ifelse(is.na(`Date décès`), as.Date("2025-07-10"), as.Date(`Date décès`) )) %>%
  mutate(first=as.numeric(`date 1ère consultation`)) %>%
  mutate(elapsed=final-first) %>%
  select(Anonymisation, died, elapsed)


survival_curve$elapsed <- survival_curve$elapsed/30.44

library(survival)
library(survminer)

# Create the survival object
surv_obj <- Surv(time = survival_curve$elapsed,
                 event = survival_curve$died)

# Fit KM curve
km_fit <- survfit(surv_obj ~ 1)


km_fit


#        n events median 0.95LCL 0.95UCL
# [1,] 175    149   46.2    40.5    52.2


# Plot
surv_plot <- ggsurvplot(km_fit,
           data = survival_curve,
           conf.int = TRUE,
           palette = "#124A87",
           surv.median.line="hv",
           test.for.trend = TRUE,
           cumevents = TRUE,
           cumcensor = TRUE,
           xlab = "\n Months from 1st consultation",
           ylab = "Survival probability \n",
           risk.table = TRUE,
           tables.height = 0.15,
           ggtheme = theme_minimal())



# Make sure all date columns are Date objects
mortality_clean <- mortality_df %>%
  mutate(across(
    c(`date 1ère consultation`, `date Sudoscan`, `Date décès`),
    as.Date
  ))

# Get death date per patient
mortality_clean <- mortality_clean %>%
  group_by(Anonymisation) %>%
  mutate(death_date = suppressWarnings(min(`Date décès`[`Cause fin de suivi` == "Décès"], na.rm = TRUE))) %>%
  mutate(death_date = ifelse(is.infinite(death_date), NA, death_date)) %>%
  ungroup()

# Convert back to Date (ifelse above can make it lose Date class)
mortality_clean <- mortality_clean %>%
  mutate(death_date = as.Date(death_date, origin = "1970-01-01"))

esc_visits <- mortality_clean %>%
  filter(!is.na(`date Sudoscan`)) %>%
  select(Anonymisation, 
         date_first = `date 1ère consultation`,
         date_sudoscan = `date Sudoscan`,
         umsars_1 = `Score UMSARS1`,
         umsars_2 = `Score UMSARS 2`,
         death_date)


censor_date <- as.Date("2025-07-10")

esc_visits <- esc_visits %>%
  mutate(end_followup = ifelse(!is.na(death_date), death_date, censor_date),
         end_followup = as.Date(end_followup, origin = "1970-01-01"))



tv_data <- esc_visits %>%
  arrange(Anonymisation, date_sudoscan) %>%
  group_by(Anonymisation) %>%
  mutate(start_time = as.numeric(date_sudoscan - min(date_first)),
         stop_time  = lead(start_time, default = as.numeric(end_followup[1] - min(date_first))),
         died       = ifelse(!is.na(death_date) & 
                             stop_time == as.numeric(death_date - min(date_first)), 1, 0)) %>%
  ungroup()



tv_data <- tv_data %>% drop_na() %>%
  mutate(umsars_1_10  = umsars_1  / 10,
         umsars_2_10 = umsars_2 / 10)

cox_tv_10 <- coxph(Surv(start_time, stop_time, died) ~ umsars_1_10  + 
                     cluster(Anonymisation),
                   data = tv_data)

summary(cox_tv_10)

cox_tv_10 <- coxph(Surv(start_time, stop_time, died) ~ umsars_2_10  + 
                     cluster(Anonymisation),
                   data = tv_data)

summary(cox_tv_10)




cox_feet_nocluster <- coxph(Surv(start_time, stop_time, died) ~ umsars_1_10,
                            data = tv_data)

ggforest(cox_feet_nocluster, data = tv_data,
         main = "Hazard Ratios for UMSARS 1 (per 10-point increase)",
         fontsize = 1)



cox_hands_nocluster <- coxph(Surv(start_time, stop_time, died) ~ umsars_2_10,
                            data = tv_data)

ggforest(cox_hands_nocluster, data = tv_data,
         main = "Hazard Ratios for UMSARS 2 (per 10-point increase)",
         fontsize = 1)











# --------------------
# Survival analysis DP Drops --------------------------


Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)


names(Sudoscan_df) 

drops_df <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`, 'PAS couche':FCminute10)

drops_df <- drops_df %>%
  pivot_longer(
    cols = `PAS couche`:FCminute10,
    names_to = "measure",
    values_to = "value"
  ) %>%
  # Separate type of measure (PAS, PAD, FC) from time
  mutate(
    minute = case_when(
      grepl("couche", measure) ~ 0,
      TRUE ~ readr::parse_number(measure)
    ),
    type = case_when(
      grepl("^PAS", measure) ~ "PAS",
      grepl("^PAD", measure) ~ "PAD",
      grepl("^FC", measure)  ~ "FC"
    )
  ) %>%
  select(Anonymisation, `date Sudoscan`, type, minute, value)


drops_df <- drops_df %>% drop_na()

drops_df <- drops_df %>%
  left_join(drops_df %>% filter(minute==0) %>% rename("first"="value") %>% select(-minute))

drops_df <- drops_df %>%
  mutate(perc=(value-first)/first)

drops_df <- drops_df %>% group_by(Anonymisation, `date Sudoscan`, type) %>%
  summarise(perc=min(perc)) %>%
  filter(type!="FC") %>% ungroup() %>% distinct() %>%
  spread(key=type, value=perc)



Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

mortality_df <- Sudoscan_df %>% select(Anonymisation, `date 1ère consultation`, `date Sudoscan`,
                                       `Cause fin de suivi`, `Date décès` ) 


mortality_df <- mortality_df %>% left_join(drops_df)
 

mortality_df$`date 1ère consultation` <- as.Date(mortality_df$`date 1ère consultation`)
mortality_df$`date Sudoscan` <- as.Date(mortality_df$`date Sudoscan`)
mortality_df$`Date décès` <- as.Date(mortality_df$`Date décès`)


mortality_df <- mortality_df %>% filter(!is.na(`date Sudoscan`)) %>% filter(!is.na(PAD))

survival_curve <- mortality_df %>% group_by(Anonymisation) %>%
select(Anonymisation, `date 1ère consultation`)  %>%
  distinct()  %>% filter(`date 1ère consultation`==min(`date 1ère consultation`)) %>%
  distinct() %>%
  left_join(
    mortality_df %>% group_by(Anonymisation) %>%
      select(Anonymisation, `Date décès`)  %>%
      distinct()  %>% filter(`Date décès`==min(`Date décès`, na.rm=T)) %>%
      distinct()
  ) %>%
  mutate(died=ifelse(is.na(`Date décès`),0,1)) %>%
  mutate(final=ifelse(is.na(`Date décès`), as.Date("2025-07-10"), as.Date(`Date décès`) )) %>%
  mutate(first=as.numeric(`date 1ère consultation`)) %>%
  mutate(elapsed=final-first) %>%
  select(Anonymisation, died, elapsed)


survival_curve$elapsed <- survival_curve$elapsed/30.44

library(survival)
library(survminer)

# Create the survival object
surv_obj <- Surv(time = survival_curve$elapsed,
                 event = survival_curve$died)

# Fit KM curve
km_fit <- survfit(surv_obj ~ 1)


km_fit


#        n events median 0.95LCL 0.95UCL
# [1,] 175    149   46.2    40.5    52.2


# Plot
surv_plot <- ggsurvplot(km_fit,
           data = survival_curve,
           conf.int = TRUE,
           palette = "#124A87",
           surv.median.line="hv",
           test.for.trend = TRUE,
           cumevents = TRUE,
           cumcensor = TRUE,
           xlab = "\n Months from 1st consultation",
           ylab = "Survival probability \n",
           risk.table = TRUE,
           tables.height = 0.15,
           ggtheme = theme_minimal())



# Make sure all date columns are Date objects
mortality_clean <- mortality_df %>%
  mutate(across(
    c(`date 1ère consultation`, `date Sudoscan`, `Date décès`),
    as.Date
  ))

# Get death date per patient
mortality_clean <- mortality_clean %>%
  group_by(Anonymisation) %>%
  mutate(death_date = suppressWarnings(min(`Date décès`[`Cause fin de suivi` == "Décès"], na.rm = TRUE))) %>%
  mutate(death_date = ifelse(is.infinite(death_date), NA, death_date)) %>%
  ungroup()

# Convert back to Date (ifelse above can make it lose Date class)
mortality_clean <- mortality_clean %>%
  mutate(death_date = as.Date(death_date, origin = "1970-01-01"))

esc_visits <- mortality_clean %>%
  filter(!is.na(`date Sudoscan`)) %>%
  select(Anonymisation, 
         date_first = `date 1ère consultation`,
         date_sudoscan = `date Sudoscan`,
         DBP = PAD,
         SBP = PAS,
         death_date)


censor_date <- as.Date("2025-07-10")

esc_visits <- esc_visits %>%
  mutate(end_followup = ifelse(!is.na(death_date), death_date, censor_date),
         end_followup = as.Date(end_followup, origin = "1970-01-01"))



tv_data <- esc_visits %>%
  arrange(Anonymisation, date_sudoscan) %>%
  group_by(Anonymisation) %>%
  mutate(start_time = as.numeric(date_sudoscan - min(date_first)),
         stop_time  = lead(start_time, default = as.numeric(end_followup[1] - min(date_first))),
         died       = ifelse(!is.na(death_date) & 
                             stop_time == as.numeric(death_date - min(date_first)), 1, 0)) %>%
  ungroup()


tv_data$DBP <- tv_data$DBP*100
tv_data$SBP <- tv_data$SBP*100

tv_data <- tv_data %>% drop_na() %>%
  mutate(DBP_10  = DBP  / 10,
         SBP_10 = SBP / 10)

cox_tv_10 <- coxph(Surv(start_time, stop_time, died) ~ DBP_10  + 
                     cluster(Anonymisation),
                   data = tv_data)

summary(cox_tv_10)

cox_tv_10 <- coxph(Surv(start_time, stop_time, died) ~ SBP_10  + 
                     cluster(Anonymisation),
                   data = tv_data)

summary(cox_tv_10)




cox_feet_nocluster <- coxph(Surv(start_time, stop_time, died) ~ DBP_10,
                            data = tv_data)

ggforest(cox_feet_nocluster, data = tv_data,
         main = "Hazard Ratios for Diastolic BP (per 10-percent-point increase)",
         fontsize = 1)



cox_hands_nocluster <- coxph(Surv(start_time, stop_time, died) ~ SBP_10,
                            data = tv_data)

ggforest(cox_hands_nocluster, data = tv_data,
         main = "Hazard Ratios for Systolic BP (per 10-percent-point increase)",
         fontsize = 1)





















# -------------------- 
# % Abnormal sudoscan ----------
Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`,
                               `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`) %>% drop_na()


test %>% arrange(Anonymisation, `date Sudoscan`) %>% 
  group_by(Anonymisation) %>% filter(`date Sudoscan`==min(`date Sudoscan`)) %>%
  mutate(`ESC Mains Moyennes (µS)`=ifelse(`ESC Mains Moyennes (µS)`<60,1,0)) %>%
  mutate(`ESC Pieds Moyennes  (µS)`=ifelse(`ESC Pieds Moyennes  (µS)`<70,1,0)) %>%
  #group_by(`ESC Mains Moyennes (µS)`) %>% count() # 123 / 123+52 = 70%
  group_by(`ESC Pieds Moyennes  (µS)`) %>% count() # 112 / 112+63 = 64%



test %>% arrange(Anonymisation, `date Sudoscan`) %>% 
  group_by(Anonymisation) %>% filter(`date Sudoscan`==max(`date Sudoscan`)) %>%
  mutate(`ESC Mains Moyennes (µS)`=ifelse(`ESC Mains Moyennes (µS)`<60,1,0)) %>%
  mutate(`ESC Pieds Moyennes  (µS)`=ifelse(`ESC Pieds Moyennes  (µS)`<70,1,0)) %>%
  group_by(`ESC Mains Moyennes (µS)`) %>% count() # 140 / 140+35 = 80%
  #group_by(`ESC Pieds Moyennes  (µS)`) %>% count() # 143 / 143+32 = 82%


Sudoscan_df <- read_xlsx(path="Sudoscans.xlsx", trim_ws = TRUE)

test <- Sudoscan_df %>% select(Anonymisation, `date Sudoscan`,
                               `ESC Pieds Moyennes  (µS)`, `ESC Mains Moyennes (µS)`, 
                               `Score UMSARS 2`, `Score UMSARS1` ) %>% drop_na()


test %>% arrange(Anonymisation, `date Sudoscan`) %>% 
  group_by(Anonymisation) %>% filter(`date Sudoscan`==max(`date Sudoscan`)) %>%
  mutate(ESC_group=ifelse(`ESC Mains Moyennes (µS)`<60,"Below","Above") ) %>%
  ggplot(aes(`ESC Mains Moyennes (µS)`, `Score UMSARS1`, colour=ESC_group, value=ESC_group)) +
  geom_jitter(shape=1, stroke=1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))
        



test %>% arrange(Anonymisation, `date Sudoscan`) %>% 
  group_by(Anonymisation) %>% filter(`date Sudoscan`==max(`date Sudoscan`)) %>%
  #mutate(`Score UMSARS 2`=ifelse(`Score UMSARS 2`>30,"yes","no")) %>%
  ggplot(aes(`ESC Mains Moyennes (µS)`, `ESC Pieds Moyennes  (µS)`, colour=`Score UMSARS 2`, fill=`Score UMSARS 2`)) +
    geom_jitter(shape=1, stroke=2) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

# Fit a plane of ESC hands vs ESC feet and UMSARS 1 or UMSARS2 (2 plots) at the first and last visit
        
df <- test %>% arrange(Anonymisation, `date Sudoscan`) %>% 
  group_by(Anonymisation) %>% filter(`date Sudoscan`==max(`date Sudoscan`))
        
library(plotly)
library(dplyr)


# 3D scatter plot
fig <- plot_ly(
  df,
  x = ~`ESC Mains Moyennes (µS)`,
  y = ~`ESC Pieds Moyennes  (µS)`,
  z = ~`Score UMSARS 2`,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4, color = ~`Score UMSARS 2`, colorscale = "Viridis")
)

# Fit linear model (plane: z ~ x + y)
fit <- lm(`Score UMSARS 2` ~ `ESC Mains Moyennes (µS)` + `ESC Pieds Moyennes  (µS)`, data = df)

# Create grid for plane
x_seq <- seq(min(df$`ESC Mains Moyennes (µS)`), max(df$`ESC Mains Moyennes (µS)`), length.out = 30)
y_seq <- seq(min(df$`ESC Pieds Moyennes  (µS)`), max(df$`ESC Pieds Moyennes  (µS)`), length.out = 30)
grid <- expand.grid(`ESC Mains Moyennes (µS)` = x_seq,
                    `ESC Pieds Moyennes  (µS)` = y_seq)
grid$z <- predict(fit, newdata = grid)

# Add regression plane
fig <- fig %>% add_trace(
  x = grid$`ESC Mains Moyennes (µS)`,
  y = grid$`ESC Pieds Moyennes  (µS)`,
  z = grid$z,
  type = "mesh3d",
  opacity = 0.4
)

fig

# ------- 
