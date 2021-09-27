library(tidyverse)
library(haven)
library(extrafont)
library(RStata)

loadfonts(device = "win")

RStata::chooseStataBin()
                       
# "\"C:\\Program Files\\Stata17\\StataSE-64\""

stata("do scripts/recode.do")


# 

theme_ef <- function () { 
  theme_minimal(base_size=12) %+replace%
    theme(
      axis.title = element_blank(),
      title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(family = "FiraGO", face="bold", size = 14),
      text = element_text(family= "FiraGO"),
      plot.title = element_text(size=16, face="bold", family="FiraGO"),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.subtitle = element_text(size=12, family="FiraGO"),
      axis.text = element_text(size=12, family="FiraGO", color = "black"),
      strip.text.x = element_text(size=12, family= "FiraGO", angle=0, hjust=0.06),
      legend.position = "none"
    )
}

## p2

p2 <- data.frame()

for (i in 1:9) {
  vec_apply <- paste0("p2_", i)
  vec_apply
  read.csv(paste0("tables/frequency/", vec_apply, ".csv"), header = T, sep = "\t")%>%
    mutate(group = colnames(.)[1],)%>%
    setNames(., c("var", "prop", "group")) %>%
    mutate(
      group = str_replace_all(group, "\\.", " "),
      group = str_replace_all(group, "Main source of information regarding July 5 events     ", ""),
      group = str_replace(group, "Acquaintances  not part", "Acquaintances who did not participate"),
      group = str_replace(group, "Acquaintances  part", "Acquaintances who participated"),
      group = str_trim(group),
      var = str_trim(var),
    )%>%
    bind_rows(., p2) -> p2
}

p2 %>%
  filter(var == "Mentioned"  | ( var == "DK/RA" & group == "Acquaintances who participated"))%>%
  mutate(var = ifelse(var %in% c("DK/RA"), var, group),
         var = fct_reorder(var, prop),
         var = fct_relevel(var, c("DK/RA"), after=0),
         # group = str_wrap(group, width = 30),
         ) -> p2



# EN

p2 %>%
  ggplot(aes(var, prop, fill=var, label=ifelse(prop <= 0.5, "", round(prop, 0))))+
  geom_col()+
  scale_fill_manual(values=c("#999999", "#8F5D5D", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B"))+
  ylim(0, 101)+
  coord_flip()+
  geom_text(hjust = -1.1, nudge_x = 0.1, family="FiraGO", size=4, fontface = "bold")+
  labs(
    title = "Where did you hear from about the July 5 events?"
  )+
  theme_ef()

ggsave("visuals/frequency/en/p2.png", width=13, height=5)


## p3


p3 <- data.frame()

for (i in 1:8) {
  vec_apply <- paste0("p3_", i)
  vec_apply
  read.csv(paste0("tables/frequency/", vec_apply, ".csv"), header = T, sep = "\t")%>%
    mutate(group = colnames(.)[1],)%>%
    setNames(., c("var", "prop", "group")) %>%
    mutate(
      group = str_replace_all(group, "\\.", " "),
      group = str_replace_all(group, "The main organizer of Tbilisi Pride   ", ""),
      group = str_replace(group, "External forces   foreigners", "External forces / foreigners"),
      group = str_replace(group, "Civil Movement Shame", "The Shame Movement"),
      group = str_replace(group, "Sexual minorities", "LGBTQ+ people"),
      group = str_replace(group, "Government   Georgian Dream", "Government / Georgian Dream"),
      group = str_trim(group),
      var = str_trim(var),
    )%>%
    bind_rows(., p3) -> p3
}

p3 %>%
  filter(var == "Mentioned"  | ( var == "DK/RA" & group == "Other"))%>%
  mutate(var = ifelse(var %in% c("DK/RA"), var, group),
         var = fct_reorder(var, prop),
         var = fct_relevel(var, c("DK/RA"), after=0),
         qname = "Pride March?",
  ) -> p3

## p4


p4 <- data.frame()

for (i in 1:13) {
  vec_apply <- paste0("p4_", i)
  vec_apply
  read.csv(paste0("tables/frequency/", vec_apply, ".csv"), header = T, sep = "\t")%>%
    mutate(group = colnames(.)[1],)%>%
    setNames(., c("var", "prop", "group")) %>%
    mutate(
      group = str_replace_all(group, "\\.", " "),
      group = str_replace_all(group, "The main organizer of second demonstrations    ", ""),
      group = str_replace(group, "External forces foreigners", "External forces / foreigners"),
      group = str_replace(group, "Parish   Believers", "Parishioners / believers"),
      group = str_replace(group, "Church   Patriarchate", "Church / Patriarchate"),
      group = str_replace(group, "Government   Georgian Dream", "Government / Georgian Dream"),
      group = str_trim(group),
      var = str_trim(var),
    )%>%
    bind_rows(., p4) -> p4
}

p4 %>%
  filter(var == "Mentioned"  | ( var == "DK/RA" & group == "Other"))%>%
  mutate(var = ifelse(var %in% c("DK/RA"), var, group),
         var = fct_reorder(var, prop),
         var = fct_relevel(var, c("DK/RA"), after=0),
         qname = "Counter-demonstration?",
  ) -> p4

p3%>%
  bind_rows(p4)%>%
  group_by(qname)%>%
  mutate(var = fct_reorder(var, prop),
         var = fct_relevel(var, c("DK/RA"), after=0),
  )%>%
  ggplot(aes(var, prop, fill=var, label=ifelse(prop <= 0.5, "", round(prop, 0))))+
  geom_col()+
  scale_fill_manual(values=c("#999999", "#8F5D5D", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B", "#3D405B"))+
  ylim(0, 101)+
  coord_flip()+
  facet_wrap(~qname, scales = "free")+
  geom_text(hjust = -1.1, nudge_x = 0.1, family="FiraGO", size=4, fontface = "bold")+
  labs(
    title = "Who were organizers of the ...",
    subtitle = "Out of 85% who have heard about July 5 events"
  )+
  theme_ef()

ggsave("visuals/frequency/en/p3_4.png", width=13, height=5)


### p8

read.csv("tables/crosstabs/p8_agegroup.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop")%>%
  mutate(group = "Age groups")  -> p8_agegroup

read.csv("tables/crosstabs/p8_stratum.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Settlement type") -> p8_stratum

read.csv("tables/crosstabs/p8_party.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Party identification") -> p8_party

bind_rows(p8_agegroup, p8_stratum, p8_party) %>%
  mutate(
    labels = str_trim(labels),
    labels = factor(labels, levels = c("Yes", "No", "DK/RA")),
         labels = fct_rev(labels),
  )%>%
  filter(cat != "Total")%>%
  mutate(
    cat = str_replace(cat, "X18.34$", "18-34"),
         cat = str_replace(cat, "X35.54$", "35-54"),
         cat = str_replace(cat, "X55.$", "55+"),
         cat = str_replace(cat, "DK.RA", "DK/RA"),
         cat = str_replace_all(cat, "\\.", " "),
         cat = factor(cat, levels = c("18-34", "35-54", "55+", "Capital", "Urban", "Rural",
                                      "Government", "Opposition", "Unaffiliated", "DK/RA")),
         cat = fct_rev(cat),
         group = factor(group, levels = c("Age groups", "Settlement type", "Party identification"))) -> p8_demo

p8_demo%>%
  ggplot(aes(cat, prop, fill=labels, label=ifelse(prop <= 0.5, "", round(prop, 0))))+
  geom_col()+
  scale_fill_manual(values=c("#999999", "#cb997e", "#006d77"),
                    guide = guide_legend(reverse = T))+
  facet_wrap(~group, scales = "free", ncol=1)+
  coord_flip()+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", size=4, fontface = "bold")+
  theme_ef()+
  labs(
    title = "Would Tbilisi Pride have endangered Georgia?",
    subtitle = "Out of 85% who have heard about July 5 events"
  )+
  theme(legend.position = "bottom",
        strip.text = element_text(angle=0, hjust=0.06))

ggsave("visuals/crosstabs/p8_demography.png", width=12, height=5)


# p9 

p9 <- data.frame()

for (i in 1:7) {
  vec_apply <- paste0("p9_", i)
  vec_apply
  read.csv(paste0("tables/frequency/", vec_apply, ".csv"), header = T, sep = "\t")%>%
    mutate(group = colnames(.)[1],)%>%
    setNames(., c("var", "prop", "group")) %>%
    mutate(
      group = str_replace_all(group, "\\.", " "),
      group = str_replace_all(group, "Regarding July 5 events how would you rate work of the ", ""),
      group = str_replace_all(group, "Regarding July 5 events how would you rate work of ", ""),
      group = str_replace_all(group, "foreing", "Foreign"),
      group = str_replace_all(group, "journalists", "Journalists"),
      group = str_replace_all(group, "police", "Police"),
      group = str_replace_all(group, "Zurabishvili", "Zourabichvili"),
      var = str_replace_all(var, "Very Negatively", "Very negatively"),
      var = str_replace_all(var, "Very Positively", "Very positively"),
      group = str_trim(group),
      var = str_trim(var),
    )%>%
    bind_rows(., p9) -> p9
}

p9 %>%
  filter(var %in% c("Very positively", "Positively")) %>%
  group_by(group)%>%
  summarize(prop_sort=sum(prop)) -> p9_sort


p9 %>%
left_join(p9_sort, by="group")%>%
  mutate(var = factor(var, levels = c("Very negatively", "Negatively", "Neither positively, nor negatively",
                                      "Positively","Very positively",  "DK/RA"),
                      ordered = T),
         var = fct_relevel(var, c("DK/RA"), after=0),
         label = fct_reorder(group, prop_sort, .desc=T))%>%
  ggplot(aes(label, prop, fill=var, label=ifelse(prop <= 0.5, "", round(prop, 0))))+
  geom_col(position="stack")+
  scale_fill_manual(values=c("#999999", "#cb997e", "#ddbea9", "#ffe8d6", "#83c5be", "#006d77"),
                    guide = guide_legend(reverse = T))+
  ylim(0, 101)+
  coord_flip()+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", size=4, fontface = "bold")+
  facet_wrap(~label, scales = "free", ncol=1)+
  labs(
    title = "How would you evaluate the actions of the following during the July 5 events in Tbilisi?",
    subtitle = "Out of 85% who have heard about July 5 events"
  )+
  theme_ef()+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        strip.text = element_text(angle=0, hjust=0.07, size=12),
        strip.background.y = element_rect(fill="lightgrey"))

ggsave("visuals/frequency/en/p9.png", width=12, height=6)

## P5 P6 TS

p5_6 <- data.frame()

vec_apply <- c("p5.csv", "q5_2013.csv", "p6.csv", "q6_2013.csv")

vec_apply <- c("p7.csv", "q6_2013.csv")

for (i in seq_along(vec_apply)) {
  read.csv(paste0("tables/frequency/", vec_apply[i]), header = T, sep = "\t")%>%
    mutate(group = colnames(.)[1],)%>%
    setNames(., c("var", "prop", "group")) %>%
    mutate(
      year = vec_apply[i],
      group = "Physical violence is acceptable against people endangering national values",
      var = str_trim(var),
      var = fct_rev(var),
    )%>%
    bind_rows(., p5_6) -> p5_6
}

p5_6 %>%
  mutate(
    year = str_replace_all(year, "q6_2013.csv", "2013"),
    year = str_replace_all(year, "p7.csv", "2021"),
  )%>%
  ggplot(aes(group, prop, fill=var, label=ifelse(prop <= 0.5, "", round(prop, 0))))+
  geom_col(position="stack")+
  scale_fill_manual(values=c("#999999", "#cb997e", "#006d77"),
                    guide = guide_legend(reverse = T))+
  ylim(0, 101)+
  coord_flip()+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", size=4, fontface = "bold")+
  facet_wrap(~year, scales = "free", ncol=1)+
  labs(
    title = "Do you agree or disagree that it is acceptable to exercise physical violence\nagainst those who endanger national values?",
    subtitle = "Tbilisi residents who have heard about May 17 events among respondents interviewed in 2013, and those aware of the July 5 events in 2021"
  )+
  theme_ef()+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        strip.text = element_text(angle=0, hjust=0.07, size=12),
        strip.background.y = element_rect(fill="lightgrey"))


ggsave("visuals/frequency/en/p5_6.png", width=12, height=4)
