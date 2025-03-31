# datset
eufs <- read.csv("eufs.csv")
str(eufs)
colnames(eufs)
rownames(eufs)

eufs <- eufs %>%
  mutate_if(is.character, as.factor)
#totalclub
eufs$tc <- apply(eufs[,17:52],MARGIN=1,FUN=sum)
#total club percentage
eufs$tcp <- round(100*apply(eufs[,17:52],MARGIN=1,FUN=sum)/36,digits=1)
# sponsor club
eufs$ts <- apply(eufs[,53:70],MARGIN=1,FUN=sum)
# sponsor club percentage
eufs$tsp <- round(100*apply(eufs[,53:70],MARGIN=1,FUN=sum)/36,digits=1)

write.csv(eufs, "eufs.csv",row.names=F)
#table1
league_cols <-c("ucl","uel","epl","LaLiga","Bundesliga","SerieA","Ligue1")
league_counts <- sapply(eufs[,league_cols],function(x)sum(x=="Yes",na.rm=TRUE))
league_percent <- round((league_counts/567)*100,digits=1)
league_map <-c(
  ucl         = "UCL – Champions League",
  uel         = "UEL – Europa League",
  epl         = "EPL – English Premier League",
  LaLiga      = "LaLiga – Spain",
  Bundesliga  = "Bundesliga – Germany",
  SerieA      = "Serie A – Italy",
  Ligue1      = "Ligue 1 – France"
)

table1<-data.frame(
  Variable = "League",
  League   = league_map[names(league_counts)],
  n        = as.vector(league_counts),
  `%`      = as.vector(league_percent)
)
print(table1)

#table 2
club_cols <-colnames(eufs)[17:52]

club_counts <- sapply(eufs[, 17:52], function(x) sum(as.numeric(as.character(x)) == 1, na.rm = TRUE))
club_percent <-round( (club_counts /567)*100,1)
club_df <-data.frame(
  Team=names(club_counts),
  Percent=club_percent)
club_df_desc <-club_df[order(-club_df$Percent),]
top10 <-head(club_df_desc,10)

bottom5 <- tail(club_df_desc,5)
bottom5 <-bottom5[order(bottom5$Percent),]

top10_table <- data.frame(
  Variable="Top-10 Club logos",
  Team= top10$Team,
  '%'=round(top10$Percent,1))
bottom5_table <-data.frame(
  Variable="Bottom-5 Club Logos",
  Team= bottom5$Team,
  '%'=round(bottom5$Percent,1))

print(top10_table)
print(bottom5_table)

#table3

spon_cols <-colnames(eufs)[53:70]

spon_counts <- sapply(eufs[, 53:70], function(x) sum(as.numeric(as.character(x)) == 1, na.rm = TRUE))
spon_percent <-round( (spon_counts /567)*100,1)
spon_df <-data.frame(
  Team=names(spon_counts),
  Percent=spon_percent)
spon_df_desc <-spon_df[order(-spon_df$Percent),]
top10 <-head(spon_df_desc,10)

bottom5 <- tail(spon_df_desc,5)
bottom5 <-bottom5[order(bottom5$Percent),]

top10_table <- data.frame(
  Variable="Top-10 Club Sponsor logos",
  Team= top10$Team,
  '%'=round(top10$Percent,1))
bottom5_table <-data.frame(
  Variable="Bottom-5 Club Sponsor Logos",
  Team= bottom5$Team,
  '%'=round(bottom5$Percent,1))

print(top10_table)
print(bottom5_table)


#table 4
library(dplyr)
df_filtered <- eufs %>%
  filter(hours != "No Response")
table4 <-df_filtered %>%
  group_by(hours) %>%
  summarise(
    n=n(),
    M=mean(tcp,na.rm=TRUE),
    SD=sd(tcp,na.rm=TRUE))

table4$hours <- factor(table4$hours,levels=c("0 Hours", "1-4 Hours", "5+ Hours"))
table4<- table4[order(table4$hours),]
print(table4)

#question 5
table5 <- boxplot(tcp ~ hours, data = df_filtered,
        col = c("red", "blue", "green"),  
        main = "Club Logo Recognition by Hours Spent Watching Football",
        xlab = "Hours Spent Watching European Football Per Week",
        ylab = "Percentage of Club Logos Recognized")
print(table5)

#table 6

df_filtered <- eufs %>%
  filter(gender %in% c("Male","Female"))

table6 <- df_filtered %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    M = mean(tcp, na.rm = TRUE),
    SD = sd(tcp, na.rm = TRUE)
  ) %>%
  ungroup()
table6 <- table6 %>%
  mutate(gender = recode(gender,
                         "Male" = "Male Participants",
                         "Female" = "Female Participants")) %>%
  mutate(gender = factor(gender, 
                         levels = c("Male Participants", "Female Participants"))) %>%
  arrange(gender)

print(table6)

#table7
df_filtered <- eufs %>%
  filter(gender %in% c("Male","Female")) %>%
  droplevels()
table7 <- boxplot(tcp ~ gender, data = df_filtered,
                  col = c("lightpink", "skyblue"),  
                  main = "Number of Club Logo Recognized by Gender",
                  xlab = "Gender",
                  ylab = "Number of Recognized Club Logos")
print(table7)

#table 8
df_filtered <- eufs %>%
  filter(age != "No Response")
table8 <- df_filtered %>%
  group_by(age) %>%
  summarise(
    n=n(),
    M=mean(tsp,na.rm=TRUE),
    SD=sd(tsp,na.rm=TRUE)) %>%
  ungroup()
table8$age <- factor(table8$age,
                     levels=c("18-21","22-25","26-29","30-41","42+"))
table8 <- table8[order(table8$age),]
print(table8)

#table 9
df_filtered <- eufs %>%
  filter(age != "No Response")
table9 <- boxplot(tsp~age,data=df_filtered,
                  col=c("red","blue","green","yellow","purple"),
                  main="Sponsor Logo Recognition by Age",
                  xlab="Age Groups",
                  ylab="Percentage of Sponsor Logos Recognized")
  