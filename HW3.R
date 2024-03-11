#1
#a
gay_df = read.csv("~/Downloads/gay.csv")
wave1_df = subset(gay_df, subset = wave == 1)
summary(wave1_df)
no_contact_1 = subset(wave1_df, subset = (treatment == "No Contact"))
dim(no_contact_1)
recycling_gay_1 = subset(wave1_df, subset = (treatment == "Recycling Script by Gay Canvasser"))
dim(recycling_gay_1)
marriage_gay_1 = subset(wave1_df, subset = (treatment == "Same-Sex Marriage Script by Gay Canvasser"))
dim(marriage_gay_1)
recycling_straight_1 = subset(wave1_df, subset = (treatment == "Recycling Script by Straight Canvasser"))
dim(recycling_straight_1)
marriage_straight_1 = subset(wave1_df, subset = (treatment == "Same-Sex Marriage Script by Straight Canvasser"))
dim(marriage_straight_1)
#b
mean(subset(gay_df, subset=(wave == 2 & study == 1 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 2 & study == 1 & treatment == "Same-Sex Marriage Script by Straight Canvasser"))$ssm)
#c
mean(subset(gay_df, subset=(wave == 2 & study == 1 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 2 & study == 1 & treatment == "Recycling Script by Gay Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 2 & study == 1 & treatment == "Same-Sex Marriage Script by Straight Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 2 & study == 1 & treatment == "Recycling Script by Straight Canvasser"))$ssm)
#d
mean(subset(gay_df, subset=(wave == 7 & study == 1 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 7 & study == 1 & treatment == "Same-Sex Marriage Script by Straight Canvasser"))$ssm)
#e
no_contact_2 = subset(gay_df, subset = (wave == 1 & study == 2 & treatment == "No Contact"))
dim(no_contact_2)
marriage_gay_2 = subset(gay_df, subset = (wave == 1 & study == 2 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))
dim(marriage_gay_2)
#f
mean(subset(gay_df, subset=(wave == 2 & study == 2 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
#g
mean(subset(gay_df, subset=(wave == 1 & study == 2 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 2 & study == 2 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 3 & study == 2 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 4 & study == 2 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
mean(subset(gay_df, subset=(wave == 7 & study == 2 & treatment == "Same-Sex Marriage Script by Gay Canvasser"))$ssm)
#2

#a
leaders = read.csv("~/Downloads/leaders.csv")
summary(leaders)
dim(leaders)
unique(leaders$country)
(2001 - 1878) / nrow(leaders)
#b
leaders$dead <- ifelse(leaders$result == "dies within a day after the attack" | 
                         leaders$result == "dies between a day and a week" | 
                         leaders$result == "dies between a week and a month" | 
                         leaders$result == "dies, timing unknown", 1, 0)
sum(leaders$dead)
#c
mean(leaders$politybefore)
mean(leaders$polityafter)
mean(leaders$age[leaders$dead == 0])
mean(leaders$age[leaders$dead == 1])
#d
mean(leaders$interwarbefore)
mean(leaders$interwarafter)
mean(leaders$civilwarbefore)
mean(leaders$civilwarafter)
leaders$warbefore <- ifelse(leaders$civilwarbefore == 1 | 
                              leaders$interwarbefore == 1, 1, 0)
mean(leaders$warbefore)
#e
leader_dead_df = subset(leaders, subset=(dead == 1))
mean(leader_dead_df$polityafter - leader_dead_df$politybefore)
leaders$warafter <- ifelse(leaders$civilwarafter == 1 | 
                              leaders$interwarafter == 1, 1, 0)
mean(leaders$warafter[leaders$dead == 1] - leaders$warbefore[leaders$dead == 1])

