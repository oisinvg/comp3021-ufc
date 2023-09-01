install.packages("ggplot2")
install.packages("dplyr")
install.packages("GGally")
install.packages("plotly")
install.packages("GG3D")

library(viridis)
library(GGally)
library(ggplot2)
library(dplyr)
library(plotly)
library(grid)

colorrange = colorRampPalette(c("green", "orange","red"))
mycolors = colorrange(20)

ufc <- read.csv("C:\\Users\\oisin\\OneDrive\\Uni\\Computer Science\\Year 3\\Term 2 and 3\\COMP3021 - Fundamentals of Information Visualisation\\Coursework\\src\\ufc-master.csv", header = TRUE) # nolint # nolint


#1.	Which fighter wins more, red or blue?
#2.	Does the weight class have an influence on the chance of KO?
#3.	How often does the older fighter beat the younger fighter?
#4.	Does having more wins than one’s opponent lead to a better chance of winning?
#5.	Did a fighter’s height and reach advantage (where present) have any correlation to finishing fights?
#6. What is the distribution of submission types?
#7. How did stances match up against each other?

#1.	Which fighter wins more, red or blue?

red_wins = nrow(ufc[ufc$Winner == 'Red',]) #2,859
blue_wins = nrow(ufc[ufc$Winner == 'Blue',]) #2,037
#the number of fights in which the red corner, then blue corner, won

q1data <- data.frame( #create df for corner, win count 
  corner=c("Red", "Blue"),
  value=c(red_wins, blue_wins)
)

q1 = pie(q1data$value,q1data$corner, main = "q1: Share of wins between red and blue corner", col=c("red","blue"))
#pie chart for red and blue wins. red has more.
q1 #plot it

#2.	Does the weight class have an influence on the chance of KO?

weightclasses = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", "Women's Featherweight", "Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Light Heavyweight", "Heavyweight")
finishrates = c() #empty vector for df
finishcounts = c() #"
finish_sum = 0 #running count of ALL ko/tko's

for (w in weightclasses) {
  div = ufc[ufc$weight_class == w,] #select division
  finish_count = length(grep("KO/TKO", div$finish)) #number of KO/TKO
  finish_rate = finish_count / nrow(div) #find its KO rate
  finish_rate <- round(finish_rate, 3) #round to 3 d.p.
  finish_sum <- finish_sum + finish_count #add to running count
  finishrates <- append(finishrates, finish_rate) #append to empty vector
  finishcounts <- append(finishcounts, finish_count)
}

q2data <- data.frame( #create df for division, decimal finish rate
  division=weightclasses,
  rate=finishrates,
  count=finishcounts
)

q2data$division <- reorder(q2data$division, -q2data$rate)

#q2data$division <- factor(q2data$division, levels = q2data$division[order(q2data$count)]) #to stop alphabetical sorting

q2 <- ggplot(data=q2data, aes(x=division, y=rate)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=rate), vjust = 1.6, color='white', size=3) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
  ggtitle("q2: KO/TKO rate per division")

#bar chart for finish rate by weight class. positive correlation
q2 #plot it


q2.1 <- ggplot(q2data, aes(x="",y=count,fill=division)) +
  geom_bar(stat='identity', width=1, color='white') +
  coord_polar("y",start=0) +
  ggtitle("q2.1: Share of KO/TKO finishes across division") +
  theme_void()

#pie chart showing distribution of ko's across divisions
q2.1 #plot it

q2data$division <- reorder(q2data$division, q2data$count)

q2.2 <- ggplot(q2data, aes(x=division,y=count)) +
  geom_point() +
  geom_segment( aes(x=division, xend=division, y=0, yend=count)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
  coord_flip() +
  ggtitle("q2.1: Sum of KO/TKO finishes per division")

#lollipop chart showing ranked KO count per division
q2.2 #plot it


#3.	How often does the older fighter beat the younger fighter?

#e.g. R_fighter: Thiago Santos; B_fighter: Johnny Walker; age_dif: -8; Winner: Red
# Thiago Santos was 8 years the elder and he won.

ufc<-mutate(ufc, Winner_age = case_when(
  age_dif <= 0 & Winner == "Red" ~ "Elder",
  age_dif <= 0 & Winner == "Blue" ~ "Younger",
  age_dif > 0 & Winner == "Red" ~ "Younger",
  age_dif > 0 & Winner == "Blue" ~ "Elder",
))

oldWin <- nrow(ufc[ufc$Winner_age=="Elder",])
youngWin <- nrow(ufc[ufc$Winner_age=="Younger",])

q3data <- data.frame(
  label = c("Older fighter wins", "Younger fighter wins"),
  values = c(oldWin, youngWin)
)

q3 <- ggplot(q30data, aes(x=label,y=values, fill=label)) +
  geom_bar(stat='identity') +
  ggtitle("q3: Old wins vs young wins") +
  geom_text(aes(label = values), colour='white', size=5, vjust=1.6) +
  scale_fill_manual(values=c("red","darkblue"))
q3

q3.1 <- pie(q30data$values, q30data$label, main="q3.1: Share of old wins vs young wins", init.angle=90, col=rainbow(2))
q3.1

blueOld <- subset(ufc, age_dif>0) #fights where red age < blue age
redOld <- subset(ufc, age_dif<=0) #fights where red age > blue age
redOldWin <- nrow(redOld[redOld$Winner=="Red",]) #older red win count
redOldLose <- nrow(redOld[redOld$Winner=="Blue",]) #older red lose count
blueOldWin <- nrow(blueOld[blueOld$Winner=="Blue",]) #older blue win count
blueOldLose <- nrow(blueOld[blueOld$Winner=="Red",]) #older blue lose count

q3.2data <- data.frame( #dataframe for stacked bar charts: sets of values
  specie = c(rep("Older fighter wins",2) , rep("Younger fighter wins",2)),
  condition = rep(c("Red was older", "Blue was older")),
  value = c(redOldWin, blueOldWin, blueOldLose, redOldLose)
)

q3.2 <- ggplot(q3data, aes(fill=condition, y=value,x=specie)) + #stacked barchart
  geom_bar(position='stack',stat='identity') +
  scale_fill_manual(values=c("blue","red","blue","red"), "Age difference") +
  ggtitle("q3.2: Old wins vs young wins (stack)") +
  geom_text(aes(label=value), position = position_stack(vjust=0.8), color='white', size=5) +
  theme(axis.title.x=element_blank(), axis.title.y =element_blank())
q3.2

#4.	Does having more wins than one’s opponent lead to a better chance of winning?

ufc <- mutate(ufc, Winner_rec = case_when( #Add a winner record column for which record won the fight
  win_dif == 0 ~ "Matched",
  win_dif < 0 & Winner == "Red" ~ "Stronger",
  win_dif < 0 & Winner == "Blue" ~ "Weaker",
  win_dif > 0 & Winner == "Red" ~ "Weaker",
  win_dif > 0 & Winner == "Blue" ~ "Stronger"
))

strongerWins = nrow(ufc[ufc$Winner_rec=="Stronger",])
weakerWins = nrow(ufc[ufc$Winner_rec=="Weaker",])
matchedWins = nrow(ufc[ufc$Winner_rec=="Matched",])

q4data <- data.frame (
  labels = c("Stronger record wins", "Weaker record wins"),
  data = c(strongerWins, weakerWins)
)

q4data$labels = reorder(q4data$labels, -q4data$data)

q4 <- ggplot(q4data, aes(x=labels,y=data, fill=labels)) +
  geom_bar(stat='identity') +
  ggtitle("q4: Stronger record wins vs weaker record wins") +
  geom_text(aes(label = data), colour='white', size=5, vjust=1.6) +
  scale_fill_manual(values=c("red","darkblue"))
q4

strongerWins1 = nrow(ufc[ufc$Winner_rec=="Stronger" & abs(ufc$win_dif)==1,]) #stronger record wins and has 1 more win
strongerWins2 = nrow(ufc[ufc$Winner_rec=="Stronger" & abs(ufc$win_dif)==2,]) #stronger record wins and has 2 more wins
strongerWins3 = nrow(ufc[ufc$Winner_rec=="Stronger" & abs(ufc$win_dif)>3,]) #stronger record wins and has 3+ more wins
weakerWins1 = nrow(ufc[ufc$Winner_rec=="Weaker" & abs(ufc$win_dif)==1,]) #weaker record wins and has 1 more win
weakerWins2 = nrow(ufc[ufc$Winner_rec=="Weaker" & abs(ufc$win_dif)==2,]) #weaker record wins and has 2 more wins
weakerWins3 = nrow(ufc[ufc$Winner_rec=="Weaker" & abs(ufc$win_dif)>3,]) #weaker record wins and has 3+ more wins

q4.1data <- data.frame (
  specie <- c(rep("Stronger record wins",3) , rep("Weaker record wins",3)),
  condition <- rep(c("1","2","3+"),2),
  value = c(strongerWins1, strongerWins2, strongerWins3, weakerWins1, weakerWins2, weakerWins3)
)

q4.1 <- ggplot(q4.1data, aes(fill=condition, y=value, x=specie)) +
  geom_bar(position='stack',stat='identity') +
  scale_fill_manual(values=rep(c("green","orange","red"),2),"Win discrepancy") +
  ggtitle("q4.1: Stronger record wins vs weaker record wins") +
  geom_text(aes(label=value), position = position_stack(vjust=0.8), color='white', size=5) +
  theme(axis.title.x=element_blank(), axis.title.y =element_blank())
q4.1


#5.	Did a fighter’s height advantage (where present) have any correlation to striking efficacy (accuracy, significant strikes landed per minute, KO/TKO wins rate)?

ufc$height_dif <- ufc$B_Height_cms - ufc$R_Height_cms
ufc$reach_dif <- ufc$B_Reach_cms - ufc$R_Reach_cms

q5data <- data.frame(
  heightdif = jitter(abs(ufc$height_dif),50),
  reachdif = jitter(abs(ufc$reach_dif),75),
  sigstrikedif = abs(ufc$sig_str_dif)
)

q5data <- q5data[-697,] #remove anomalous reach measurement

q5data <- mutate(q5data, sigstrikedifrange = case_when(
  0 <= sigstrikedif & sigstrikedif < 50 ~ "0-50",
  50 <= sigstrikedif & sigstrikedif < 100 ~ "50-100",
  sigstrikedif >= 100 ~ "100+"
))

q5data$sigstrikedifrange <- factor(q5data$sigstrikedifrange, levels = c("0-50","50-100","100+"))

q5 <- ggplot(q5data, aes(x=heightdif, y=reachdif)) +
  geom_point(aes(colour=sigstrikedifrange,alpha=sigstrikedifrange)) +
  ggtitle("q5: Height, reach, and significant strike difference") +
  scale_colour_brewer(palette = "Set1",direction=-1) +
  labs(x='Height difference in cms',y='Reach difference in cms')
q5

#6. What is the distribution of submission types?

subtypes = unique(ufc[ufc$finish=="SUB",]$finish_details)
subcounts = c()

for (s in subtypes) {
  subcounts <- append(subcounts, nrow(ufc[ufc$finish_details==s,]))
}

q6data <- data.frame (
  labels = subtypes,
  values = subcounts
)

q6data <- q6data[-7,] #remove blank subtypes

q6data$labels <- reorder(q6data$labels, -q6data$values) #sort dataframe by values descending

q6 <- ggplot(q6data, aes(x=labels, y=values)) +
  geom_bar(stat='identity',fill='steelblue') +
  geom_text(aes(label=values), vjust = -0.6, color='black', size=2) +
  labs(x="Submission types", y='Count') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
  ggtitle("q6: Submission types and number of occurences")
q6

q6.1 <- ggplot(q6data, aes(x="",y=values,fill=labels)) +
  geom_bar(stat='identity', width=1, color='white') +
  coord_polar("y",start=0) +
  ggtitle("q6.1: Distribution of submission types") +
  theme_void()
q6.1

ggplotly(q6.1, tooltip='text')

#7. How did stances match up against each other?

ufc<-ufc[-186,]
ufc<-ufc[-237,] #remove blank stance rows
ufc$B_Stance[ufc$B_Stance=='Switch '] <- 'Switch'
ufc$R_Stance[ufc$R_Stance=='Switch '] <- 'Switch'

ufc <- mutate(ufc, Winner_stance=case_when( #adds a Winner's stance column
  Winner=="Red" ~ R_Stance,
  Winner=="Blue" ~ B_Stance
))

stances = unique(ufc$Winner_stance)
stances = stances[!stances == 'Open Stance'] #do not analyse for Open Stance

####
challenger = c()
opponent = c()
winrate = c()
wincount = c()

#nested loop
for (st1 in stances) { #for some stance 'st1'
  challenger <- append(challenger, rep(st1,2))
  st1_ = ufc[ufc$R_Stance == st1 | ufc$B_Stance == st1,] #set of fights where this stance was present
  for(st2 in stances[!stances==st1]) { #st2=some stance other than st1
    st1_st2 = st1_[st1_$R_Stance==st2 | st1_$B_Stance==st2,] #set of st1 vs st2 stance fights. removes same-stance matchups
    if (nrow(st1_st2) > 0) { #if no matches for st1 vs st2
      st1_st2_st1_wins = nrow(st1_st2[st1_st2$Winner_stance==st1,]) #number of times when st1 beat st2
      st1_st2_winrate = st1_st2_st1_wins / nrow(st1_st2) #decimal ratio of when st1 beats st2
    } else {
      st1_st2_winrate = 0
      st1_st2_st1_wins = 0
    }
    opponent <- append(opponent, st2)
    winrate <- append(winrate, st1_st2_winrate)
    wincount <- append(wincount, st1_st2_st1_wins)
  }
}

q7data <- data.frame ( #form to dataframe for stacked barchart
  challenger = challenger,
  opponent = opponent,
  winrate = winrate,
  wincount = wincount
)

q7 <- ggplot(q7data, aes(fill=opponent, y=winrate, x=challenger)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y='Decimal win rate for challenger', x="Challenger's stance", fill="Opponent's stance") +
  ggtitle("q7: Win ratios of challengers' stances vs other stances") +
  geom_text(aes(label = opponent),colour = "white", size = 3, hjust = 1.2, position = position_dodge(.9)) +
  coord_flip()
q7 #plot dodged

q7.1 <- ggplot(q7data, aes(fill=opponent, y=wincount, x=challenger)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y='Win count for challenger', x="Challenger's stance", fill="Opponent's stance") +
  ggtitle("q7.1: Win counts of challengers' stances vs other stances")
q7.1 #plot dodged

q7.2data <- q7data
for (s in stances) {
  fightcount = nrow(ufc[ufc$R_Stance == ufc$B_Stance & ufc$R_Stance==s,])
  q7.2data=rbind(q7.2data, data.frame(challenger = s,opponent=s, winrate=NaN,wincount=fightcount))
}

q7.2 <- ggplot(q7.2data, aes(x=challenger, y=opponent, fill=winrate)) + #Heatmap
  geom_tile() +
  ggtitle("q7.2: Heatmap for win rate for challenger against opponent") +
  scale_fill_gradientn(colours = c("white", "red", "darkred"))
q7.2

q7.3 <- ggplot(q7.2data, aes(x=challenger, y=opponent, fill=wincount)) + #Heatmap
  scale_fill_gradientn(colours = c("white", "red", "darkred")) +
  geom_tile() +
  ggtitle("q7.3: Heatmap for win counts for challenger against opponent")
q7.3

#8. Which has a greater correlation to finishes - striking accuracy or takedown accuracy?

bstrikeaccuracy = ufc$B_avg_SIG_STR_pct
rstrikeaccuracy = ufc$R_avg_SIG_STR_pct
btkdaccuracy = ufc $B_avg_TD_pct
rtkdaccuracy = ufc $R_avg_TD_pct
bwin = ufc$B_wins / (ufc$B_wins + ufc$B_losses)
rwin = ufc$R_wins / (ufc$R_wins + ufc$R_losses)


q80data <- data.frame (
  stracc =rstrikeaccuracy,
  tkdacc = rtkdaccuracy,
  winrate =rwin
)
q80data <- na.omit(q80data)

q8data <- data.frame (
  stracc = (bstrikeaccuracy + rstrikeaccuracy)/2,
  tkdacc = (btkdaccuracy + rtkdaccuracy)/2,
  winrate = (bwin + rwin) / 2,
  index = rep(1,nrow(ufc))
)
q8data <- na.omit(q8data)


q8 <- ggparcoord(q80data,
                 columns = c(1,2,3),
                 scale = "globalminmax",
                 title = "q8: Striking accuracy, takedown accuracy, and win rate ratios",
                 alphaLines = 0.05) +
  labs(x='Striking and takedown accuracy, win rate',y='Decimal ratio')
q8

q8.1 <- ggplot(q8data, aes(x=stracc, y=tkdacc, col=winrate)) +
  geom_point(aes(color=winrate)) +
  ggtitle("q8.1: Striking accuracy, takedown accuracy, and win rate ratios")
q8.1
q8.2 <- plot_ly(y=q8data$stracc, x=q8data$tkdacc, z=q8data$winrate,
                type='scatter3d',mode='markers', marker =list(color = q8data$winrate, colorscale = c('green', 'red')))
q8.2<- q8.2 %>% layout(title='q8.2: Striking accuracy, takedown accuracy, and win rate ratios', scene=list(xaxis=list(title='Takedown accuracy %'),
                                  yaxis=list(title='Striking accuracy %'),
                                  zaxis=list(title='Win rate %')),
                       annotations=list(x = 1.13,y = 1.05,text='Win rate %',showarrow=FALSE))
q8.2

