#setting working directory
setwd("C:/Users/moghe/Desktop/Prob/fifa-world-cup-2022-prediction-main/data")

#installing other packages
install.packages('dplyr')
library('dplyr')

#reading data
df_historical_data=read.csv('FIFA_WorldCup_Matches.csv',TRUE,",")
df_roundOf16=read.csv('RoundOf16.csv',TRUE,",")

#to display all rows
options(max.print=100000)

#displaying data
print(df_historical_data)
print(df_roundOf16)

#Team Strength Calculation
#separating data of home and away teams
# 'select' will store only those columns whose number is given
df_home = select(df_historical_data,1,4,5)
df_away = select(df_historical_data,2,4,5)

#renaming columns
#because goalsScored is actually conceded by away team
colnames(df_home)=c("Team","GoalsScored","GoalsConceded")
colnames(df_away)=c("Team","GoalsConceded","GoalsScored")

print(df_home)

#Row Binding
df_binded = rbind(df_home,df_away)

#Team strength
#average of goals scored and conceded by team
df_team_strength=df_binded%>%group_by(Team)%>%
  summarise(GoalScored = mean(GoalsScored),
            GoalConceded = mean(GoalsConceded))

#displaying data
print(df_team_strength)

#Points Prediction
predict_Winner<-function(home,away)
{
  #getting indices of countries
  # 'which' returns index from dataframe
  HomeRowIndex=which(df_team_strength$Team==home)
  AwayRowIndex=which(df_team_strength$Team==away)
  
  #getting their scored goals average in home and away
  scored=as.double(df_team_strength[HomeRowIndex,2])
  lamb_home = as.double(scored)
  
  scored=as.double(df_team_strength[AwayRowIndex,2])
  lamb_away = as.double(scored)
  
  prob_draw = prob_home = prob_away = 0
  
  for(x in 0:11){
    for(y in 0:11){
      p = dpois(x, lamb_home) * dpois(y, lamb_away)
      
      if (x == y){
        prob_draw = p + prob_draw
      }
      else if (x > y){
        prob_home = p+prob_home
      }
      else{
        prob_away = p+prob_away
      }
    }
  }
  
  points_home = 3 * prob_home + prob_draw
  points_away = 3 * prob_away + prob_draw
  
  #winner of match based on points
  if(points_home>points_away)
    return(home)
  else
    return(away)
}

#round of 16
for(x in 1:8)
{
  if(x==1)
    df_quarterFinal=NULL
  
  df_quarterFinal=rbind(df_quarterFinal,
                        c(predict_Winner(df_roundOf16[x,1],df_roundOf16[x,2])))
}

print(df_quarterFinal)

#quarter finals
for(x in seq(from=1, to=8, by=2))
{
  if(x==1)
    df_semiFinal=NULL
  
  df_semiFinal=rbind(df_semiFinal,
                        c(predict_Winner(df_quarterFinal[x,1],df_quarterFinal[x+1,1])))
}
print(df_semiFinal)

#Semifinals
for(x in seq(from=1, to=4, by=2))
{
  if(x==1)
    df_Final=NULL
  
  df_Final=rbind(df_Final,
                     c(predict_Winner(df_semiFinal[x,1],df_semiFinal[x+1,1])))
}
print(df_Final)

#Finals
Winner=predict_Winner(df_Final[1,1],df_Final[2,1])
print(Winner)