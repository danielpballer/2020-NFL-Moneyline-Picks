#Data Cleaning and creating the winners data
cleaning = function(Scores){
  Scores %>% 
  mutate(home_team = case_when(
    home_team == "Packers" ~ "Green Bay Packers",
    home_team == "Football Team" ~ "Washington Football Team",
    home_team == "Bills" ~ "Buffalo Bills",
    home_team == "Falcons" ~ "Atlanta Falcons",
    home_team == "Ravens" ~ "Baltimore Ravens",
    home_team == "Chiefs" ~ "Kansas City Chiefs",
    home_team == "Titans" ~ "Tenessee Titans",
    home_team == "Colts" ~ "Indianapolis Colts",
    home_team == "Bengals" ~ "Cincinnati Bengals",
    home_team == "49ers" ~ "San Francisco 49ers",
    home_team == "Giants" ~ "New York Giants",
    home_team == "Lions" ~ "Detroit Lions",
    home_team == "Steelers" ~ "Pittsburgh Steelers",
    home_team == "Texans" ~ "Houston Texans",
    home_team == "Broncos" ~ "Denver Broncos",
    home_team == "Buccaneers" ~ "Tampa Bay Buccaneers",
    home_team == "Cardinals" ~ "Arizona Cardinals",
    home_team == "Bears" ~ "Chicago Bears",
    home_team == "Panthers" ~ "Carolina Panthers",
    home_team == "Eagles" ~ "Philadelphia Eagles",
    home_team == "Jets" ~ "New York Jets",
    home_team == "Vikings" ~ "Minnesota Vikings",
    home_team == "Dolphins" ~ "Miami Dolphins",
    home_team == "Jaguars" ~ "Jacksonville Jaguars",
    home_team == "Browns" ~ "Cleveland Browns",
    home_team == "Chargers" ~ "Los Angeles Chargers",
    home_team == "Seahawks" ~ "Seattle Seahawks",
    home_team == "Cowboys" ~ "Dallas Cowboys",
    home_team == "Patriots" ~ "New England Patriots",
    home_team == "Saints" ~ "New Orleans Saints",
    home_team == "Raiders" ~ "Oakland Raiders",
    home_team == "Rams" ~ "Los Angeles Rams")) %>%
  mutate(away_team = case_when(
    away_team == "Packers" ~ "Green Bay Packers",
    away_team == "Redskins" ~ "Washington Redskins",
    away_team == "Bills" ~ "Buffalo Bills",
    away_team == "Falcons" ~ "Atlanta Falcons",
    away_team == "Ravens" ~ "Baltimore Ravens",
    away_team == "Chiefs" ~ "Kansas City Chiefs",
    away_team == "Titans" ~ "Tenessee Titans",
    away_team == "Colts" ~ "Indianapolis Colts",
    away_team == "Bengals" ~ "Cincinnati Bengals",
    away_team == "49ers" ~ "San Francisco 49ers",
    away_team == "Giants" ~ "New York Giants",
    away_team == "Lions" ~ "Detroit Lions",
    away_team == "Steelers" ~ "Pittsburgh Steelers",
    away_team == "Texans" ~ "Houston Texans",
    away_team == "Broncos" ~ "Denver Broncos",
    away_team == "Buccaneers" ~ "Tampa Bay Buccaneers",
    away_team == "Cardinals" ~ "Arizona Cardinals",
    away_team == "Bears" ~ "Chicago Bears",
    away_team == "Panthers" ~ "Carolina Panthers",
    away_team == "Eagles" ~ "Philadelphia Eagles",
    away_team == "Jets" ~ "New York Jets",
    away_team == "Vikings" ~ "Minnesota Vikings",
    away_team == "Dolphins" ~ "Miami Dolphins",
    away_team == "Jaguars" ~ "Jacksonville Jaguars",
    away_team == "Browns" ~ "Cleveland Browns",
    away_team == "Chargers" ~ "Los Angeles Chargers",
    away_team == "Seahawks" ~ "Seattle Seahawks",
    away_team == "Cowboys" ~ "Dallas Cowboys",
    away_team == "Patriots" ~ "New England Patriots",
    away_team == "Saints" ~ "New Orleans Saints",
    away_team == "Raiders" ~ "Oakland Raiders",
    away_team == "Rams" ~ "Los Angeles Rams")) %>%
  mutate(winner = case_when(home_score > away_score ~ home_team,
                            home_score == away_score ~ "TIE",
                            home_score < away_score ~ away_team)) 
}

#Creating a list of winners for each week

weekly_winners = function(weeks){
  Scores %>% select(week, winner) %>% 
    filter(week == weeks) %>%
    select(winner)
}

#Creating a list of how many games each week.

week_number_games = function(weeks){
  dim(winners[[weeks]])[1]
}

#needs work, getting cbs wins per week
CBS_weekly = function(winners, cbs){
  CBS_winners = if_else(winners[[weeks]] == CBS_tot[[weeks]], 1, 0)
}

#Creating a list of everyones picks for each week
games_fn = function(x) {janitor::clean_names(x) %>%
    select(starts_with("game"))}  #Creating the set of just games

#Sorting function to get the prediction for each game and number for and against during 1 week.   
sorting = function(x) {t_x <- sort(table(x), decreasing=TRUE) # get the frequencies and sort them in decreasing order
list(Prediction=names(t_x)[1], # name of the value with highest frequency
     votes_for=t_x[1], # highest frequency
     votes_against=t_x[2]) # Lowest Freq
}

#function to appy the sorting function to each week and create a list of predictions for each week.
pred_table_fn = function(y){data.frame(Game=colnames(y), # apply sorting to each column
                                       t(sapply(y,sorting))) %>% 
    # turning NA in votes against column into 0
    mutate(votes_against = ifelse(is.na(votes_against)==T, 0, as.integer(votes_against))) %>% 
    #choosing prediction or tie if no prediction
    mutate(Prediction = ifelse(as.numeric(votes_for) == as.numeric(votes_against), "Tie", as.character(Prediction))) 
}

#function to add who won to the predictions
adding_winners = function(x,y){
  x %>% add_column(y)
}

#Function to get weekly results for instructors
results_fn = function(x,y){
  x %>% select(Prediction, Winner = winner, votes_for, votes_against) %>% 
    mutate(Correct = ifelse(Prediction =="Tie", "--", ifelse(Prediction == Winner, "Yes", "No"))) %>%
    mutate(`Correct Votes` = ifelse(Prediction == Winner, votes_for, ifelse(Winner == "TIE", 0, votes_against))) %>% 
    mutate(`Correct Percent` = round(as.numeric(`Correct Votes`)/(dim(inst.picks[[length(inst.picks)]])[1]),4)) %>%
    add_column(Game = 1:y) %>% 
    select(Game, Prediction, Winner, Correct, `Correct Votes`, `Correct Percent`)
}  

#Function to get weekly results for cadets
c_results_fn = function(x,y){
  x %>% select(Prediction, Winner = winner, votes_for, votes_against) %>% 
    mutate(Correct = ifelse(Prediction =="Tie", "--", ifelse(Prediction == Winner, "Yes", "No"))) %>%
    mutate(`Correct Votes` = ifelse(Prediction == Winner, votes_for, ifelse(Winner == "TIE", 0, votes_against))) %>% 
    mutate(`Correct Percent` = round(as.numeric(`Correct Votes`)/(dim(cdt.picks[[length(cdt.picks)]])[1]),4)) %>%
    add_column(Game = 1:y) %>% 
    select(Game, Prediction, Winner, Correct, `Correct Votes`, `Correct Percent`)
}  

#Function to get weekly results for combined picks
comb_results_fn = function(x,y){
  x %>% select(Prediction, Winner = winner, votes_for, votes_against) %>% 
    mutate(Correct = ifelse(Prediction =="Tie", "--", ifelse(Prediction == Winner, "Yes", "No"))) %>%
    mutate(`Correct Votes` = ifelse(Prediction == Winner, votes_for, ifelse(Winner == "TIE", 0, votes_against))) %>% 
    mutate(`Correct Percent` = round(as.numeric(`Correct Votes`)/(dim(inst.picks[[length(inst.picks)]])[1]+dim(cdt.picks[[length(cdt.picks)]])[1]),4)) %>%
    add_column(Game = 1:y) %>% 
    select(Game, Prediction, Winner, Correct, `Correct Votes`, `Correct Percent`)
}  

#function to pick how many games correct, incorrect, and not picked
weekly_group_correct_fn = function(x){
  count(x,Correct)
  
}

#function to calculate the number of games picked each week
weekly_games_picked_fn = function(x, y){
  (y-ifelse(identical(x$n[which(x$Correct == "--")], integer(0))==TRUE, 0,
            x$n[which(x$Correct == "--")]))
}

#Function to get how many correct picks each week
weekly_group_correct_picks_fn = function(x){
  x$n[which(x$Correct == "Yes")]
}

#Function to calculate the win percentage each week
weekly_win_percentage_fn = function(x,y){
  round(x/y,4)
}

# Creating a function to calculate weekly correct picks 
indiv_weekly_pred = function(x,y,z){
  indiv = x %>%
    select(Name, starts_with("Game")) 
  
  games_wk = indiv %>%
    select(-Name)
  
  indiv_correct_wk = NULL
  help = NULL
  for (i in 1:length(indiv$Name)){
    for(j in 1:length(games_wk)){
      help[j] = if_else(games_wk[i,j]==y[j,1],1,0)
      indiv_correct_wk[i] = sum(help)
    }
  }
  
  week_num = glue("Week {z}")
  season_wk = indiv %>%
    select(Name) %>%
    add_column(!!week_num := indiv_correct_wk)
  
  return(season_wk)
}

#function to get individual percentages for each week.
indiv_percent = function(x,y){
  x %>% mutate(x[,2]/y)
}

#function to combine cadet and inst picks
comb_picks_fn = function(x,y){
  bind_rows(x,y)
}

#function for matching cadet and instructor picks.
matched_fn = function(x,y){
  x %>% add_column(C_Pred = y$Prediction) %>% 
    mutate(Prediction = ifelse(Prediction == C_Pred, Prediction, "--"),
           Correct = ifelse(Prediction == C_Pred, Correct, "--")) %>% 
    select(Game, Prediction, Correct)
}

#function to get the matched percent correct each week.  
matched_percent_fn = function(x){
  Yes = x %>% 
    count(Correct) %>% 
    filter(Correct =="Yes") %>% 
    select(n) %>% 
    pull()
  
  No = x %>% 
    count(Correct) %>% 
    filter(Correct =="No") %>% 
    select(n) %>% 
    pull()
  
  return(round(Yes/(Yes+No),4))
}
