

# Examples of Code I have written 
# In the 2020-2021 timeframe 


dataRange = function(data, currentdate,lookBackdate, type=c('raw', 'range')[1]){
  lookBackdate = as.Date(lookBackdate)
  currentdate = as.Date(currentdate)
  if(type == 'raw') 
    return(data <- data %>% filter(date == currentdate)%>% 
             mutate(DAYS = as.Date(currentdate) - as.Date(lookBackdate),
                    countperDAYS = count/as.numeric(DAYS),
                    rate = count/base, 
                    rateper100k = 100000 * rate,
                    dailyCaseperExpect = 100000 * countperDAYS/base))
  if(type == 'range') 
    return(data <- data %>% filter(date >= lookBackdate & date <= as.Date(currentdate)) 
           %>% group_by(GROUP) %>% 
             mutate(diffcount = max(count) - min(count),
                    DAYS = as.Date(currentdate) - as.Date(lookBackdate),
                    countperDAYS = diffcount/as.numeric(DAYS),
                    rate = diffcount/base,
                    rateper100k = 100000 * rate, 
                    dailyCaseperExpect = 100000 * countperDAYS/base))
  
}

# Examples
dataRange(newdat, currentdate = "2020-05-18", lookBackdate  = "2020-03-09", type = 'raw')
dataRange(newdat, currentdate = "2020-07-01", lookBackdate  = "2020-05-18", type = 'range')

rollingCorr = function(thedata, min, max,xvar, yvar){
  plotData <- NULL
  xvar = as.name(xvar)
  yvar = as.name(yvar)
  for (intervalLength in(seq(7,35,by=7))) {
    min <- ymd(min)
    max <- ymd(max)
    totalDays <- 1+max-min
    for(i in 1:(totalDays-intervalLength)){
      tmp <- dataRange(thedata, currentdate =  min+i+intervalLength, lookBackdate  = min+i, type = 'range')
      ddRslt <- as_tibble(plyr::ddply(tmp, "boro", summarise, corr= cor(!!xvar, !!yvar, use = "na.or.complete"))) %>% mutate(midDate= min+round(i+intervalLength/2),intervalLength=intervalLength)
      plotData <- rbind(plotData,ddRslt)
    }
  }
  return(plotData)
}

intervalPlotbyBoro = function(intervalData,timeInterval, firstvar, secondvar){
  newIntervaldata = intervalData %>% filter(intervalLength == timeInterval)
  ggplot(newIntervaldata) +
    aes(midDate, corr, color = boro)+ 
    geom_smooth() + 
    labs(title = paste("Corr between",firstvar, "and", secondvar, sep = " "), 
         caption = paste(timeInterval, "day period",sep = " ")) + 
    xlab("date") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    facet_wrap(.~boro,nrow=2,ncol=3)
}

removeBigdiff <- function(data,count.col.num, threshold.upper, threshold.lower){
  diff = c()
  for(i in 1:nrow(data)){
    diff[i] <- data[i,count.col.num] - data[i - 1,count.col.num]
  }
  data <- cbind(data,diff)
  data <- subset(data, diff < threshold.upper & diff > threshold.lower)
  return(data)
}


for(i in 1:length(zipcode)){
  a[[i]] <- data %>% filter(aVariable == zipcode[i]) %>% 
    select(covidVariables)
  for(j in 1:length(a)){
    b[[j]] <- removeBigdiff(data = a[[j]], count.col.num =  2, # Bad way to index looking back on this 2020 code but a quick solution
                            threshold.upper  = 10, threshold.lower = -10000)
  }
}

data.removed <- merge_recurse(b)

sd.pool <- function(a, b) {
  top = var(a) * (length(a)- 1) + var(b) * (length(b) - 1) bottom = (length(a) + length(b)) - 2
  var.pooled = (top/bottom) * (1/length(a) + 1/length(b)) return(var.pooled)
}

u.stat = function(data){
  hare.vec = c()
  tort.vec = c()
  vector.of.one <- as.vector(data[,1]) vector.of.two <- as.vector(data[,2]) for(i in 1:nrow(data)){
    hare.vec[i] <- sum(vector.of.one[i] < data[,2])
    tort.vec[i] <- sum(vector.of.two[i] < data[,1])
  }
  stat.one = sum(hare.vec)
  stat.two = sum(tort.vec)
  return(list(stat.one,stat.two))
}


negbin = function(N,p,r) { results = rep(0, N) for(i in 1:N){
  x=0
  k=0
  while(x < r){
    x = x + sample(c(0,1), size = 1, replace = TRUE, prob = c(1-p, p))
    k=k+1 }
  results[i] = k
  } 
return(results) 
}


hypertwo = function(N,m,r,k) {
  x = rep(0, N)
  tagged = rep(1,r)
  nottagged = rep(0,(m-r))
  pop = append(tagged,nottagged) for(i in 1:N){
    x[i] = sum(sample(pop,k))
    }
  return(x) 
}

# Data Wrangling 
years <- c('2020advSeason.csv','2019advSeason.csv', 
           '2018advSeason.csv','2017advSeason.csv',
           '2016advSeason.csv','2015advSeason.csv',
           '2014advSeason.csv','2013advSeason.csv',
           '2012advSeason.csv','2011advSeason.csv' )

yearCol <- c("2020", "2019", 
             "2018", "2017", 
             "2016", "2015", 
             "2014", "2013", 
             "2012", "2011")



dataList <- dataList.ply <- vector("list", length(years))

for(i in 1:length(dataList)){
  dataList[[i]] <- read_csv(years[i])
  dataList[[i]] <- dataList[[i]][-nrow(dataList[[i]]),-c(18,23,28)] # Remove legaue avge and blank rows
  dataList[[i]] <- dataList[[i]] %>% row_to_names(row_number = 1)
  dataList[[i]]$Year <-  yearCol[i]
  
}

teamLevel <- as.data.frame(do.call(rbind, dataList))

cats <- c("Team", "Arena", "Year")

names(teamLevel)[18] <- 'oFFeFG'
names(teamLevel)[22] <- 'deFFeFG'
names(teamLevel)[19] <- 'oFFTOV'
names(teamLevel)[23] <- 'deFFTOV'
names(teamLevel)[21] <- 'oFF_FT_FGA'
names(teamLevel)[25] <- 'deFF_FT_FG'

teamLevel$Team <-gsub("\\*","",teamLevel$Team)

write.csv(teamLevel,"teamLevel.csv", row.names = FALSE)

years.ply <- c('2020advPlayer.csv','2019advPlayer.csv', 
               '2018advPlayer.csv','2017advPlayer.csv',
               '2016advPlayer.csv','2015advPlayer.csv',
               '2014advPlayer.csv','2013advPlayer.csv',
               '2012advPlayer.csv','2011advPlayer.csv' )


for(i in 1:length(dataList.ply)){
  dataList.ply[[i]] <- read_csv(years.ply[i])
  dataList.ply[[i]]$Year <-  yearCol[i]
  
}

playerLevel <- as.data.frame(do.call(rbind, dataList.ply))
write.csv(playerLevel,"playerLevel.csv", row.names = FALSE)

# Vizuialitions 
data %>% recipe(variable~.) %>%
  step_dummy(variable, one_hot = T) %>%
  prep() %>%
  bake(data) %>%
  group_by(variable, variable) %>% summarise(oneHotvariables, mean)

dataDum %>%
  gather(Variable, Value, -c(SomeVars)) %>%
  fitler(Variable %in% someVars ) %>% 
  ggplot(.) + 
  aes(vars, vars, group = vars, color = vars, fill = vars) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~.Variable) + 
  theme_bw()


teamLevel <- read.csv('teamLevel.csv')
cats <- c('Team', "Arena", "Year")
nums <- c('Attend.', 'Attend..G')
varss <- names(teamLevel)[-c(1,2,26,27,28, 29)]
teamNames <- unique(teamLevel$Team)
plotList <- vector("list", length(teamNames))
names(plotList) <- teamNames 
teamLevel <- teamLevel  %>%
  mutate_each_(funs(factor(.)),cats )

for(i in 1:length(plotList)){
  plotList[[i]] <- melt(teamLevel) %>%
    filter(Team == teamNames[i]) %>%
    ggplot(.) + 
    aes(value)+ 
    geom_histogram() +
    facet_wrap(~variable + Team, scales = "free_x")
  
}
plotList

ggplot(teamLevel) + 
  aes(Year, W, color = Team, group = Team, fill = Team) + 
  geom_point() + geom_line() + facet_wrap(~Team)

teamLevel %>% group_by(Year) %>%
  summarise_at(.vars = varss ,
               .funs = mean) %>%
  gather(Variable, value, -Year) %>%
  ggplot(.) + 
  aes(Year, value) + 
  geom_point() +  
  geom_line() +
  facet_wrap(~Variable, scales = "free_x")

teamLevel %>% group_by(Team, Year) %>%
  summarise_at(.vars = varss ,
               .funs = mean) %>%
  gather(Variable, value, -c(Team, Year)) %>%
  ggplot(.) + 
  aes(Year, value, group = Team, color = Team, Fill = Team) + 
  geom_point() +  
  geom_line() +
  facet_wrap(~Variable, scales = "free_x")

statsbyYear <- teamLevel %>% group_by(Year) %>%
  summarise_at(.vars = varss ,
               .funs = c(Mean="mean", Sd="sd"))
statsbyTeam <- teamLevel %>% group_by(Team) %>%
  summarise_at(.vars = varss ,
               .funs = c(Mean="mean", Sd="sd"))
statsByTeamAndYear <- teamLevel %>% group_by(Team, Year) %>%
  summarise_at(.vars = varss ,
               .funs = c(Mean="mean", Sd="sd"))
statsbyYear
statsbyTeam
statsByTeamAndYear


# Workflow referenced from Julia Silge that I used 
# for my ML projects 

split <- initial_split(data, strata = var)
train <- training(split)
test <- testing(split)

tree_rec <- recipe(Var~., data = train)

tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)


tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)

trees_folds <- vfold_cv(trees_train)

doParallel::registerDoParallel()

tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


# Grid Search 
rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  levels = 5
)

regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(legal_status ~ .,
      data = juice(tree_prep) %>% select(-tree_id)
  ) %>%
  vip(geom = "point")


final_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(trees_split)

final_res %>%
  collect_metrics()

# Niave way to see training/testing accuary over mtrys 
trys <- 1:100
trainVec <- testVec <- vector('list', length(trys))

for(i in trys){
  mod <- randomForest(var~., data = train, mtry = i, importance = T)
  trainVec[[i]]  <- confusionMatrix(predict(mod, train), var)$overall["Accuaracy"]
  testVec[[i]]  <- confusionMatrix(predict(mod, test), var)$overall["Accuaracy"]
}

training <- as.data.frame(unlist(trainVec))
test <- as.data.frame(unlist(testVec))
data <- cbind(training,testing,trys)
colnames(data) <- c("train", 'test', "index")

ggplot(data) +
  aes(index, train) + 
  geom_line() +
  geom_line(data = data, aes(x = index, y = test), color = "red") +
  scale_x_continuous(breaks = pretty(data$index, n = 99))


# Rf Simulation 

iter <- 1000
imp <- imp2 <- list
rslt <- vector('list', length(varNames))
names(rslt) <- varNames

for(i in 1:iter){
  mod <- randomForest(var~., data = train, mtry = bestMtry, importance = T) # bestMtry you get my looking at the graph above 
  imp <- as.data.frame(randomForest::importance(mod, type = 1))
  trainVec[[i]]  <- confusionMatrix(predict(mod, train), var)$overall["Accuaracy"]
  testVec[[i]]  <- confusionMatrix(predict(mod, test), var)$overall["Accuaracy"]
  for(j in 1:length(rslt)){
    rslt[[j]][i] <- imp[[i]] %>% filter(rownames == varNames[j]) 
  }
}





# Xgboost # Workflow referenced from Julia Silge that I used 
# for my ML projects 

vb_split <- initial_split(vb_df, strata = variable)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     
  sample_size = tune(), mtry = tune(),         
  learn_rate = tune(),                        
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), vb_train),
  learn_rate(),
  size = 30
)

xgb_wf <- workflow() %>%
  add_formula(outComevar~ .) %>%
  add_model(xgb_spec)



vb_folds <- vfold_cv(vb_train, strata = outComevar)


doParallel::registerDoParallel()
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# Best Model
show_best(xgb_res, "roc_auc")

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)


final_xgb %>%
  fit(data = vb_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")


# Bayesian Net

teamLevel <- read.csv('teamLevel.csv')

cats <- c('Team', "Arena", "Year")
nums <- c('Attend.', 'Attend..G')

varss <- names(teamLevel)[-c(1,2,26,27,28, 29)]

teamLevel <- teamLevel  %>%
  mutate_each_(funs(factor(.)),cats )

for(i in 2:25){
  teamLevel[,i] <- as.numeric(teamLevel[,i])
}
teamLevel$Rk <- as.numeric(teamLevel$Rk)
copy <- teamLevel
copy <- na.omit(copy)

bg <- hc(copy)

viewer(bg,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Team Level Network",
       bayesianNetwork.subtitle = "First Network",
       bayesianNetwork.footer = "Fig. 1"
)

fitted <- bn.fit(bg, data)
fitted$outcomeVariable # to see condtional probs

# Pathway derived from rf, xgBoost, and vizualiation
# Now we can measure the stength of the pathway
model <- "outcomeVar ~ var1 + var2 + var3"
fit <- sem(model, data)
summary(fit, standardized = T, fit.measures = T, rsq = T)



# Casual inf - Pscores 
log.res <- glm(treat~., family="binomial", data = df)
p_scores <- predict(log.res, df, type = 'response')
df <- cbind(df, p_scores)

matcher <- function(pop_df, samp_df){
  treat <- 'treat'
  matched <- 'matched'
  
  
  matches <- arm::matching(z=samp_df[treat], score=p_scores, replace = T)
  samp_df[matched] <- matches$cnts
  
  matched_df <- samp_df %>% filter(matched > 0 )
  
  data <- list(pop_df, samp_df, matched_df)
  names(data) <- c('Population', "Sample", "Matched")
  
  contin <- c("integer", "numeric")
  
  loopNames <- colnames(data$Sample)
  notLoop <- c("treat","matched", "p_scores")
  loopNames <- loopNames[! loopNames %in% notLoop] 
  
  mean.pre.treat <- mean.pre.control <- mean.match.treat <- vector("list", length(loopNames))
  mean.match.control <- stdBias.pre <- stdBias.match <- vector("list", length(loopNames))
  sd.pre.treat <- sd.pre.control <- sd.pre.ratio <-  sd.match.treat <-  vector("list", length(loopNames))
  sd.match.control <- sd.match.ratio <-  vector("list", length(loopNames))
  
  names(mean.pre.treat) <- names(mean.pre.control) <- names(mean.match.treat) <- loopNames
  names(mean.match.control) <- names(stdBias.match) <- names(stdBias.pre) <- loopNames
  names(sd.pre.treat) <- names(sd.pre.control) <- names(sd.pre.ratio) <- loopNames
  names(sd.match.treat) <- names(sd.match.control) <- names(sd.match.ratio) <-  loopNames
  
  wt <- data$Matched %>% filter(treat == 0) %>% pull(matched)
  
  for(i in 1:length(loopNames)){
    mean.pre.treat[[i]] <- mean(data$Sample  %>% filter(treat == 1) %>% pull(loopNames[i]))
    mean.pre.control[[i]] <- mean(data$Sample  %>% filter(treat == 0) %>% pull(loopNames[i]))
    mean.match.treat[[i]] <- mean(data$Matched  %>% filter(treat == 1) %>% pull(loopNames[i]))
    mean.match.control[[i]] <- weighted.mean(data$Matched  %>% filter(treat == 0) %>% pull(loopNames[i]), wt)
    
    sd.pre.treat[[i]] <- sd(data$Population  %>% filter(treat == 1) %>% pull(loopNames[i]))
    sd.pre.control[[i]] <- sd(data$Sample  %>% filter(treat == 0) %>% pull(loopNames[i]))
    sd.pre.ratio[[i]] <- sd.pre.control[[i]]/sd.pre.treat[[i]]
    
    if(max(data$Sample %>% pull(loopNames[i])) > 1){
      stdBias.pre[[i]] <-  (mean.pre.treat[[i]] - mean.pre.control[[i]])/sd(data$Population %>% filter(treat == 1) %>% pull(loopNames[i]))
      stdBias.match[[i]] <- weighted.mean(data$Matched %>% pull(loopNames[i]), data$Matched  %>% pull(matched))/var(data$Matched %>% filter(treat == 1) %>% pull(loopNames[i])) 
      
      sd.match.treat[[i]] <- sqrt(var(data$Matched %>% filter(treat == 1) %>% pull(loopNames[i])))
      sd.match.control[[i]] <- sqrt(Hmisc::wtd.var(data$Matched %>% filter(treat == 0) %>% pull(loopNames[i]), data$Matched %>% filter(treat == 0) %>% pull(matched)))
      sd.match.ratio[[i]] <- sd.match.control[[i]]/sd.match.treat[[i]]
      
    } else {
      stdBias.pre[[i]] <- mean.pre.treat[[i]] - mean.pre.control[[i]]
      stdBias.match[[i]] <- mean.match.treat[[i]] - mean.match.control[[i]]
      
      sd.pre.ratio[[i]] <- 0
      sd.match.ratio[[i]] <- 0 
    }
  }
  
  pre.match.treatment.group.mean  <- unlist(mean.pre.treat) 
  pre.match.control.group.mean <- unlist(mean.pre.control)
  matched.treatment.group <-  unlist(mean.match.treat)
  matched.control.group <- unlist(mean.match.control)
  pre.match.mean.difference <- unlist(stdBias.pre)
  matched.mean.difference  <- unlist(stdBias.match)
  Ratio.STD.prematch.groups <- unlist(sd.pre.ratio)
  Ratio.STD.match.groups  <- unlist(sd.match.ratio)
  
  
  pTable <- as.data.frame(cbind(pre.match.treatment.group.mean, pre.match.control.group.mean,
                                matched.treatment.group,matched.control.group,
                                pre.match.mean.difference,matched.mean.difference,
                                Ratio.STD.prematch.groups,Ratio.STD.match.groups))
  
  pTable <- cbind(Variable = rownames(pTable ), pTable )
  rownames(pTable) <- 1:nrow(pTable)
  pTable$Variable <- as.factor(pTable$Variable)
  
  final_data <- list(pTable,data$Sample, data$Matched)
  names(final_data) <- c('pTable', 'Sample', 'Matched')
  return(final_data)
}

matching_set <- matcher(pop_df, samp_df)


# Getting P score with rf 
# Workflow referenced from Julia Silge that I used 
# for my ML projects 

df$treat = as.factor(df$treat)
trees_split <- initial_split(df)
flow  <- recipe(treat ~ ., data = df)
tree_prep <- prep(flow )
juiced <- juice(tree_prep)


tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")


tune_wf <- workflow() %>%
  add_recipe(flow ) %>%
  add_model(tune_spec)


doParallel::registerDoParallel()


trees_folds <- vfold_cv(df)

rf_grid <- grid_regular(
  mtry(range = c(1, 5)),
  min_n(range = c(1, 3)),
  levels = 2
)


regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)


best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(treat ~ .,
      data = df
  ) %>%
  vip(geom = "point")


final_wf <- workflow() %>%
  add_recipe(flow) %>%
  add_model(final_rf)


final_res <- final_wf %>%
  last_fit(trees_split)

final_res %>%
  collect_metrics()


# Paramaters found from the rf worflow 
rfMod <- randomForest(as.factor(treat) ~ st99 + bw + booze + preterm, data = df, mtry = 5, importance = T, keep.forest=TRUE)
probs <- as.data.frame(predict(rfMod, df, type = "prob"))
names(probs) <- c("zero", 'one')
p_scores <- as.data.frame(probs$one)
p_scores <- p_scores %>% transmute(p_scores = probs$one)
df <- cbind(df,p_scores)
matching_set <- matcher(hw4, df)

# Logistic regression 
log.res <- glm(as.factor(treat)~ st99 + bw + I(log(bw)) + I(bw^2) + booze + preterm, family="binomial", data = df)
p_scores <- predict(log.res, df, type = 'response')
df <- cbind(df, p_scores)
matching_set <- matcher(hw4, df)


# GAM
mod_gam1 <- mgcv::gam(treat ~ s(bw, bs="cr") + st99  + booze + preterm, data=df)
p_scores <- abs(predict(mod_gam1, df, type = 'response'))
df <- cbind(df, p_scores)
matching_set <- matcher(hw4, df)

# IPTW
log.res <- glm(treat~., family="binomial", data = df)
p_scores <- predict(log.res, df, type = 'response')
df <- cbind(df, p_scores)
df <- df %>% mutate(IPTW = ifelse(treat == 1, p_scores,(p_scores/(1-p_scores))))



matcherRewrite <- function(df, pop.df,treat,wt){
  #treat <- treat
  
  loopNames <- colnames(df)
  notLoop <- c("treat","matched", "p_scores", 'IPTW')
  loopNames <- loopNames[! loopNames %in% notLoop] 
  
  f <- sapply(df, max) > 1
  contin <- f[f == TRUE]
  binary  <- f[f == FALSE]
  
  continNames <- names(contin)
  binaryNames <- names(binary)
  
  continNames <- continNames[! continNames %in% notLoop] 
  binaryNames <- binaryNames[! binaryNames %in% notLoop] 
  
  preMatchmeans <- df %>% group_by(treat) %>% summarise_at(loopNames, mean, na.rm = TRUE)
  preMatchmeans <- preMatchmeans[,loopNames ]
  
  Matchedmeans <- df %>% group_by(treat) %>% summarise_all(funs(weighted.mean(., get(wt))))
  Matchedmeans <- Matchedmeans[,loopNames ]
  
  # Std bias pre match
  # Pre match Treat STD
  preMatchstdtreat <- pop.df %>% group_by(treat) %>% summarise_at(loopNames, sd, na.rm = TRUE)
  preMatchstdtreat <- preMatchstdtreat[2,loopNames ]
  # Pre match Control 
  preMatchstdCon <- df %>% group_by(treat) %>% summarise_at(loopNames, sd, na.rm = TRUE)
  preMatchstdCon <- preMatchstdCon[1, loopNames]
  
  # Std Bias Pre
  stdBias.pre <- (preMatchmeans[2, continNames] - preMatchmeans[1, continNames])/preMatchstdtreat[,continNames]
  stdBias.pre.temp <- (preMatchmeans[2, binaryNames] - preMatchmeans[1, binaryNames])
  stdBias.pre <- append(stdBias.pre,stdBias.pre.temp)
  
  
  # Std bias match
  stdBias.match.temp <- df %>% summarise_all(funs(weighted.mean(., get(wt))))/df %>% filter(treat == 1) %>% summarise_all(funs(weighted.var(., get(wt))))
  stdBias.match.con <- stdBias.match.temp[,continNames]
  
  stdBias.match.bin  <- Matchedmeans[2, binaryNames] - Matchedmeans[1, binaryNames]
  stdBias.match <- append(stdBias.match.con, stdBias.match.bin)
  
  # Std Ratio 
  # Std Ratio PreMatch
  stdpreRatio <- preMatchstdCon/preMatchstdtreat
  stdpreRatio[, binaryNames] <- 0
  
  # Std Ratio  match
  sd.match.treat <- sqrt(df %>% filter(treat == 1) %>% summarise_all(funs(weighted.var(., get(wt)))))
  sd.match.treat <- sqrt(df %>% filter(treat == 1) %>% summarise_at(loopNames, var, na.rm = TRUE))
  
  sd.match.control <- sqrt(df %>% filter(treat == 0) %>% summarise_all(funs(Hmisc::wtd.var(., get(wt)))))
  sd.match.control <- sd.match.control[, loopNames]
  
  
  ratio.match <- sd.match.control/sd.match.treat
  ratio.match[, binaryNames] <- 0
  
  
  preMeans <- as.data.frame(t(preMatchmeans))
  names(preMeans) <- c("mn0", "mn1")
  preMeans <- cbind(Variable = rownames(preMeans), preMeans )
  rownames(preMeans) <- 1:nrow(preMeans)
  
  matchMeans <- as.data.frame(t(Matchedmeans))
  names(matchMeans) <- c("mn0.m", "mn1.m")
  matchMeans<- cbind(Variable = rownames(matchMeans), matchMeans )
  rownames(matchMeans) <- 1:nrow(matchMeans)
  
  diff.pre <- as.data.frame(unlist(stdBias.pre))
  names(diff.pre) <- 'diff'
  diff.pre <- cbind(Variable = rownames(diff.pre), diff.pre )
  rownames(diff.pre) <- 1:nrow(diff.pre)
  
  diff.m <- as.data.frame(unlist(stdBias.match))
  names(diff.m) <- 'diff.m'
  diff.m  <- cbind(Variable = rownames(diff.m ), diff.m  )
  rownames(diff.m ) <- 1:nrow(diff.m)
  
  ratio <- as.data.frame(t(stdpreRatio))
  names(ratio) <- 'ratio'
  ratio   <- cbind(Variable = rownames(ratio  ), ratio  )
  rownames(ratio ) <- 1:nrow(ratio )
  
  ratio.m <- as.data.frame(t(ratio.match))
  names(ratio.m) <- 'ratio.m'
  ratio.m <- cbind(Variable = rownames(ratio.m  ), ratio.m  )
  rownames(ratio.m) <- 1:nrow(ratio.m )
  
  
  pTable <- plyr::join_all(list(matchMeans, preMeans, diff.pre,diff.m, ratio, ratio.m ), by='Variable', type='left')
  
  return(pTable)
  
}

tab <- matcherRewrite(df,hw4, treat, 'IPTW')
