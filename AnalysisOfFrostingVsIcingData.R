# analysis of frosting vs. icing data
# 
# This is an analysis of the data on the different between frosting and icing I 
# collected just for fun/out of curiosity. I thought I'd make the data and code 
# for analysis avalible in case anyone was interested in looking over it or 
# wanted to replicate the study. The results are written up on my blog, Making 
# Noise and Hearing Things. This is a personal project so the commenting/copy
# editing isn't going to be perfect.
# 
# If you have any questions or comments please contact me at rctatman@uw.edu. 
# Thanks!

# loading in what we need ----
# load in libraries we'll need
library(plyr)
library(data.table)

# read in data
data <- read.csv(file.choose()) # this will open a file selection window. simply choose the correct file
na.omit(data)
head(data) #this should show the first few lines of the file

# refactoring -- this will make the data and analysis easier to read
names(data) <- c("Timestamp", "US","State","Age","Baking","FrostingVsIcing",
                 "Cupcake","Doughnuts","FondantCake","BundtCake","Roll","Comments" )

# plots ----
# some basic plots to see the lay of the land
plot(data$FrostingVsIcing~data$Baking)
plot(data$Cupcake~data$FrostingVsIcing)
plot(data$Doughnuts~data$FrostingVsIcing)
plot(data$FondantCake~data$FrostingVsIcing)
plot(data$BundtCake~data$FrostingVsIcing)
plot(data$Roll~data$FrostingVsIcing)

# so just looking over this,  it does look like people who think they're
# interchangeable are less homogeonous in thier results. People who think
# they're different things seem to have higher rates of agreement

# Role of lexical judgement, knowladge, age ----

# now let's split our data into "different" and "same"
data <- as.matrix(data)
data[data == "Either frosting or icing"] <- "Either"
data <- as.data.frame(data)

different <- data[data$FrostingVsIcing == "Different things",]
same <- data[data$FrostingVsIcing == "Different words for the same thing",]

# does the level of baking knowladge affect lexical categories?
BakingKnowladgeByJudgement <- rbind(same = table(same$Baking), diff = table(different$Baking))
chisq.test(BakingKnowladgeByJudgement) # so it doesn't look like baking knowladge is the key factor here
colnames(BakingKnowladgeByJudgement) <- c("","High", "Medium", "Very Low", "Low")
barplot(prop.table(BakingKnowladgeByJudgement, 1)[,c(2,3,5,4)], beside = T, legend = c("No", "Yes"),
        main = "Difference Between Frosting and Icing? \n By Level of Baking Knowladge", ylim = c(0,1))
# what role does age play?
age <- rbind(same = table(same$Age), diff = table(different$Age))
age <- age[,c(1:4,6)] # exclude over 60 group because there's no one in it
chisq.test(age) # so it doesn't look like age is important either
colnames(age) <- c("21-30","31-40","41-50","51-60","Over 60","Under 21")
barplot(prop.table(age, 1)[,c(5,1,2,3)], beside = T, legend = c("No", "Yes"),
        main = "Difference Between Frosting and Icing? \n By Age of Respondent", ylim = c(0,1))

# how does this play into the proudction judgements?
# cupcake data
cupcake <- rbind(same = table(same$Cupcake), different = table(different$Cupcake))
barplot(prop.table(cupcake, 1), beside=TRUE, legend = row.names(cupcake), main = "Cupcake", ylim = c(0,1))
chisq.test(cupcake) #strong effect!

#Doughnut data
Doughnut <- rbind(same = table(same$Doughnut), diff = table(different$Doughnut))
barplot(prop.table(Doughnut, 1), beside=TRUE, legend = row.names(Doughnut), main = "Doughnuts", ylim = c(0,1))
chisq.test(Doughnut) # no effect

#FondantCake data
FondantCake <- rbind(same = table(same$FondantCake), diff = table(different$FondantCake))
barplot(prop.table(FondantCake, 1), beside=TRUE, legend = row.names(FondantCake), main = "Cake with Fondant", ylim = c(0,1))
chisq.test(FondantCake) # weak effect

#BundtCake data
BundtCake <- rbind(same = table(same$BundtCake), diff = table(different$BundtCake))
barplot(prop.table(BundtCake, 1), beside=TRUE, legend = row.names(BundtCake))
chisq.test(BundtCake) # no effect at p = 0.05

#Roll data
Roll <- rbind(same = table(same$Roll), diff = table(different$Roll))
barplot(prop.table(Roll, 1), beside=TRUE, legend = row.names(Roll))
chisq.test(Roll) # effect

# now let's try looking at all responses
chisq.test(cbind(cupcake,Doughnut,FondantCake,BundtCake,Roll)) 
#strong effect, but I'm not sure this is a good way to ask this wuesiton

# main take-away from this part: some people do have a strong distinction, while
# other's don't. Thier initial judgements are borne out by thier responses to
# the stimuli. Also, different items do get different judgements. 

# mapping data -----

# documentation of the rMaps package can be found here: https://github.com/ramnathv/rMaps
require(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')
library(rMaps)
library(RColorBrewer)
library(ggplot2)

# you need to modify the ichoropleth funciton here because it won't work if
# there's multiples of a value (like a column that reads 1, 1, 2, 3 won't work) so that's what this is
ichoropleth <- function(x, data, pal = "Blues", ncuts = 5, animate = NULL, play = F, map = 'usa', legend = TRUE, labels = TRUE, ...){
  d <- Datamaps$new()
  fml = lattice::latticeParseFormula(x, data = data)
  data = transform(data, 
                   fillKey = cut(
                     fml$left, 
                     unique(quantile(fml$left, seq(0, 1, 1/ncuts))),
                     ordered_result = TRUE
                   )
  )
  fillColors = brewer.pal(ncuts, pal)
  d$set(
    scope = map, 
    fills = as.list(setNames(fillColors, levels(data$fillKey))), 
    legend = legend,
    labels = labels,
    ...
  )
  if (!is.null(animate)){
    range_ = summary(data[[animate]])
    data = dlply(data, animate, function(x){
      y = toJSONArray2(x, json = F)
      names(y) = lapply(y, '[[', fml$right.name)
      return(y)
    })
    d$set(
      bodyattrs = "ng-app ng-controller='rChartsCtrl'"  
    )
    d$addAssets(
      jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
    )
    if (play == T){
      d$setTemplate(chartDiv = sprintf("
                                       <div class='container'>
                                       <button ng-click='animateMap()'>Play</button>
                                       <span ng-bind='year'></span>
                                       <div id='{{chartId}}' class='rChart datamaps'></div>
                                       </div>
                                       <script>
                                       function rChartsCtrl($scope, $timeout){
                                       $scope.year = %s;
                                       $scope.animateMap = function(){
                                       if ($scope.year > %s){
                                       return;
                                       }
                                       map{{chartId}}.updateChoropleth(chartParams.newData[$scope.year]);
                                       $scope.year += 1
                                       $timeout($scope.animateMap, 1000)
                                       }
                                       }
                                       </script>", range_[1], range_[6])
      )
      
    } else {
      d$setTemplate(chartDiv = sprintf("
                                       <div class='container'>
                                       <input id='slider' type='range' min=%s max=%s ng-model='year' width=200>
                                       <span ng-bind='year'></span>
                                       <div id='{{chartId}}' class='rChart datamaps'></div>          
                                       </div>
                                       <script>
                                       function rChartsCtrl($scope){
                                       $scope.year = %s;
                                       $scope.$watch('year', function(newYear){
                                       map{{chartId}}.updateChoropleth(chartParams.newData[newYear]);
                                       })
                                       }
                                       </script>", range_[1], range_[6], range_[1])
      )
    }
    d$set(newData = data, data = data[[1]])
    
  } else {
    d$set(data = dlply(data, fml$right.name))
  }
  return(d)
}

# modify the dataset so that it's the correct format
judgementByState <- as.data.frame.matrix(table(state = data$State, judgement = data$FrostingVsIcing))
judgementByState <- cbind(judgementByState, state = rownames(judgementByState))
judgementByState <- judgementByState[2:dim(judgementByState)[1],]
judgementByState <- cbind(judgementByState, # proportion of those who say they're different
                          prop = judgementByState$`Different things`/
                            (sum(judgementByState$`Different things` + 
                                   judgementByState$`Different words for the same thing` +
                                   judgementByState$`Not sure`)))
judgementByState <- cbind(judgementByState, percentDiff = 100 * judgementByState$prop) #percent

# maps of the data in different configurations
ichoropleth(`Different things` ~ state,data =judgementByState, map = "usa")
ichoropleth(`Different words for the same thing` ~ state,data =judgementByState, map = "usa")
ichoropleth(`Not sure` ~ state,data =judgementByState, map = "usa")
ichoropleth(prop ~ state,data =judgementByState, map = "usa")
# this gives us the percentage of people who say they're different by state
# (darker = more people say they're different)
ichoropleth(percentDiff ~ state,data =judgementByState, map = "usa", legend = F, ncuts = 3)
