dotatraindata<-read.csv(file="dota2Train.csv",header=F)
View(dotatraindata)
v <- c(111,115,125,139,146,157,164,172,189,193,205,214,232,243,252,262)
dotatraindata[, 2] <- findInterval(dotatraindata[, 2], v)
#将地区分类
attach(dotatraindata)
library(ggplot2)
counts1<-table(V1)
counts1
counts2<-table(V1,V2)
barplot(counts2,main = "Cluster ID  Effect Bar Plot",xlab="Cluster ID",ylab="Frequency",legend=rownames(counts2),beside=TRUE)
library(vcd)
ggplot(data=dotatraindata,aes(x=V1,fill=V3))+geom_bar()+facet_wrap(~V3,nrow=3)+ labs(title="Game mode Bar Plot",x="Game mode",y="Frequency")
ggplot(data=dotatraindata,aes(x=V1,fill=V4))+geom_bar()+facet_grid(V4~.)+
    labs(title="Game type Bar Plot",x="Game type",y="Frequency")
library(gmodels)
CrossTable(V1,V4)
Mytable1<-xtabs(~V3+V4+V1,data=dotatraindata)
Mytable1
counts3<-table(V5)
counts3
counts4<-table(V1,V5)
barplot(counts4,main = "Hero1 Effect Bar Plot",
        xlab="Hero1",ylab="Frequency",legend=rownames(counts4))
mytable2<-xtabs(~V1+V5,data=dotatraindata)
mytable2
prop.table(mytable2,2)

#查看每个英雄的胜率和使用频率的关系
lapply(dotatraindata[,c(5:117)], unique)
dotatraindatatotal<-dotatraindata[,c(-28,-112)]
countf<-function(x){
    count<-list()
    p<-vector()
    q<-vector()
    for(i in 1:111){
        count<-table(x[,1],x[,i+4])
        q[i]<-count[1,1]+count[2,1]+count[1,3]+count[2,3]
        p[i]<-(count[1,1]+count[2,3])/q[i]
    }
    l<-cbind(p,q)
    return(l)
    
}
wp_count<-countf(dotatraindatatotal)
wp_count
wp_count<-data.frame(wp_count)
herolag<-c( "antimage","axe",
            "bane","bloodseeker",
            "crystal_maiden","drow_ranger",
            "earthshaker","juggernaut",
            "mirana","morphling",         
            "nevermore","phantom_lancer",     
            "puck","pudge",              
            "razor","sand_king",          
            "storm_spirit","sven",               
            "tiny","vengefulspirit",   
            "windrunner","zuus",               
            "kunkka",        
            "lina","lion",               
            "shadow_shaman", "slardar",        
            "tidehunter","witch_doctor",      
            "lich","riki",               
            "enigma","tinker",             
            "sniper","necrolyte",          
            "warlock","beastmaster",        
            "queenofpain","venomancer",         
            "faceless_void","skeleton_king",      
            "death_prophet","phantom_assassin",   
            "pugna","templar_assassin",   
            "viper","luna",               
            "dragon_knight","dazzle",             
            "rattletrap","leshrac",            
            "furion","life_stealer",       
            "dark_seer","clinkz",             
            "omniknight","enchantress",        
            "huskar","night_stalker",      
            "broodmother","bounty_hunter",      
            "weaver","jakiro",             
            "batrider","chen",               
            "spectre","ancient_apparition", 
            "doom_bringer","ursa",               
            "spirit_breaker","gyrocopter",         
            "alchemist","invoker",            
            "silencer","obsidian_destroyer", 
            "lycan","brewmaster",         
            "shadow_demon","lone_druid",         
            "chaos_knight","meepo",              
            "treant","ogre_magi",          
            "undying","rubick",             
            "disruptor","nyx_assassin",       
            "naga_siren","keeper_of_the_light",
            "wisp","visage",             
            "slark","medusa",             
            "troll_warlord","centaur",            
            "magnataur","shredder",           
            "bristleback","tusk",               
            "skywrath_mage","abaddon",            
            "elder_titan","legion_commander",   
            "techies","ember_spirit",       
            "earth_spirit",  
            "terrorblade","phoenix",            
            "oracle","winter_wyvern",      
            "arc_warden")
wp_count<-cbind(herolag,wp_count)
wp_count<-wp_count[order(-wp_count$p),]
wp_countsub<-wp_count[c(seq(1,10)),]
wp_countsub<-wp_countsub[order(wp_countsub$q),]
View(wp_countsub)
library(ggplot2)
ggplot(wp_countsub,aes(x=reorder(herolag,p),y=p,fill=q))+geom_bar(stat = "identity")

######################第二种游戏类型下的使用频率和胜率
dotatraindatatype2<-dotatraindata[dotatraindata$V4==2,]
lapply(dotatraindatatype2[,c(5:117)], unique)
dotatraindatatype2<-dotatraindatatype2[,c(-28,-112)]
countf<-function(x){
    count<-list()
    p<-vector()
    q<-vector()
    for(i in 1:111){
        count<-table(x[,1],x[,i+4])
        q[i]<-count[1,1]+count[2,1]+count[1,3]+count[2,3]
        p[i]<-(count[1,1]+count[2,3])/q[i]
    }
    l<-cbind(p,q)
    return(l)
    
}
wp_counttype2<-countf(dotatraindatatype2)
wp_counttype2
wp_counttype2<-data.frame(wp_counttype2)
herolag<-c( "antimage","axe",
            "bane","bloodseeker",
            "crystal_maiden","drow_ranger",
            "earthshaker","juggernaut",
            "mirana","morphling",         
            "nevermore","phantom_lancer",     
            "puck","pudge",              
            "razor","sand_king",          
            "storm_spirit","sven",               
            "tiny","vengefulspirit",   
            "windrunner","zuus",               
            "kunkka",        
            "lina","lion",               
            "shadow_shaman", "slardar",        
            "tidehunter","witch_doctor",      
            "lich","riki",               
            "enigma","tinker",             
            "sniper","necrolyte",          
            "warlock","beastmaster",        
            "queenofpain","venomancer",         
            "faceless_void","skeleton_king",      
            "death_prophet","phantom_assassin",   
            "pugna","templar_assassin",   
            "viper","luna",               
            "dragon_knight","dazzle",             
            "rattletrap","leshrac",            
            "furion","life_stealer",       
            "dark_seer","clinkz",             
            "omniknight","enchantress",        
            "huskar","night_stalker",      
            "broodmother","bounty_hunter",      
            "weaver","jakiro",             
            "batrider","chen",               
            "spectre","ancient_apparition", 
            "doom_bringer","ursa",               
            "spirit_breaker","gyrocopter",         
            "alchemist","invoker",            
            "silencer","obsidian_destroyer", 
            "lycan","brewmaster",         
            "shadow_demon","lone_druid",         
            "chaos_knight","meepo",              
            "treant","ogre_magi",          
            "undying","rubick",             
            "disruptor","nyx_assassin",       
            "naga_siren","keeper_of_the_light",
            "wisp","visage",             
            "slark","medusa",             
            "troll_warlord","centaur",            
            "magnataur","shredder",           
            "bristleback","tusk",               
            "skywrath_mage","abaddon",            
            "elder_titan","legion_commander",   
            "techies","ember_spirit",       
            "earth_spirit",  
            "terrorblade","phoenix",            
            "oracle","winter_wyvern",      
            "arc_warden")
wp_counttype2<-cbind(herolag,wp_counttype2)
wp_counttype2prank<-wp_counttype2[order(-wp_counttype2$p),]
wp_counttype2sub<-wp_counttype2prank[c(seq(1,10)),]
ggplot(wp_counttype2sub,aes(x=reorder(herolag,p),y=p))+geom_bar(stat = "identity")
wp_counttype2qrank<-wp_counttype2[order(-wp_counttype2$q),]
View(wp_counttype2qrank)
wp_counttype2sub2<-wp_counttype2qrank[c(seq(1,10)),]
ggplot(wp_counttype2sub2,aes(x=reorder(herolag,q),y=q))+geom_bar(stat = "identity")
######################第三种游戏类型下的使用频率和胜率
dotatraindatatype3<-dotatraindata[dotatraindata$V4==3,]
lapply(dotatraindatatype3[,c(5:117)], unique)
dotatraindatatype3<-dotatraindatatype3[,c(-28,-112)]
countf<-function(x){
    count<-list()
    p<-vector()
    q<-vector()
    for(i in 1:111){
        count<-table(x[,1],x[,i+4])
        q[i]<-count[1,1]+count[2,1]+count[1,3]+count[2,3]
        p[i]<-(count[1,1]+count[2,3])/q[i]
    }
    l<-cbind(p,q)
    return(l)
    
}
wp_counttype3<-countf(dotatraindatatype3)
wp_counttype3
wp_counttype3<-data.frame(wp_counttype3)
herolag<-c( "antimage","axe",
            "bane","bloodseeker",
            "crystal_maiden","drow_ranger",
            "earthshaker","juggernaut",
            "mirana","morphling",         
            "nevermore","phantom_lancer",     
            "puck","pudge",              
            "razor","sand_king",          
            "storm_spirit","sven",               
            "tiny","vengefulspirit",   
            "windrunner","zuus",               
            "kunkka",        
            "lina","lion",               
            "shadow_shaman", "slardar",        
            "tidehunter","witch_doctor",      
            "lich","riki",               
            "enigma","tinker",             
            "sniper","necrolyte",          
            "warlock","beastmaster",        
            "queenofpain","venomancer",         
            "faceless_void","skeleton_king",      
            "death_prophet","phantom_assassin",   
            "pugna","templar_assassin",   
            "viper","luna",               
            "dragon_knight","dazzle",             
            "rattletrap","leshrac",            
            "furion","life_stealer",       
            "dark_seer","clinkz",             
            "omniknight","enchantress",        
            "huskar","night_stalker",      
            "broodmother","bounty_hunter",      
            "weaver","jakiro",             
            "batrider","chen",               
            "spectre","ancient_apparition", 
            "doom_bringer","ursa",               
            "spirit_breaker","gyrocopter",         
            "alchemist","invoker",            
            "silencer","obsidian_destroyer", 
            "lycan","brewmaster",         
            "shadow_demon","lone_druid",         
            "chaos_knight","meepo",              
            "treant","ogre_magi",          
            "undying","rubick",             
            "disruptor","nyx_assassin",       
            "naga_siren","keeper_of_the_light",
            "wisp","visage",             
            "slark","medusa",             
            "troll_warlord","centaur",            
            "magnataur","shredder",           
            "bristleback","tusk",               
            "skywrath_mage","abaddon",            
            "elder_titan","legion_commander",   
            "techies","ember_spirit",       
            "earth_spirit",  
            "terrorblade","phoenix",            
            "oracle","winter_wyvern",      
            "arc_warden")
wp_counttype3<-cbind(herolag,wp_counttype3)
View(wp_counttype3)
wp_counttype3prank<-wp_counttype3[order(-wp_counttype3$p),]
wp_counttype3sub<-wp_counttype3prank[c(seq(1,10)),]
ggplot(wp_counttype3sub,aes(x=reorder(herolag,p),y=p))+geom_bar(stat = "identity")
wp_counttype3qrank<-wp_counttype3[order(-wp_counttype3$q),]
wp_counttype3sub2<-wp_counttype3qrank[c(seq(1,10)),]
ggplot(wp_counttype3sub2,aes(x=reorder(herolag,q),y=q))+geom_bar(stat = "identity")

dotatraindata1<-dotatraindata[dotatraindata$V1==1,]
lapply(dotatraindata1[,c(5:117)], unique)
dotatraindata1<-dotatraindata1[,c(-28,-112)]
#去除没有被选择过的英雄变量
herocount<-lapply(dotatraindata1[,c(5:115)], table)
colnames(herocount)[1]<-c("Pick")
herocount<-herocount[,c(1,seq(from=2,to=222,by=2))]
which.max(herocount[1,])
#计算出赢的情况下敌方使用的最频繁的英雄位置（hero9）
which.max(herocount[3,])
#计算出赢的情况下我方使用的最频繁的英雄位置（hero9）

dotatraindata2<-dotatraindata[dotatraindata$V1==-1,]
lapply(dotatraindata2[,c(5:117)], unique)
dotatraindata2<-dotatraindata2[,c(-28,-112)]
#去除没有被选择过的英雄变量
herocount2<-lapply(dotatraindata2[,c(5:115)], table)
colnames(herocount2)[1]<-c("Pick")
herocount2<-herocount2[,c(1,seq(from=2,to=222,by=2))]
which.max(herocount2[1,])
#计算出输的情况下敌方使用的最频繁的英雄位置（hero9）
which.max(herocount2[3,])
#计算出输的情况下我方使用的最频繁的英雄位置（hero44）



library("pROC")
library("kknn") 
library("MASS")
library("class")
library("e1071")
#导入数据
data_train<- read.csv("dota2Train.csv", header = F, stringsAsFactors = T)
data_test<- read.csv("dota2Test.csv", header = F, stringsAsFactors = T)
#去除0值
data_train<- data_train[, c(-28,-112)]
data_test<- data_test[, c(-28, -112)]
#将地区分类
v <- c(111,115,125,139,146,157,164,172,189,193,205,214,232,243,252,262)
data_train[, 2] <- findInterval(data_train[, 2], v)
data_test[, 2] <- findInterval(data_test[, 2], v)
#转为因子
for (i in 1:115) {
    data_train[, i] <- as.factor(data_train[, i])
}
for (i in 1:115) {
    data_test[, i] <- as.factor(data_test[, i])
}
#####SVM
sv<-svm(V1~.,data=data_train)
sv_pre<- predict(sv,data_test,probability = T)
table(sv_pre, real)
sv_roc<- roc(real, sv_pre) 
plot(dec_roc, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, 
     grid = c(0.1, 0.2), grid.col = c("green", "red"), max.auc.polygon = TRUE,  
     auc.polygon.col = "skyblue", print.thres = TRUE, xlab = "特异度", 
     ylab = "灵敏度", main = "SVM结果")
####KNN
knn.pre<- knn(data_train[, -1], data_test[, -1], data_train[, 1], 1)
table(knn.pre, data_test[, 1])
#k=2
knn.pre<- knn(data_train[, -1], data_test[, -1], data_train[, 1], 2)
table(knn.pre, data_test[, 1])
#kknn
fit_kknn = kknn(V1~.,data_train,data_test[,-1],k=1)  
fit = fitted(fit_kknn)  
table(data_test[,1],fit)


rm(list = ls())
data1 <- read.csv("dota2Train.csv", header = T)
testdata<- read.csv("dota2Test.csv", header = T)
names(data1)
###将英雄对应的变量名重新命名
for (i in 1:113) {
    names(data1)[4+i] <- paste("hero",i,sep = "")
}

###讲其余的变量也重新命名
names(data1)[1] <- "results"
names(data1)[2] <- "id"
names(data1)[3] <- "mode"
names(data1)[4] <- "type"

##将因变量类型转为因子型
class(data1$results)
data1$results <- factor(data1$results, levels = c(-1, 1), labels = c("failure", "victory"))
summary(data1$results)

###讲地区变为因子型
class(data1$id)
data1$region <- 1
data1$region[data1$id == 121 | data1$id == 122 | data1$id == 123 | data1$id == 124] <- 2
data1$region[data1$id == 131 | data1$id == 132 | data1$id == 133 | data1$id == 134 | data1$id == 135 |
                 data1$id == 136 | data1$id == 137 | data1$id == 138] <- 3
data1$region[data1$id == 144 | data1$id == 145] <- 4
data1$region[data1$id == 151 | data1$id == 152 | data1$id == 153 | data1$id == 154 | data1$id == 155 |
                 data1$id == 156] <- 5
data1$region[data1$id == 161 | data1$id == 223 | data1$id == 224 | data1$id == 225 | data1$id == 227 |
                 data1$id == 231 ]<- 6
data1$region[data1$id == 171] <- 7
data1$region[data1$id == 181 | data1$id == 182 | data1$id == 183 | data1$id == 184 | data1$id == 185 |
                 data1$id == 186 | data1$id == 187 | data1$id == 188] <- 8
data1$region[data1$id == 191 | data1$id == 192] <- 9
data1$region[data1$id == 204] <- 10
data1$region[data1$id == 211 | data1$id == 212 | data1$id == 213] <- 11
data1$region[data1$id == 232] <- 12
data1$region[data1$id == 241] <- 13
data1$region[data1$id == 251] <- 14
data1$region[data1$id == 261] <- 15

data1$region <- factor(data1$region, levels = c(1:15), labels = c("US West", "US East", "Europe West", "South Korea",
                                                                  "Southeast Asia", "China", "Australia", "Russia", "Europe East", "South America", "South Africa",
                                                                  "12", "Chile", "Peru", "India"))
summary(data1$region)
###将mode转为因子型
data1$mode <- factor(data1$mode, levels = c(1:9), c("All Pick", "Captains Mode", "Random Draft", "Single Draft",
                                                    "All Random", "INTRO/DEATH", "The Diretide", 
                                                    "Reverse Captains Mode", "Greeviling"))
data1$type <- factor(data1$type, levels = c(1:3), labels = c("Practice", "Tournament", "Tutorial"))

##将这些英雄的使用状况因子化
for (i in 1:113) {
    data1[ ,4 + i]<- factor(data1[ ,4 + i], levels = c(-1, 0, 1), labels = c("usebyopponent", "none", "usebyus"))
}



############################################################################
######构建逻辑回归模型，基于多种惩罚项，对比结果,随机分成4:1的训练集和测试集

data2 <- data1[, -2]
model1 <- glm(results ~ ., data = data2, family = binomial(link = "logit"))
##由于模型报错，说明有个变量取值一样对于所有观测值，因此通过下面列表来查找
check<- list()
for (i in 1:113) {
    check[[i]] <- summary(data2[, 3+i])
}
check

##检查发现第27和111变量从来没有队伍用过，因此这里删掉这两个变量
data2 <- data2[, -c(27, 111)]

##构建分割点函数
cutoff<- function(n,p){
    pp<-1
    i<-0
    while (pp>=0.02) {
        model.predfu<-rep("failure",n)
        model.predfu[model4.prob > 0.2 + i*0.001]<-"victory"
        pp<- abs(p-sum(model.predfu=="failure")/n)
        i<-i+1
    }
    cut<-0.2+(i-1)*0.001
    return(cut)
}

###first one: 先构建所有变量的全部模型，按照训练集和测试集的比例划分

train<- sample(1:nrow(data2), 2 * nrow(data2)/3)
test<- (-train)
model1 <- glm(results ~ ., data = data2[train, ], family = binomial(link = "logit"))
summary(model1)

model1.prob <- predict(model1, newdata = data2[test, ], type = "response")
contrasts(data2$results)

summary(data2[train, ]$results)
43867/92649

cutoff(61766, 29212/61766)
model1.pred <- rep("failure", 30883)
model1.pred[model1.prob > 0.516] <- "victory"

table(model1.pred, data2[test,]$results)
##画ROC曲线
rocplot<- function(pred, truth, ...){
    predob<- prediction(pred, truth)
    #打印AUc
    perf.auc<- performance(predob, measure = 'auc', x.measure = 'cutoff')
    #
    perf<- performance(predob, 'tpr','fpr')
    df<- data.frame(x = attributes(perf)$x.values[[1]],y = attributes(perf)$y.values[[1]])  
    p    <- ggplot(data = df)
    p + geom_line(aes(x,y),colour = "yellowgreen",size = 1) + 
        geom_ribbon(aes(x,ymin = 0,ymax = y),fill = alpha("yellowgreen",0.5)) +
        labs(title = paste("ROC Curve & AUC:",(perf.auc@y.values))) + 
        xlab("Specificity") +
        ylab("Sensitivity") +
        theme(plot.title = element_text(size = 17)) 
}

rocplot((model1.prob), data2[test, ]$results)

###second one: 结合LASSO

library(glmnet)
xmatrix<- model.matrix(results~.,data = data2)
class(xmatrix)
xmatrix<- xmatrix[, -1]
train<- sample(1:nrow(xmatrix), 2*nrow(xmatrix)/3)
test<- (-train)
model2 <- glmnet(xmatrix[train, ], data2[train, ]$results, family = "binomial", alpha = 1)
print(model2)


cv.out<- cv.glmnet(xmatrix[train, ], data2$results[train], alpha = 1, family = "binomial")
plot(cv.out)
bestlam<- cv.out$lambda.min

model2.prob <- predict(model2, s = 0.009, newx = xmatrix[test, ], type = "response")
model2.pred<-rep("failure",30883)
summary(data2[train,]$results)
cutoff(61766, 29186/61766)
model2.pred[model2.prob> 0.523]<-"victory"
table(model2.pred, data2$results[test])
coef(model2, s=0.009)


rocplot((model2.prob),data2[test,]$results)

###third one: 结合SCAD
library(ncvreg)
train<- sample(1:nrow(xmatrix), 2*nrow(xmatrix)/3)
test<- (-train)
cv.out1 <- cv.ncvreg(xmatrix[train, ], data2$results[train], nfolds = 5, family = "binomial", penalty = "SCAD")
summary(cv.out1)
print(cv.out1)

model3 <- ncvreg(xmatrix[train, ], data2[train, ]$results, family = "binomial", penalty = "SCAD")
summary(model3)
print(model3)


tt<- model3$beta[,17:19]
model3.prob <- predict(model3, xmatrix[test, ], type = "response")[,16]
model3.pred<-rep("failure",30883)
summary(data2[train,]$results)
cutoff(61766, 29137/61766)
model3.pred[model3.prob> 0.514]<-"victory"
table(model3.pred, data2$results[test])
rocplot((model3.prob),data2[test,]$results)

###third one: 结合mcp
library(ncvreg)
train<- sample(1:nrow(xmatrix), 2*nrow(xmatrix)/3)
test<- (-train)
cv.out2 <- cv.ncvreg(xmatrix[train, ], data2$results[train], nfolds = 5, family = "binomial", penalty = "MCP")
summary(cv.out2)
print(cv.out2)

model4 <- ncvreg(xmatrix[train, ], data2[train, ]$results, family = "binomial", penalty = "MCP")
summary(model3)
print(model4)
sum(model4$beta[,15] != 0)
tt<- model4$beta[,15:16]

model4.prob <- predict(model4, xmatrix[test, ], type = "response")[,15]
model4.pred<-rep("failure",30883)
summary(data2[train,]$results)
cutoff(61766, 29195/61766)
model4.pred[model4.prob> 0.517]<-"victory"
table(model4.pred, data2$results[test])
rocplot((model4.prob),data2[test,]$results)