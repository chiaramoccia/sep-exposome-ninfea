library(robustHD)
library(SpectralClMixed)
library(foreign)
library(nnet)
library(stargazer)
library(DescTools)

# Remove rows with any missing data from the dataset
dataset=na.omit(dataset)
# Remove the variable 'cohab_0' from the dataset
dataset=subset(dataset,select = -cohab_0)

# Initialize variables for loop
i=NULL
class=NULL
dataset1=NULL
dataset2=NULL
# Loop through each column of dataset
for (i in 1:dim(dataset)[2]) {
  print(class(dataset[,i]))
  # Factors are left unchanged
  if (class(dataset[,i])=="factor"){
  }
  # Standardize numeric columns: subtract mean and divide by sd
  else if (class(dataset[,i])=="numeric"){
    w=standardize(dataset[,i],centerFun = mean, scaleFun = sd)
    dataset[,i]=as.numeric(w)
  }
}

# Set random seed for reproducibility
set.seed(1)
res=NULL
# for (i in 2:10) {
#   res[[i]]=mspec(dataset, k = i,starts=100)
#   
# }
# Perform spectral clustering on the dataset with 3 clusters
# 'starts' controls the number of random initializations, 'its' is max iterations
res=mspec(dataset, k = 3,starts=100,its = 1000)
summary(res)
table(res$cluster)#579 410 451



#-------------------- code for k parameter tuning
# res=mspec(dataset, k = 2,starts=100)
# summary(res)
# prop.table(table(inc_cat[res$cluster==2]))
# # The most effective category weight is: 
# #   0.55
# # 0.55
# # The between divided by within sum of squares is: 
# #   3.42731661121272
# # The total within sum of squares is: 
# #   135.014733854885
# # cluster_size
# # 1   2 
# # 821 619 
# 
# 
# res1=mspec(dataset, k = 3,starts=100)
# ww=res$cluster
# 
# table(ww,res1$cluster)
# 
# table(res$cluster)
# # The most effective category weight is: 
# #   1e-04
# # The between divided by within sum of squares is: 
# #   1.98725245664062
# # The total within sum of squares is: 
# #   293.290531770813
# # cluster_size
# # 1   2   3 
# # 579 451 410 
# 
# res=mspec(dataset, k = 4,starts=100)
# table(res$cluster)
# summary(res)
# # The most effective category weight is: 
# #   1e-04
# # The between divided by within sum of squares is: 
# #   1.48141326040469
# # The total within sum of squares is: 
# #   412.495226601487
# # cluster_size
# # 1   2   3   4 
# # 366 341 367 366  
# 
# 
# res=mspec(dataset, k = 5,starts=100)
# table(res$cluster)
# summary(res)
# # The most effective category weight is: 
# #   1e-04
# # The between divided by within sum of squares is: 
# #   1.24010258332289
# # The total within sum of squares is: 
# #   494.600979731748
# # cluster_size
# # 1   2   3   4   5 
# # 303 264 341 258 274 
# 
# res=mspec(dataset, k = 6,starts=100)
# table(res$cluster)
# summary(res)
# # The most effective category weight is: 
# #   0.9999
# # The between divided by within sum of squares is: 
# #   1.17034305182647
# # The total within sum of squares is: 
# #   552.464247066011
# # cluster_size
# # 1   2   3   4   5   6 
# # 309 214 221 215 238 243
# 
# 
# 
# res=mspec(dataset, k = 7,starts=100)
# table(res$cluster)
# summary(res)
# ww=res$cluster
# # The most effective category weight is: 
# #   0.9999
# # The between divided by within sum of squares is: 
# #   1.22983477492894
# # The total within sum of squares is: 
# #   548.27246388124
# # cluster_size
# # 1   2   3   4   5   6   7 
# # 215 216 139 204 191 277 198  
# #within sum of squares
# x=c(135.014733854885,293.290531770813, 412.495226601487,494.600979731748,552.464247066011,548.27246388124)
# y=c(2,3,4,5,6,7)
# plot(y,x)
# x=c(3.42731661121272,1.98725245664062,1.48141326040469,1.24010258332289,1.17034305182647,1.22983477492894)
# y=c(2,3,4,5,6,7)
# plot(y,x)
# The Sum of Squares Method
# clustering validation method to select the optimal number 
# of cluster by minimizing the within-cluster sum of squares 
# (a measure of how tight each cluster is) and maximizing the between-cluster 
# sum of squares (a measure of how seperated each cluster is from the others).

income_dic=NULL
income_dic[inc_cat=="high"]="high"
income_dic[inc_cat=="medium" | inc_cat=="low"]="low"
levels(income_dic)<-c("high","low")
income_dic=as.data.frame(income_dic)
rownames(income_dic)=rownames(covariate)
income_dic=income_dic[rownames(dataset),]
dataset$income_dic=income_dic
covariate=covariate[rownames(dataset),]
# Add 'income_dic' and covariate variables to dataset
dataset1=cbind(dataset,covariate)


dataset=dataset1
# Add cluster membership as a factor variable to dataset
dataset$cluster=res$cluster
class(dataset$cluster)
table(dataset$cluster)
dataset$cluster=as.factor(dataset$cluster)

# Re-label cluster factor levels based on cluster sizes to ensure consistent labeling
if (table(dataset$cluster)[1]==451 & table(dataset$cluster)[2]==579 & table(dataset$cluster)[3]==410) {
  levels(dataset$cluster)=c("1","2","3")
} else if (table(dataset$cluster)[1]==451 & table(dataset$cluster)[2]==410 & table(dataset$cluster)[3]==579) {
  levels(dataset$cluster)=c("1","3","2")
} else if (table(dataset$cluster)[1]==579 & table(dataset$cluster)[2]==410 & table(dataset$cluster)[3]==451) {
  levels(dataset$cluster)=c("2","3","1")
} else if (table(dataset$cluster)[1]==579 & table(dataset$cluster)[2]==451 & table(dataset$cluster)[3]==410) {
  levels(dataset$cluster)=c("2","1","3")
}else if (table(dataset$cluster)[1]==410 & table(dataset$cluster)[2]==451 & table(dataset$cluster)[3]==579) {
  levels(dataset$cluster)=c("3","1","2")
}else {
  levels(dataset$cluster)=c("3","2","1")
}
# Set cluster factor levels explicitly
dataset$cluster=factor(dataset$cluster,levels = c("1","2","3"))


# Set cluster "1" as reference for multinomial logistic regression
dataset$cluster = relevel(dataset$cluster, ref = "1")
# Fit multinomial logistic regression with cluster as response and income category + some covariates as predictors
multi1 = multinom(dataset$cluster ~ income_dic+covariate$abroad_mo+covariate$parity3+covariate$agebirth_m_y, data=dataset)
summary(multi1)
# Output regression results to an HTML file
stargazer(multi1, type="text", out="multi1.htm")
# Calculate and exponentiate odds ratios and confidence intervals for each cluster vs reference
exp(cbind(OR = coef(multi1)[1,], confint(multi1)[,,1]))
exp(cbind(OR = coef(multi1)[2,], confint(multi1)[,,2]))
# Calculate pseudo R-squared for model fit quality
PseudoR2(multi1, which = NULL)




## Code for Fig. 3
A=NULL
i=NULL
for (i in 1:dim(dataset)[2]-5){
  print(class(dataset[,i]))
  
  if (class(dataset[,i])=="factor"){
    a=table(dataset[,i])[2]/dim(dataset)[1]#46 si smoke/1714 prevalenza di esposti in tutta la pop
    b=subset(dataset,dataset$cluster=="1")
    b1=table(b[,i])[2]/dim(b)[1]-a#prop di individui si-propo individui si in pop
    b=subset(dataset,dataset$cluster=="2")
    b2=table(b[,i])[2]/dim(b)[1]-a
    b=subset(dataset,dataset$cluster=="3")
    b3=table(b[,i])[2]/dim(b)[1]-a
    A[[i]]=cbind(a,b1,b2,b3,class(dataset[,i]))
  }
  else if (class(dataset[,i])=="numeric"){
    dataset[,i]=as.numeric(scale(dataset[,i]))
    a=mean(dataset[,i])
    b=subset(dataset,dataset$cluster=="1")
    b1=mean(b[,i])-a
    b=subset(dataset,dataset$cluster=="2")
    b2=mean(b[,i])-a
    b=subset(dataset,dataset$cluster=="3")
    b3=mean(b[,i])-a
    A[[i]]=cbind(a,b1,b2,b3,class(dataset[,i]))
  }
}
df <- data.frame(matrix(unlist(A), nrow=length(A), byrow=TRUE))
df$X1=as.numeric(df$X1)
df$X2=as.numeric(df$X2)
df$X3=as.numeric(df$X3)
df$X4=as.numeric(df$X4)
str(df)
names(df)=c("pop","cluster1","cluster2","cluster3","classe")
df$variabili=names(dataset)[1:43]
df_cont=subset(df,df$classe=="numeric")
df_dic=subset(df,df$classe=="factor")
library(reshape2)
#Specify id.vars: the variables to keep but not split apart on
df_cont$classe=NULL
long=melt(df_cont)

long=subset(long,long$variable!="pop")
long$variabili <- c("a_vegetables" ,         "b_fruits" ,             "c_dairy" ,     "d_fish" ,               "e_meat" ,              
                    "f_eggs" ,               "g_grain" ,     "h_pulses" ,             "i_potatoes" ,           "l_humidity" ,          
                    "m_temperature" ,        "n_UV" ,      "o_surface temperature" ,"p_No2" ,                "q_Nox" ,               
                    "r_PM10" ,               "s_PM25" ,               "t_PMcoarse" ,           "u_PM25absorbance" ,     "v_night noise" ,           
                    "z_distance road" ,      "za_buildings" ,   "zb_facilities" ,"zc_connectivity" , "zd_shannon" ,     
                    "ze_walkability" ,        "zf_NDVI" ,               "zg_bus lines" ,          "zh_bus stops" ,   
                    "a_vegetables" ,         "b_fruits" ,             "c_dairy" ,     "d_fish" ,               "e_meat" ,              
                    "f_eggs" ,               "g_grain" ,     "h_pulses" ,             "i_potatoes" ,           "l_humidity" ,          
                    "m_temperature" ,        "n_UV" ,      "o_surface temperature" ,"p_No2" ,                "q_Nox" ,               
                    "r_PM10" ,               "s_PM25" ,               "t_PMcoarse" ,           "u_PM25absorbance" ,     "v_night noise" ,           
                    "z_distance road" ,      "za_buildings" ,   "zb_facilities" ,"zc_connectivity" , "zd_shannon" ,     
                    "ze_walkability" ,        "zf_NDVI" ,               "zg_bus lines" ,          "zh_bus stops" ,         "a_vegetables" ,         "b_fruits" ,             "c_dairy" ,     "d_fish" ,               "e_meat" ,              
                    "f_eggs" ,               "g_grain" ,     "h_pulses" ,             "i_potatoes" ,           "l_humidity" ,          
                    "m_temperature" ,        "n_UV" ,      "o_surface temperature" ,"p_No2" ,                "q_Nox" ,               
                    "r_PM10" ,               "s_PM25" ,               "t_PMcoarse" ,           "u_PM25absorbance" ,     "v_night noise" ,           
                    "z_distance road" ,      "za_buildings" ,   "zb_facilities" ,"zc_connectivity" , "zd_shannon" ,     
                    "ze_walkability" ,        "zf_NDVI" ,               "zg_bus lines" ,          "zh_bus stops"  )


ggplot(long, aes(x=value, y=variabili,fill=variable)) +
  theme_bw()+
  geom_bar(position="dodge",stat="identity") +
  scale_fill_manual(values=c("#040302","#F5AC4B","#4B94F5"))+
  facet_grid(variabili ~ ., scales = "free", space = "free") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(size = 9,angle = 0),
        strip.background=element_rect(fill = "light grey",colour = "light grey"),
        panel.border = element_rect(fill = NA,colour = "light grey"))

ggplot(long, aes(x=value, y=variabili,fill=variable)) +
  geom_bar(position="dodge",stat="identity") +
  facet_grid(rows = vars(variabili)) +
  theme(strip.text.y = element_text(angle = 0))

df_dic$classe=NULL
long=melt(df_dic)

long=subset(long,long$variable!="pop")
long$variabili=c("a_pets",                     "b_smoke",                      
                 "d_childcare",                "e_tv",                       "f_breastfeeding",           
                 "g_sweet beverages",          "h_sugar products",           "i_solid food",              
                 "l_traffic major load",       "m_blue",              "n_green",            
                 "o_population density",       "p_unhealty food facilities", 
                 
                 "a_pets",                     "b_smoke",                       
                 "d_childcare",                "e_tv",                       "f_breastfeeding",           
                 "g_sweet beverages",          "h_sugar products",           "i_solid food",              
                 "l_traffic major load",       "m_blue",              "n_green",            
                 "o_population density",       "p_unhealty food facilities",
                 
                 "a_pets",                     "b_smoke",                        
                 "d_childcare",                "e_tv",                       "f_breastfeeding",           
                 "g_sweet beverages",          "h_sugar products",           "i_solid food",              
                 "l_traffic major load",       "m_blue",              "n_green",            
                 "o_population density",       "p_unhealty food facilities")


ggplot(long, aes(x=value, y=variabili,fill=variable)) +
  theme_bw()+
  geom_bar(position="dodge",stat="identity") +
  scale_fill_manual(values=c("#040302","#F5AC4B","#4B94F5"))+
  facet_grid(variabili ~ ., scales = "free", space = "free") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(size = 6,angle = 0),
        strip.background=element_rect(fill = "light grey",colour = "light grey"),
        panel.border = element_rect(fill = NA,colour = "light grey"))