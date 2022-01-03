###########################################################################################################
###导入的包################################################################################################
library(haven)
library(dplyr)
library(tidyr)
library(readxl)
library(cdabookdb)
library(vcdExtra)
library(ggplot2)
library(cowplot)

###########################################################################################################
###导入的处理过后的数据####################################################################################
df <- data.frame(read_excel("G:/0RUC/0课件/统计基础/CLHLS/Rcode/trans_depression.xlsx"))
df <- df %>% select(-(...1:id))%>% 
  mutate(depression_score=as.integer(df$depression>=10))%>%
  select(-starts_with("b3"))
dim(df)
###########################################################################################################
#一些提前定义的函数########################################################################################
###########################################################################################################
#用于计算sobel统计量
sobel.z <- function(a,b)
{
  result=a*b/sqrt(a^2+b^2+1)
  return(result)
}
#用于计算一个模型deviance的p值
devia <- function(m)
{
  dev=m$null.deviance-m$deviance
  df=m$df.null-m$df.residual
  return(1-pchisq(dev,df))
}

###关于因变量的饼图##############################
table(as.integer(df$depression>=20))
as.data.frame(table(df$depression))
cut0 <- c(4069,4897)
cut1 <- c(5025,3941)
cut2 <- c(6180,2786)
cut3 <- as.data.frame(table(df$depression))
colnames(cut3)=c('频率','抑郁程度')

depression_plot <- function(cut)
{
  dt <-data.frame("抑郁程度"=cut,"频率"=c('没有抑郁症状','有抑郁症状'))
  myLabel <-as.vector(dt$频率)
  myLabel <- paste(myLabel,"(",round(dt$抑郁程度 / sum(dt$抑郁程度)*100,2),"%)",sep='')
  
  p <- ggplot(dt,aes(x="",y=抑郁程度,fill=频率))+
    geom_bar(stat='identity')+
    coord_polar(theta="y")+
    theme(axis.ticks=element_blank(),
          axis.text.x = element_blank(),
          legend.title=element_blank(),
          legend.position = 'top',
          legend.text = element_text(size=14),
          plot.title=element_text(hjust=0.5))+
    scale_fill_discrete(breaks=dt$频率,labels=myLabel)+
    labs(title='',
         x='',
         y='')

  return(p)
}
(pie0 <- depression_plot(cut0))
(pie1 <- depression_plot(cut1))
(pie2 <- depression_plot(cut2))

dt <- cut3
myLabel <-as.vector(dt$频率)
myLabel <- paste(myLabel,"(",round(dt$抑郁程度 / sum(dt$抑郁程度)*100,2),"%)",sep='')

p <- ggplot(dt,aes(x="",y=抑郁程度,fill=频率))+
  geom_bar(stat='identity',width=1)+
  coord_polar(theta="y")+
  theme(axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_blank(),
        legend.text = element_text(size=10),
        legend.position = 'top',
        plot.title=element_text(hjust=0.5))+
  scale_fill_discrete(breaks=dt$频率,labels=myLabel)+
  labs(title='',
       x='',
       y='')
p

###########################################################################################################
#以"太极"为自变量的逻辑回归分析############################################################################
#初步的相关性分析################################################################################################
CMHtest(table(df$taichi,df$depression_score))
#打太极拳的频率有序，但是抑郁程度无序，chisq=17.032,prob=2.002e-4,有很强的证据表明打太极拳的频率与抑郁程度之间存在关联
#于是我们运用广义线性模型对两个变量做了最初步的拟合
m0=glm(depression_score~factor(taichi),family=binomial(),data=df)
summary(m0)
exp(c(-0.37185,-0.60858))
m01=glm(depression_score~factor(taichi),family=binomial(link="probit"),data=df)
summary(m01)

###########################################################################################
#######加入人口统计学变量##################################################################
m1=glm(depression_score~as.factor(taichi)+age+sex,family=binomial(),data=df)
summary(m1)
anova(m0,m1)
#性别效应加入的必要性
taichi_df <- spread(as.data.frame(table(df$taichi,df$sex,df$depression_score)),
                    Var3,Freq)
head(taichi_df)
deg=nrow(taichi_df)-length(coef(m0))
#X2
X2=sum(resid(m0,type="pearson")^2)
X2_pvalue=1-pchisq(X2,deg)
c(X2=X2,pvalue=X2_pvalue)
#G2
G2=sum(resid(m0,type="deviance")^2)
G2_pvalue=1-pchisq(G2,deg)
c(G2=G2,pvalue=G2_pvalue)
#说明了性别效应是存在的


df<- df%>% mutate(taichi_score=as.integer(df$taichi>1))
m00=glm(depression_score~taichi_score+age+sex,family=binomial(),data=df)
summary(m00)
devia(m00)
anova(m1,m00)
#采用taichi_score

###模型的拟合优度检验X^2与G^2############################################################################
CMHtest(table(df$taichi,df$sex))
CMHtest(table(df$taichi,df$depression_score,df$sex))
mantelhaen.test(table(df$taichi,df$depression_score,df$sex))

#-----异质性分析——————————————————————
m001=glm(depression_score~taichi_score+age,family=binomial(),data=df%>%filter(sex==0))
summary(m001)
m002=glm(depression_score~taichi_score+age,family=binomial(),data=df%>%filter(sex==1))
summary(m002)
devia(m002)
#——————于是只分析男性——————————————————————————
od=exp(-1.904-0.733+0.018)
(pi=od/(1+od))

od=exp(-1.904+0.018)
(pi=od/(1+od))


###########################################################################################################
#箱线图############################################################################
taichi_plot <- function(data,gender="总体",ca='(a)')
{
  fg<- ggplot(data, aes(x=taichi,y=depression,color=taichi,group=taichi))+
    geom_boxplot()+
    scale_x_discrete(limits=c('从不','有时','经常'))+
    labs(title=gender,
         subtitle='太极与抑郁程度得分',
         caption=ca,
         x='打太极拳的频率',
         y='抑郁程度得分（得分越高抑郁程度越高）')+
    theme(legend.position = 'none',
          plot.title=element_text(hjust=0.5))
  
  return(fg)
}
fg1 <- taichi_plot(df)
fg2 <- taichi_plot(df%>%filter(sex==0),gender='女性',ca='(b)')
fg3 <- taichi_plot(df%>%filter(sex==1),gender='男性',ca='(c)')


taichi_plot1 <- function(data,gender="总体",ca='(d)')
{
  fg<- ggplot(data, aes(x=taichi_score,y=depression,group=taichi_score))+
    geom_boxplot(aes(color=factor(taichi_score)))+
    scale_x_continuous(breaks=c(0,1),
                       labels=c('很少','偶尔或经常'))+
    labs(title=gender,
         subtitle='太极与抑郁程度得分',
         caption=ca,
         x='打太极拳的频率',
         y='抑郁程度得分（得分越高抑郁程度越高）')+
    theme(legend.position = 'none',
          plot.title=element_text(hjust=0.5))+
    scale_colour_manual(breaks=c('0','1'),
                        values=c("#bf812d","#35978f"))
  
  return(fg)
}
fg4 <- taichi_plot1(df)
fg5 <- taichi_plot1(df%>%filter(sex==0),gender='女性',ca='(e)')
fg6 <- taichi_plot1(df%>%filter(sex==1),gender='男性',ca='(f)')
plot_grid(fg1,fg2,fg3,
          fg4,fg5,fg6,ncol=3,align='vh')
#############################################################################


###########################################################################################
#######加入社会经济水平变量################################################################
m2=glm(depression_score~taichi_score+age+
         as.ordered(edu)+social_eco+as.factor(prov)+as.factor(hukou)+as.factor(residence),family=binomial(),data=df%>%filter(sex==1))
summary(m2)
m2_backward=step(m2,direction="backward",trace=T)
summary(m2_backward)

m2_final=glm(depression_score~taichi_score+age+
               as.ordered(edu)+social_eco+as.factor(prov)+as.factor(hukou),family=binomial(),data=df%>%filter(sex==1))
summary(m2_final)
anova(m002,m2_final)
devia(m2_final)

###########################################################################################
#######加入家庭社会支持变量################################################################
m3=glm(depression_score~taichi_score+age+
         as.ordered(edu)+social_eco+as.factor(prov)+as.factor(hukou)+
         as.factor(marriage)+as.factor(coresidence),family=binomial(),data=df%>%filter(sex==1))
m3_backward=step(m3,direction="backward",trace=T)
summary(m3_backward)
summary(m3)
anova(m2_final,m3)
devia(m3)


#################################################################################################
#########加入健康习惯变量########################################################################
m4=glm(depression_score~taichi_score+age+
         as.ordered(edu)+social_eco+as.factor(prov)+as.factor(hukou)+
         as.factor(marriage)+as.factor(coresidence)+
         as.factor(smoke)+as.factor(alcohol)+as.factor(exercise)+BMI+as.factor(fruit)+as.factor(vegetable)+as.factor(fish)+as.factor(nut)+as.factor(tea),
       family=binomial(),data=df%>%filter(sex==1))
m4_backward=step(m4,direction="backward",trace=T)
summary(m4_backward)

m4_final=glm(depression_score~taichi_score+age+
               social_eco+as.factor(prov)+hukou+
               marriage+as.factor(coresidence)+
               alcohol+exercise+vegetable+nut,
             family=binomial(),data=df%>%filter(sex==1))
summary(m4_final)
anova(m3,m4_final)
devia(m4_final)

####################################################################################################
########加入健康状态变量###########################################################################
m5=glm(depression_score~taichi_score+age+
         social_eco+as.factor(prov)+hukou+
         marriage+as.factor(coresidence)+
         alcohol+exercise+vegetable+nut+
         as.factor(self_rated_health)+as.factor(disease)+ADL1+ADL2+ADL3+ADL4+ADL5+ADL6,
       family=binomial(),data=df%>%filter(sex==1))
m5_backward=step(m5,direction="backward",trace=T)
summary(m5_backward)

m50=glm(depression_score~taichi_score+self_rated_health+age+
          social_eco+as.factor(prov)+hukou+
          marriage+as.factor(coresidence)+
          exercise+vegetable+nut,
        family=binomial(),data=df%>%filter(sex==1))
summary(m50)
anova(m4_final,m50)
devia(m50)
###############################################################################################################

###中介效应分析
#depression~taichi_score+self_rated_health,显然taichi_score不显著但是self_rated_health显著
summary(m50)

#self_rated_health~taichi_score
CMHtest(table(df$taichi,df$self_rated_health))
m52=glm(self_rated_health~taichi_score+age+
          social_eco+as.factor(prov)+hukou+
          marriage+as.factor(coresidence)+
          exercise+vegetable+nut,
        family=binomial(),data=df%>%filter(sex==1))
summary(m52)

#进行中介效应分析
a=2.541
b=-12.599
(z1.mediation=sobel.z(a,b))
1-pnorm(-z1.mediation)

library(mediation)
df_trans <- df%>%mutate(prov2=as.integer(prov==2),prov3=as.integer(prov==3),prov4=as.integer(prov==4),
                        coresidence2=as.integer(coresidence==2),coresidence3=as.integer(coresidence==3))%>%
  filter(sex==1)

tm52 <- glm(self_rated_health~taichi_score+age+
              social_eco+prov2+prov3+prov4+hukou+
              marriage+coresidence2+coresidence3+
              exercise+vegetable+nut,
            family=binomial(),data=df_trans)
tm50 <- glm(depression_score~taichi_score+self_rated_health+age+
              social_eco+prov2+prov3+prov4+hukou+
              marriage+coresidence2+coresidence3+
              exercise+vegetable+nut,
            family=binomial(),data=df_trans)


mod.med=mediate(tm52,tm50,treat='taichi_score',mediator='self_rated_health',sims=100,boot=T)
summary(mod.med)


table(df$depression_score,df$taichi)