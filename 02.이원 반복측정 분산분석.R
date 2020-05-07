###  Ch10.분산분석2

###  2020/05/07 keonwoo park

## 01.이원 반복측정 분석 ###################

# 사례 병원에서 새로운 치료를 개발
# 치료가 통증에 효과가 있는지를 일반치료제와 가짜치료제 대조군을 나누고  
# 치료전과 후에 통증이 차이가 있는지를 검정하였다.
# 귀무가설 : 그룹과 반복간에는 상호작용이 없다 H0: ab=0



# 01.데이터 불러오기
twrma <- read.csv('Ch1002.TWRMA.csv',
                header=T,
                na.strings = '.')

twrma$group <- factor(twrma$group,
                   levels=c(1,2),
                   labels=c('대조군','실험군'))
twrma$time <- factor(twrma$time,
                   levels=c(1,2),
                   labels=c('사전','사후'))
str(twrma)


#02.기본통계치 확인
library(psych)
describeBy(twrma$painscore, twrma$time:twrma$group, mat=F)



# 03. 그래프그리기 (박스그래프, 히스토그램)
plot(painscore ~ group + time, data=twrma)
boxplot(painscore ~ group * time, data= twrma)


library(ggplot2) 
ggplot(twrma, aes(x= group, y= painscore)) +
  geom_boxplot(outlier.color = 'red') +
  facet_wrap(~time) +
  ggtitle('방법')



# 04. 통계분석

# 구형성(sphericity) 검정 그룹 3개이상일 때만 나옴.
# 본 예제는 변수가 2개 이기 떄문에 구형성 검정 없음
require(car)
twrma.matrix <- cbind(twrma$painscore[twrma$time=='사전'],
                      twrma$painscore[twrma$time=='사후'])
twrma.model.lm <- lm(twrma.matrix ~ 1) # ~1 : 데이터를 하나로 묶기
time.f <- factor(c('사전','사후'))
options(contrasts = c('contr.sum','contr.poly'))
twrma.result.mt <-Anova(twrma.model.lm,
                      idata = data.frame(time.f),
                      idesign=~time.f,
                      type = "III")

summary(twrma.result.mt, multivariate = T)




# ANOVA 검정 (방법1=방법2)
twrma.result <- aov(painscore ~ time*group+ Error(id), data = twrma)
summary(twrma.result)

twrma.result <- aov(painscore ~ time + group+ time:group, data = twrma)
summary(twrma.result)
# 그룹간의 차이 time:group 확인.



#상호작용효과 그래프
interaction.plot(twrma$time, twrma$group, twrma$painscore)
# 상호작용효과 그래프에서는 범주현 자료 부터 쓰고 수치형자료를 넣는다. 


# 사후검정
# 상호작용이 있을 경우 : 그룹별로 나누어서 분석
# 상호작용이 없을 경우 : 각 변수별 주효과 분석(t-test, ANOVA분석)

# 상호작용이 있을 경우 : 그룹별로 나누어서 분석

# 실험 전, 후 시간에 따른 비교
pre <- twrma[twrma$time=='사전',]
post <- twrma[twrma$time=='사후',]

t.test(painscore ~ group,
       data=pre,
       alternative = c('two.sided'),
       var.equal = TRUE,
       conf.level = 0.95)
# 사전에는 그룹간의 차이가 없다.
t.test(painscore ~ group,
       data=post,
       alternative = c('two.sided'),
       var.equal = TRUE,
       conf.level = 0.95)
# 사후에는 그룹간의 차이가 명백히 있다.




# 그룹에 따른 사전-사후 비교
controlG <- twrma[twrma$group=='대조군',]
treatG <- twrma[twrma$group=='실험군',]

t.test(painscore ~ time,
       data=controlG,
       alternative = c('two.sided'),
       var.equal = TRUE,
       conf.level = 0.95)
# 사전에는 그룹간의 차이가 없다.
t.test(painscore ~ time,
       data=treatG,
       alternative = c('two.sided'),
       var.equal = TRUE,
       conf.level = 0.95)
