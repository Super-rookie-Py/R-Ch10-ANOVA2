###  Ch10.분산분석2

###  2020/05/07 keonwoo park

## 01.이원 배치분석 ###################

## 사례
# 치킨의 맛을 결정하는 두 가지 요인은 튀길 때의 기름온도와
# 튀기는 방법이다. 
# 온도를 200도, 300도로 하고, 튀기는 방법을 오븐과 기름으로 한다.  
# 온도와 시간이 맛을 결정하는데 중요한 요인인가? 

# 온도와 방법의 상호작용 검정을 해야한다.
# 튀기는 온도와 방법은 상호작용이 없다.
# H0: ab=0


# 01.데이터 불러오기
twa <- read.csv('Ch1001.TWA.csv',
                header=T,
                na.strings = '.')
twa$meth <- factor(twa$meth,
                   levels=c(1,2),
                   labels=c('오븐','기름'))
twa$temp <- factor(twa$temp,
                   levels=c(1,2),
                   labels=c('200도','300도'))
str(twa)


#02.기본통계치 확인
library(psych)
describeBy(twa$taste, twa$meth:twa$temp, mat=F)



# 03. 그래프그리기 (박스그래프, 히스토그램)
plot(taste ~ meth + temp, data=twa)
boxplot(taste ~ meth*temp, data= twa)


library(ggplot2) 
ggplot(twa, aes(x= temp, y= taste)) +
  geom_boxplot(outlier.color = 'red') +
  facet_wrap(~meth) +
  ggtitle('방법* 온도에 따른 맛')



# 04. 통계분석

twa.result <- aov(taste ~ meth + temp + meth:temp, data=twa)
#방법, 온도, 상호작용값을더함. 상호작용부터 확인
summary(twa.result)


#상호작용효과 그래프
interaction.plot(twa$meth, twa$temp, twa$taste)
# 상호작용효과 그래프에서는 범주현 자료 부터 쓰고 수치형자료를 넣는다. 


# 사후검정
# 상호작용이 있을 경우 : 그룹별로 나누어서 분석
# 상호작용이 없을 경우 : 각 변수별 주효과 분석(t-test, ANOVA분석)

# 상호작용이 있을 경우 : 그룹별로 나누어서 분석
tw1 <- twa[twa$meth=='오븐',]
tw2 <- twa[twa$meth=='기름',]

t.test(taste ~ temp,
       data=tw1,
       alternative = c('two.sided'),
       var.equal = TRUE,
       conf.level = 0.95)
t.test(taste ~ temp,
       data=tw2,
       alternative = c('two.sided'),
       var.equal = TRUE,
       conf.level = 0.95)

# 일반 분석 TukeyHSD 결과 해석
TukeyHSD(twa.result)
tukeyPlot <- TukeyHSD(twa.result)
plot(tukeyPlot)


