###  Ch10.분산분석2

###  2020/05/07 keonwoo park

## 04.이원 반복측정 분석 실습 ###################

# 사례 호흡과 뇌파와의 관계 자료
# 호흡을 3번 (1:1, 7:3, 3:7) 측정
# 성별에 따라 호흡이 뇌파간 차이가 있는가?
# ch1al에 대해검증 


# 01.데이터 불러오기
bre <- read.csv('Ch1004.호흡과 뇌파.csv',
                  header=T,
                  na.strings = '.')


bre$breath <- factor(bre$breath,
                      levels=c(1,2,3),
                      labels=c('1:1','7:3','3:7'))
bre$sex <- factor(bre$sex,
                     levels=c(1,2),
                     labels=c('남자','여자'))
str(bre)


#02.기본통계치 확인
library(psych)
describeBy(bre$ch1al, bre$sex:bre$breath, mat=F)



# 03. 그래프그리기 (박스그래프, 히스토그램)
boxplot(ch1al ~ sex * breath, data= bre)


library(ggplot2) 
ggplot(bre, aes(x= sex, y= ch1al)) +
  geom_boxplot(outlier.color = 'red') +
  facet_wrap(~breath) +
  ggtitle('방법')



# 04. 통계분석

# 구형성(sphericity) 검정 그룹 3개이상일 때만 나옴.
# 본 예제는 변수가 2개 이기 떄문에 구형성 검정 없음
require(car)
bre.matrix <- cbind(bre$ch1al[bre$breath=='1:1'],
                    bre$ch1al[bre$breath=='7:3'],
                    bre$ch1al[bre$breath=="3:7"])
bre.model.lm <- lm(bre.matrix ~ 1) # ~1 : 데이터를 하나로 묶기
breath.f <- factor(c('1:1','7:3','3:7'))
options(contrasts = c('contr.sum','contr.poly'))
bre.result.mt <-Anova(bre.model.lm,
                        idata = data.frame(breath.f),
                        idesign=~time.f,
                        type = "III")

summary(bre.result.mt, multivariate = T)
# 호흡에 따라 차이가 없게나옴.



# ANOVA 검정 (방법1=방법2)
bre.result <- aov(ch1al ~ breath*sex+ Error(id), data = bre)
summary(bre.result)

bre.result <- aov(ch1al ~ breath + sex, data = bre)
summary(bre.result)
# 그룹간의 차이 time:group 확인.



#상호작용효과 그래프
interaction.plot(bre$breath, bre$sex, bre$ch1al)
# 상호작용효과 그래프에서는 범주현 자료 부터 쓰고 수치형자료를 넣는다. 
# 상호작용이 없다.

