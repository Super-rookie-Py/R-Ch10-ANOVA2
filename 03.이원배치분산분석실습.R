###  Ch10.분산분석2

###  2020/05/07 keonwoo park

## 03.이원 배치분석 실습 ###################

## 사례
# 재학생을 대상으로 현황조사함.
# 학부와 학년에 따라 비교과운영은 차이가 있는가?
# 학부와 학년간에 상호작용효과가 있는지 검증하시오.


# 01.데이터 불러오기
edu <- read.csv('Ch1003.교육역량분석.csv',
                header=T,
                na.strings = '.')

edu$학부 <- factor(edu$학부,
                   levels=c(1,2,3,4),
                   labels=c('신학','사회복지','아동상담','경영'))
edu$학년 <- factor(edu$학년,
                   levels=c(1,2,3,4),
                   labels=c('1학년','2학년','3학년','4학년'))
str(edu)


#02.기본통계치 확인
library(psych)
describeBy(edu$비교과운영, edu$학부:edu$학년, mat=F)



# 03. 그래프그리기 (박스그래프, 히스토그램)
plot(비교과운영 ~ 학부 + 학년, data=edu)
boxplot(비교과운영 ~ 학부*학년, data= edu)


library(ggplot2) 
ggplot(edu, aes(x= 학년, y=비교과운영)) +
  geom_boxplot(outlier.color = 'red') +
  facet_wrap(~학부) +
  ggtitle('학부*학년에 따른 비교과운영')



# 04. 통계분석

edu.result <- aov(비교과운영 ~ 학부 + 학년 + 학부:학년, data=edu)
#방법, 온도, 상호작용값을더함. 상호작용부터 확인
summary(edu.result)
# 상호작용이 없네.. ?

# 상호작용효과가 의미가 없어서 제거해주고 다시 계산.
edu.result <- aov(비교과운영 ~ 학부 + 학년, data=edu)
summary(edu.result)


#상호작용효과 그래프
interaction.plot(edu$학부, edu$학년, edu$비교과운영)
# 상호작용효과 그래프에서는 범주현 자료 부터 쓰고 수치형자료를 넣는다. 

# 사후검정
# 일반 분석 TukeyHSD 결과 해석
TukeyHSD(edu.result)
tukeyPlot <- TukeyHSD(edu.result)
plot(tukeyPlot)


# 결론 상호작용효과가 없기 때문에 
# 상호작용 효과가 없었습니다.
# 학부에따라서, 학년에 따라서 차이가 없었습니다.
