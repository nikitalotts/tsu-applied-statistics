library(caret)
library(dplyr)

# 1. 1
RENT_DATA <- read.csv("C:/Users/Acer/Desktop/Rent 22 01 11.csv" , header = TRUE, sep = ";", dec = ".")
head(RENT_DATA)
RENT_DATA = transform(RENT_DATA, 
                             m2 = as.numeric(gsub(",", ".", gsub("\\.", "", m2))),
                             district = as.character(district),
                             walls = as.logical(walls)
)

#CONSTS
N = nrow(RENT_DATA)
ALPHA = 0.05

# 1. 2
# ���������� OneHotEncoding
dummy <- dummyVars(" ~ .", data=RENT_DATA)
RENT_DATA <- data.frame(predict(dummy, newdata = RENT_DATA)) 
head(RENT_DATA)
pair_model <- lm(rent ~ m2, data = RENT_DATA)
plot(pair_model,  which=1, col='blue', main="Rent:Pair model")
print(summary(pair_model))
cat('RSS ������ ������:', mean(pair_model$residuals ** 2), '\n')

# �������� �� ������������
qf(p=1 - ALPHA, df1=1, df2=N - 2)

# 1.3
SQUARE = 33
new_flat=c(1, SQUARE)
price = pair_model$coefficients%*%new_flat
cat('������ ��� �������� ��������', SQUARE, '�2 ��������: ', price, '\n')

head(RENT_DATA_no_corr)
# 2.1
# ������ �������� wallsFALSE � district4, ��� ��� �� ���������� OneHotEncoding(�� ����, ��������� ����������)
# ��� ���� ����� �� ��������� �������� ��������������������
# ������ district1 ����� ������ ������ ����� ���������
RENT_DATA_no_corr = RENT_DATA %>% select(-c('wallsFALSE', 'district1'))
# ������ rooms ��� ��� ����� ��� � m2 ������� ����������
cor(RENT_DATA_no_corr)
RENT_DATA_no_corr = RENT_DATA_no_corr %>% select(-c('rooms'))

head(RENT_DATA_no_corr)

# ����� 
mmodel <- lm(rent ~ ., data = RENT_DATA_no_corr)
plot(mmodel,  which=1, col='blue', main="Rent:Multiple model")

# 2.2
print(summary(mmodel))
temp_sum = summary(mmodel)
temp_sum
# �������� �� ������������
qf(p=1 - ALPHA, df1=1, df2=N - 2)
cat('RSS �������� ������ ������������� ���������:', mean(mmodel$residuals ** 2), '\n')
cat('�����: R-squared ���������� �� ~ 0.07, RSE ���� �� ~200\n')

# �����, ��� �������� floor � wallsTRUE - ���������, ������ ��
RENT_DATA_no_corr = RENT_DATA_no_corr %>% select(-c('floor', 'wallsTRUE'))

head(RENT_DATA_no_corr)

mmodel <- lm(rent ~ ., data = RENT_DATA_no_corr)
plot(mmodel)
# ���� �� ������������ ��������
# v >= test - nd
shapiro.test(mmodel$residuals)

temp_sum1 = summary(mmodel)
temp_sum1

SQUARE = 33
DISTRICT1 = 0
DISTRICT2 = 1
DISTRICT3 = 0
new_flat=c(1, SQUARE, DISTRICT1, DISTRICT2, DISTRICT3)
price = mmodel$coefficients%*%new_flat
cat(
  '������ ��� �������� ��������:', SQUARE, '\n',
  '�� �����:', FLOOR, '\n',
  '� ������� ������:', WALLSTRUE == 1, '\n',
  '��������:', price, '\n'
  )



# 3.1
flats_pure_data <- read.table("C:/Users/Acer/Desktop/flats 1.txt" , header = TRUE, sep = "\t", dec = ",")
head(flats_pure_data)
length(flats_pure_data$Type)

# 3.3
# ���������� OneHotEncoding
flats_transformed_data = transform(flats_pure_data, 
                                   M2 = as.numeric(M2),
                                   Type = as.factor(Type),
                                   F = as.character(F)
)

head(flats_transformed_data)
dummy <- dummyVars(" ~ .", data=flats_transformed_data)
flats_transformed_data_encoded <- data.frame(predict(dummy, newdata = flats_transformed_data)) 
# ������ �������� Typest � Fno, ��� ��� �� ���������� OneHotEncoding
# ��� ���� ����� �� ��������� �������� ��������������������
head(flats_transformed_data_encoded)
flats_transformed_data_encoded_no_corr = flats_transformed_data_encoded %>% select(-c('Type.st', 'Fyes'))
head(flats_transformed_data_encoded_no_corr)
# ������ ������-������� �� ��������
resid = wls_model$residuals
quartiles <- quantile(resid, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(resid)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
resid = resid[resid > Lower & resid < Upper]

norm_data = flats_transformed_data_encoded_no_corr %>% filter(!row_number() %in% c(43, 47, 48))
length(norm_data$Type)

cor(norm_data)

ro_model <- lm(Rent ~ M2 + Typefl + Total + M2 + Fno + Floor ,
               data = norm_data
)

summary(ro_model)
boxplot(norm_data$M2 ~ norm_data$Type.fl)
# Fno � Total ��������� ����������� - ������ ��
norm_data = norm_data %>% select(-c('Floor', 'Total'))

norm_data

ro_model <- lm(Rent ~ M2 + Type.fl + M2 + Fno ,
               data = norm_data
)

# ������� �� ���������� � �� ����������������
shapiro.test(ro_model$residuals)
gqtest(ro_model, data = norm_data, fraction = 18, order.by = ~ .)

# ����
head(norm_data)

tmpM2 = norm_data$M2 / norm_data$M2
tmpRent = norm_data$Rent / norm_data$M2
norm_data$M2 = tmpM2
norm_data$Rent = tmpRent
head(norm_data)

weights = 1 / norm_data$M2
weights
ro_model <- lm(Rent ~ M2 + Type.fl + M2 + Fno ,
               data = norm_data,
               weights = weights
)

summary(ro_model)

# ������� �� ���������� � �� ����������������
shapiro.test(ro_model$residuals)
gqtest(ro_model, data = norm_data, fraction = 18, order.by = ~ .)

plot(ro_model, which=1, col='blue', main="No outliers, multiple model")

# 3.2
# �������� �� ������������
# qf(p=1 - ALPHA, df1=1, df2=50 - 2)
print(summary(model))
cat('flats: RSS ������ ������:', mean(model$residuals ** 2), '\n')

#����������� ������

m_model <- lm(Rent ~ . , data = flats_transformed_data_encoded_no_corr)
plot(m_model,  which=1, col='blue', main="Multiple model")

# 3.4
print(summary(m_model))
# # �������� �� ������������
qf(p=1 - ALPHA, df1=1, df2=50 - 2)
cat('flats: RSS �������� ������ ������������� ���������:', mean(m_model$residuals ** 2), '\n')
cat('�����: R-squared ����� �� ~0.08, RSE ���� �� ~40')
 
# 3.5
cat('�����: RSE �������� ������ ������������� ��������� ����, �� �� ������')

# 3.6
library(lmtest)
 
gqtest(m_model, data = flats_transformed_data_encoded_no_corr, fraction = 18, order.by = ~ M2 + Typefl + Floor + Fyes + Total)
cat('�����: �������������������� �������� ������������� ������  ������������\n')
 
# 3.7.1
# ��������� ���������� �� ��� � ������� ����������� ���
weight <- 1 / lm(abs(m_model$residuals) ~ m_model$fitted.values)$fitted.values^2
wls_model <- lm(Rent ~ . ,
               data = flats_transformed_data_encoded_no_corr, weights=weight)
 
 
plot(wls_model, which=1, col='blue', main="WLS model")

summary(wls_model)
gqtest(wls_model, data = flats_transformed_data_encoded_no_corr, fraction = 18, order.by = ~ M2 + Typefl + Floor + Fyes + Total)
cat('�����: R-squared �����, RSE ���� � ~1000 ��� \n')
cat('�����: ���������� ��� ����� ���������� �� ��������������������\n')
 
# 3.7.2 - ������ ������
# ��������� ���������� �� ���� � ������� ����������� ���
wt <- 1 / m_model$fitted.values^2
gls_model <- lm(Rent ~ . ,
               data = flats_transformed_data_encoded_no_corr,
               weights = wt
               )
plot(gls_model, which=1, col='blue', main="GLS model")
summary(gls_model)
gqtest(gls_model, data = flats_transformed_data_encoded_no_corr, fraction = 18, order.by = ~ M2 + Typefl + Floor + Fyes + Total)
 
cat('�����: R-squared �����, RSE ���� � ~1000 ��� \n')
cat('�����: ���������� ��� ����� ���������� �� ��������������������\n')
