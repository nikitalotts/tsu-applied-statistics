library(caret)
library(dplyr)


source_data <- read.table("C:/Users/Acer/Desktop/auto-mpg-data-original.txt" , header = FALSE,
                          sep = "", dec = ".",
                          col.names = c('mpg', 'cylinders', 'displacement',
                            'horsepower', 'weight', 'acceleration', 'model year', 'origin', 'car name'))


# ��� ��� �������� ������� ������� - ������ ������
source_data = source_data %>% drop_na(mpg)

# � �������� �������� �������, ��� horsepower ����� 6 ��������� - ������� �������� ��
source_data = source_data %>% mutate(across(c(horsepower), ~replace_na(., median(., na.rm=TRUE))))
length(source_data$mpg)

# �������� ������, � ���� ��, ��������� - �� ����� ������� ��������� ����������
# ������ ���.

source_data = subset(source_data, select = -c(car.name) )
length(source_data)

# � ��� ���� ���� �������������� ������� - ������ - ���������� OHE
source_data = transform(source_data, origin = as.character(origin))
dummy <- dummyVars(" ~ .", data=source_data)
source_data <- data.frame(predict(dummy, newdata = source_data)) 

# ������ �������� origin3, ��� ��� �� ���������� OneHotEncoding
# ��� ���� ����� �� ��������� �������� ��������������������
source_data = source_data %>% select(-c('origin3'))
source_data

# ����������� ������
source_data = as.data.frame(scale(source_data))
source_data

model <- lm(mpg ~ . , data = source_data)
wt <- 1 / model$fitted.values^2
wls_model <- lm(mpg ~ . , data = source_data, weights=wt)

plot(model, which=1, col='blue', main="Auto-mpg")
summary(model)

gqtest(model, data = source_data, fraction = 146, order.by = ~ weight + model.year + origin1 + origin2 + acceleration + horsepower + displacement + cylinders)
