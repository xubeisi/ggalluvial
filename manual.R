library(ggalluvial)

data(vaccinations)
data <- transform(vaccinations,
                  response = factor(response, rev(levels(response))))
data$test <- 1:nrow(data)
data$test <- ifelse(data$test %% 2 == 1,"red","white")

data$tt <- 1

df <- data
ggplot(df, aes(x = survey, y = tt, stratum = subject, alluvium = subject), fill=test) + geom_stratum(alpha = 0.8,width = 0.33) + geom_text(aes(label = subject), stat = 'stratum', size = rel(5)) +geom_flow(aes(fill=response), stat = 'alluvium', lode.guidance = 'forward', width = 0.33) + theme_bw()

ggplot(df, aes(x = survey, y = tt, stratum = subject, alluvium = subject)) + geom_stratum(fill=df$test,alpha = 0.8,width = 0.33) + geom_text(aes(label = subject), stat = 'stratum', size = rel(5)) +geom_flow(aes(fill=response), stat = 'alluvium', lode.guidance = 'forward', width = 0.33) + theme_bw()


data(UCBAdmissions)
data <- as.data.frame(UCBAdmissions)
data$sub <- 1:nrow(data)
data$test <- ifelse(data$sub %% 2 == 1,"red","white")
data$tt <- 1
dfw <- data

ggplot(dfw, aes(y = Freq, axis1 = Gender, axis2 = Dept, axis3 = Admit)) + geom_stratum(fill=dfw$test,alpha = 0.8,width = 0.33) + geom_text(aes(label = test), stat = 'stratum', size = rel(5)) +geom_flow(aes(fill=test), stat = 'alluvium', lode.guidance = 'forward', width = 0.33) + theme_bw()

dfw$AA <- 1:nrow(dfw)
dfw$CC <- rev(c(10:nrow(dfw),rep(NA,5),101:104))
dfw$BB <- c(15:nrow(dfw),1:14)

ggplot(dfw, aes(y = tt, axis1 = AA, axis2 = BB, axis3 = CC)) + geom_stratum(alpha = 0.8,width = 0.33) + geom_text(aes(label = after_stat(stratum)), stat = 'stratum', size = rel(5)) +geom_flow(aes(fill=Dept), stat = 'alluvium', lode.guidance = 'forward', width = 0.33) + theme_bw()

ggplot(dfw, aes(y = tt, axis1 = AA, axis2 = BB, axis3 = CC)) + geom_stratum(alpha = 0.8,width = 0.33) + geom_text(aes(label = Dept), stat = 'stratum', size = rel(5)) +geom_flow(aes(fill=Dept), stat = 'alluvium', lode.guidance = 'forward', width = 0.33) + theme_bw()
