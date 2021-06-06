library(dplyr)
data_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F )
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=data_table)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=data_table))
library(tidyverse) 
coil_data <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F )
head(coil_data)
total_summary <- summarise(coil_data, Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
lot_summary <- coil_data %>% group_by(Manufacturing_Lot) %>% summarise(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
table <- coil_data 
table2 <- coil_data %>% sample_n(25)
t.test(log10(table$PSI),mu=1500)
    
group_1 <- c("Lot1")
group_2 <- c("Lot2")
group_3 <- c("Lot3")


t.test(formula = PSI ~ Manufacturing_Lot,
       data = coil_data, mu=1500,
       subset = Manufacturing_Lot %in% c(group_1, group_2))

t.test(formula = PSI ~ Manufacturing_Lot,
       data = coil_data, mu=1500,
       subset = Manufacturing_Lot %in% c(group_1, group_3))

t.test(formula = PSI ~ Manufacturing_Lot,
       data = coil_data, mu=1500,
       subset = Manufacturing_Lot %in% c(group_2, group_3))



       