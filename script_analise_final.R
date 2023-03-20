install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("lme4")
install.packages("lmerTest")
install.packages('lessR')
install.packages("ggplot2")
install.packages("stargazer")

require(languageR)
require(ggrepel)
require(car)
library(stargazer)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(lessR)
library(ggplot2)


###############
##### GET ALL DATA FILES
###############

# get all data files names
participant_list = list.files(pattern="*.csv")

# preparing for 'for loop' to fetch all data
data <- data.frame()
count <- 1;
participants_questionnaire <- data.frame()
L1_count <- 0
L2_count <- 0
L3_count <- 0
L4_count <- 0
run_time_all <- c()

#fetch all data
for (p in participant_list) {
  # fetch participant data
  participant_data <- read.csv(file = participant_list[count])
  
  #get participant's experiment running time
  run_time <- tail(participant_data$time_elapsed, n=1)
  run_time_all[count-1] <- run_time
  print(paste("p", count, " = ", run_time/60000, sep=""))
  
  # add participant's code as variable
  participant_data$participant <- paste("p", count, sep="")
  
  # fetch proficiency and add as variable
  row_participant_proficiency <- which(participant_data$internal_node_id == "0.0-34.0")
  participant_proficiency <- substring(participant_data$response[row_participant_proficiency], 8, 9)
  participant_data$proficiency <- participant_proficiency
  
  # insert proficiency as high or low
  if (participant_proficiency == "A1" | participant_proficiency == "A2" | participant_proficiency == "B1") {
    participant_data$proficiency_group <- "low"
    #print("low proficiency")
  } else if (participant_proficiency == "B2" | participant_proficiency == "C1" | participant_proficiency == "C2") {
    participant_data$proficiency_group <- "high"
    #print("high proficiency")
  }
  
  # find out which experimental list each participant saw
  is_L1_item <- which(participant_data$stimulus == "../audio_problems/EN_F_6+9.mp3")
  is_L2_item <- which(participant_data$stimulus == "../audio_problems/EN_M_6+9.mp3")
  is_L3_item <- which(participant_data$stimulus == "../audio_problems/PT_F_6+9.mp3")
  is_L4_item <- which(participant_data$stimulus == "../audio_problems/PT_M_6+9.mp3")
  
  # insert list number as a column in each participant
  if (length(is_L1_item) != 0){
    L1_count <- 1 + L1_count
    participant_data$exp_list <- "L1"
  } else if (length(is_L2_item) != 0){
    L2_count <- 1 + L2_count
    participant_data$exp_list <- "L2"
  } else if (length(is_L3_item) != 0){
    L3_count <- 1 + L3_count
    participant_data$exp_list <- "L3"
  } else if (length(is_L4_item) != 0){
    L4_count <- 1 + L4_count
    participant_data$exp_list <- "L4"
  }
  
  
  print(paste("L1 = ", L1_count, "; L2 = ", L2_count, "; L3 = ", L3_count, "; L4 = ", L4_count, sep=""))
  
  # remove Practice trials
  
  # fetch questionnaire responses
  # questionnaire_answers <- data.frame()
  # questionnaire_answers <- rbind(questionnaire_answers, as.data.frame(participant_data[1:35,7]))
  # questionnaire_answers$participantID <- paste("p", count, sep="")
  # participants_questionnaire <- rbind(participants_questionnaire, questionnaire_answers)
  
  
  data <- rbind(data, participant_data)
  count <- count + 1
}
print(run_time_all)
ave(run_time_all)

# export questionnaire responses from all participants to a .csv file
# remove the hash in the line below to export!
# write.csv(participants_questionnaire, "/Users/joaoluiz/Documents/RStudio/exp_ma_analysis/All_questionnaire_responses.csv", row.names = TRUE)

###############
##### CLEANING DATA
###############

#remove unnecessary columns
data <- subset(data, select = -c(view_history, time_elapsed, question_order, internal_node_id, audio_file))

#move rows of column stimulus_language down by 1 so stimulus_language stays in the same row as rts and accuracy
data <- data %>% mutate(stimulus_language=lag(stimulus_language))

#remove excess rows
experimental_data <- which(data$trial_part == "show_options")
data <- data[experimental_data, ]

#remove practice trials
prac_stimuli <- c("../audio_problems/Prac_PT_F_12x3.mp3", "../audio_problems/Prac_EN_M_21x4.mp3", "../audio_problems/Prac_EN_M_24x4.mp3", "../audio_problems/Prac_PT_F_24+9.mp3", "../audio_problems/Prac_EN_M_34-6.mp3", "../audio_problems/Prac_PT_F_37x2.mp3", "../audio_problems/Prac_PT_F_38+2.mp3", "../audio_problems/Prac_EN_M_47+5.mp3", "../audio_problems/Prac_PT_F_53-9.mp3", "../audio_problems/Prac_EN_M_56+7.mp3", "../audio_problems/Prac_PT_F_61-3.mp3", "../audio_problems/Prac_PT_F_64db4.mp3", "../audio_problems/Prac_EN_M_78db6.mp3", "../audio_problems/Prac_EN_M_84db4.mp3", "../audio_problems/Prac_EN_M_87-8.mp3", "../audio_problems/Prac_PT_F_96db3.mp3")
data <- data %>% filter(stimulus_problem != prac_stimuli[1] & stimulus_problem != prac_stimuli[2] & stimulus_problem != prac_stimuli[3] & stimulus_problem != prac_stimuli[4] & stimulus_problem != prac_stimuli[5] & stimulus_problem != prac_stimuli[6] & stimulus_problem != prac_stimuli[7] & stimulus_problem != prac_stimuli[8] & stimulus_problem != prac_stimuli[9] & stimulus_problem != prac_stimuli[10] & stimulus_problem != prac_stimuli[11] & stimulus_problem != prac_stimuli[12] & stimulus_problem != prac_stimuli[13] & stimulus_problem != prac_stimuli[14] & stimulus_problem != prac_stimuli[15] & stimulus_problem != prac_stimuli[16])

#replace string 'null' by NA
data[data == "null"] <- NA

#replace missing values with median
#data <- data %>% mutate(rt = replace(rt, is.na(rt), median(rt, na.rm=TRUE)))

#fixing "correct_answer" column and calculate "correct" column
data$correct_answer[data$correct_answer == "answer_left"] <- "q"
data$correct_answer[data$correct_answer == "answer_right"] <- "p"
data$correct = ifelse(as.character(data$correct_answer) == as.character(data$response), TRUE, FALSE)

# set rt as numeric
data$rt = as.numeric(as.character(data$rt))

#set proficiency as factor
data$proficiency <- as.factor(data$proficiency)
data$proficiency_group <- as.factor(data$proficiency_group)

#split data for each participant
split_data <- split(data, f = data$participant)
for (p_num in c(seq(1:38))) {
  p_name <- paste("p", p_num, sep="")
  assign(p_name, split_data[[p_name]])
}

count <- 0
data_no_NA <- data.frame()
for(p_name in c(paste("p", seq(1:37), sep=""))) {
  curr_participant <- get(p_name)
  
  #calculate mean and SD per participant
  curr_participant$subjmean = ave(curr_participant$rt,curr_participant$participant, FUN=function(x) mean(x, na.rm=TRUE))
  curr_participant$subjsd = ave(curr_participant$rt,curr_participant$participant, FUN=function(x) sd(x, na.rm=TRUE))
  
  #calculate max and minimum RT allowed through STD (mean +- 2*STD)
  curr_participant$RT_max = curr_participant$subjmean + (2.5*curr_participant$subjsd)
  curr_participant$RT_min = curr_participant$subjmean - (2.5*curr_participant$subjsd)
  
  #subset data to remove extremes
  curr_participant = subset(curr_participant, rt<(curr_participant$subjmean+2.5*curr_participant$subjsd))
  curr_participant = subset(curr_participant, rt>(curr_participant$subjmean-2.5*curr_participant$subjsd))
  
  #calculate accuracy per participant
  this_participant_accuracy <- round(sum(curr_participant$correct, na.rm=TRUE)*90/100, 2)
  curr_participant$accuracy <- this_participant_accuracy
  
  # if (this_participant_accuracy > 55) {
  #   assign(paste("no_na", p_name, sep=""), curr_participant)
  #   data_no_NA <- rbind(data_no_NA, curr_participant)
  #   print(count)
  #   count <- count +1
  # }
}

# unique(data_no_NA$participant)

###############
##### PLOTTING ACCURACY PER CONDITION
###############

#pie chart for accuracy on every participant, every condition
accuracy_table_names <- c("timeout", "correct", "wrong")
#accuracy_table_values <- c(sum(is.na(data$correct)), sum(data$correct, na.rm=TRUE), length(data$correct[data$correct == FALSE]) - sum(is.na(data$correct)))
accuracy_table_values <- c(sum(is.na(data_no_NA$correct)), sum(data_no_NA$correct, na.rm=TRUE), length(data_no_NA$correct[data_no_NA$correct == FALSE]) - sum(is.na(data_no_NA$correct)))
accuracy_table <- data.frame(accuracy_table_names, accuracy_table_values)
colnames(accuracy_table) <- c("accuracy", "n")
accuracy_table_labels_names <- c(paste0(accuracy_table$accuracy[1], " (", accuracy_table$n[1], ")"), paste0(accuracy_table$accuracy[2], " (", accuracy_table$n[2], ")"),paste0(accuracy_table$accuracy[3], " (", accuracy_table$n[3], ")"))
pie(x = accuracy_table$n, labels = accuracy_table_labels_names, main = "accuracy_all")

#pie chart for accuracy on simple_sum
#data_simple_sum <- subset(data, data$condition == "simple_sum") 
data_simple_sum <- subset(data_no_NA, data_no_NA$condition == "simple_sum")
accuracy_table_values_simple_sum <- c(sum(is.na(data_simple_sum$correct)), sum(data_simple_sum$correct, na.rm=TRUE), length(data_simple_sum$correct[data_simple_sum$correct == FALSE]) - sum(is.na(data_simple_sum$correct)))
accuracy_table_simple_sum <- data.frame(accuracy_table_names, accuracy_table_values_simple_sum)
colnames(accuracy_table_simple_sum) <- c("accuracy", "n")
accuracy_table_simple_sum_labels_names <- c(paste0(accuracy_table_simple_sum$accuracy[1], " (", accuracy_table_simple_sum$n[1], ")"), paste0(accuracy_table_simple_sum$accuracy[2], " (", accuracy_table_simple_sum$n[2], ")"),paste0(accuracy_table_simple_sum$accuracy[3], " (", accuracy_table_simple_sum$n[3], ")"))
pie(x = accuracy_table_simple_sum$n, main="Accuracy simple sum", labels = accuracy_table_simple_sum_labels_names)

#pie chart for accuracy on complex_sum
#data_complex_sum <- subset(data, data$condition == "complex_sum") 
data_complex_sum <- subset(data_no_NA, data_no_NA$condition == "complex_sum")
accuracy_table_values_complex_sum <- c(sum(is.na(data_complex_sum$correct)), sum(data_complex_sum$correct, na.rm=TRUE), length(data_complex_sum$correct[data_complex_sum$correct == FALSE]) - sum(is.na(data_complex_sum$correct)))
accuracy_table_complex_sum <- data.frame(accuracy_table_names, accuracy_table_values_complex_sum)
colnames(accuracy_table_complex_sum) <- c("accuracy", "n")
accuracy_table_complex_sum_labels_names <- c(paste0(accuracy_table_complex_sum$accuracy[1], " (", accuracy_table_complex_sum$n[1], ")"), paste0(accuracy_table_complex_sum$accuracy[2], " (", accuracy_table_complex_sum$n[2], ")"),paste0(accuracy_table_complex_sum$accuracy[3], " (", accuracy_table_complex_sum$n[3], ")"))
pie(x = accuracy_table_complex_sum$n, labels = accuracy_table_complex_sum_labels_names, main = "Accuracy complex sum")

#pie chart for accuracy on simple_subtraction
#data_simple_subtraction <- subset(data, data$condition == "simple_subtraction")
data_simple_subtraction <- subset(data_no_NA, data_no_NA$condition == "simple_subtraction")
accuracy_table_values_simple_subtraction <- c(sum(is.na(data_simple_subtraction$correct)), sum(data_simple_subtraction$correct, na.rm=TRUE), length(data_simple_subtraction$correct[data_simple_subtraction$correct == FALSE]) - sum(is.na(data_simple_subtraction$correct)))
accuracy_table_simple_subtraction <- data.frame(accuracy_table_names, accuracy_table_values_simple_subtraction)
colnames(accuracy_table_simple_subtraction) <- c("accuracy", "n")
accuracy_table_simple_subtraction_labels_names <- c(paste0(accuracy_table_simple_subtraction$accuracy[1], " (", accuracy_table_simple_subtraction$n[1], ")"), paste0(accuracy_table_simple_subtraction$accuracy[2], " (", accuracy_table_simple_subtraction$n[2], ")"),paste0(accuracy_table_simple_subtraction$accuracy[3], " (", accuracy_table_simple_subtraction$n[3], ")"))
pie(x = accuracy_table_simple_subtraction$n, labels = accuracy_table_simple_subtraction_labels_names, main = "Accuracy simple subtraction")

#pie chart for accuracy on complex_subtraction
#data_complex_subtraction <- subset(data, data$condition == "complex_subtraction") 
data_complex_subtraction <- subset(data_no_NA, data_no_NA$condition == "complex_subtraction")
accuracy_table_values_complex_subtraction <- c(sum(is.na(data_complex_subtraction$correct)), sum(data_complex_subtraction$correct, na.rm=TRUE), length(data_complex_subtraction$correct[data_complex_subtraction$correct == FALSE]) - sum(is.na(data_complex_subtraction$correct)))
accuracy_table_complex_subtraction <- data.frame(accuracy_table_names, accuracy_table_values_complex_subtraction)
colnames(accuracy_table_complex_subtraction) <- c("accuracy", "n")
accuracy_table_complex_subtraction_labels_names <- c(paste0(accuracy_table_complex_subtraction$accuracy[1], " (", accuracy_table_complex_subtraction$n[1], ")"), paste0(accuracy_table_complex_subtraction$accuracy[2], " (", accuracy_table_complex_subtraction$n[2], ")"),paste0(accuracy_table_complex_subtraction$accuracy[3], " (", accuracy_table_complex_subtraction$n[3], ")"))
pie(x = accuracy_table_complex_subtraction$n, labels = accuracy_table_complex_subtraction_labels_names, main = "Accuracy complex subtraction")

#pie chart for accuracy on simple_multiplication
#data_simple_multiplication <- subset(data, data$condition == "simple_multiplication") 
data_simple_multiplication <- subset(data_no_NA, data_no_NA$condition == "simple_multiplication")
accuracy_table_values_simple_multiplication <- c(sum(is.na(data_simple_multiplication$correct)), sum(data_simple_multiplication$correct, na.rm=TRUE), length(data_simple_multiplication$correct[data_simple_multiplication$correct == FALSE]) - sum(is.na(data_simple_multiplication$correct)))
accuracy_table_simple_multiplication <- data.frame(accuracy_table_names, accuracy_table_values_simple_multiplication)
colnames(accuracy_table_simple_multiplication) <- c("accuracy", "n")
accuracy_table_simple_multiplication_labels_names <- c(paste0(accuracy_table_simple_multiplication$accuracy[1], " (", accuracy_table_simple_multiplication$n[1], ")"), paste0(accuracy_table_simple_multiplication$accuracy[2], " (", accuracy_table_simple_multiplication$n[2], ")"),paste0(accuracy_table_simple_multiplication$accuracy[3], " (", accuracy_table_simple_multiplication$n[3], ")"))
pie(x = accuracy_table_simple_multiplication$n, labels = accuracy_table_simple_multiplication_labels_names, main = "Accuracy simple multiplication")

#pie chart for accuracy on complex_multiplication
#data_complex_multiplication <- subset(data, data$condition == "complex_multiplication") 
data_complex_multiplication <- subset(data_no_NA, data_no_NA$condition == "complex_multiplication")
accuracy_table_values_complex_multiplication <- c(sum(is.na(data_complex_multiplication$correct)), sum(data_complex_multiplication$correct, na.rm=TRUE), length(data_complex_multiplication$correct[data_complex_multiplication$correct == FALSE]) - sum(is.na(data_complex_multiplication$correct)))
accuracy_table_complex_multiplication <- data.frame(accuracy_table_names, accuracy_table_values_complex_multiplication)
colnames(accuracy_table_complex_multiplication) <- c("accuracy", "n")
accuracy_table_complex_multiplication_labels_names <- c(paste0(accuracy_table_complex_multiplication$accuracy[1], " (", accuracy_table_complex_multiplication$n[1], ")"), paste0(accuracy_table_complex_multiplication$accuracy[2], " (", accuracy_table_complex_multiplication$n[2], ")"),paste0(accuracy_table_complex_multiplication$accuracy[3], " (", accuracy_table_complex_multiplication$n[3], ")"))
pie(x = accuracy_table_complex_multiplication$n, labels = accuracy_table_complex_multiplication_labels_names, main = "Accuracy complex multiplication")

#pie chart for accuracy on simple_division
#data_simple_division <- subset(data, data$condition == "simple_division") 
data_simple_division <- subset(data_no_NA, data_no_NA$condition == "simple_division")
accuracy_table_values_simple_division <- c(sum(is.na(data_simple_division$correct)), sum(data_simple_division$correct, na.rm=TRUE), length(data_simple_division$correct[data_simple_division$correct == FALSE]) - sum(is.na(data_simple_division$correct)))
accuracy_table_simple_division <- data.frame(accuracy_table_names, accuracy_table_values_simple_division)
colnames(accuracy_table_simple_division) <- c("accuracy", "n")
accuracy_table_simple_division_labels_names <- c(paste0(accuracy_table_simple_division$accuracy[1], " (", accuracy_table_simple_division$n[1], ")"), paste0(accuracy_table_simple_division$accuracy[2], " (", accuracy_table_simple_division$n[2], ")"),paste0(accuracy_table_simple_division$accuracy[3], " (", accuracy_table_simple_division$n[3], ")"))
pie(x = accuracy_table_simple_division$n, labels = accuracy_table_simple_division_labels_names, main = "Accuracy simple division")

#pie chart for accuracy on complex_division
#data_complex_division <- subset(data_EN, data_EN$condition == "complex_division") 
data_complex_division <- subset(data_no_NA, data_no_NA$condition == "complex_division")
accuracy_table_values_complex_division <- c(sum(is.na(data_complex_division$correct)), sum(data_complex_division$correct, na.rm=TRUE), length(data_complex_division$correct[data_complex_division$correct == FALSE]) - sum(is.na(data_complex_division$correct)))
accuracy_table_complex_division <- data.frame(accuracy_table_names, accuracy_table_values_complex_division)
colnames(accuracy_table_complex_division) <- c("accuracy", "n")
accuracy_table_complex_division_labels_names <- c(paste0(accuracy_table_complex_division$accuracy[1], " (", accuracy_table_complex_division$n[1], ")"), paste0(accuracy_table_complex_division$accuracy[2], " (", accuracy_table_complex_division$n[2], ")"),paste0(accuracy_table_complex_division$accuracy[3], " (", accuracy_table_complex_division$n[3], ")"))
pie(x = accuracy_table_complex_division$n, labels = accuracy_table_complex_division_labels_names, main = "Accuracy complex division")


###############
##### PLOTTING ACCURACY PER PARTICIPANT
###############

acc_list_p <- "p1"
acc_list_acc <- 0
acc_list_value <- "a"
acc_list_perc <- 0

acc_list <- data.frame(acc_list_p, acc_list_acc, acc_list_value, acc_list_perc)
count_acc <- 1
for(p_name in c(paste("p", seq(1:37), sep=""))) {
  curr_participant <- split_data[[p_name]]
  
  acc_list[count_acc, 1] <- p_name
  acc_list[count_acc, 2] <- "correct"
  amount_correct <- sum(curr_participant$correct, na.rm=TRUE)
  acc_list[count_acc, 3] <- amount_correct
  acc_list[count_acc, 4] <- round(amount_correct*100/90, 2)
  count_acc <- count_acc + 1
  
  acc_list[count_acc, 1] <- p_name
  acc_list[count_acc, 2] <- "wrong"
  amount_wrong <- length(curr_participant$correct[curr_participant$correct == FALSE]) - sum(is.na(curr_participant$correct))
  acc_list[count_acc, 3] <- amount_wrong
  acc_list[count_acc, 4] <- round(amount_wrong*100/90, 2)
  count_acc <- count_acc + 1
  
  acc_list[count_acc, 1] <- p_name
  acc_list[count_acc, 2] <- "timeout"
  amount_timeout <- sum(is.na(curr_participant$correct))
  acc_list[count_acc, 3] <- amount_timeout
  acc_list[count_acc, 4] <- round(amount_timeout*100/90, 2)
  count_acc <- count_acc + 1
  
  plot_acc_part <- data.frame(
  labels=c("Correct", "Wrong", "Timeout"),
  amounts=c(amount_correct, amount_wrong, amount_timeout),
  labels = c(round(amount_correct*100/90, digits=1), round(amount_wrong*100/90, digits=1), round(amount_timeout*100/90, digits=1))
  )
}

acc_list$acc_list_value <- as.numeric(acc_list$acc_list_value)

ggplot(acc_list, aes(fill=acc_list_acc, x=acc_list_p, y=acc_list_value)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values=c("#00BA38", "#fca304", "#F8766D")) +
  coord_flip()


###############
##### PLOTTING ACCURACY PER CONDITION
###############

acc_list_condition <- "p1"
acc_list_acc <- 0
acc_list_amount <- "a"
acc_list_lang <- "EN"

acc_list_per_condition_and_lang <- data.frame(acc_list_condition, acc_list_acc, acc_list_amount, acc_list_lang)
count_acc <- 1

for(condition in unique(data$condition)) {
  curr_condition <- get(paste("data_", condition, sep=""))
  
  for (language in c("EN", "PT")){
    curr_condition_lang <- subset(curr_condition, curr_condition$stimulus_language == language)
    
    curr_total_problems <- sum(curr_condition_lang$correct, na.rm=TRUE) + (length(curr_condition_lang$correct[curr_condition_lang$correct == FALSE]) - sum(is.na(curr_condition_lang$correct)))
    
    acc_list_per_condition_and_lang[count_acc, 1] <- condition
    acc_list_per_condition_and_lang[count_acc, 2] <- "correct"
    amount_correct <- sum(curr_condition_lang$correct, na.rm=TRUE)
    acc_list_per_condition_and_lang[count_acc, 3] <- round((amount_correct / curr_total_problems), digits=4) * 100
    acc_list_per_condition_and_lang[count_acc, 4] <- language
    count_acc <- count_acc + 1
    # 
    # plot_acc_condition <- data.frame(
    #   labels=c("Correct", "Wrong", "Timeout"),
    #   amounts=c(amount_correct, amount_wrong, amount_timeout)
  # )
  }
}

acc_list_per_condition_and_lang$acc_list_amount <- as.numeric(acc_list_per_condition_and_lang$acc_list_amount)


ggplot(acc_list_per_condition_and_lang, aes(x=acc_list_condition, y=acc_list_amount, fill=acc_list_lang, label = acc_list_amount)) +
  geom_bar(position="dodge", stat="identity") + 
  ylab("Percentage of correct answers") +
  xlab("Conditions") + 
  labs(title = "Accuracy across all conditions", fill = "Language") +
  geom_text(position = position_dodge(0.9), vjust=0.5, hjust=1.2, colour="white") +
  coord_flip()
  

for (plot_condition in unique(data_no_NA$condition)) {

  curr_condition_plot <- subset(acc_list_per_condition_and_lang, acc_list_per_condition_and_lang$acc_list_condition == plot_condition)
  
  print(ggplot(curr_condition_plot, aes(fill=acc_list_acc, x=acc_list_lang, y=acc_list_amount)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(values=c("#00BA38", "#fca304", "#F8766D")) +
    labs(title = paste("Accuracy for ", plot_condition, sep=""), fill = "Legend") +
    xlab("Accuracy percentage") +
    ylab("Conditions"))
  
}


###############
##### EXTRACTING MEANS AND SDs
###############
mean(subset(data_complex_division, stimulus_language=="PT")$rt)
sd(subset(data_complex_division, stimulus_language=="PT")$rt)

mean(subset(data_complex_division, stimulus_language=="EN")$rt)
sd(subset(data_complex_division, stimulus_language=="EN")$rt)

mean(data_no_NA$rt)
sd(data_no_NA$rt)

mean(data_simple_sum$rt)
sd(data_simple_sum$rt)

mean(data_simple_subtraction$rt)
sd(data_simple_subtraction$rt)

mean(data_simple_multiplication$rt)
sd(data_simple_multiplication$rt)

mean(data_simple_division$rt)
sd(data_simple_division$rt)

mean(data_complex_sum$rt)
sd(data_complex_sum$rt)

mean(data_complex_subtraction$rt)
sd(data_complex_subtraction$rt)

mean(data_complex_multiplication$rt)
sd(data_complex_multiplication$rt)

mean(data_complex_division$rt)
sd(data_complex_division$rt)

###############
##### PLOTTING OTHER VARIABLES
###############

#plotting proficiency
proficiency_df <- data.frame(data_no_NA$proficiency)
proficiency_var <- data_no_NA$proficiency
PieChart(proficiency_var, data = proficiency_df, hole = 0)

#plotting rts in total
hist(data_no_NA$rt, main="Histogram of every participant's RT", xlab="RT")

#plotting rts per condition individually
hist(data_simple_sum$rt)
hist(data_complex_sum$rt)
hist(data_simple_subtraction$rt)
hist(data_complex_subtraction$rt)
hist(data_simple_multiplication$rt)
hist(data_complex_multiplication$rt)
hist(data_simple_division$rt)
hist(data_complex_division$rt)

#plotting rts per condition and language
ggplot(data_complex_multiplication, aes(x=rt, fill=stimulus_language)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position="identity") +
  ggtitle("RTs for complex multiplication between Portuguese and English")

#plotting rt per condition per language
ggplot(data_no_NA, aes(rt, condition, fill=stimulus_language)) +
  geom_boxplot()

###PLOTTING RT PER CONDITION FOR EACH PROFICIENCY BAND
#plotting rt per confition per proficiency band -> stimuli in PT
ggplot(data_PT, aes(rt, condition, fill=proficiency_group)) +
  geom_boxplot() +
  ggtitle("RTs for Portuguese across conditions, compared by proficiency group")

#plotting rt per confition per proficiency band -> stimuli in EN
ggplot(data_EN, aes(rt, condition, fill=proficiency_group)) +
  geom_boxplot() +
  ggtitle("RTs for English across conditions, compared by proficiency group")

#plotting rt per participant
boxplot(data_no_NA$rt~data_no_NA$participant)

#plotting rt per proficiency
ggplot(data_no_NA, aes(x=proficiency, y=rt, fill=stimulus_language)) +
  geom_boxplot()
ggplot(data_no_NA, aes(x=proficiency, y=rt)) +
  geom_boxplot()


data_EN <- subset(data_no_NA, stimulus_language == "EN")
data_PT <- subset(data_no_NA, stimulus_language == "PT")

data_EN_sub_add <- subset(data_EN, condition == "simple_sum" | condition == "complex_sum" | condition == "simple_subtraction" | condition == "complex_subtraction")
data_PT_sub_add <- subset(data_PT, condition == "simple_sum" | condition == "complex_sum" | condition == "simple_subtraction" | condition == "complex_subtraction")

data_EN_div_mult <- subset(data_EN, condition == "simple_division" | condition == "complex_division" | condition == "simple_multiplication" | condition == "complex_multiplication")
data_PT_div_mult <- subset(data_PT, condition == "simple_division" | condition == "complex_division" | condition == "simple_multiplication" | condition == "complex_multiplication")

subset EN sub/add
subset EN div/mult
subset PT sub/add
subset PT div/mult
(rodar modelo pra cada subset)

m_EN_sub_add_1 <- lmer(rt ~ proficiency_group + condition + proficiency_group*condition + (1|participant), data=data_EN_sub_add)
summary(m_EN_sub_add_1)
class(m_EN_sub_add_1)<- "lmerMod"
stargazer(m_EN_sub_add_1, type = "html", report = "vct*",
          digits = 3,
          digit.separator = "")

m_EN_div_mult_1 <- lmer(rt ~ proficiency_group + condition + proficiency_group*condition + (1|participant), data=data_EN_div_mult)
summary(m_EN_div_mult_1)
class(m_EN_div_mult_1)<- "lmerMod"
stargazer(m_EN_div_mult_1, type = "html", report = "vct*",
          digits = 3,
          digit.separator = "")

m_PT_sub_add_1 <- lmer(rt ~ proficiency_group + condition + proficiency_group*condition + (1|participant), data=data_PT_sub_add)
summary(m_PT_sub_add_1)
class(m_PT_sub_add_1)<- "lmerMod"
stargazer(m_PT_sub_add_1, type = "html", report = "vct*",
          digits = 3,
          digit.separator = "")

m_PT_div_mult_1 <- lmer(rt ~ proficiency_group + condition + proficiency_group*condition + (1|participant), data=data_PT_div_mult)
summary(m_PT_div_mult_1)
class(m_PT_div_mult_1)<- "lmerMod"
stargazer(m_PT_div_mult_1, type = "html", report = "vct*",
          digits = 3,
          digit.separator = "")

m1 <- lmer(rt ~ proficiency_group + tipo2 + proficiency*tipo2 + (1|participant), data=data)
summary(m1)