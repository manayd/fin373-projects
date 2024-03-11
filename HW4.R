#1
#a
rosca = read.csv("~/Downloads/rosca.csv")
rosca$treatment <- ifelse(rosca$encouragement == 1, "control", ifelse(rosca$safe_box == 1, "safebox", "lockbox"))
nrow(rosca[rosca$treatment == "control", ])  
nrow(rosca[rosca$treatment == "safebox", ])  
nrow(rosca[rosca$treatment == "lockbox", ]) 
#b
nrow(rosca)
rosca_followup <- rosca[rosca$has_followup2 == 1, ]
nrow(rosca_followup)
nrow(rosca_followup[rosca_followup$treatment == "control", ])  
nrow(rosca_followup[rosca_followup$treatment == "safebox", ])  
nrow(rosca_followup[rosca_followup$treatment == "lockbox", ])  
#c
mean(rosca_followup[rosca_followup$treatment == "control", ]$fol2_amtinvest)
mean(rosca_followup[rosca_followup$treatment == "safebox", ]$fol2_amtinvest)
mean(rosca_followup[rosca_followup$treatment == "lockbox", ]$fol2_amtinvest)
#d
function_d <- function(treatment) {
  print(mean(rosca_followup[rosca_followup$treatment == treatment, ]$bg_female))
  print(mean(rosca_followup[rosca_followup$treatment == treatment, ]$bg_married))
  print(mean(rosca_followup[rosca_followup$treatment == treatment, ]$bg_b1_age))
}

function_d("control")
function_d("safebox")
function_d("lockbox")
#e
mean(rosca_followup[rosca_followup$treatment == "control" & rosca_followup$bg_married == 1, ]$fol2_amtinvest)
mean(rosca_followup[rosca_followup$treatment == "safebox" & rosca_followup$bg_married == 1, ]$fol2_amtinvest)
mean(rosca_followup[rosca_followup$treatment == "lockbox" & rosca_followup$bg_married == 1, ]$fol2_amtinvest)
mean(rosca_followup[rosca_followup$treatment == "control" & rosca_followup$bg_married == 0, ]$fol2_amtinvest)
mean(rosca_followup[rosca_followup$treatment == "safebox" & rosca_followup$bg_married == 0, ]$fol2_amtinvest)
mean(rosca_followup[rosca_followup$treatment == "lockbox" & rosca_followup$bg_married == 0, ]$fol2_amtinvest)

#2

#a
predimed = read.csv("~/Downloads/predimed.csv")
total_rows = nrow(predimed)
nrow(predimed[predimed$event == "Yes", ]) / total_rows
nrow(predimed[predimed$group == "MedDiet + Nuts" | predimed$group == "MedDiet + VOO" , ]) / total_rows
nrow(predimed[(predimed$group == "MedDiet + Nuts" | predimed$group == "MedDiet + VOO") & predimed$event == "Yes", ]) / total_rows
any_meddiet <- predimed[predimed$group == "MedDiet + Nuts" | predimed$group == "MedDiet + VOO" , ]
nrow(any_meddiet[any_meddiet$event == "Yes", ]) / nrow(any_meddiet)
#b
control_predimed <- predimed[predimed$group == "Control", ]
nrow(control_predimed[control_predimed$event == "Yes", ]) / nrow(control_predimed)
#d
nrow(any_meddiet[any_meddiet$sex == "Male" & any_meddiet$event == "Yes", ]) / nrow(any_meddiet[any_meddiet$sex == "Male", ])
nrow(any_meddiet[any_meddiet$sex == "Female" & any_meddiet$event == "Yes", ]) / nrow(any_meddiet[any_meddiet$sex == "Female", ])
nrow(predimed[predimed$sex == "Male" & predimed$event == "Yes" & predimed$group == "Control", ]) / nrow(predimed[predimed$sex == "Male" & predimed$group == "Control", ])
nrow(predimed[predimed$sex == "Female" & predimed$event == "Yes" & predimed$group == "Control", ]) / nrow(predimed[predimed$sex == "Female" & predimed$group == "Control", ])
