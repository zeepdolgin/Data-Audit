## Hive Data Audit Analysis Results (Arseniy Dolgin)

library("readxl")
# some additional libraries that I might end up using:
library(tidyverse)
library(plotly)

original_table <-  read_excel("Data/Hive Annotation Job Results.xlsx")

## I run the following command to verify that each column was loaded into the R tibble with a correct data type. 
str(original_table)

## I have decided to analyse this table by following the rules described in the
# background facts; that is, I formulated the rules as logical statements and my
# plan is to add at least 5 more columns to the table:
#   rule_1_violation; (1 if violated, 0 if not),
#   rule_2_violation; (1 if violated, 0 if not),
#   rule_3_violation; (1 if violated, 0 if not),
#   rule_4_violation; (1 if violated, 0 if not),
#   sum_of_violations; (adds the outputs of the 4 previous columns).
# duplicate the table and do the work there in order not to mess up the original by accident...

table_in_work <- original_table

## Adding column that tests for rule 1:
## if definition list, should be tabular and semantic:
table_in_work$rule_1_violation <- ifelse(table_in_work$definition_list == TRUE & table_in_work$tabular == TRUE & table_in_work$semantic == TRUE, 0, ifelse(table_in_work$definition_list == FALSE, 0, 1))

## Adding column that tests for rule 2:
## if semantic, should be tabular:
table_in_work$rule_2_violation <- ifelse(table_in_work$semantic == TRUE & table_in_work$tabular == TRUE, 0, ifelse(table_in_work$semantic == FALSE, 0, 1))

## Adding column that tests for rule 3:
## if not tabular, should neither be semantic nor definition list:
table_in_work$rule_3_violation <- ifelse(table_in_work$tabular == FALSE & table_in_work$definition_list == FALSE & table_in_work$semantic == FALSE, 0, ifelse(table_in_work$tabular == TRUE, 0, 1))

## Adding column that tests for rule 4:
## if tabular and either has a header row or a header column, should be semantic:
## I could not remember the syntax for chaining OR (|) with AND (&) in an if statement,
## so I made a helper column that returns TRUE if either header_row or header_column is TRUE, and returns FALSE otherwise:

table_in_work$helper_column <- ifelse(table_in_work$header_row == TRUE | table_in_work$header_column == TRUE, TRUE, FALSE)

table_in_work$rule_4_violation <- ifelse(table_in_work$tabular == TRUE & table_in_work$helper_column == TRUE & table_in_work$semantic == FALSE, 1, 0)

## remove the helper column, since unlikely to be needed:

table_in_work <- table_in_work[-c(11)]

## create a column that sums up all of the rule violations:

table_in_work$violations_sum <- table_in_work$rule_1_violation + table_in_work$rule_2_violation + table_in_work$rule_3_violation + table_in_work$rule_4_violation


# At this point the data is ready for analysis visualizations.

# For the next part I have decided to improve the visual representation of my analysis and build several pie charts that I gradually improved to be more informative.

# count the number of times each violation occurs:

sum_rule_1_viol <- sum(table_in_work$rule_1_violation)
sum_rule_2_viol <- sum(table_in_work$rule_2_violation)
sum_rule_3_viol <- sum(table_in_work$rule_3_violation)
sum_rule_4_viol <- sum(table_in_work$rule_4_violation)

# lets build a pie chart of violation sums:

slices <- c(sum_rule_1_viol, sum_rule_2_viol, sum_rule_3_viol, sum_rule_4_viol)
lbls <- c("Number of Rule 1 vilations", "Number of Rule 2 vilations", "Number of Rule 3 vilations", "Number of Rule 4 vilations" )
pie(slices, labels = lbls, main="Pie chart of occurances of violations.")

# this basic pie chart does not include the count labels inside each slice, a ggplot2 pie chart is needed:

table_for_pie <- data.frame(lbls, slices)
table_for_pie

# pie chart flows from a bar chart:

ggplot(table_for_pie, aes(x = "", y = slices, fill = lbls)) +
  geom_bar(width = 1, stat = "identity")

# first ggplot2 pie chart:

ggplot(table_for_pie, aes(x = "", y = slices, fill = lbls)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  labs(x = "", y = "", title = "Number of each Rule Violation Occurances: \n",
       fill = "Colour Choices") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))

# adding more labels:

table_for_pie_percent <- table_for_pie %>%
  mutate(lbls = factor(lbls, 
                       levels = lbls[length(lbls):1]),
         cumulative = cumsum(slices),
         midpoint = cumulative - slices / 2,
         labels = paste0(round((slices/ sum(slices)) * 100, 1), "%"))

table_for_pie_percent

# ggplot2 Pie Chart with percentage labels

ggplot(table_for_pie_percent, aes(x = "", y = slices, fill = lbls)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  labs(x = "", y = "", title = "Percent of each Rule Violation Occurances: \n",
       fill = "Rule Violations") + 
  geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
            fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10)) 

# adding counts to the pie chart:

table_for_pie_labels <- table_for_pie_percent %>%
  mutate(lbls = factor(lbls, levels = lbls[length(lbls):1]),
         cumulative = cumsum(slices),
         midpoint = cumulative - slices / 2,
         labels = paste0(round((slices/ sum(slices)) * 100, 1), "%", " (", slices, ") "))

# pie chart with counts of each violation:

ggplot(table_for_pie_labels, aes(x = "", y = slices, fill = lbls)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  labs(x = "", y = "", title = "Number of each Rule Violation Occurances: \n",
       fill = "Rule Violations") + 
  geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
            fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))  

# write to a CSV file the table_in_work that includes evaluation columns:

write.csv(table_in_work, "C:/Users/Uebi Nubov/Desktop/Hive Table Analysis/Hive Data Audit Prompt/Output/HAJR_Audit.csv")


# While the pie charts are fun, in this particular case they might be a bit misleading.

# Create a new column called violation_present, which will take a value of TRUE if column violations_sum is greater that 0, and FALSE otherwise:

table_in_work$violation_present <- ifelse(table_in_work$violations_sum > 0, TRUE, FALSE)

summary(table_in_work$violation_present)
# From this table we can see that 4379 rows (files) were labeled entirely correctly, while 621 rows were labeled with 1 or more mistakes. 

# the following code outputs 2 CSV files - one that contains all rows that were labeled correctly, and one that contains all the rows that have mistakes in their labels:

mislabeled_rows <- table_in_work[table_in_work$violation_present,]
# Show mislabeled_rows:
mislabeled_rows
# write results to file:
write.csv(mislabeled_rows, "C:/Users/Uebi Nubov/Desktop/Hive Table Analysis/Hive Data Audit Prompt/Output/mislabeled_rows.csv")

correctly_labeled_rows <- table_in_work[!table_in_work$violation_present,]
# Show correctly_labeled_rows:
correctly_labeled_rows
# write results to file:
write.csv(correctly_labeled_rows, "C:/Users/Uebi Nubov/Desktop/Hive Table Analysis/Hive Data Audit Prompt/Output/correctly_labeled_rows.csv")


##My recommendation is to either try and create rules for fixing minor mislabeling mistakes (less than 2 violations), and try to correct for those automatically, or to simply say that even a single violation is a mistake and return the mislabeled rows for review, while keeping the correctly labeled rows.**
## This concludes my analysis, thank you for your time! :)*
  
