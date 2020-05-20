library(readxl)
library(xlsx)
library(dplyr)
library(dbplyr)
library(rpivotTable)


getwd()
# Read people data from excel file sheet 2 
people_data <- read_xlsx("data_test_(1).xlsx", sheet = 2)
head(people_data)

# Read revernue data from excel file sheet 3 
revenue_data <- read_xlsx("data_test_(1).xlsx", sheet = 3 )
head(revenue_data)

#coverting customer close date into date format to take out timestamps if any
revenue_data <- revenue_data %>% mutate(customer_close_date = as.Date(revenue_data$customer_close_date))


head(revenue_data)
nrow(revenue_data)

#inner join to combine people data with sales data
combine_data <- inner_join(people_data, revenue_data, by = "employee_id")

#left join to ger all the people and teams names with no sales and revenue
combine_data2 <- left_join(people_data, revenue_data, by = "employee_id")


# Extracting month from date field
nrow(combine_data)
combine_data <- combine_data %>% mutate(month = format(customer_close_date, format = "%B")) 

#filter to get team names with no sales or revenue data
no_sales_data <-  combine_data2 %>% filter(is.na(customer_id))   
head(combine_data2)
head(no_sales_data)

# Group by team for no sales data
no_sales_data <- combine_data2 %>% filter(manager_name != "Eliana Bernard" ) %>% group_by(team) %>% 
                 summarise(team_rev = sum(revenue_amount))%>%  arrange(-team_rev)
#

# Group by team for sales data
sales_data <- combine_data %>% filter(manager_name != "Eliana Bernard" ) %>% group_by(team) %>% 
  summarise(team_rev = sum(revenue_amount))%>%  arrange(-team_rev)
#View(sales_data)

# Group by team and month for sales data to calculate revenue for each team by month and also to eliminate records 
# with manager name "Eliana Bernard"
final_data <- combine_data %>% filter(manager_name != "Eliana Bernard" ) %>% group_by(team, month ) %>%
          summarise(team_rev = sum(revenue_amount))%>%  arrange(team_rev) %>% mutate(team_rev = formatC(team_rev, format = "f", digits = 3))

head(final_data)
#View(final_data)
# conver month as factor so that it can be sorted
final_data$month = factor(final_data$month, levels = month.name)
final_data <- final_data %>% arrange(month)
head(final_data, 10)
#View(final_data)


############ Teams with no Sales Data

# Filter rows such that ther are in no sales data but not in sales data. So, In that way we
# can filter all the teams with no or 0 revenue for three months
teams_no_sales <- anti_join(no_sales_data , sales_data,  by = "team")
#View(teams_no_sales)

# Append dummy Month bucker so that it can be merged with sales data
teams_no_sales$January = '0'
teams_no_sales$February = '0'
teams_no_sales$March = '0'
teams_no_sales = teams_no_sales[-2]
head(teams_no_sales)

###########
head(final_data)
nrow(final_data)
#Pivot sales data, team as rows and month as columns
pivot <- final_data %>% pivot_wider(names_from = month, values_from = team_rev , values_fill = list(team_rev = 0)) 
nrow(pivot)

# Merge sales and no sales data into different data frames. Now we got all the teams data by month
pivot_data <- NULL
pivot_data <- bind_rows(pivot, teams_no_sales)
nrow(pivot_data)

# replace all 'NA', values by zero. These are the records from no sales 
pivot_data$January <- pivot_data$January %>% replace_na(0.000)
pivot_data$February <- pivot_data$February %>% replace_na(0.000)
pivot_data$March <- pivot_data$March %>% replace_na(0.000)


#View(pivot_data)
#calculate total revenue for each team
pivot_final <- pivot_data %>% mutate(total_revenue = as.numeric(January) + as.numeric(February) + as.numeric(March)) %>% mutate(total_revenue = formatC(total_revenue, format = "f", digits = 3))
#pivot_data
pivot_final <- pivot_final %>% arrange(team)
#View(pivot_final)
head(pivot_final, 10)


#saving final output data into excel on local drive
openxlsx::write.xlsx(pivot_final, file = "C://R wrkDir//Result.xlsx", sheetName = "team_rev_by_month", col.names = TRUE, row.names = TRUE, append = FALSE )


# Pivot longer, converting from wider to longer to generate Pivot table using rpivotTable() which is inherit HTML widgets.
#built around the pivottable library.
pivotl <- pivot_longer(pivot_data, cols = c("January", "February", "March" ), names_to = "month" , values_to = "team_rev" ) 
head(pivotl, 10)
rpivotTable(pivotl, rows = "team", cols = c("month"), vals = "team_rev",aggregatorName = "Sum", sorter = c("month")
            , rendererName = "Heatmap", width = "100%", height = "400px")
 


