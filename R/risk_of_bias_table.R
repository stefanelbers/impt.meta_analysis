#preparation
#before using this script, run the load_packages and load_data scripts.

#obtain dataset Risk of Bias

#create table
t_rob <- select(dat_rob, `study id`, author, year, 6:17)
  

reactable(t_rob,
          defaultSorted = "author",
          defaultSortOrder = "asc",
          style = list(fontFamily = "Arial Narrow", fontSize = "14px"),
          defaultColDef = colDef(
            header = function(value) gsub("_", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 0),
            align = "left",
            minWidth = 70,
            headerStyle = list(background = "#f7f7f8")
          ),
          columns = list(
            `study id` = colDef(minWidth = 50),
            author = colDef(minWidth = 80),
            year = colDef(minWidth = 50)
          ),
          bordered = FALSE,
          highlight = FALSE,
          striped = FALSE,
          searchable = TRUE,
          showPageSizeOptions = TRUE,
          onClick = "expand"
)


#summary statistics ROB

rob_desc <- t_rob[4:12]

for (i in seq_along(rob_desc)) {            # 2. sequence
  print(colnames(rob_desc[i]))
  print(Freq(rob_desc[i]))
}

t_rob$author_year <- paste(t_rob$author, t_rob$year, sep = " " )

t_rob_sum <- t_rob

names(t_rob_sum)[4] <- "01. inclusion criteria"
names(t_rob_sum)[5] <- "02. measurement of condition"
names(t_rob_sum)[6] <- "03. valid methods of identification"
names(t_rob_sum)[7] <- "04. consecutive inclusion"
names(t_rob_sum)[8] <- "05. complete inclusion"
names(t_rob_sum)[9] <- "06. reporting demographics"
names(t_rob_sum)[10] <- "07. reporting clinical information"
names(t_rob_sum)[11] <- "08. outcomes reported"
names(t_rob_sum)[12] <- "09. location reported"
names(t_rob_sum)[13] <- "10. statistical analysis and dropout"
names(t_rob_sum)[14] <- "overall appraisal"

t_rob_sum[t_rob_sum=="high"] <- "no"
t_rob_sum[t_rob_sum=="low"] <- "yes"

t_rob_sum <- select(t_rob_sum, author_year, "1. inclusion criteria", "2. measurement of condition", "3. valid methods of identification",
                    "4. consecutive inclusion", "5. complete inclusion", "6. reporting demographics", "7. reporting clinical information",
                    "8. outcomes reported", "9. location reported", "10. statistical analysis and dropout", "overall appraisal")

t_rob_sum <- as.data.frame(t_rob_sum)

rob_barplot <- rob.summary(t_rob_sum, name.high = "no", name.low = "yes", name.unclear = "unclear", studies = t_rob_sum$author_year, table = TRUE)


#save rob figures to preferred EJP format (PDF  )

pdf(file='figure2.pdf', width = 45, height = 18) 
rob.summary(t_rob_sum, name.high = "no", name.low = "yes", name.unclear = "unclear", studies = t_rob_sum$author_year, table = TRUE); dev.off() 
