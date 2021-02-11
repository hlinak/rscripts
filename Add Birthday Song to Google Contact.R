#Add Birthday #1 Song to Google Contacts
#Contact: hlinak AT GEEmail PERIOD com

library(rvest)
library(dplyr)

path <- "FILLMEIN" #The folder where your contact file is saved (and where the output will be saved)
contact_csv <- "FILLMEIN" #The initial google contact file
output_csv <- "formated_birthdays.csv" #The output file, note the header will need to be edited manually to 'fix' the headers so as to remove the '.'s
field_number <- "3" #The next custom field number in the 

var1 <- paste("Custom.Field.",field_number,"...Type",sep='')
var2 <- paste("Custom.Field.",field_number,"...Value",sep='')

contacts <- read.csv(paste(path, contact_csv, sep=''))

contacts_new <- contacts %>%
  mutate(BirthdayAsDate = as.Date(Birthday, format="%Y-%m-%d"))

formated_birthdays <- contacts_new %>%
  filter(!is.na(BirthdayAsDate)) 

non_formated_birthdays <- contacts_new %>%
  filter(is.na(BirthdayAsDate) & Birthday != '')

for(i in 1:nrow(formated_birthdays)) {
    url <- paste("https://www.birthdayjams.com/us/",format(formated_birthdays[i,"BirthdayAsDate"], "%Y/%m/%d"), sep='')
    html <- read_html(url)
    title <- html %>%
      html_nodes(".visible-xs h2") %>%
      html_text() 
    title <- gsub("\t", "", gsub("\n", "", title))
    
    artist <- html %>%
      html_nodes(".visible-xs h3") %>%
      html_text() 
    artist <- gsub("\t", "", gsub("\n", "", artist))
    
    title_artist <- paste(title, " - ", artist)
    if(title_artist != "   -   ") {
      formated_birthdays[i,var1] <- "Birthday Song"
      formated_birthdays[i,var2] <- title_artist
    }
}

write.csv(select(formated_birthdays, 
                 Name,
                 Given.Name,
                 Family.Name,
                 !!var1,
                 !!var2), 
          paste(path, output_csv, sep=''))

