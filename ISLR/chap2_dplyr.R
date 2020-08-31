library(dplyr)
# https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf

df_primary <- tribble(
  ~ID, ~y,
  "A", 5,
  "B", 5,
  "C", 8,
  "D", 0,
  "F", 9)
df_secondary <- tribble(
  ~ID, ~y,
  "A", 30,
  "B", 21,
  "C", 22,
  "D", 25,
  "E", 29)

left_join(df_primary, df_secondary, by ='ID')
right_join(df_primary, df_secondary, by = 'ID')
inner_join(df_primary, df_secondary, by ='ID')
full_join(df_primary, df_secondary, by = 'ID')

# Create origin dataframe(

producers <- data.frame(   
  surname =  c("Spielberg","Scorsese","Hitchcock","Tarantino","Polanski"),    
  nationality = c("US","US","UK","US","Poland"),    
  stringsAsFactors=FALSE)

# Create destination dataframe
movies <- data.frame(    
  surname = c("Spielberg",
              "Scorsese",
              "Hitchcock",
              "Hitchcock",
              "Spielberg",
              "Tarantino",
              "Polanski"),    
  title = c("Super 8",
            "Taxi Driver",
            "Psycho",
            "North by Northwest",
            "Catch Me If You Can",
            "Reservoir Dogs","Chinatown"),                
  stringsAsFactors=FALSE)

# Merge two datasets
m1 <- merge(producers, movies, by.x = "surname")
m1


# Change name of ` movies ` dataframe
colnames(movies)[colnames(movies) == 'surname'] <- 'name'
# Merge with different key value
m2 <- merge(producers, movies, by.x = "surname", by.y = "name")
# Print head of the data
head(m2)
