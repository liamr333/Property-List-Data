library(RMySQL)

database_name = 'propdblist1'
endpoint = 'property-list-db-test.cjmg2oceahv0.us-east-1.rds.amazonaws.com'
password = 'yGBsRPgXnE'

analysts <- function() {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  # use prop_list
  query_result <- dbSendQuery(mysqlconnection, "select * from PROP_LISTS.Analyst")
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  df$Analyst_Name
}

properties <- function() {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query_result <- dbSendQuery(mysqlconnection, "select Prop_ID from PROP_LISTS.Property")
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  df$Prop_ID
}


get_lists_from_analyst <- function(analyst) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("select Prop_List_Name from PROP_LISTS.Prop_List where Prop_List_Creator = \'", analyst, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  df$Prop_List_Name
}


get_properties_from_list <- function(list_name) {
  # for our example, we are pretending this is a list selector in the Harris county dashboard
  county = "Harris"
  
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("select Property.Prop_ID from PROP_LISTS.Prop_List inner join PROP_LISTS.Entry on PROP_LISTS.Prop_List.Prop_List_Name = PROP_LISTS.Entry.Entry_List_Name inner join PROP_LISTS.Property on PROP_LISTS.Property.Prop_ID = PROP_LISTS.Entry.Entry_Prop_ID where PROP_LISTS.Prop_List.Prop_List_Name = \'",
                  list_name, "\' and PROP_LISTS.Property.Prop_County = \'", county, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  df$Prop_ID
}


analyst_exists <- function(analyst_name) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("select * from PROP_LISTS.Analyst where Analyst_Name = \'", analyst_name, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  nrow(df) > 0
}


add_analyst <- function(analyst_name) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("insert into PROP_LISTS.Analyst (Analyst_Name) values (\'", analyst_name, "\')")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
}

list_exists <- function(list_name) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("select * from PROP_LISTS.Prop_List where Prop_List_Name = \'", list_name, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  nrow(df) > 0
}

add_list <- function(list_creator, list_name, list_description) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("insert into PROP_LISTS.Prop_List (Prop_List_Name, Prop_List_Creator, Prop_List_Description) values (\'", list_name, "\', \'", list_creator, "\', \'", list_description, "\')")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
}

entry_exists <- function(entry_list_name, entry_prop_id) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("select * from PROP_LISTS.Entry where Entry_List_Name = \'", entry_list_name, "\' and Entry_Prop_ID = \'", entry_prop_id, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  nrow(df) > 0
}

add_entry <- function(entry_list_name, entry_prop_id) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("insert into PROP_LISTS.Entry (Entry_List_Name, Entry_Prop_ID) values (\'", entry_list_name, "\', \'", entry_prop_id, "\')")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
}

delete_entry <- function(entry_list_name, entry_prop_id) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("delete from PROP_LISTS.Entry where Entry_List_Name = \'", entry_list_name, "\' and Entry_Prop_ID = \'", entry_prop_id, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
}

get_num_properties_in_list <- function(list_name) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host=endpoint, 
                               port=3306, 
                               user='admin', 
                               password=password)
  
  query <- paste0("select * from PROP_LISTS.Entry where Entry_List_Name = \'", list_name, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  nrow(df)
}
