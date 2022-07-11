library(DBI)
source("connection.r")

q_bmi <- "select lpad(floor(r00004_f0004/10), 2, 0) as id, r00001_f0003 as gender, age(r00001_f0005, d00074_f0002) as age, d00074_f0005 as category
from D00074, r00001, r00004
where d00074_f0001 = r00001_f0002
and d00074_f0001 = r00004_f0003"

create_query_cat <- function(tableName, sic, inDate, plotValue){
  q <- gsub("D00074",tableName,q_bmi) 
  q <- gsub("d00074_f0001",sic,q)
  q <- gsub("d00074_f0002",inDate,q)
  q <- gsub("d00074_f0005",plotValue,q)
  q <- gsub("\n"," ",q)
  return(q) 
}

qb_legend <- "select col_name,code,name from life_shiny.v_mdb_derivative_col_codelist where col_name='D00074_F0005' order by CODE"

get_legend_text <- function(colName="D00074_F0005"){
  q_legend <- gsub("D00074_F0005",colName,qb_legend)
  legends <- get.data(q_legend)
  code <- legends[,2]
  text <- legends[,3]
  legend_text <- paste(code,text,sep=": " )
  return(legend_text)
}

# for categorical data
get_group_data_cat <- function(data, in_gender = "all", in_age = "all", groupvars = "id", in_category = "all"){
  if(in_gender == "all") in_gender <- unique(data$gender[!is.na(data$gender)])
  if(in_age == "all") in_age <- unique(data$age_class[!is.na(data$age_class)])
  if(in_category == "all") in_category <- unique(data$category[!is.na(data$category)])
  group <- data %>% filter(gender %in% in_gender,age_class %in% in_age, category %in% in_category) %>% group_by_(.dots = groupvars)
  dplyr::summarise(group, num = sum(num))
}

# selected_data for table and show for map
value_to_show_cat <- function(data, in_gender,in_age, in_stats, in_category="all", map="ot", filter_value="0"){
  common_colnames <- c("Category","Absolut frequency")
  if (map == "ot"){
    id <- "id"
    name <- ot_label[,3:4]
  }
  else if (map == "sbz"){
    id <- "sbz.id"
    name <- sbz_label_points[,c(1,4)]
  }
    if (in_age == "all" & in_gender == "all"){
    groupvars <- id
    table_colnames <-c("Ortsteil ID","Name",common_colnames)
    # for formating
    start_col <- 3
  }
  else if (in_age == "all" & in_gender != "all"){
    groupvars <- c(id,"gender")
    table_colnames<-c("Ortsteil ID","Name","Gender",common_colnames)
    start_col <- 4
  }
  else if (in_gender == "all" & in_age != "all"){
    groupvars <- c(id,"age_class")
    table_colnames<-c("Ortsteil ID","Name","Age Groups",common_colnames)
    start_col <- 4
  }
  else{
    groupvars <- c(id,"gender","age_class")
    table_colnames<-c("Ortsteil ID","Name","Gender","Age Groups",common_colnames)
    start_col <- 5
  }
    # categorical data needs extra grouping by category
    groupvars <- c(groupvars,"category")
    selected_data <- get_group_data_cat(data=data, in_gender=in_gender, in_age =in_age, in_category = in_category, groupvars = groupvars)
    
    if (map == "sbz"){
      colnames(selected_data)[1] <- "id"
      id <- "id"
      table_colnames[1] <- "Stadtbezirk ID"
    }

    # number of samples in each region (id)
    idSum <- selected_data %>% group_by(id) %>% summarise(idSum = sum(num))
    selected_data <- left_join(selected_data,idSum,by="id")

    # filter data using 'idSum' (absolute frequency of each region) 
    selected_data_filtered <- filter(as.data.frame(selected_data), idSum > filter_value)
    selected_data <- merge(name, selected_data_filtered, by="id")
    # prepare fill data
    colNum <- match(in_stats,names(selected_data_filtered)) 
    colCat <- match("category",names(selected_data_filtered))  
    show <- selected_data_filtered[,c(1,colCat,colNum)] # select num and category
    colnames(show)[1] <- "id"

    # remove idSum
    selected_data <- select(selected_data,-idSum)
    colnames(selected_data) <- table_colnames
    
    # for default legend text
    categories <- sort(unique(show$category))
    return(list(selected_data,show))
}

