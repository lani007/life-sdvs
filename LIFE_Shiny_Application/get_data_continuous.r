library(DBI)
source("connection.r")

get.data <- function(query){
  fetch(dbSendQuery(ldb,query),n=-1)}

q_title_from_db <- "select TITLE from life_shiny.mdb_derivative where object_name=ONAME"

q_hg <- "select lpad(floor(r00004_f0004/10), 2, 0) as id, r00001_f0003 as gender, age(r00001_f0005, d00079_f0002) as age, d00079_f0005 as value
              from D00079, r00001, r00004
              where d00079_f0001 = r00001_f0002
              and d00079_f0001 = r00004_f0003 
              and d00079_f0004 = 1"
# bmi
qt <- "select lpad(floor(r00004_f0004/10), 2, 0) as id, r00001_f0002 as sic, r00001_f0003 as gender, age(r00001_f0005, D00074_F0002) as age, D00074_F0004 as value from D00074, r00001, r00004 where D00074_F0001 = r00001_f0002 and D00074_F0001 = r00004_f0003"

qb <- "select lpad(floor(r00004_f0004/10), 2, 0) as id, r00001_f0003 as gender, age(r00001_f0005, d00079_f0002) as age, d00079_f0005 as value
from D00079, r00001, r00004
where d00079_f0001 = r00001_f0002
and d00079_f0001 = r00004_f0003"

create_query <- function(tableName, sic, inDate, plotValue){
  q <- gsub("D00079",tableName,qb) 
  q <- gsub("d00079_f0001",sic,q)
  q <- gsub("d00079_f0002",inDate,q)
  q <- gsub("d00079_f0005",plotValue,q)
  q <- gsub("\n"," ",q)
  return(q) 
}

add_data_col <- function(data){
  data$num <- 1
  max_age <- ceiling(max(data$age))
  # split into age classes
  age_class <- as.vector(cut(as.numeric(data$age), breaks=c(18,40,60,max_age),labels =c("(18,40]","(40,60]","(60,80+]") ))
  # age_class_name <<- as.vector(sort(unique(age_class)))
  data$age_class <- age_class
  # add sbz.id into data
  sbz_labels <- ot_label[,c(1,3)]
  data <- merge(sbz_labels,data)
  return(data)
}

generate_data <- function(q=q_hg,userData=NULL){
  if (is.null(userData)){
    data <- get.data(q)
    colnames(data) <- tolower(colnames(data))
    # generate userData example
    # csvData <<-data
    cc <- complete.cases(data)
    data <- data[cc,]
  }else if (is.null(q)){
    data <- userData
    colnames(data) <- tolower(colnames(data))
    cc <- complete.cases(data)
    data <- data[cc,]
  }
  data <- add_data_col(data)
  return(data)
}

get_group_data <- function(data, in_gender = "all", in_age = "all", groupvars = "id"){
  if(in_gender == "all") in_gender <- unique(data$gender[!is.na(data$gender)])
  if(in_age == "all") in_age <- unique(data$age_class[!is.na(data$age_class)])
  group <-  data %>% filter(gender %in% in_gender, age_class %in% in_age) %>% group_by_(.dots = groupvars)
  dplyr::summarise(group, num = sum(num), mean = mean(value), sdV = sd(value),
                   median = median(value),  q1V = quantile(value,0.25), q3V = quantile(value,0.75)) 
}

# selected_data for table and show for map
value_to_show <- function(data, in_gender,in_age, in_stats, map="ot",filter_value){
  common_colnames <- c("Absolut frequency", "Mean","Standard deviation","Standard error","95% Confidence interval", "Median","Q1","Q3")
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
  selected_data <- get_group_data(data=data, in_gender=in_gender, in_age =in_age, groupvars = groupvars)
  # filter data using 'num' (absolute frequency)
  selected_data_filtered <- filter(selected_data, num > filter_value)
  
  # calculate 95% CI
  selected_data <- mutate(rowwise(selected_data),se=sdV/sqrt(num), lci=mean+qt(0.025,df=num-1)*se, uci=mean+qt(0.975,df=num-1)*se) 
  selected_data <- mutate(rowwise(selected_data), ci=paste(as.character(format(lci,digits=2,nsmall = 2)),as.character(format(uci,digits=2,nsmall = 2)),sep="~"))  
  selected_data <- select(selected_data,-lci,-uci)
  selected_data <- selected_data %>% select(-median,-q1V,-q3V, everything())
  
  if( nrow(selected_data_filtered)== 0 ) warning('No data to fill the plot. Check if the filter value is set too high.')
  selected_data <- cbind(selected_data[,1:start_col-1],format(selected_data[,start_col:ncol(selected_data)],digits = 2,nsmall = 2))
  if (map == "sbz"){
    colnames(selected_data)[1] <- "id"
    table_colnames[1] <- "Stadtbezirk ID"
  }
  selected_data <- merge(name, selected_data, by="id")
  
  # label with stats value
  colnames(selected_data)[2] <- "name" 
  stats_label <- select(selected_data,id,name,num,mean,sdV,median)
  stats_label <- mutate(rowwise(stats_label), name.num=paste(name,num,sep=": "), name.mean=paste(name,paste(mean,sdV,sep = "\U00b1"),sep=": "),name.median=paste(name,median,sep=": "))  
  stats_label <- select(stats_label,id,name,name.num, name.mean, name.median)

  # prepare fill data
  colNum <- match(in_stats,names(selected_data_filtered))
  show <- selected_data_filtered[,c(1,colNum)]
  colnames(show)[1] <- "id"
  
  # fill values for ALL ids (filter function removed some rows)
  if (map == "ot"){
    empty_id <- data.frame(ot_label_points$id)
    colnames(empty_id) <- "id"
    show <- dplyr::left_join(empty_id, show, by="id")
    stats_label <- dplyr::left_join(empty_id, stats_label, by="id")
  }
  else if (map == "sbz"){
    empty_id1 <-data.frame(sbz_label_points$id)
    colnames(empty_id1) <- "id"
    empty_id <- data.frame(unique(ot_label$sbz.id))
    colnames(empty_id) <- "id"
    show <- dplyr::left_join(empty_id, show, by= "id")
    stats_label <- dplyr::left_join(empty_id1, stats_label, by="id")
  }
  colnames(selected_data) <- table_colnames
  out <- list(selected_data, show, stats_label)
  return(out)
}

# default values for external labels
bottom <- c("70", "04","03", "42", "43", "33","50")
left<- c("66","65","62","63","61","60")
right <- c("91", "12", "06", "01", "20", "21","14","25")
