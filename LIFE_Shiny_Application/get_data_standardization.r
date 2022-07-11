get_age_std <- function(data, ot.id, in_gender = "all", in_age = "all", groupvars = "id"){
  if(in_gender == "all") in_gender <- unique(data$gender[!is.na(data$gender)])
  if(in_age == "all") in_age <- unique(data$age_class[!is.na(data$age_class)])
  group <-  data %>% filter(id==ot.id, gender %in% in_gender ,age_class %in% in_age) %>% group_by_(.dots = groupvars)
  dplyr::summarise(group, num = sum(num), mean = mean(value), median = median(value))
}

make_leipzig_age <- function(){
  c1<-c("(18,40]",0.39, 0.40, 0.37)
  c2<-c("(40,60]",0.30, 0.32, 0.28)
  c3<-c("(60,80+]",0.31, 0.28, 0.35)
  std <-rbind(c1,c2,c3)
  colnames(std) <- c("age_class","all","male","female")
  std <- as.data.frame(std)
  cols = c(2,3,4)
  std[,cols] = apply(std[,cols], 2, function(x) as.numeric(as.character(x)))
  return(std)
}

std <-make_leipzig_age()

get_std_df <- function(data=hg_data,in_gender="all",map="ot",filter_value=0){
  if (map == "ot"){
    id <- "id"
    name <- ot_label[,3:4]
  }
  else if (map == "sbz"){
    id <- "sbz.id"
    name <- sbz_label_points[,c(1,4)]
  }
  age_df_a <-get_group_data(data=data, in_gender=in_gender, in_age = "all", groupvars = c("id","age_class"))
  print(head(age_df_a))
  dfall <-as.data.frame(age_df_a)
  write.csv(print(dfall), "data/shiny_example_data/hg_std_male_all_age.csv")
  age_df <- select(age_df_a,id, age_class,num, mean)
  df <-merge(age_df,std,by="age_class")
  df <- df[with(df,order(id,age_class)),]
  if (in_gender=="all"){
    df$prop <-df$all
  } else if (in_gender=="1"){
    df$prop <-df$male
  } else if (in_gender=="2"){
    df$prop <-df$female
  }
  df$s_mean <- df$mean*df$prop
  df <- select(df,id,age_class,num,mean,prop,s_mean)
  dfs <- df %>% group_by(id) %>% summarise(num=sum(num),s_mean=sum(s_mean),nr=length(id))
  dfs <- as.data.frame(dfs)
  dfs <- filter(dfs,nr == 3)
  # get mean for each region 
  mean_df <-get_group_data(data=data, in_gender=in_gender, in_age = "all", groupvars = c("id"))
  age_df <- select(mean_df,id,num, mean)
  dfTable <- left_join(age_df,dfs,by="id")
  dfTable <- select(dfTable,id,num.x,mean,s_mean)
  dfTable_filtered <- filter(dfTable, num.x > filter_value)
  show <- select(dfTable_filtered,id,s_mean)

    colnames(show)[1] <- "id"
  if (map == "ot"){
    empty_id <- data.frame(ot_label_points$id)
    colnames(empty_id) <- "id"
    show <- dplyr::left_join(empty_id, show, by="id")
    col1 <- "Ortsteil ID"
  }
  else if (map == "sbz"){
#     empty_id1 <-data.frame(sbz_label_points$id)
#     colnames(empty_id1) <- "id"
    empty_id <- data.frame(unique(ot_label$sbz.id))
    colnames(empty_id) <- "id"
    show <- dplyr::left_join(empty_id, show, by= "id")
    col1 <- "Stadtbezirk ID"
  }
  colnames(dfTable) <- c(col1,"Absolute frequency","Mean","Age standardized mean")
  return(list(as.data.frame(dfTable),show,show))
} 
