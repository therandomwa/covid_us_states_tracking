# load Aijin's data
df3 = read.csv("../Data/meta_2020-05-14_aw.csv")
# load Chistian's data
load("../Data/meta_2020-05-14-cbp.rda")
df = full_data_20200514
col_num = grep("age|gender|race", colnames(df))
df2 = as.data.frame(df)
df2[,col_num] = NA
df2[,col_num] = 
  sapply(df[,col_num], function(y){
    sapply(y, 
           function(x){
             x = as.data.frame(x)
             if (all(is.na(x[,2]))){
               return (NA)
             }
             paste0(x[,1], ":", x[,2]) %>% 
               sub(".*?_", "", .) %>% 
               paste0(collapse = "; ")
           })})
df2$last.update = df2$last.update %>% format("%m/%d/%Y")

meta = rbind.fill(df3, df2)
meta$X = NULL
meta = meta[order(meta$state_name), ]
meta[meta == ""] = NA
meta$positivity.rate = NULL
meta$county.details = NULL
meta$Link = NULL
meta = meta[-grep("Guam|Virgin Island|Puerto Rico", meta$state_name),]
write.csv(meta, file = paste0("../Data/Archive/meta_final_", Sys.Date()-2, ".csv"),
          row.names = F)
