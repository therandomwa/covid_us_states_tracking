# load Aijin's data
df3 = read.csv("../Data/meta_2020-05-13_aw.csv")
# load Chistian's data
load("../Data/meta_2020-05-13-cbp.rda")
df = compiled_data_20200513
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
names(df2) = gsub("last.updat", "last.update", names(df2))
df2$last.update = df2$last.update %>% format("%m/%d/%Y")

meta = rbind.fill(df3, df2)
meta$X = NULL
meta = meta[order(meta$state_name), ]
meta[meta == ""] = NA
meta$positivity.rate = NULL
meta$county.details = NULL
write.csv(meta, file = paste0("../Data/meta_v2_", Sys.Date(), ".csv"),
          row.names = F)
