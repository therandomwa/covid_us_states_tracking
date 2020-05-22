load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}
# load Aijin's data
df_aw = read.csv("meta_2020-05-19_aw.csv")
# load Chistian's data
df_cbp = load_object("meta_2020-05-19-cbp.rda")
col_num = grep("age|gender|race", colnames(df_cbp))
df2_cbp = as.data.frame(df_cbp)
df2_cbp[,col_num] = NA
df2_cbp[,col_num] = 
  sapply(df_cbp[,col_num], function(y){
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
df2_cbp$last.update = df2_cbp$last.update %>% format("%m/%d/%Y")

meta = rbind.fill(df_aw, df2_cbp)
meta$X = NULL
meta = meta[order(meta$state_name), ]
meta[meta == ""] = NA
meta$positivity.rate = NULL
meta$county.details = NULL
meta$Link = NULL
write.csv(meta, file = paste0("meta_final_", Sys.Date(), ".csv"),
          row.names = F)
