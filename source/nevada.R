get_nevada = function() {
  
  # 05/04/2020 Note:
  # I tried to really dig into the iframe data but
  # the web scraping doesn't return any data back
  
  # This code gets down to the lowest node possible to try to get at the data
  nevada = read_html("https://nvhealthresponse.nv.gov/") %>% 
    html_nodes(".js-side-cta") %>% 
    html_nodes(".split-content-section") %>% 
    html_nodes(".split-content-section__container") %>% 
    html_nodes(".content-intro") %>% 
    html_nodes("iframe") %>% 
    xml_attrs() %>% 
    .[[1]] %>% 
    .[1] %>% # This returns another html that comes from the iframe
    read_html() %>% 
    html_nodes("#pbiAppPlaceHolder")
  
  return(nevada)
}