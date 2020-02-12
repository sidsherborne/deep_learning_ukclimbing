# Housekeeping
library(magrittr)
library(dplyr)
library(httr)
library(rvest)
library(tidyr)
library(stringr)
library(quanteda)
library(pbapply)
useragent <- "Mozilla/2.0 (compatible; MSIE 3.0B; Windows NT)"

# Functions
getBetween <- function(start, end, html) {
  return(gsub(paste0("^.*",start,"\\s*|\\s*",end,".*$"), "", html))
  #gsub(paste0("(?<=", start, ")(.*)(?= ", end, ")"), "", html, perl = T)
}

getBetweenAll <- function(start, end, html) {
  str_extract_all(html, paste0(start,"(.*?)*",end), simplify = FALSE)
}

read_crag <- function(crag_id) {
  session <- paste0("https://www.ukclimbing.com/logbook/crag.php?id=",crag_id) %>% html_session(user_agent(useragent))
  session_html <- session %>% read_html() %>% as.character()
  
  crag_name <- getBetween("<title>UKC Logbook - ","</title>", session_html)
  crag_rocktype <- getBetween("<b>Rocktype</b>", "<br>", session_html)
  crag_n_climbs <- getBetween("<p><b>Climbs</b> ", "<br>", session_html)
  crag_altitude <- getBetween("<b>Altitude</b> ", "m a.s.l", session_html) %>% extract_numeric()
  crag_faces <- getBetween("<b>Faces</b> ", "</p>", session_html)
  crag_long <- getBetween("lng = ", ",", session_html)
  crag_lat <- getBetween("lat = ", ",", session_html)
  crag_oop <- session %>% html_node(xpath = "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"col-sm-5\", \" \" ))]//div") %>% as.character() %>% str_count(pattern="book.php")
  crag_books <- getBetweenAll("book.php\\?id=", '"', session_html) %>% extract2(1) %>% extract_numeric() %>% unique() %>% length() %>% subtract(crag_oop)
  crag_butresses <- str_count(session_html,"class=\"buttress\"")
  crag_comments <- session %>% html_node("#feedback") %>% as.character() %>% str_count("profile.php")
  crag_mods <- session_html %>% str_count("/forums/profile.php\\?id=") %>% subtract(crag_comments)
  
  # Initialising Data Frame With Names of Climbs
  climbs_df <- session %>% html_nodes(".climb ") %>% html_nodes(".name") %>% html_nodes("a") %>% html_text() %>% tibble()
  climbs_df <- climbs_df %>% rename("name" = ".")
  
  # Adding Climb IDs
  climbs_df <- climbs_df %>% mutate(id_climb = session %>% html_nodes(".climb ") %>% html_attr("data-id"))
  
  # Adding Buttress IDs
  climbs_df <- climbs_df %>% mutate(id_buttress = session %>% html_nodes(".climb ") %>% html_attr("data-buttress"))
  
  # Adding Number of Logs
  climbs_df <- climbs_df %>% mutate(n_logs = session %>% html_nodes(".climb ") %>% html_nodes(".number_logs_col") %>% html_text())
  
  climbs_df <- climbs_df %>% mutate(n_logs = gsub("[[:space:]]", "0", n_logs), n_logs = as.numeric(n_logs))
  
  climbs_df$crag_name <- crag_name
  climbs_df$crag_rocktype <- crag_rocktype
  climbs_df$crag_n_climbs <- crag_n_climbs %>% as.numeric()
  climbs_df$crag_altitude <- crag_altitude %>% as.numeric()
  climbs_df$crag_faces <- crag_faces
  climbs_df$crag_long <- crag_long %>% as.numeric()
  climbs_df$crag_lat <- crag_lat %>% as.numeric()
  climbs_df$crag_oop <- crag_oop %>% as.numeric()
  climbs_df$crag_books <- crag_books %>% as.numeric()
  climbs_df$crag_butresses <- crag_butresses %>% as.numeric()
  climbs_df$crag_comments <- crag_comments %>% as.numeric()
  climbs_df$crag_mods <- crag_mods %>% as.numeric()
  
  session <- paste0("https://www.ukclimbing.com/photos/item.php?crag=",crag_id) %>% html_session(user_agent(useragent))
  session_html <- session %>% read_html() %>% as.character() 
  crag_pics <- session %>% html_node(xpath="//p") %>% as.character() %>% gsub(pattern = "[\r\n]", replacement = "") %>% getBetween( start = ").<br>", end ="photos of this crag") %>% extract_numeric()
  crag_av_vote <- getBetween("average rating <b>","</b> votes", session_html) %>% as.numeric()
  crag_tot_votes <- getBetween("</b> votes <b>","</b>", session_html) %>% extract_numeric()
  
  climbs_df$crag_pics <- crag_pics %>% as.numeric()
  climbs_df$crag_av_vote <- crag_av_vote %>% as.numeric()
  climbs_df$crag_tot_votes <- crag_tot_votes %>% as.numeric()
  return(climbs_df)
}

read_climb <- function(climb_id) {
  session <- html_session(paste0("https://www.ukclimbing.com/logbook/c.php?i=",climb_id), user_agent(useragent))
  session_html <- session %>% read_html() %>% as.character()
  print(climb_id)
  if(grepl("There are no ascents logged for this climb.",session_html)==FALSE) {
    #height <- session %>% html_node(".tab-pane.active") %>% html_node("font") %>% html_text()
    #fa <- session %>% html_node(".text-muted.small") %>% html_text()
    logbook_table <- session %>% html_node("#public_logbooks") %>% html_node(".mb-0") %>% html_table(fill=T) %>% tibble() %>% use_series(".")
    names(logbook_table) <- c("climber","date","style","comment")
    logbook_table <- logbook_table %>% dplyr::filter(!climber==date)
    
    # Split comment and partners.
    # Unfortunately breaks down when people use the word "with" in comments, this leads to some clean up being required.
    logbook_table <- logbook_table %>% tidyr::separate(comment, c("comment", "partners"), sep = "(\\with\\b)(?!.*\\b\\1\\b)", fill = "right", perl = TRUE)
    
    # If last chrachter in partners is a full stop then join it with comment.
    logbook_table <- logbook_table %>% mutate(comment = if_else(str_sub(partners,start=-1)==".", paste(comment, partners, sep = "with"), comment))
    logbook_table <- logbook_table %>% mutate(partners = if_else(str_sub(partners,start=-1)==".", NA_character_, partners))
    
    # If long partner then join with comment.
    logbook_table <- logbook_table %>% mutate(comment = if_else(nchar(partners)>60, paste(comment, partners, sep = "with"), comment))
    logbook_table <- logbook_table %>% mutate(partners = if_else(nchar(partners)>60, NA_character_, partners))
    
    # If more than 2 words but no commas then join with comment.
    logbook_table <- logbook_table %>% mutate(comment = if_else(str_count(partners,",")==0 & str_count(partners, '\\w+')>2, paste(comment, partners, sep = "with"), comment))
    logbook_table <- logbook_table %>% mutate(partners = if_else(str_count(partners,",")==0 & str_count(partners, '\\w+')>2, NA_character_, partners))
    
    # Count number of partners.
    logbook_table <- logbook_table %>% mutate(n_partners = str_count(partners,",") + 1)
    logbook_table <- logbook_table %>% mutate(n_partners = if_else(is.na(partners)==TRUE, 0, n_partners))
    
    # Split date into Day/Month and Year, replace NA with current year.
    logbook_table <- logbook_table %>% tidyr::separate(date, c("date", "year_climb"), sep = ", ", fill = "right") %>% mutate(year_climb = replace(year_climb,is.na(year_climb), format(Sys.Date(), "%Y")))
    # Split Day/Month into Day and Month.
    logbook_table <- logbook_table %>% tidyr::separate(date, c("day_climb", "month_climb"), sep = " ", fill = "right")
    
    # Cleaning Up Missing Date Info
    logbook_table <- logbook_table %>% mutate(month_climb = if_else(grepl("\\?",day_climb), gsub("\\?","",day_climb), month_climb))
    logbook_table <- logbook_table %>% mutate(day_climb = if_else(grepl("\\?",day_climb), NA_character_, day_climb)) 
    logbook_table <- logbook_table %>% mutate(month_climb = if_else(month_climb=="", NA_character_, month_climb)) 
    
    # Split style into Discipline and Style
    logbook_table <- logbook_table %>% tidyr::separate(style, c("discipline_climb", "style_climb"), sep = " ", fill = "right") %>% mutate(style_climb = replace(style_climb,is.na(style_climb), "-"))
    
    # Hidden Climbers
    logbook_table <- logbook_table %>% mutate(hidden = if_else(climber == "Hidden", 1, 0))
    
    # Get Stars
    stars_html <- session %>% html_node(xpath = "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"bg-light\", \" \" ))]") %>% html_node("#poll3") %>% as.character()
    stars <- stars_html %>% getBetweenAll('data-n="','"',.) %>% lapply(., extract_numeric) %>% extract2(1) %>% abs()
    mode_stars <- max(stars)
    if(mode_stars == min(stars)) {
      rating <- "None"
    } else {
      position_stars <- match(mode_stars, stars)
      if (position_stars == 1) { rating <- "Three" } else if (position_stars == 2) { rating <- "Two"} else if (position_stars == 3) { rating <- "One"} else if (position_stars == 4) { rating <- "Zero"} else if (position_stars == 5) { rating <- "Rubbish"} else { rating <- "Missing"}
    }
    
    # Get Grade
    grade <- session %>% html_node(xpath = "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"text-nowrap\", \" \" ))]") %>% html_text() %>% trimws()
    
    # Adding columns to table.
    logbook_table$id_climb <- climb_id
    logbook_table$rating_climb <- rating
    logbook_table$grade_climb <- grade
    
    # Only return if there have been ascents logged.
    return(logbook_table)
  } 
}

# Scraping the Stanage Crags
## Stanage North: 99
## Stanage Plantation: 101
## Stanage Popular: 104
stanage <- c(99,101,104)
stanage_climbs <- lapply(stanage, read_crag) %>% bind_rows()

# Scraping all climbs.
all_climbs <- pblapply(stanage_climbs$id_climb, read_climb)
all_climbs_bound <- all_climbs %>% bind_rows()

# Merging with crag.
crag_climb_data <- merge(stanage_climbs, all_climbs_bound, by = "id_climb", all = TRUE)

# Creating a document feature matrix from the comments after removing punctuation and stopwords.
# Trimming so that only words used more than 50 times are included.
dfm <- crag_climb_data %>% use_series("comment") %>% tokens(remove_punct = TRUE) %>% dfm() %>% dfm_remove(stopwords("english")) %>% dfm_trim(min_termfreq = 50, verbose = T)
most_common_words <- dfm %>% textstat_frequency(n = 5000) %>% filter(!is.na(feature)) %>% use_series("feature") 
dfm <- dfm %>% convert(to = "data.frame") %>% select(most_common_words)

# Binding with Main Data
crag_climb_data <- crag_climb_data %>% bind_cols(dfm)

# Flattening to single row per climb.
crag_climb_data_flat <- crag_climb_data %>% select(-one_of(c("name","climber","comment","partner")))
crag_climb_data_flat <- crag_climb_data_flat %>% fastDummies::dummy_cols(select_columns = c("id_buttress","crag_name","crag_rocktype","crag_faces","day_climb","month_climb","year_climb","discipline_climb","style_climb","rating_climb","grade_climb"), remove_selected_columns = TRUE)
crag_climb_data_flat <- crag_climb_data_flat %>% group_by(id_climb) %>% summarise_all(mean, na.rm = TRUE) %>% tibble()
