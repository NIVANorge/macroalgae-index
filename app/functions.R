
add_param_names <- function(df, options){
  # #sum_shore_desc,
  # turbid_water, sand_scouring, ice_scouring, 
  
  # #sum_dominant_type, 
  # ravine, fractured, boulders, 
  # steep, unspec_hard, rocks, shingle, 
  
  # #sum_other_type, 
  # shallow_pool, large_pool, deep_pool, 
  # small_pool, cave, overhang,
  # other_sub, 
  # points_adjust, points
  
  df <- df %>%
    rowwise() %>%
    mutate(group=parameter_group(Parameter, options)) %>%
    mutate(name=parameter_name(Parameter, options)) %>%
    ungroup() %>%
    relocate(name, .before=1) %>%
    relocate(group, .before=1)
  
  return(df)
}

parameter_group <- function(Parameter, options){
  #browser()
  grps <- names(options)
  val <- NA_character_
  if(Parameter == "points_adjust"){
    val <- "Poeng justering"
  }else if(Parameter == "points"){
    val <- "Total"
  }else{
    for(grpi in grps){
      suboptions <- options[[grpi]]
      i <- 1
      for(item in names(suboptions)){
        if(i==1){
          val_grp <- suboptions[[item]][["row_name"]]
        }
        i <- i + 1
        if(item == Parameter){
          val <- val_grp
        }
      }
    }
  }
  return(val)
}

parameter_name <- function(Parameter, options){
  #browser()
  grps <- names(options)
  val <- NA_character_
  if(Parameter == "points_adjust"){
    val <- "Poeng justering"
  }else if(Parameter == "points_adjust"){
    val <- "Poeng"
  }else{
    for(grpi in grps){
      suboptions <- options[[grpi]]
      for(item in names(suboptions)){
        if(item == Parameter){
          val <- suboptions[[item]][["row_name"]]
        }
      }
    }
  }
  return(val)
}



observation_info <- function(df, options, 
                             opts=c("species_header","points_adjust")){

  df_stns <- data.frame()
  grps <- names(options)[!names(options) %in% opts]
  for(grpi in grps){
    dfi <- observation_info_grp(df, options, grpi)
    if(nrow(df_stns)>0){
      df_stns <- df_stns %>%
        left_join(dfi, by="col")
    }else{
      df_stns <- dfi
      ncol1 <- ncol(dfi) + 1
    }
  }
  
  points_adjust <- ifelse(is.null(options$points_adjust),0,
                          options$points_adjust)
  df_stns <- df_stns %>%
    mutate(points_adjust = points_adjust) 
  
  ncol2 <- ncol(df_stns)
  
  df_stns <- df_stns %>%
    mutate(points = rowSums(across(ncol1:ncol2), na.rm=T )) %>%
    mutate(points = points + points_adjust)
    
  return(df_stns)
}



observation_info_grp <- function(df, options, info_group){

  sub_options <- options[[info_group]]
  
  info_names <- names(sub_options)
  
  info_rows_ix <- paste0("row_ix_", info_names)
  info_rows <- lapply(sub_options, function(x) x[["row_name"]]) %>% unlist()
  
  info_points <- lapply(sub_options, function(x) x[["points"]])
  
  row_names <- df %>% 
    purrr::pluck(1)
  
  row_ix <- info_rows %>% 
    purrr::map(match, row_names) %>% 
    unlist()
  
  if(sum(is.na(row_ix))>0){
    msg <- info_rows[is.na(row_ix)] %>% 
      paste0(collapse=";")
    msg <- paste0("missing rows: ",msg)
    cli::cli_warn(msg)
  }
  
  dfobs <- data.frame()
  for(i in 1:length(info_names)){

    iname <- info_names[i]
    if(iname != info_group){
    #irow <- row_ix[match(info_rows[i],row_names)]
    irow <- match(info_rows[i],row_names)
    assign(info_rows_ix[i], irow)
    dfi <- stn_param_values(irow, df, name=info_names[i], points=info_points[[i]])
    
    if(nrow(dfobs)>0){
      dfobs <- dfobs  %>%
        full_join(dfi, by="col")
    }else{
      dfobs <- dfi
    }
    }
  }
  
  
  # if("points" %in% names(sub_options[[1]])){
  #   col_name <- paste0("sum_", info_group)
  #   dfobs <-dfobs %>%
  #     mutate(!!col_name := rowSums(across(2:ncol(dfobs)), na.rm=T )) %>%
  #     relocate(!!col_name, .after=1)
  # }
  return(dfobs)
}

stn_type_points <- function(recorded, points=c(0)){
  recorded <- as.numeric(recorded)
  recorded <- ifelse(is.na(recorded),0,recorded)
  recorded <- ifelse(recorded>0,1,recorded)
  value <- ifelse(length(points)==1,
                  ifelse(recorded==1, points, 0),
                  ifelse(recorded==0, points[1], points[2]))
  return(value)
}


stn_param_values <- function(row_ix, df, name="value", type="character", points=NULL, first_col=3){
  
  type <- ifelse(stringr::str_detect(name, "date"), "date", type)
  type <- ifelse(stringr::str_detect(name, "time"), "time", type)
  type <- ifelse(is.null(points), type, "points")
  
  res  <- df[row_ix,first_col:ncol(df)] %>%
    pivot_longer(cols=everything(), names_to = "col", values_to = "value") %>%
    mutate(col=stringr::str_remove_all(col, "\\.")) %>%
    mutate(col=as.numeric(col))
  
  res <- res %>%
    filter(!is.na(value))

 
  if(type=="date"){
    res <- res %>%
      mutate(value=as.numeric(value)) %>%
      mutate(value=openxlsx::convertToDateTime(value))
  }else if(type=="time"){
    res <- res %>%
      mutate(value=as.numeric(value)) %>%
      mutate(value=openxlsx::convertToDateTime(value)) %>%
      mutate(value = format.POSIXct(value, "%H:%M"))
  }else if(type=="points"){
    res <- res %>%
      rowwise() %>%
      mutate(value=stn_type_value(value, points)) %>%
      ungroup()
    
  }
  
  names(res)[names(res)=="value"] <- name
  return(res)
}



read_excel_sheet <- function(sheet, xl_path){
  require(readxl)
  df <- readxl::read_excel(sheet=sheet, path=xl_path, col_names=F, col_types = "text")
  return(df)
}

read_excel_all <- function(xl_path, progress=NULL){
  require(readxl)
  sheets <- readxl::excel_sheets(xl_path)
  
  list_df <- list()

  for(i in 1:length(sheets)){
    df <- read_excel_sheet(sheets[i], xl_path)
    list_df[[i]] <- df
    if(!is.null(progress)){
      progress$set(value = i+1)
    }
  }
  #list_df <- lapply(sheets, read_excel_sheet, xl_path)
  names(list_df) <- sheets
  return(list_df)
}

default_sheet <- function(){
  return("Strandobservasjon")
} 


defaults <-function(){
  read.table("defaults.csv",sep=";",header=T)
}

.options_list <- function(row_name_project = NA_character_,
                    row_name_stn_code = NA_character_,
                    row_name_old_code = NA_character_,
                    row_name_stn_name = NA_character_,
                    row_name_date = NA_character_,
                    row_name_observer = NA_character_,
                    row_name_recorder = NA_character_,
                    row_name_coord_type = NA_character_,
                    row_name_long = NA_character_,
                    row_name_lat = NA_character_,
                    row_name_water_level = NA_character_,
                    row_name_time_low = NA_character_,
                    
                    row_name_species_header = NA_character_,
                    points_adjust = NA,
                    
                    # shore desc
                    row_name_shore_desc = NA_character_, 
                    row_name_turbid_water = NA_character_,   
                    row_name_sand_scouring = NA_character_,  
                    row_name_ice_scouring = NA_character_,
                    points_turbid_water = NA,
                    points_sand_scouring = NA,
                    points_ice_scouring = NA,
                    
                    # dominant shore type
                    row_name_dominant_shore = NA_character_, 
                    row_name_ravine = NA_character_,         
                    row_name_fractured = NA_character_,      
                    row_name_boulders = NA_character_,       
                    row_name_steep = NA_character_,          
                    row_name_unspec_hard = NA_character_,    
                    row_name_rocks = NA_character_,          
                    row_name_shingle = NA_character_,  
                    points_ravine = NA,
                    points_fractured = NA,
                    points_boulders = NA,
                    points_steep = NA,
                    points_unspec_hard = NA, 
                    points_rocks = NA,
                    points_shingle = NA,
                    
                    # dominant shore types
                    row_name_other_shore = NA_character_,    
                    row_name_shallow_pool = NA_character_,   
                    row_name_large_pool = NA_character_,     
                    row_name_deep_pool = NA_character_,      
                    row_name_small_pool = NA_character_,     
                    row_name_cave = NA_character_,           
                    row_name_overhang = NA_character_,       
                    row_name_other_sub = NA_character_,
                    
                    points_shallow_pool = NA,
                    points_large_pool = NA,
                    points_deep_pool = NA,
                    points_small_pool = NA,
                    points_cave = NA,
                    points_overhang = NA,
                    points_other_sub = NA
                    
){

 station <- list(
      project=list(row_name=row_name_project),
      stn_code=list(row_name=row_name_stn_code),
      old_code=list(row_name=row_name_old_code),
      stn_name=list(row_name=row_name_stn_name),
      date=list(row_name=row_name_date),
      observer=list(row_name=row_name_observer),
      recorder=list(row_name=row_name_recorder),
      coord_type=list(row_name=row_name_coord_type),
      long=list(row_name=row_name_long),
      lat=list(row_name=row_name_lat),
      water_level=list(row_name=row_name_water_level),
      time_low=list(row_name=row_name_time_low))
         
      # shore description 
      shore_desc <- list(
        shore_desc     =list(row_name=row_name_shore_desc),
        turbid_water   = list(row_name=row_name_turbid_water, 
                              points = points_turbid_water),   
        sand_scouring  = list(row_name=row_name_sand_scouring,
                              points = points_sand_scouring),
        ice_scouring   = list(row_name = row_name_ice_scouring,
                              points = points_ice_scouring))
    
      # dominant shore type 
      dominant_shore <- list(
        dominant_shore  = list(row_name=row_name_dominant_shore),
        ravine         = list(row_name=row_name_ravine,  
                              points=points_ravine),         
        fractured      = list(row_name=row_name_fractured,  
                              points=points_fractured),             
        boulders       = list(row_name=row_name_boulders,  
                              points=points_boulders),               
        steep          = list(row_name=row_name_steep,  
                              points=points_steep),                   
        unspec_hard    = list(row_name=row_name_unspec_hard,  
                              points=points_unspec_hard),             
        rocks          = list(row_name=row_name_rocks,  
                              points=points_rocks),                   
        shingle        = list(row_name=row_name_shingle,  
                              points=points_shingle))    
      
         # other shore types 
      other_shore = list(
        other_shore   = list(row_name=row_name_other_shore),
        shallow_pool   = list(row_name=row_name_shallow_pool,  
                              points=points_shallow_pool),  
        large_pool     = list(row_name=row_name_large_pool,      
                              points=points_large_pool),   
        deep_pool      = list(row_name=row_name_deep_pool,     
                              points=points_deep_pool),     
        small_pool     = list(row_name=row_name_small_pool,    
                              points=points_small_pool),     
        cave           = list(row_name=row_name_cave,          
                              points=points_cave),     
        overhang       = list(row_name=row_name_overhang,      
                              points=points_overhang),     
        other_sub      = list(row_name=row_name_other_sub,  
                              points=points_other_sub)) 
      
    return(
        list(station=station,
             species_header=row_name_species_header,
             points_adjust = points_adjust,
             shore_desc=shore_desc,
             dominant_shore=dominant_shore,
             other_shore=other_shore)
    )
}

options <- .options_list(row_name_project   = row_name_project,
                         row_name_stn_code  = row_name_stn_code,
                         row_name_old_code  = row_name_old_code,
                         row_name_stn_name  = row_name_stn_name,
                         row_name_date      = row_name_date,
                         row_name_observer  = row_name_observer,
                         row_name_recorder  = row_name_recorder,
                         row_name_coord_type = row_name_coord_type,
                         row_name_long      = row_name_long,
                         row_name_lat       = row_name_lat,
                         row_name_water_level = row_name_water_level,
                         row_name_time_low  = row_name_time_low,
                         
                         # species recorded section
                         row_name_species_header = row_name_species_header,
                         points_adjust = points_adjust_NO,
                         
                         # shore description 
                         row_name_shore_desc     = row_name_shore_desc,     
                         row_name_turbid_water   = row_name_turbid_water,   
                         row_name_sand_scouring  = row_name_sand_scouring,  
                         row_name_ice_scouring   = row_name_ice_scouring,   
                         points_turbid_water     = points_turbid_water,
                         points_sand_scouring    = points_sand_scouring,
                         points_ice_scouring     = points_ice_scouring,
                         
                         # dominant shore type= # dominant shore type
                         row_name_dominant_shore = row_name_dominant_shore, 
                         row_name_ravine         = row_name_ravine,         
                         row_name_fractured      = row_name_fractured,      
                         row_name_boulders       = row_name_boulders,       
                         row_name_steep          = row_name_steep,          
                         row_name_unspec_hard    = row_name_unspec_hard,    
                         row_name_rocks          = row_name_rocks,          
                         row_name_shingle        = row_name_shingle,   
                         points_ravine           = points_ravine,
                         points_fractured        = points_fractured,
                         points_boulders         = points_boulders,
                         points_steep            = points_steep,
                         points_unspec_hard      = points_unspec_hard, 
                         points_rocks            = points_rocks,
                         points_shingle          = points_shingle,

                         # dominant shore types= # dominant shore types
                         row_name_other_shore    = row_name_other_shore,    
                         row_name_shallow_pool   = row_name_shallow_pool,   
                         row_name_large_pool     = row_name_large_pool,     
                         row_name_deep_pool      = row_name_deep_pool,      
                         row_name_small_pool     = row_name_small_pool,     
                         row_name_cave           = row_name_cave,           
                         row_name_overhang       = row_name_overhang,       
                         row_name_other_sub      = row_name_other_sub,
                         points_shallow_pool     = points_shallow_pool,
                         points_large_pool       = points_large_pool,
                         points_deep_pool        = points_deep_pool,
                         points_small_pool       = points_small_pool,
                         points_cave             = points_cave,
                         points_overhang         = points_overhang,
                         points_other_sub        = points_other_sub
 
)

