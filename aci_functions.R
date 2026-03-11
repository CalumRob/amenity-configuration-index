treat_buildings <- function(data, years = c("14","17","20","23")){
  setnafill(data, cols = c(2:ncol(data)), fill = 0)
   
   data <- data %>% filter(substr(building_id,nchar(building_id)-1, nchar(building_id)) %in% years)
#   
#   
   data <- data %>% mutate(CA115 = CA115 + CA301_Bio + CA302_Bio + CA108_Bio + CA202_Bio + CA103_Bio,
                         CE403 = CE403 + ce403, CB111 = cb111 + CB111) %>%
   select(-c(CA301_Bio,CA302_Bio,CA108_Bio,CA202_Bio,CA103_Bio,ce403,cb111))
   
 data <- data %>% mutate(CA115 = CA115 + CA302_Bio,
                         CA301 = CA301_Bio + CA301,
                         CA202 = CA202 + CA202_Bio,
                         CH303 = CH303 + CH303_Bio,
                         CC201 = CC201 + CC201_Bio,
                         CA103 = CA103 + CA103_Bio,
                         CA108 = CA108 + CA108_Bio,
                         CE403 = CE403 + ce403, 
                         CB111 = cb111 + CB111,
                         CC202 = CC202_Bio + CC202) %>%
   select(-c(CA301_Bio, CA202_Bio, CA302_Bio, CC202_Bio,
   CA108_Bio,CA103_Bio, CC201_Bio, CH303_Bio,
             ce403,cb111,CC203))
#   
# 
#   
#   
 data[, "Car related" := "CG102" + "CG103" + "CG104" + "CG201" + "CG202"]
 data[, "Motorbike related" := "CG106" + "CG204"]
 data[, (voitures_a_aggreger) := NULL]
 data[, (motos_a_aggreger) := NULL]
 
 data <- data %>% filter(!(CODE_ACTIVITE == one_of(filtered_out_amenities, voitures_a_aggreger, motos_a_aggreger)))


return(data)
}

TCI <- function(buildings_data, RCA = F, years = c("All"), for_aci = F, rolling = F, return_similarity = F){
  
  buildings_all <- data.table()
  
  if("All" %in% years){
    years = c("2014","2017","2020","2023")
  }
  
  if(isTRUE(rolling)){
    
    
    full_df = data.table()
    
    for(current_year in years){
      
      buildings_all <- data.table()
      
      if(current_year == "2014"){
        good_years = c("14","17")
      }
      if(current_year == "2017"){
        good_years = c("14","17","20")
      }
      if(current_year == "2020"){
        good_years = c("17","20","23")
      }
      if(current_year == "2023"){
        good_years = c("20","23")
      }
      
      temp_build = buildings_data %>% select(id,CODE_ACTIVITE,all_of(paste0("Year",good_years)))
      
      yearcol = paste0("Year",substr(current_year,3,4))
      
      good_codes <- unique(temp_build[as.vector(temp_build[,..yearcol] > 0),]$CODE_ACTIVITE)
      
      temp_build <- temp_build %>% filter(CODE_ACTIVITE %in% good_codes)
      
      for(good_year in good_years){
        
        temp = temp_build %>%
          pivot_wider(., id_cols = "id",
                      names_from = "CODE_ACTIVITE", 
                      values_from = paste0("Year",good_year),
                      values_fill = 0)
        
        if(paste0("20", good_year) == current_year){
          good_codes = names(temp)[colSums(temp) > 0]
        }
        
        # if(!(names(buildings_all) == names(temp%>% 
        #                                    select(all_of(good_codes))))){
        #   browser()
        # }
        # print(year)
        # print(good_year)
        # print(setdiff(names(temp), names(buildings_all)))
        # print(setdiff(names(buildings_all), names(temp)))
        
        buildings_all = rbind(buildings_all, temp %>% 
                                mutate(id = paste0(good_year,"_",id)))
        
        
      }
      
      
      gc()
      
      #buildings_all <- buildings_all %>% select(all_of(good_codes))
      
      mat <- as.matrix(buildings_all %>% select(-id),rownames = buildings_all$id)
      mat[is.na(mat)] <- 0
      
      
      if(length(mat[, colSums(mat) == 0])>0){
        message("Warning! One column (amenity type) will be supressed.\nYou should probably check that out.")
      }
      
      mat <- mat[rowSums(mat) > 0, ]
      mat <- mat[, colSums(mat) > 0]
      mat <- mat[complete.cases(mat),]
      
      rca <- mat
      rca[is.na(rca)] <- 0
      rca[rca < 1] <- 0
      rca[rca > 0] <- 1
      
      if(isTRUE(RCA)){
        mat <- rca
      }
      
      ubiquity = colSums(rca)/length(good_years)
      
      tm = (t(mat) / colSums(mat)) %*% (mat / rowSums(mat))
      
      if(isTRUE(return_similarity)){
        return(tm)
      }
      
      #TCI <- eigen((t(mat) / colSums(mat)) %*% (mat / rowSums(mat)))
      eig = eigen(tm)
      magnitudes <- Mod(eig$values)
      ordering <- order(magnitudes, decreasing = TRUE)
      second_leading_eigenvalue <- eig$values[ordering[2]]
      second_leading_eigenvector <- eig$vectors[, ordering[2]]
      
      if(any(Im(second_leading_eigenvalue) > 0) | any(Im(second_leading_eigenvector) > 0)){
        stop("The second leading eigenvalue or eigenvector is complex.\n Good luck, that sounds like a bit of a headache.")
      }else{
        TCI = Re(second_leading_eigenvector)
      }
      
      #TCI <- TCI$vectors[, 2]
      TCI_unsc <- TCI
      TCI <- (TCI - abs(mean(TCI, na.rm = T))) / sd(TCI)
      
      # print(year)
      # print(class(rca))
      # print(class(ubiquity))
      # print(head(TCI))
      if (cor(TCI, ubiquity, use = "pairwise.complete.obs", 
              method = "spearman") > 0){ 
        TCI <- TCI * (-1)
        TCI_unsc <- TCI_unsc * (-1)
      }
      
      df = as.data.frame(TCI)
      
      df$id = as.character(colnames(mat))
      names(df)[1] = "Complexity"
      
      df$Complexity = as.numeric(df$Complexity)
      
      df$Complexity_unscaled = TCI_unsc
      
      df <- df %>% mutate(rank = rank(-Complexity))
      
      avg_diversity = EconGeo::MORt(rca, steps = 1)
      df$diversity = avg_diversity
      df$ubiquity = ubiquity
      
      df$Year = current_year
      
      full_df <- rbind(full_df, df)
    }
    
    return(full_df)
  }
  
  
  if("2014" %in% years){
    buildings_14 <- buildings_data %>%
                                pivot_wider(., id_cols = "id",
                                            names_from = "CODE_ACTIVITE", 
                                            values_from = "Year14",
                                            values_fill = 0)
    
    buildings_all = rbind(buildings_all, buildings_14 %>% mutate(id = paste0("14_",id),
                                                                 Year = "2014"))
  }
  if("2017" %in% years){
    buildings_17 <- pivot_wider(buildings_data, id_cols = "id", 
                                names_from = "CODE_ACTIVITE", 
                                values_from = "Year17",
                                values_fill = 0)
    
    buildings_all = rbind(buildings_all, buildings_17 %>% mutate(id = paste0("17_",id),
                                                                 Year = "2017"))
  }
  if("2020" %in% years){
    buildings_20 <- pivot_wider(buildings_data, id_cols = "id", 
                                names_from = "CODE_ACTIVITE", 
                                values_from = "Year20",
                                values_fill = 0)
    
    buildings_all = rbind(buildings_all, buildings_20 %>% mutate(id = paste0("20_",id),
                                                                 Year = "2020"))
  }
  if("2023" %in% years){
    buildings_23 <- pivot_wider(buildings_data, id_cols = "id", 
                                names_from = "CODE_ACTIVITE", 
                                values_from = "Year23",
                                values_fill = 0)
    
    buildings_all = rbind(buildings_all, buildings_23 %>% mutate(id = paste0("23_",id),
                                                                 Year = "2023"))
    
  }
  
  gc()
  
  if(!("2023" %in% years)){
    buildings_all <- buildings_all %>% select(-CC203)
  }
  
  mat <- as.matrix(buildings_all %>% select(-id,-Year),rownames = buildings_all$id)
  #rca[is.na(rca)] <- 0
  
  if(for_aci == T){
    return(mat)
  }
  
  rca <- mat
  rca[rca < 1] <- 0
  rca[rca > 0] <- 1
  
  if(isTRUE(RCA)){
    mat <- rca
  }
  
  if(length(rca[, colSums(rca) == 0])>0){
    message("Warning! One column (amenity type) will be supressed. You should probably check that out.")
  }
  
  mat <- mat[rowSums(mat) > 0, ]
  mat <- mat[, colSums(mat) > 0]
  mat <- mat[complete.cases(mat),]
  
  #yci = EconGeo::TCI(rca)
  
  ubiquity = colSums(rca)
  
  tm = (t(mat) / colSums(mat)) %*% (mat / rowSums(mat))
  TCI <- eigen((t(mat) / colSums(mat)) %*% (mat / rowSums(mat)))
  TCI = eigen(tm)
  TCI <- TCI$vectors[, 2]
  TCI <- (TCI - mean(TCI, na.rm = T)) / sd(TCI)
  
  if (cor(TCI, ubiquity, use = "pairwise.complete.obs", 
          method = "spearman") > 0){ 
    TCI <- TCI * (-1)}
  
  df = as.data.frame(TCI)
  
  df$id = as.character(colnames(mat))
  names(df)[1] = "Complexity"
  
  df$Complexity = as.numeric(df$Complexity)
  
  df <- df %>% mutate(rank = rank(-Complexity))
  
  avg_diversity = EconGeo::MORt(rca, steps = 1)
  df$diversity = avg_diversity
  df$ubiquity = ubiquity
  
  return(df)
}

ACI_fixed <- function(buildings_data, tci_data, steps = 20, need_minus = F, full_sum = F, RCA = F,
                      years = c("All"), rolling = F){
  
  #buildings_data <- buildings_together
  mat <- TCI(buildings_data, years = years, for_aci = T, rolling = F)
  
  if("All" %in% years){
    years = c("2014","2017","2020","2023")
  }
  
  if(isTRUE(rolling)){
    
    full_aci_df = data.table()
    
    for(current_year in years){
      
      small_mat = subset(mat, substr(rownames(mat),1,2) == substr(current_year, 3,4))
      
      small_mat[is.na(small_mat)] <- 0
      
      small_mat <- small_mat[rowSums(small_mat) > 0, ]
      small_mat <- small_mat[, colSums(small_mat) > 0]
      small_mat <- small_mat[complete.cases(small_mat),]
      
      rca <- small_mat
      rca[rca < 1] <- 0
      rca[rca > 0] <- 1
      nrow(small_mat)
      
      if(isTRUE(RCA)){
        small_mat <- rca
      }
      
      
      ubiquity = colSums(rca)
      
      small_tci = tci_data[tci_data$Year == current_year,]
      
      TCI = small_tci$Complexity_unscaled
      
          
      if(isTRUE(full_sum)){
        sum_vec <- apply(small_mat, 1, function(x) sum(x * TCI))
        result <- as.data.frame(cbind(rownames(small_mat), sum_vec))
        names(result)[1] = "id"
      }else{
        sum_vec <- apply(small_mat, 1, function(x) sum(x * TCI))/rowSums(small_mat)
        result <- as.data.frame(cbind(rownames(small_mat), sum_vec))
        names(result)[1] = "id"
      }
      
      result$id = substr(as.character(result$id),4, nchar(result$id))
      result$Year = current_year
      result <- result %>% rename(Complexity = sum_vec)
      result$Complexity_unscaled = as.numeric(result$Complexity)
      
      if (cor(TCI, ubiquity, use = "pairwise.complete.obs", 
              method = "spearman") > 0){ 
        result$Complexity_unscaled <- result$Complexity_unscaled * (-1)}
      
      result$Complexity = (result$Complexity_unscaled - mean(result$Complexity_unscaled)) /
        (sd(result$Complexity_unscaled))
      
      result <- result %>% group_by(Year)%>% mutate(yearly_rank = rank(-Complexity)) %>% 
        ungroup() %>% mutate(rank = rank(-Complexity))
      
      # print(year)
      # print(mean(result$Complexity_unscaled))
      # print(sd(result$Complexity_unscaled))
      
      #rca0 = input_to_rca(rca, level = 0)
      diversity = rowSums(rca)
      result$diversity = diversity
      base_average_ubiquity = (1/rowSums(rca)) * rowSums (t(t(rca != 0 )*colSums(rca)))
      result$ubiquity = base_average_ubiquity
      
      
      full_aci_df = rbind(full_aci_df, result)
      
    }
    
    return(full_aci_df)
  }
  
  #nrow(mat)
  mat[is.na(mat)] <- 0
  
  rca <- mat
  rca[rca < 1] <- 0
  rca[rca > 0] <- 1
  nrow(mat)
  56000*4
  if(isTRUE(RCA)){
    mat <- rca
  }
  
  if(length(rca[rowSums(rca) == 0, ])>0){
    message("Warning! One column (amenity type) will be supressed. You should probably check that out.")
  }
  
  mat <- mat[rowSums(mat) > 0, ]
  mat <- mat[, colSums(mat) > 0]
  mat <- mat[complete.cases(mat),]
  # yci <- eigen((t(rca) / colSums(rca)) %*% (rca / rowSums(rca)))
  # yci <- Re(yci$vectors[, 2])
  # 
  # yci = setNames(
  #   (yci - mean(yci, na.rm = T)) / sd(yci, na.rm = T),
  #   colnames(rca))
  
  
  ubiquity = colSums(rca)
  
  # if (cor(yci, ubiquity, use = "pairwise.complete.obs", 
  #         method = "spearman") > 0){ 
  #   yci <- yci * (-1)}
  # 
  #yci = RQCI(rca)
  #df = as.data.frame(yci)
  TCI = tci_data$Complexity
  
  if(isTRUE(full_sum)){
    sum_vec <- apply(mat, 1, function(x) sum(x * TCI))
    result <- as.data.frame(cbind(rownames(mat), sum_vec))
    names(result)[1] = "id"
  }else{
    sum_vec <- apply(mat, 1, function(x) sum(x * TCI))/rowSums(mat)
    result <- as.data.frame(cbind(rownames(mat), sum_vec))
    names(result)[1] = "id"
  }
  result$id = as.character(result$id)
  result$Year = substr(result$id,1,2)
  result <- result %>% rename(Complexity = sum_vec)
  result$Complexity_unscaled = as.numeric(result$Complexity)
  rescale <- function(x) {
    ((x) - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                    min(x, na.rm = TRUE)) * 100
  }
  result$Complexity = (result$Complexity_unscaled - mean(result$Complexity_unscaled)) /
    (sd(result$Complexity_unscaled))
  
  result <- result %>% group_by(Year)%>% mutate(yearly_rank = rank(-Complexity)) %>% 
    ungroup() %>% mutate(rank = rank(-Complexity))
  
  #rca0 = input_to_rca(rca, level = 0)
  diversity = rowSums(rca)
  result$diversity = diversity
  base_average_ubiquity = (1/rowSums(rca)) * rowSums (t(t(rca != 0 )*colSums(rca)))
  result$ubiquity = base_average_ubiquity
  
  result$id = substr(result$id,4,nchar(result$id))
  return(result)
}

RQCI <- function(buildings_data, RCA = F){
  
  rca <- as.matrix(buildings_data %>% select(-building_id),rownames = buildings_data$building_id)
  rca[is.na(rca)] <- 0
  
  if(isTRUE(RCA)){
    rca <- input_to_rca(rca, level = 0)
  }
  
  rca <- rca[rowSums(rca) > 0, ]
  rca <- rca[, colSums(rca) > 0]
  rca <- rca[complete.cases(rca),]
  
  #yci = EconGeo::TCI(rca)
  
  ubiquity = colSums(rca)
  
  tm = (t(rca) / colSums(rca)) %*% (rca / rowSums(rca))
  TCI <- eigen((t(rca) / colSums(rca)) %*% (rca / rowSums(rca)))
  TCI = eigen(tm)
  TCI <- TCI$vectors[, 2]
  TCI <- (TCI - mean(TCI, na.rm = T)) / sd(TCI)
  
  if (cor(TCI, ubiquity, use = "pairwise.complete.obs", 
          method = "spearman") > 0){ 
    TCI <- TCI * (-1)}
  
  df = as.data.frame(TCI)
  
  df$id = as.character(colnames(rca))
  names(df)[1] = "Complexity"
  
  df$Complexity = as.numeric(df$Complexity)
  
  df <- df %>% mutate(rank = rank(-Complexity))
  
  avg_diversity = EconGeo::MORt(rca, steps = 1)
  df$diversity = avg_diversity
  df$ubiquity = ubiquity
  
  return(df)
}

ACI <- function(buildings_data, tci_data,steps = 20, need_minus = F, full_sum = F, rca = F){
  
  #buildings_data <- buildings_all
  rca <- as.matrix(buildings_data %>% select(-building_id),rownames = buildings_data$building_id)
  rca[is.na(rca)] <- 0
  
  if(isTRUE(rca)){
    rca <- input_to_rca(rca, level = 0)
  }
  
  rca <- rca[rowSums(rca) > 0, ]
  rca <- rca[, colSums(rca) > 0]
  rca <- rca[complete.cases(rca),]
  
  # yci <- eigen((t(rca) / colSums(rca)) %*% (rca / rowSums(rca)))
  # yci <- Re(yci$vectors[, 2])
  # 
  # yci = setNames(
  #   (yci - mean(yci, na.rm = T)) / sd(yci, na.rm = T),
  #   colnames(rca))
  
  
  ubiquity = colSums(rca)
  
  # if (cor(yci, ubiquity, use = "pairwise.complete.obs", 
  #         method = "spearman") > 0){ 
  #   yci <- yci * (-1)}
  # 
  #yci = RQCI(rca)
  #df = as.data.frame(yci)
  TCI = tci_data$Complexity
  
  if(isTRUE(full_sum)){
    sum_vec <- apply(rca, 1, function(x) sum(x * TCI))
    result <- as.data.frame(cbind(rownames(rca), sum_vec))
    names(result)[1] = "id"
  }else{
    sum_vec <- apply(rca, 1, function(x) sum(x * TCI))/rowSums(rca)
    result <- as.data.frame(cbind(rownames(rca), sum_vec))
    names(result)[1] = "id"
  }
  result$id = as.character(result$id)
  result$Year = substr(result$id,nchar(result$id)-1,nchar(result$id))
  result <- result %>% rename(Complexity = sum_vec)
  result$Complexity_unscaled = as.numeric(result$Complexity)
  rescale <- function(x) {
    ((x) - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                    min(x, na.rm = TRUE)) * 100
  }
  result$Complexity = (result$Complexity_unscaled - mean(result$Complexity_unscaled)) /
    (sd(result$Complexity_unscaled))
  
  result <- result %>% group_by(Year)%>% mutate(yearly_rank = rank(-Complexity)) %>% 
    ungroup() %>% mutate(rank = rank(-Complexity))
  
  rca0 = input_to_rca(rca, level = 0)
  diversity = rowSums(rca0)
  result$diversity = diversity
  base_average_ubiquity = (1/rowSums(rca)) * rowSums (t(t(rca != 0 )*colSums(rca)))
  result$ubiquity = base_average_ubiquity
  
  return(result)
}

input_to_rca <- function(mat, level, custom = F){
  
  if(level == 0){
    
    mat[mat < 1] <- 0
    mat[mat > 0] <- 1
    rca <- mat
    
  }else{
    
    if(isTRUE(custom)){
      # calculate the world export value in each product
      world_export <- colSums(mat)
      
      # calculate the number of countries
      n_country <- nrow(mat)
      
      rca <- mat / (world_export / n_country)
      
    }else{
      #rca <- sapply( mat, as.numeric )
      rca <- t(t(mat/rowSums(mat))/(colSums(mat)/sum(mat)))
    }
    rca[rca < level] <- 0
    rca[rca > 0 ] <- 1
    rca = as.matrix(rca)
    rownames(rca) = rownames(mat)
    rca[is.nan(rca)] <- 0
    
    
  }
  
  return(rca)
} 
