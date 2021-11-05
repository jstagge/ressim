#' @export
elev_to_area <- function(elev, stage_stor){
	require(tidyverse)
	require(zoo)
	
	stage_stor_temp <- stage_stor %>%
		select(elev_ft, area_acre) %>%
		mutate(col = "original") %>%
		bind_rows(data.frame(elev_ft = elev, area_acre = NA, col = "new")) %>%
		arrange(elev_ft) %>%
		mutate(area_acre_est=zoo::na.approx(area_acre,elev_ft))
	
	result_df <- stage_stor_temp %>%
		filter(col == "new")

	return(unlist(result_df$area_acre_est))
}

#' @export
elev_to_vol <- function(elev, stage_stor){
	require(tidyverse)
	require(zoo)
	
	stage_stor_temp <- stage_stor %>%
		select(elev_ft, volume_acft) %>%
		mutate(col = "original") %>%
		bind_rows(data.frame(elev_ft = elev, volume_acft = NA, col = "new")) %>%
		arrange(elev_ft) %>%
		mutate(vol_acft_est=zoo::na.approx(volume_acft,elev_ft))
	
	result_df <- stage_stor_temp %>%
		filter(col == "new")

	return(unlist(result_df$vol_acft_est))
}

#' @export
vol_to_elev <- function(vol, stage_stor){
	require(tidyverse)
	require(zoo)
	
	stage_stor_temp <- stage_stor %>%
		select(volume_acft, elev_ft) %>%
		mutate(col = "original") %>%
		bind_rows(data.frame(volume_acft = vol, elev_ft = NA, col = "new")) %>%
		arrange(volume_acft) %>%
		mutate(elev_ft_est=zoo::na.approx(elev_ft, volume_acft))
	
	result_df <- stage_stor_temp %>%
		filter(col == "new")

	return(unlist(result_df$elev_ft_est))
}