# Define Functions


#' Function for Lon/Lat -> GEOID (General)
#'
#' @param pointsDF A data.frame whose first column contains longitudes and
#                   whose second column contains latitudes.
#' @param geo A submitted geospatial polygon for census block groups
#' @param name_col Name of a column in geo that supplies data point to return
#' @return Returns geography names
#' @examples
#' lonlat_to_geoid(df, geo = tigris::states())
#' @export

lonlat_to_geoid <- function(pointsDF,
                                 geo,
                                 name_col = "GEOID") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  geo <- st_transform(geo, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  ## Find names of state (if any) intersected by each point
  geo_names <- geo[[name_col]]
  ii <- as.integer(st_intersects(pts, geo))

  return(geo_names[ii])
}







# *****************************************

  ### Build some Functions - Normalize the points to be at the n.nn2 or n.nn7 positions

# This function finds the last digit for a piece of input, relative to the amount of precision desired
figure_last_digit <- function(input, precision){
  # figure_last_digit(input, precision)

  decimal_pos = str_locate(string = input, pattern = fixed("."))
  input_at_precision = str_sub(string = input, start = 0, end = (decimal_pos[1,"start"] + precision))

  input_str_length = str_length(input_at_precision)
  output = str_sub(string = input_at_precision, start = input_str_length, end = input_str_length)

  return(output)

}

#' Function for getting the latitude and longitude rounded to proper level for use with dim_geo objects
#'
#' @param input A longitude or latitude value to be rounded
#' @param dim_geo_level The level of rounding to use
#' @return Returns rounded longitude/latitude value
#' @examples
#' round_lat_long(-84.82549, 4)
#' @export

round_lat_long = function(input, dim_geo_level){
  # round_lat_long(input, dim_geo_level)
  #This function Rounds an input lat/long to the appropriate dim_geo_level
  #  e.g. -84.82549 becomes -84.827 at dim_geo_level 4
  ## round_lat_long(-84.82549, 4)

  if(input == 0){
    # Screen for zeros
    transformed_input = 0

  }else{

    if(dim_geo_level == 1){
      # 100 Km - uses just the data left of the decimal point (e.g. -84.82548917 -> -84)
      transformed_input = round(input, 0)
      # print(input)
      # print(paste(input, " - initial input "))
      # print(paste(transformed_input, " - initial transformed_input "))

    }else if(dim_geo_level == 2){
      # 10 Km - uses the 1 digit rounded value (e.g. -84.82548917 -> -84.8)
      transformed_input = round(input, 1)

    }else if(dim_geo_level == 3){
      # 5 Km - uses the 2 digit value, normalizes to the 2 and 7 positions (e.g. -84.82548917 -> -84.83 -> -84.82)


      # print(input)
      # print(paste(input, " - initial input "))

      transformed_input = round(input, 2)
      # print(transformed_input)
      # print(paste(transformed_input, " - transformed input after rounding"))

      # Issue is... we need to figure out how long the string is so we can force padding right of the decimal to a standard
      # e.g. 8.123
      # e.g. -111.100
      # Basic formula is... length of the string left of the deimal + 1 (for the decimal) + desired length right of the decimal
      decimal_pos = str_locate(string = transformed_input, pattern = fixed("."))
      decimal_pos = decimal_pos[1,"start"]
      # Warning! Need to screen for input that does not have a decimal point (e.g. 40.00000000 would be stored and input as 40) round_lat_long(40, 3)
      if(is.na(decimal_pos)){
        # This means the input did not contain any decimals. Take the original input and str_c a .0
        transformed_input = str_c(c(transformed_input, ".0"), collapse = "")
        desired_length = str_length(string = transformed_input) + 1

      }else{
        # do nothing
        desired_length = decimal_pos + 2

      }

      # print(paste(decimal_pos, " - Decimal position"))
      # print(paste(desired_length, " - desired_length"))

      transformed_input = str_pad(string = transformed_input, width = desired_length, side = "right", pad = "0")
      # print(paste(transformed_input, " - transformed input after str_padding right with zeros"))

      last_digit = str_sub(string = transformed_input, start = str_length(transformed_input), end = -1)
      # print(paste(last_digit, " - last digit"))

      last_digit_new = fivehundred_digit_massaging(last_digit)
      # print(paste(last_digit_new, " - new last digit after massaging"))
      # print(last_digit_new)

      input_str_length = str_length(transformed_input)
      input_minus_last = str_sub(transformed_input, 0, input_str_length-1)
      transformed_input = str_c(c(input_minus_last, last_digit_new), collapse = "")
      # print(paste(transformed_input, " - transformed input after new last digit replacement"))

      transformed_input = as.double(transformed_input)
      # print(paste(transformed_input, " - transformed input after as.double() transformation"))


    }else if(dim_geo_level == 4){
      # (e.g. -84.82548917 -> -84.83)
      transformed_input = round(input, 2)

    }else if(dim_geo_level == 5){
      # .5 Km / 500M - uses a 3 digit rounded value, normalized to the 2 and 7 positions (e.g. -84.82548917 -> -84.825 -> -84.827)

      # print(input)
      # print(paste(input, " - initial input "))

      transformed_input = round(input, 3)
      # print(transformed_input)
      # print(paste(transformed_input, " - transformed input after rounding"))

      # Issue is... we need to figure out how long the string is so we can force padding right of the decimal to a standard
      # e.g. 8.123
      # e.g. -111.100
      # Basic formula is... length of the string left of the deimal + 1 (for the decimal) + desired length right of the decimal

      decimal_pos = str_locate(string = transformed_input, pattern = fixed("."))
      decimal_pos = decimal_pos[1,"start"]
      # Warning! Need to screen for input that does not have a decimal point (e.g. 40.00000000 would be stored and input as 40) round_lat_long(40, 5)
      if(is.na(decimal_pos)){
        # This means the input did not contain any decimals. Take the original input and str_c a .0
        transformed_input = str_c(c(transformed_input, ".0"), collapse = "")
        desired_length = str_length(string = transformed_input) + 2

      }else{
        # do nothing
        desired_length = decimal_pos + 3

      }

      # print(paste(decimal_pos, " - Decimal position"))
      # print(paste(desired_length, " - desired_length"))

      transformed_input = str_pad(string = transformed_input, width = desired_length, side = "right", pad = "0")
      # print(paste(transformed_input, " - transformed input after str_padding right with zeros"))

      last_digit = str_sub(string = transformed_input, start = str_length(transformed_input), end = -1)
      # print(paste(last_digit, " - last digit"))

      last_digit_new = fivehundred_digit_massaging(last_digit)
      # print(paste(last_digit_new, " - new last digit after massaging"))
      # print(last_digit_new)

      input_str_length = str_length(transformed_input)
      input_minus_last = str_sub(transformed_input, 0, input_str_length-1)
      transformed_input = str_c(c(input_minus_last, last_digit_new), collapse = "")
      # print(paste(transformed_input, " - transformed input after new last digit replacement"))

      transformed_input = as.numeric(transformed_input)
      # print(paste(transformed_input, " - transformed input after as.double() transformation"))


    }else if(dim_geo_level == 6){
      # (e.g. -84.82548917 -> -84.825)
      transformed_input = round(input, 3)

    }else if(dim_geo_level == 7){
      # default 8 digit lat/long no transformation
      transformed_input = round(input, 8)

    }else{
      return(paste("Error, bad dim_geo_level input (", dim_geo_level, ") is not an option"))
    }

  }#outside the else for the if Zero check

  if(is.na(transformed_input)){
    # print(input)
    print(paste(input, " - initial input "))
    print(paste(transformed_input, " - transformed input after as.double() transformation"))

  }else{
    return(transformed_input)

  }

}


### Define a function to handle the transformation of the last digit to normalize to entries ending in 2 and 7.
fivehundred_digit_massaging <- function(old_num){

  # print(paste("old digit is ", old_num))

  if(is.na(old_num)){
    # Screen for NA
    print(old_num)
    return("error")
  }else{


    # print(old_num)
      trans = c()

      trans[0] = 2
      trans[1] = 2
      trans[2] = 2
      trans[3] = 2
      trans[4] = 2

      trans[5] = 7
      trans[6] = 7
      trans[7] = 7
      trans[8] = 7
      trans[9] = 7

      trans["0"] = 2
      trans["1"] = 2
      trans["2"] = 2
      trans["3"] = 2
      trans["4"] = 2

      trans["5"] = 7
      trans["6"] = 7
      trans["7"] = 7
      trans["8"] = 7
      trans["9"] = 7

      new = trans[old_num]
      # print(paste("new digit is ", new))

      return(new)
  }



}

target_dim_geo <- function(lat_input, lon_input, dimgeo){
  #Requires dimgeo to have a lattitude_1 and longitude_1 fiield
  # print(paste("lat_input is - ",lat_input," lon_input - ",lon_input))

  lat_lower_bound = as.double(round_lat_long(lat_input, 1) - 1)
  lat_upper_bound = as.double(round_lat_long(lat_input, 1) + 1)
  lon_lower_bound = as.double(round_lat_long(lon_input, 1) - 1)
  lon_upper_bound = as.double(round_lat_long(lon_input, 1) + 1)

  # print(paste("lat_lower_bound is - ",lat_lower_bound," lat_upper_bound - ",lat_upper_bound))
  # print(paste("lon_lower_bound is - ",lon_lower_bound," lon_upper_bound - ",lon_upper_bound))
  # print(lat_lower_bound)
  # print(lat_upper_bound)
  # print(lon_lower_bound)
  # print(lon_upper_bound)

  return(
  dimgeo %>% filter(
    lattitude_1 > lat_lower_bound,
    lattitude_1 < lat_upper_bound,

    longitude_1 > lon_lower_bound,
    longitude_1 < lon_upper_bound
  )
  )

}



# fsf6 = fsf5 %>% sample_n(100)

get_closest_dimgeo_any <- function(df, dimgeo){
  # df - the submitted
  # dimgeo - dataframe of dimgeo values you want to find the closest value to


  # Filter dim_geo to just the common starting set
  dimgeo_processor = target_dim_geo(lat_input = fsf6[i,"latitude_final_3"], lon_input = fsf6[i,"longitude_final_3"], dimgeo) %>% select(longitude, lattitude, id)

  geoidpos = distHaversine(df %>% select(1, 2), dimgeo_processor %>% select(longitude, lattitude), r=6378137) %>% which.min()

  return(dimgeo$id[geoidpos])

}

# start = get_nanotime()

# dimgeo_any = dimgeo_final %>% filter(
#     lattitude_1 > min(fsf6$latitude_final_3) %>% round_lat_long(dim_geo_level = 1),
#     lattitude_1 < max(fsf6$latitude_final_3) %>% round_lat_long(dim_geo_level = 1),
#
#     longitude_1 > min(fsf6$longitude_final_3) %>% round_lat_long(dim_geo_level = 1),
#     longitude_1 < max(fsf6$longitude_final_3) %>% round_lat_long(dim_geo_level = 1)
#   )

# dimgeo_ids = c()
# for(i in 1:nrow(fsf6)){
#
#
#   dimgeo_ids = c(dimgeo_ids, get_closest_dimgeo_any(fsf6[i,] %>% select(longitude_final_3, latitude_final_3), dimgeo_any))
#
# }
# fsf6$dim_geo_id = dimgeo_ids
# end = get_nanotime()

# time_diff(start, end, nrow(fsf6))

### Version of get_closest_dimgeo


# get_closest_dimgeo = function(df, dimgeo){
#   # df - the submitted
#   # dimgeo - dataframe of dimgeo values you want to find the closest value to
# geoidpos = distHaversine(df %>% select(1, 2), dimgeo %>% select(longitude, lattitude), r=6378137) %>% which.min()
#
#  return(dimgeo$id[geoidpos])
#
# }
#
# start = get_nanotime()
# dimgeo_ids = c()
# for(i in 1:nrow(fsf6)){
#
#   # Filter dim_geo to just the common starting set
#   dimgeo_processor = dimgeo_final %>% filter(
#     lattitude == fsf6$latitude_final_3[i],
#     longitude == fsf6$longitude_final_3[i]
#     ) %>% select(longitude, lattitude, id)
#
#
#   dimgeo_ids = c(dimgeo_ids, get_closest_dimgeo(fsf6[i,c("latitude_final_3","longitude_final_3")], dimgeo_processor))
#
# }
# fsf6$dim_geo_id = dimgeo_ids
# end = get_nanotime()
#
# # time = end - start
# # time_to_run = time / 1000000000
# # time_to_run / nrow(fsf6)
# time_diff(start, end, nrow(fsf6))
# # nrow(fsf6) *  0.3071468


###############

time_diff <- function(start, end, nrows = 0){
  # start = get_nanotime()
  # end = get_nanotime()
  # time_diff(start, end, nrow(dataframe))

  time = end - start
  time_to_run = time / 1000000000
  if(nrows > 0){
    seconds_per_row = time_to_run / nrows
  }else{
    seconds_per_row = is.na(1)
  }


  if(time_to_run > 60){
    return(paste("Time to Run was ",time_to_run,". That is ",(time_to_run/60)," minutes total. Seconds per Row is ", round(seconds_per_row, 6)))

  }else{
    return(paste("Time to Run was ",time_to_run,". Seconds per Row is ", round(seconds_per_row, 6)))

  }

}
