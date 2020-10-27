##we want a composite photo for viewing that has the 6 key photos of each crossing
source('R/packages.R')
source('R/functions.R')


##get list of files (site_ids) in the photo folder
site_id_list <- list.files(path = paste0(getwd(), '/data/photos/'), full.names = T) %>% 
  basename() 

##lets look to see that we have all the right photo names
find_photo_names <- function(site_id){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet') %>% 
    as_tibble()
}

test <- site_id_list %>% 
  map(find_photo_names)

##once you review the test results and you are good then run your function over the list
site_id_list %>% 
  map(make_photo_comp_cv)





##------------used this to make the function
# site_id <- '3054'
# # photos_id1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>% 
# #   stringr::str_subset(., 'barrel|outlet|upstream')
# # photos_id2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>% 
# #   stringr::str_subset(., 'downstream|road|inlet')
# 
# ##read in the photos
# # photos_images1 <- image_read(photos_id1)
# 
# photos_images1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>% 
#   stringr::str_subset(., 'barrel|outlet|upstream') %>% 
#   image_read() 
# 
# photos_images2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>% 
#   stringr::str_subset(., 'downstream|road|inlet') %>% 
#   image_read() 
# # 
# photos_stack1 <-image_append(image_scale(photos_images1, "x420")) ##1/3 the width 373.33 and half the original height
# photos_stack2 <- image_append(image_scale(photos_images2, "x420")) 
# # photos_stack1 <-image_append(image_scale(photos_images1))
# # photos_stack2 <- image_append(image_scale(photos_images2)) 
# 
# photos_stack <- c(photos_stack1, photos_stack2)  
# 
# # photos_stacked <- image_append(image_scale(photos_stack, "x200"), stack = T)
# photos_stacked <- image_append(image_scale(photos_stack), stack = T)
# photos_stacked
# 
# image_write(photos_stacked, path = paste0(getwd(), '/data/photos/', site_id, '/crossing_all.JPG'), format = 'jpg')



########################---------------resize our generic image
# im <- image_read(path = paste0(getwd(), '/data/photos/3139/no_photo_road.JPG'))
# im2 <- image_scale(im, "1120x840!")
# image_write(im2, path = paste0(getwd(), '/data/photos/3139/no_photo_road.JPG'), format = 'jpg')
  