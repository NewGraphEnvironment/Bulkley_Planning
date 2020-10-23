##we want a composite photo for viewing that has the 6 key photos of each crossing

site_id <- '124487'
# photos_id1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>% 
#   stringr::str_subset(., 'barrel|outlet|upstream')
# photos_id2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>% 
#   stringr::str_subset(., 'downstream|road|inlet')

##read in the photos
# photos_images1 <- image_read(photos_id1)

photos_images1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>% 
  stringr::str_subset(., 'barrel|outlet|upstream') %>% 
  image_read() 

photos_images2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>% 
  stringr::str_subset(., 'downstream|road|inlet') %>% 
  image_read() 

photos_stack1 <-image_append(image_scale(photos_images1, "x200"))
photos_stack2 <- image_append(image_scale(photos_images2, "x200")) 
  
photos_stack <- c(photos_stack1, photos_stack2)  

photos_stacked <- image_append(image_scale(photos_stack, "x200"), stack = T)
photos_stacked

image_write(photos_stacked, path = paste0(getwd(), '/data/photos/', site_id, '/crossing_all.JPG'), format = 'jpg')

  
