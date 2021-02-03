library(tidyverse)
rm(list = ls())
df_raw <- read_csv("data/raw/listings.csv")

# a -----------------------------------------------------------------------


# price
df <- df_raw %>% 
  mutate(gbp_price = as.numeric(sub("\\$", "", price)),
         gbp_cleaning_fee = as.numeric(sub("\\$", "", cleaning_fee)),
         n_extra_people = as.numeric(sub("\\$", "", extra_people))) %>% 
  filter(gbp_price != 0) # remove rows, that don't have a price

# property type
prop_types <- c("Apartment", "Condominium", "House", "Townhouse")
df <- df %>% 
  filter(property_type %in% prop_types) %>% 
  mutate(f_property_type = ifelse(property_type == "Townhouse", "House", 
                                  ifelse(property_type == "Condominium", "Condo", property_type)))
rm(prop_types)

# room_type
df <- df %>% 
  mutate(f_room_type = as.factor(ifelse(room_type == "Entire home/apt", "Entire/apt", 
                                        ifelse(room_type == "Private room", "Private", "Shared"))))

# cancellation policy
df <- df %>% 
  mutate(f_cancellation_policy = as.factor(ifelse(cancellation_policy == "super_strict_30" | cancellation_policy == "super_strict_60", "super_strict", cancellation_policy)))

# bed_type and neighbourhood_cleansed as factors
df <- df %>% 
  mutate(f_neighbourhood_cleansed = as.factor(neighbourhood_cleansed),
         f_bed_type = as.factor(ifelse(bed_type == "Real Bed", "Bed", "Couch")))

# host response rate
df <- df %>% 
  mutate(host_response_rate = ifelse(host_response_rate == "N/A", NA, host_response_rate)) %>% 
  mutate(n_host_response_rate = as.numeric(sub("%", "", host_response_rate, fixed = T)))

# create numeric columns
numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews","guests_included", "reviews_per_month","minimum_nights","beds")

df <- df %>% mutate_at(vars(numericals), funs("n"=as.numeric))



nnames <- df %>%
  select(ends_with("_n")) %>%
  names()

nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)

rm(numericals, nnames, nnames_i)

# create days since first review
df <- df %>% 
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

### create dummy vars
# create a list from the amenities
amenities <- list(df$amenities)

# init empty list
am_list <- list()

# clear amenities and return a list of list with amenities as elements in the list of airbnbs 
for (e in 1:length(amenities[[1]])) {
  am <- gsub("{", "", amenities[[1]][[e]], fixed = T)
  am <- gsub("}", "", am, fixed = T)
  am <- strsplit(am, ",")
  am <- gsub('"', "", am, fixed = T)
  am <- gsub("\\", "", am, fixed = T)
  am <- gsub("\\\n", "", am)
  am <- sub("^c\\(", "", gsub("\\)$", "", am))
  am <- tolower(am)
  am <- strsplit(am, ", ")
  am_list[e] <- list(am[[1]])
}

# replace amenities with cleaned version
am_list_coll <- list()
for (e in 1:length(am_list)) {
  am_list_coll[e] <- paste(am_list[e], collapse = " - ")
}
df$amenities <- am_list_coll


# init names of the new cols as a vec
newnames <- c()

# loop through each cleaned list-item and append it to newnames vector
for (e in 1:length(am_list)) {
  for (i in 1:length(am_list[[e]])) {
    newnames <- c(newnames, am_list[[e]][[i]])
  }
}

# remove duplicates
newnames <- newnames[!duplicated(newnames)]

# sort alphabetically
newnames <- sort(newnames)

# fix mistakes
newnames <- sub(" toilet", "toilet", newnames)

# filter mistakes
newnames <- newnames[newnames != "character(0"]
newnames <- newnames[newnames != "translation missing: en.hosting_amenity_49"]
newnames <- newnames[newnames != "translation missing: en.hosting_amenity_50"]

# create empty list of amenity columns
ams_in_listing <- vector(mode = "list", length = length(newnames))

# loop through the amenities and check if they are listed in each airbnb listing
# append the result of booleans (len = len(df$amenities)) to a named list corresponding
# to the amenity
for (i in 1:length(newnames)) {
  ams_in_listing[i][[1]] <- unlist(grepl(newnames[i], df$amenities))
}

# rename vectors in list
names(ams_in_listing) <- paste0("am_", gsub(" ", "_", newnames))

# create temporary data frame from amenities
tdf <- data.frame(matrix(unlist(ams_in_listing), nrow=length(ams_in_listing[[1]]), byrow=F))

# rename columns
names(tdf) <- names(ams_in_listing)

# bind cols to cleaned df
df <- cbind(df, tdf)


# b-----------------------------------------------------------------------

# check result -> remove too scarce amenities
amens <- df %>% 
  summarise_at(vars(starts_with("am_")), sum) %>% t()
amens <- data.frame(value = amens)
amens['amenity'] <- rownames(amens)
rownames(amens) <- NULL
# check distribution of amenity freq
# amens %>% ggplot(aes(value)) + geom_histogram()
amens <- amens %>% filter(value < 80) # filter below 80
am_drop <- amens$amenity
am_drop_i <- match(am_drop, colnames(df))
colnames(df)[am_drop_i] <- paste0("drop_", tolower(gsub("[^[:alnum:]_]", "", am_drop)))


# drop cols from df
df <- df %>% 
  select(!matches("^drop_"))  %>% 
  select(matches("^am_.*|^n_.*|^f_.*|^p_.*|^gbp_.*"), id,
         neighbourhood_cleansed,cancellation_policy,room_type,property_type)

# check remaining cols
# data.frame(value = tdf %>% summarise_at(vars(starts_with("am_")), sum) %>% t()) %>% arrange(value)

rm(am, am_list, amenities, ams_in_listing, tdf, e, i, newnames, am_drop, am_drop_i, amens, am_list_coll, ams)

write_csv(df, "data/clean/airbnb-edinburgh-workfile.csv")
