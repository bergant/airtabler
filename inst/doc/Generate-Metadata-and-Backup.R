## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(airtabler)

## ----generate metadata, eval=FALSE--------------------------------------------
#  # set your base id
#  base = "appVjIfAo8AJlfTkx"
#  ## create a list
#  api_metadata <- air_generate_metadata_from_api(base = "appVjIfAo8AJlfTkx")
#  str(api_metadata)

## ----add metadata table,  eval=FALSE------------------------------------------
#  status <- air_create_metadata_table(base = base,meta_data = api_metadata)

## ----descriptive metadata, eval=FALSE-----------------------------------------
#  ## create a basic descriptive metadata table
#  
#  description <- air_generate_base_description(title = "Base Title",
#                                creator = "Arkady Darell",
#                                created = "2023-04-03",
#                                description = "This base is an example for airtabler")
#  
#  
#  # We could have provided keywords as comma separated values but lets make things
#  # more interesting by presenting them as a vector
#  description_with_keywords <- air_generate_base_description(title = "Base Title",
#                                creator = "Arkady Darell",
#                                created = "2023-04-03",
#                                description = "This base is an example for airtabler",
#                                keywords = list(c("Example","R","Package","Airtable")))
#  

## ----add descriptive metadata, eval=FALSE-------------------------------------
#  
#  ## add our vanilla description table - preferred way to work with description data
#  air_create_description_table(base = base,description = description)
#  
#  
#  ## add our description with keywords and - we want keywords to be multiple select
#  ## Note this is not the preferred method but it is possible
#  
#  length(names(description_with_keywords))
#  
#  create_choices <- function(x){
#  
#    choice_list <- list()
#  
#     choice_list$choices <- purrr::map(x,function(x_item){
#               list(name = x_item)
#      })
#  
#     return(list(choice_list)) # wrap in an extra list to
#  }
#  
#  keyword_options <- create_choices(description_with_keywords$keywords[[1]])
#  
#  air_create_description_table(base = base,description = description_with_keywords,
#                               type = c(rep("singleLineText",9),"multipleSelects"),
#                               options = c(rep(NA,9),keyword_options))
#  
#  

## ----update structural metadata, eval=FALSE-----------------------------------
#  
#  # get your metadata from the api
#   metadata <- air_generate_metadata_from_api(base = base)
#  
#  # run the function
#  update_log  <- air_update_metadata_table(base,metadata)
#  
#  # the update log provides an overview of records that were updated, inserted, or deleted
#  # and fields that were created in the event that the structure of your metadata table
#  # changed.
#  

## ----update descriptive metadata, eval=FALSE----------------------------------
#  
#  base_description <- air_get_base_description_from_table(base = base,table_name = "Description",
#                                                          field_names_to_snakecase = FALSE)
#  
#  base_description$description <- "Keep on updating"
#  
#  ## since the field types are already established, its a little easier to add multipleSelect keywords
#  base_description$keywords[[1]] <- append("New Keyword",base_description$keywords[[1]])
#  
#  base_description <- base_description |>
#    dplyr::select(-createdTime)
#  
#  ## if your base_description obj has a record id field, use that for the join. Default is title
#  air_update_description_table(base = base,description = base_description,table_name = "Description",join_field = "id")
#  

## ----dump to R, eval=FALSE----------------------------------------------------
#  
#  ## pull down the metadata table from airtable
#  metadata <- air_get_metadata_from_table(base = base,table_name = "Meta Data",add_id_field = FALSE)
#  
#  airtable_base <- air_dump(base = base,metadata = metadata,description = base_description)
#  
#  summary(airtable_base)
#  

## ----dump to csv, eval=FALSE--------------------------------------------------
#  
#  #  dump to csv
#  air_dump_to_csv(table_list = airtable_base)
#  
#  # Make a change to the description table to show how hashing works
#  base_description <- air_get_base_description_from_table(base = base,table_name = "Description",
#                                                          field_names_to_snakecase = FALSE)
#  
#  ## since the field types are already established, its a little easier to add multipleSelect keywords
#  base_description$keywords[[1]] <- append("How will the hash change?",base_description$keywords[[1]])
#  
#  base_description <- base_description |>
#    dplyr::select(-createdTime)
#  
#  ## if your base_description obj has a record id field, use that for the join. Default is title
#  air_update_description_table(base = base,description = base_description,table_name = "Description",join_field = "id")
#  
#  ## pull the changed based down
#  airtable_base_updated <- air_dump(base = base,metadata = metadata,description = base_description)
#  
#  ## dump to csv
#  air_dump_to_csv(table_list = airtable_base_updated)

## ----workspace backup, eval=FALSE---------------------------------------------
#  
#  # get all bases associated with token
#  bases <- air_list_bases()
#  
#  # generate the description for our second
#  description_2 <- air_generate_base_description(title = bases$bases$name[[2]],creator = "Arkady Darell",created = Sys.Date(),description = "A base to demo bulk backups")
#  
#  description_log <- air_create_description_table(bases$bases$id[[2]],description = description_2)
#  
#  metadata_2 <- air_generate_metadata_from_api(bases$bases$id[[2]])
#  
#  metadata_log <- air_create_metadata_table(bases$bases$id[[2]],meta_data = metadata_2)
#  
#  ## add metadata and descriptive data to bases list
#  
#  base_descriptions <- purrr::map(bases$bases$id,function(base){
#    air_get_base_description_from_table(base,table_name = "Description")
#  })
#  
#  
#  base_metadata <- purrr::map(bases$bases$id,function(base){
#    air_get_metadata_from_table(base,table_name = "Meta Data")
#  })
#  
#  
#  bases$bases$description <-  base_descriptions
#  bases$bases$metadata <-  base_metadata
#  
#  base_df <- bases$bases
#  
#  for(i in 1:nrow(base_df)){
#    base_item <- base_df[i,]
#    table_list <- air_dump(base = base_item$id ,metadata = base_item$metadata[[1]],description = base_item$description[[1]])
#  
#    output_dir <- sprintf("Airtabler Workspace/%s", base_item$name)
#     air_dump_to_csv(table_list,output_dir = output_dir)
#  }
#  
#  

