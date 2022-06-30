#' Get items that differ between x and y
#'
#' Unlike setdiff, this function creates the union of x and y then
#' removes values that are in the intersect, providing values
#' that are unique to X and values that are unique to Y.
#'
#' @param x a set of values.
#' @param y a set of values.
#'
#' @return Unique values from X and Y, NULL if no unique values.
#' @export
#'
#' @examples
#' a <- 1:3
#' b <- 2:4
#'
#' set_diff(a,b)
#' # returns 1,4
#'
#' x <- 1:3
#' y <- 1:3
#'
#' set_diff(x,y)
#' # returns NULL
#'
set_diff <- function(x,y){
  u <- union(x,y)
  i <- intersect(x,y)
  j <- (u %in% i)

  if(all(j)){
    return(NULL)
  }

  diff <- u[!(j)]
  return(diff)
}

#' Pull the metadata table from airtable
#'
#' For information about creating metadata tables in your base see the
#' \href{https://ecohealthalliance.github.io/eha-ma-handbook/8-airtable.html#managing-data}{EHA MA Handbook}
#'
#' @details Requires the following fields: table_name, field_name
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_name String. Name of structural metadata table - the metadata that
#' describes how tables and fields fit together.
#'
#' @return data.frame with metadata table
#' @export
#'
air_get_metadata_from_table <- function(base, table_name){
  # get structural metadata table
  str_metadata <- airtabler::fetch_all(base,table_name)
  ## check for table_name, field_name
  names(str_metadata) <- snakecase::to_snake_case(names(str_metadata))

  required_fields <- c("table_name","field_name")
  if(!all(required_fields %in% names(str_metadata))){
    stop(glue::glue("metadata table must contain the
                    following fields: {required_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }

  return(str_metadata)
}


#' Generated Metadata from table names
#'
#' Generates a structural metadata table - the metadata that
#' describes how tables and fields fit together. Does not
#' include field types.
#'
#' @details For information about creating metadata tables in your base see the
#' \href{https://ecohealthalliance.github.io/eha-ma-handbook/8-airtable.html#managing-data}{EHA MA Handbook}
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_names Vector of strings. The names of your tables. eg c("table 1","table 2", etc.)
#' @param limit Number from 1-100. How many rows should we pull from each table to create the metdata?
#'  Keep in mind that the airtable api will not return fields with "empty" values - "", false, or [].
#'  Code runs faster if fewer rows are pulled.
#'
#' @return data.frame with structural metadata.
#' @export

air_generate_metadata <- function(base, table_names,limit=1){
  warning('Airtable does not return fields with empty values - "", false, or [].
          It is better to create a specific metdata table and
          parse that with air_get_metadata_*')
  meta_data_table <- purrr::map_dfr(table_names,function(x){
    table_x <- airtabler::air_get(base,x,limit = limit )
    fields_x <- names(table_x)

    ## guess record types?

    md_df <- data.frame(table_name = x, field_name = fields_x, field_desc = "", field_type = "")

    return(md_df)
  })

  return(meta_data_table)
}

#' Get base description from table
#'
#' Pull a table that has descriptive metadata.
#' Requires the following fields:
#' "title","primary_contact","email","base_description"
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_name String. Name of descriptive metadata table - the metadata that
#' describes the base and provides attribution
#'
#' @return data.frame with descriptive metadata.
#' @export

air_get_base_description_from_table<- function(base, table_name){
  #fetch table
  desc_table <- airtabler::fetch_all(base,table_name)
  # to snake case
  names(desc_table) <- snakecase::to_snake_case(names(desc_table))

  required_fields <- c("title","primary_contact","email","base_description")
  if(all(required_fields %in% names(desc_table))){
    return(desc_table)
  } else {

    missing_rf <- required_fields[!required_fields %in% names(desc_table)]

    desc_table[missing_rf] <- NA
    return(desc_table)
  }

}

#' Generate descriptive metadata
#'
#' Creates a data.frame that describes the base.
#'
#' @details See  \href{https://www.dublincore.org/resources/userguide/creating_metadata/}{dublin core} for inspiration about additional attributes.
#'
#' @param title String. Title is a property that refers to the name or names by
#' which a resource is formally known.
#' @param primary_contact String.  Person or entity primarily responsible for
#' making the content of a resource
#' @param email String. Email of primary_contact
#' @param base_description String. This property refers to the description of
#' the content of a resource. The description is a potentially rich source of
#' indexable terms and assist the users in their selection of an appropriate
#' resource.
#' @param ... String. Additional descriptive metadata elements. See details.
#' Additional elements can be added as name pair values e.g.
#' \code{license = "CC BY 4.0", is_part_of = "https://doi.org/10.48321/MyDMP01"}
#'
#' @return data.frame with descriptive metadata
#' @export
#'
#' @examples
#'
#' air_generate_base_description(title = "My Awesome Base" ,
#'  primary_contact= "Base Creator/Maintainer",
#'  email = "email@@example.com",
#'  base_description = "This base is used to contain my awesome data
#'  from a project studying XXX in YYY. Data in the base were collected
#'  from 1900-01-01 to 1990-01-01 by researchers at Some Long Term Project.",
#'  is_part_of = "https://doi.org/10.48321/MyDMP01",
#'  is_part_of = "https://doi.org/10.5072/zenodo_sandbox.1062705"
#'  )
#'
air_generate_base_description <- function(title = NA,primary_contact= NA,email = NA, base_description = NA,...){
  desc_table <- data.frame(title,primary_contact,email,base_description,...)
  return(desc_table)
}

### extract_base - returns a named list

#' Dump all tables from a base into R
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param metadata Data.frame.Data frame with structural metadata - describes relationship between tables and fields.
#' @param description Data.frame. Data frame with descriptive metadata - describes whats in your base and who made it.
#' Can be left as NULL if base already contains a table called description.
#' @param add_missing_fields Logical. Should fields described in the metadata data.frame be added to corresponding tables?
#'
#' @return List of data.frames. All tables from metadata plus the
#' description and metadata tables.
#' @export
#'
air_dump <- function(base, metadata, description = NULL, add_missing_fields = TRUE){

  names(metadata) <- snakecase::to_snake_case(names(metadata))

  ## check for required fields
  required_fields <- c("table_name","field_name")

  if(!all(required_fields %in% names(metadata))){
    stop(glue::glue("metadata table must contain the
                    following fields: {required_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }


  base_table_names <- unique(metadata$table_name)

  print(base_table_names)
  table_list <- base_table_names |>
    purrr::set_names() |>
    purrr::map(function(x){
      ## get fields from str_metadata

      fields_exp <- metadata[metadata$table_name == x,"field_name"]

      ## pull table - add check for blank tables
      x_table <- airtabler::fetch_all(base,x)

      if(!is.data.frame(x_table)){
        x_table <- data.frame(id = character())
      }

      ## add in missing columns if any
      fields_obs <- names(x_table)

      # check if any discrepancy between metadata and table
      fields_diff <- set_diff(fields_exp,fields_obs)
      #browser()
      if(!is.null(fields_diff)){
        # check for fields in obs not in exp - error
        obs_exp  <- setdiff(fields_obs,fields_exp)
        ignore_fields <- c("id","createdTime")
        ignore_fields_pattern <- paste(ignore_fields,collapse = "|")
        if(length(obs_exp) != 0 & !all(obs_exp %in% ignore_fields)){
          missing_fields <- obs_exp[!grepl(ignore_fields_pattern,obs_exp,ignore.case = FALSE)]
          missing_fields_glue <- paste(missing_fields, collapse = ", ")
          stop(glue::glue('The metadata table is missing the following fields from table {x}:
                          {missing_fields_glue}
                          Please update the metadata table.https://airtable.com/{base}'))
        }
        # check for fields in exp and not in obs - append unless frictionless
        if(add_missing_fields){
          exp_obs <- setdiff(fields_exp,fields_obs)
          x_table[exp_obs] <- list(character(0))
        }
      }

      return(x_table)

    })

  table_list$metadata <- metadata

  #browser()
  # check for description table
  named_description <- grepl(pattern = "description",x = names(table_list), ignore.case = TRUE)


  if(!is.null(description)){
    if(any(named_description)){
      warning("Base has a description table and a description data.frame was supplied to
              this function. Inserting description data.frame at $description. Table
              extract may be overwritten.")
    }
    table_list$description <- description
  } else {
    ### description may already be in the base, think about
    ## how best to handle this
    if(
      all(
        !named_description
      )
    ){
      ## give null description
      table_list$description <- air_generate_base_description()
    }
  }

  named_description_post <- grepl(pattern = "description",x = names(table_list), ignore.case = TRUE)

  table_list[named_description_post][[1]]$created <- Sys.Date()

  return(table_list)
}


#' Flatten list columns to character
#'
#' Similar in spirit to purrr::flatten_chr except
#' that it can handle NULL values in lists and returns outputs
#' that can be written to csv.
#'
#' @details
#' Because the outputs are intended for use in CSV files, we must use
#' double quotes to indicate that the commas separating list values do
#' not delimit cells. This conforms to RFC 4180 standard for CSVs.
#' \url{https://datatracker.ietf.org/doc/html/rfc4180}
#'
#' @param data_frame a data frame, tibble or other data frame like object
#'
#' @return data_frame with list columns converted to character vectors.
#' @export
#'
#' @examples
#'
#' data_frame <- data.frame(a = I(list(list("Hello"),
#' list("Aloha"),
#' NULL,
#' list("Hola","Bonjour","Merhaba")
#' )),
#' b = 1:4,
#' c = letters[1:4],
#' d = I(data.frame(id = 1:4, name = "bob", email = "bob@@example.com"))
#' )
#'
#' test_df <- flatten_col_to_chr(data_frame)
#'
#' str(test_df)
#'
flatten_col_to_chr <- function(data_frame){
  for(i in names(data_frame)){
    #browser()
    # get column values
    col_from_df <- data_frame[[i]]

    if(is.list(col_from_df)){
      ## create an object to hold character values
      chr_col <- as.character()
      if(is.data.frame(col_from_df)){

        n_r <- nrow(col_from_df)

        for(j in 1:n_r){
          list_element<- col_from_df[j,]
          if(is.null(list_element)){
            list_element <- ""
          }
          row_value<- sprintf('"%s"',paste(list_element,collapse = ","))

          chr_col <- append(chr_col,row_value)
        }

      } else {
        n <- length(col_from_df)

        for(j in 1:n){
          list_element<- col_from_df[[j]]
          if(is.null(list_element)){
            list_element <- ""
          }
          row_value<- sprintf('"%s"',paste(list_element,collapse = ","))

          chr_col <- append(chr_col,row_value)
        }

      }

      data_frame[i] <- chr_col
    }
  }
  return(data_frame)
}

#' Save air_dump output to csv
#'
#' Saves data.frames from air_dump to csv files. File names are determined by
#' the names of the list objects from air_dump. Files will be saved in folder
#' with a unique name, inside the folder specified by \code{output_dir}. The
#' unique name is generated from a hash of the air_dump output.
#'
#' @param table_list List. List of data.frames output from \code{air_dump}
#' @param output_dir String. Folder containing output files
#' @param overwrite Logical. Should outputs be overwritten if they already exist?
#'
#' @return list. Returns the table_list object
#' @export
air_dump_to_csv <- function(table_list,output_dir= "outputs", overwrite = FALSE){

  # create a unique id for the data
  output_id <- rlang::hash(table_list)

  # check if data already exist
  output_dir_path_final  <- sprintf("%s/%s",output_dir,output_id)
  if(dir.exists(output_dir_path_final) & !overwrite){
    message("data already exist, files not written. Set overwrite
            to TRUE ")
    return(list.files(output_dir_path_final,full.names = TRUE))
  }

  # create temp dir
  temp_path <- tempdir()
  output_dir_path <- sprintf("%s/%s",temp_path,output_id)

  ### consider using temp dir then copying once finished processing

  dir.create(output_dir_path,recursive = TRUE)

  purrr::walk2(table_list, names(table_list), function(x_table,y_table_name){
    ##  clean table name
    y_table_name <- snakecase::to_snake_case(y_table_name)
    ## clean up field names in table
    names(x_table)  <- snakecase::to_snake_case(names(x_table))

    ## clean up field names
    names(x_table)  <- snakecase::to_snake_case(names(x_table))

    x_table_flat <- flatten_col_to_chr(x_table)

    ## export to CSV

    output_file_path  <- sprintf("%s/%s.csv",output_dir_path,y_table_name)

    utils::write.csv(x_table_flat,output_file_path,row.names = FALSE)
  })

  ## copy from temp to final
  dir.create(output_dir_path_final,recursive = TRUE)
  outputs_list <- list.files(output_dir_path,full.names = T)

  file.copy(from = outputs_list,to = output_dir_path_final,recursive = FALSE ,copy.mode = TRUE)

  invisible(table_list)

}

### write to db

### recover from metadata - JS code to regenerate tables
#
# air_js_for_tables <- function(metadata){
#   names(metadata) <- snakecase::to_snake_case(names(metadata))
#   table_names <- unique(metadata$table_name)
#
#  ## not sufficient for all field types, need to think more deeply about this
#   purrr::map(table_names, function(x){
#     field_names <- metadata[metadata$table_name == x,"field_name"]
#     field_types <- metadata[metadata$table_name == x,"field_type"]
#     create_field <- sprintf('{name: "%s", type: "%s"}',field_name,field_type)
#   })
#
# }
