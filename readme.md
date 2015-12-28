# airtabler
Provides access to the [Airtable API](http://airtable.com/api)



## Install


```r
devtools::install_github("bergant/airtabler")
```

## Setup
> After you've created and configured the schema of an Airtable base from the
graphical interface, your Airtable base will provide its own API to create,
read, update, and destroy records. -  [airtable.com/api](http://airtable.com/api) 

## Get and store the API key
Generate the airtable API key from your [Airtable account](http://airtable.com/account) page.

__airtabler__ functions will read the API key from
  environment variable `AIRTABLE_API_KEY`. To start R session with the
  initialized environvent variable create an `.Renviron` file in your R home
  with a line like this:
  
`AIRTABLE_API_KEY=your_api_key_here`

To check where your R home is, type `normalizePath("~")` in your R console.

## Usage

_Note that you should replace the Airtable base identifiers and `record_id`s when running the examples._

### Get table records

```r
library(airtabler)
# define airtable base (see airtable.com/api for correct base IDs)
travel_base <- "appIS8u9n73hzwE7R"
# get data
hotels <- 
  air_get(travel_base, table_name = "Hotels")

knitr::kable(hotels[, c("id","Name", "Stars", "Price/night")])
```



id                  Name                                                           Stars    Price/night
------------------  -------------------------------------------------------------  ------  ------------
reccPOcMQaYt1tthb   Heritage Christchurch Hotel (Christchurch, New Zealand)        ****             176
receHGZJ22WyUxocl   Urikana Boutique Hotel (Teresopolis, Brazil)                   *****            146
recgKO7K15YyWEsdb   Radisson Blu Hotel Marseille Vieux Port (Marseilles, France)   ****             170
recjJJ4TX38sUwzfj   Hotel Berg (Keflavík, Iceland)                                 ***              136
recjUU2GT28yVvw7l   Sheraton Nha Trang (Nha Trang, Vietnam)                        *****            136
reckPH6G384y3suac   Grand Residences Riviera Cancun (Puerto Morelos, Mexico)       *****            278
reclG7Bd2g5Dtiw4J   Grand Budapest Hotel (Zubrowka)                                *****            156

Optional arguments to `air_get`:

* __view__ The name or ID of the view, defined on the table.
* __limit__ A limit on the number of records to be returned.
  Limit can range between 1 and 100.
* __offset__ Page offset returned by the previous list-records
  call. Note that this is represented by a record ID, not a numerical offset.
* __sortField__ The field name to use for sorting
* __sortDirection__ "asc" or "desc". The sort order in which the
  records will be returned. Defaults to asc.

### Retrieve a record
Add the `record_id` argument to get the details of a record:


```r
radisson <- 
  air_get(travel_base, "Hotels", record_id = "recgKO7K15YyWEsdb")

str(radisson$fields, max.level = 1)
```

```
## List of 9
##  $ Listing URL: chr "https://www.booking.com/hotel/fr/radisson-sas-marseille-vieux-port.html"
##  $ Name       : chr "Radisson Blu Hotel Marseille Vieux Port (Marseilles, France)"
##  $ Price/night: int 170
##  $ Amenities  : chr [1:4] "Pool" "Gym" "Restaurant" "Wifi"
##  $ Notes      : chr "Rooms with African or Provencál decor."
##  $ Country    : chr "recmSV4PR9ZCWyrk8"
##  $ Pictures   :'data.frame':	4 obs. of  6 variables:
##  $ Stars      : chr "****"
##  $ Avg Review : num 8.2
```

### Insert a record
Insert a new record with `air_insert` (API returns all record data - including new record ID):

```r
new_hotel <- 
  air_insert(travel_base, "Hotels", record_data = list(
    Name = "New hotel",
    `Price/night` = 200,
    Stars = "****",
    Amenities = c("Hiking", "Gym"),
    Notes = "Just a sample record.\nWith extra line in notes."
  ))

cat("Inserted a record with ID=", new_hotel$id, sep = "")
```

```
## Inserted a record with ID=recBQ838eVEv9pyCu
```


### Update a record
Update the price of the new hotel (API returns all record data):

```r
new_hotel <- 
  air_update(travel_base, "Hotels", new_hotel$id, record_data = list(
    `Price/night` = 180
  ))

cat("Updated a record with ID=", new_hotel$id, ". ", 
    "New price: ", new_hotel$fields$`Price/night`, sep = "")
```

```
## Updated a record with ID=recBQ838eVEv9pyCu. New price: 180
```

### Delete a record

```r
air_delete(travel_base, "Hotels", record_id = new_hotel$id)
```

```
## $deleted
## [1] TRUE
## 
## $id
## [1] "recBQ838eVEv9pyCu"
```


