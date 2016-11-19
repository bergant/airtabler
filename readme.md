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
  initialized environvent variable create an `.Renviron` file in your home directory
  with a line like this:
  
`AIRTABLE_API_KEY=your_api_key_here`

To check where your home is, type `path.expand("~")` in your R console.


## Usage

Create airtable base object:


```r
library(airtabler)

TravelBucketList <- 
  airtable(
    base = "appIS8u9n73hzwE7R", 
    tables = c("Destinations", "Hotels", "Travel Partners")
  )
```

_Note that you should replace the Airtable base identifiers and `record_id`s when running the examples._

### Get records
Use select function to get all records:

```r
hotels <- 
  TravelBucketList$Hotels$select()

knitr::kable(hotels[, c("id","Name", "Stars", "Price/night")], format = "markdown")
```



|id                |Name                                                         |Stars | Price/night|
|:-----------------|:------------------------------------------------------------|:-----|-----------:|
|reccPOcMQaYt1tthb |Heritage Christchurch Hotel (Christchurch, New Zealand)      |****  |         176|
|receHGZJ22WyUxocl |Urikana Boutique Hotel (Teresopolis, Brazil)                 |***** |         146|
|recgKO7K15YyWEsdb |Radisson Blu Hotel Marseille Vieux Port (Marseilles, France) |****  |         170|
|recjJJ4TX38sUwzfj |Hotel Berg (Keflavík, Iceland)                               |***   |         136|
|recjUU2GT28yVvw7l |Sheraton Nha Trang (Nha Trang, Vietnam)                      |***** |         136|
|reckPH6G384y3suac |Grand Residences Riviera Cancun (Puerto Morelos, Mexico)     |***** |         278|
|reclG7Bd2g5Dtiw4J |Grand Budapest Hotel (Zubrowka)                              |***** |         156|

Filter records with formula (see [formula field reference ](https://support.airtable.com/hc/en-us/articles/203255215-Formula-Field-Reference)).


```r
hotels <- 
  TravelBucketList$Hotels$select(filterByFormula = " ({Avg Review} > 8.5)" )

knitr::kable(hotels[, c("id","Name", "Stars", "Avg Review", "Price/night")])
```



id                  Name                                                       Stars    Avg Review   Price/night
------------------  ---------------------------------------------------------  ------  -----------  ------------
reccPOcMQaYt1tthb   Heritage Christchurch Hotel (Christchurch, New Zealand)    ****            8.8           176
receHGZJ22WyUxocl   Urikana Boutique Hotel (Teresopolis, Brazil)               *****           9.0           146
recjJJ4TX38sUwzfj   Hotel Berg (Keflavík, Iceland)                             ***             9.2           136
recjUU2GT28yVvw7l   Sheraton Nha Trang (Nha Trang, Vietnam)                    *****           8.8           136
reckPH6G384y3suac   Grand Residences Riviera Cancun (Puerto Morelos, Mexico)   *****           9.1           278
reclG7Bd2g5Dtiw4J   Grand Budapest Hotel (Zubrowka)                            *****           9.0           156

Sort data with sort parameter:

```r
hotels <- 
  TravelBucketList$Hotels$select(sort = list(
    list(field="Avg Review", direction = "desc"),
    list(field="Price/night", direction = "asc")
  ))


knitr::kable(hotels[, c("id","Name", "Stars", "Avg Review", "Price/night")])
```



id                  Name                                                           Stars    Avg Review   Price/night
------------------  -------------------------------------------------------------  ------  -----------  ------------
recjJJ4TX38sUwzfj   Hotel Berg (Keflavík, Iceland)                                 ***             9.2           136
reckPH6G384y3suac   Grand Residences Riviera Cancun (Puerto Morelos, Mexico)       *****           9.1           278
receHGZJ22WyUxocl   Urikana Boutique Hotel (Teresopolis, Brazil)                   *****           9.0           146
reclG7Bd2g5Dtiw4J   Grand Budapest Hotel (Zubrowka)                                *****           9.0           156
recjUU2GT28yVvw7l   Sheraton Nha Trang (Nha Trang, Vietnam)                        *****           8.8           136
reccPOcMQaYt1tthb   Heritage Christchurch Hotel (Christchurch, New Zealand)        ****            8.8           176
recgKO7K15YyWEsdb   Radisson Blu Hotel Marseille Vieux Port (Marseilles, France)   ****            8.2           170


Other optional arguments:

* __fields__ A list of fields to be returned (instead of all fields).
* __view__ The name or ID of the view, defined on the table.
* __maxRecord__ The maximum total number of records that will be returned.
* __pageSize__ The number of records returned in each request.
* __offset__ Page offset returned by the previous list-records
  call. Note that this is represented by a record ID, not a numerical offset.

### Retrieve a record
Add the `record_id` argument to get the details of a record:


```r
radisson <- 
  
  TravelBucketList$Hotels$select(record_id = "recgKO7K15YyWEsdb")

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
Insert a new record with `insert` function (API returns all record data - including new record ID):

```r
record_data <- list(
  Name = "New hotel",
  `Price/night` = 200,
  Stars = "****",
  Amenities = c("Hiking", "Gym"),
  Notes = "Just a sample record.\nWith extra line in notes."
)

new_hotel <- 
  TravelBucketList$Hotels$insert(record_data)

cat("Inserted a record with ID=", new_hotel$id, sep = "")
```

```
## Inserted a record with ID=recTaLppTmdmR4YW3
```


### Update a record
Update the price of the new hotel (API returns all record data):

```r
new_hotel <- 
  TravelBucketList$Hotels$update(
    record_id = new_hotel$id, 
    record_data = list(
      `Price/night` = 120,
      Notes = "Check out the price!!!"
    )
  )

cat("Updated a record with ID=", new_hotel$id, ". ", 
    "New price: ", new_hotel$fields$`Price/night`, sep = "")
```

```
## Updated a record with ID=recTaLppTmdmR4YW3. New price: 120
```

### Delete a record

```r
TravelBucketList$Hotels$delete(new_hotel$id)
```

```
## $deleted
## [1] TRUE
## 
## $id
## [1] "recTaLppTmdmR4YW3"
```


## Working with data frames

Standard Airtable API does not accept a table of records. 
Functions `insert` and `update` accept a data.frame and
execute transactions (call Airtable API) row by row.

Insert records with a data frame:

```r
two_records <- 
  data.frame(
    Name = c("Sample1", "Sample2"),
    `Price/night` = c(150, 180),
    Stars = c("***", "****"),
    Amenities = I(list(c("Wifi", "Pool"), c("Spa", "Laundry"))),
    Notes = c("Foo", "Bar"),
    
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

new_records <-
  TravelBucketList$Hotels$insert(two_records)
```

Update records with a data frame:

```r
# change records
record_ids <- sapply(new_records, function(x) x$id)
two_records$`Price/night` <- two_records$`Price/night` + 5
two_records$Stars <- "*****"


updated <- 
  TravelBucketList$Hotels$update(
    record_id = record_ids, 
    record_data = two_records)
```

Delete multiple records:

```r
# delete new records
record_ids <- sapply(new_records, function(x) x$id)
deleted <- 
  TravelBucketList$Hotels$delete(record_ids)
```


## Programming with airtabler

While having all airtable base tables and functions in one object 
is handy in interactive mode, it is recommended to use primitive
functions for adding, reading, updating and deleting when programming
R packages:


```r
travel_base <- "appIS8u9n73hzwE7R"

# read data
hotels <- air_select(travel_base, "Hotels")

# get one record
radisson <- air_select(travel_base, "Hotels", record_id = "recgKO7K15YyWEsdb")

# create
inserted <- air_insert(travel_base, "Hotels", record_data)

# update
updated <- air_update(travel_base, "Hotels", record_id = inserted$id, record_data)

# delete
deleted <- air_delete(travel_base, "Hotels", record_id = inserted$id)
```

