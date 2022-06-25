Pokemon API Vignette
================
Josh Baber
6/23/2022

``` r
# Packages
library(jsonlite)
library(tidyverse)
```

``` r
# render function, doesn't eval just here to copy/paste into console
rmarkdown::render('README.Rmd', 
                  output_format = "github_document",
                  output_options = list(
                    html_preview = FALSE
                    )
                  )
```

id must be a continuous range from 1 to a number!!

``` r
# Define getPokemon function, can be done with vector of strings (names) or ID numbers,
# comes from pokemon endpoint
getPokemon <- function(pokemon = NULL, id = NULL){
  # If both pokemon and id arguments provided, throw this error
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a character type
    if(!is.character(pokemon)){
      # If not a character type, show this error message
      stop("Pokemon name must be provided as a string/character vector")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/pokemon/"
    # Part of URL that is based on argument
    name <- pokemon
    # Paste the two together into one string
    fullURL <- paste0(baseURL, name)
    # Read in the JSON file from the API with the full URL string
    pokeinfo <- fromJSON(fullURL)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value/vector")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/pokemon/"
    # Part of URL that is based on argument
    pokedexnum <- id
    # Paste the two together into one string
    fullURL <- paste0(baseURL, pokedexnum)
    # Read in the JSON file from the API with the full URL string
    pokeinfo <- fromJSON(fullURL)
  }
  # Return the info on the pokemon
  return(pokeinfo)
}
```

``` r
# Define getPokeInfo function, can be done with strings (names) or id numbers,
# comes from pokemon-species endpoint
getPokeInfo <- function(pokemon = NULL, id = NULL){
  # Check if both pokemon and id arguments were passed, if so, throw this error message
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a character type
    if(!is.character(pokemon)){
      # If not a character type, show this error message
      stop("Pokemon name must be provided as a string/character vector")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/pokemon-species/"
    # Part of URL that is based on argument
    name <- pokemon
    # Paste the two together into one string
    fullURL <- paste0(baseURL, pokemon)
    # Read in the JSON file from the API with the full URL string
    pokemoninfo <- fromJSON(fullURL)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value/vector")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/pokemon-species/"
    # Part of URL that is based on argument
    pokedexnum <- id
    # Paste the two together into one string
    fullURL <- paste0(baseURL, pokedexnum)
    # Read in the JSON file from the API with the full URL string
    pokemoninfo <- fromJSON(fullURL)
  }
  # Return the info on the pokemon
  return(pokemoninfo)
}
```

``` r
# Function that combines the data from the pokemon endpoint and the pokemon-species endpoint,
# can pass it a vector of strings (names) or id numbers and optional argument vars which
# will select only specified variables from each pokemon
fullPokeInfo <- function(pokemon = NULL, id = NULL, vars = all()){
  # Checks if both a pokemon and id argument were passed, throws this error if so
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a character type
    if(!is.character(pokemon)){
      # If not a character type, show this error message
      stop("Pokemon name must be provided as a string/character vector")
    }
    # Initiate empty lists for some potential looping
    basepoke <- list()
    speciespoke <- list()
    fullpoke <- list()
    # If we received a vector of pokemon names, we can loop through them
    for(i in pokemon){
      # Grab the data from the pokemon endpoint
      basepoke[[i]] <- getPokemon(pokemon = i)
      # Grab the data from the pokemon-species endpoint
      speciespoke[[i]] <- getPokeInfo(pokemon = i)
      # Combine the two into one list
      fullpoke[[i]] <- c(basepoke[[i]], speciespoke[[i]])
    }
    # Subset the full list into just the variables specified with vars
    fullpoke2 <- lapply(fullpoke, function(x)x[vars])
    # Return the list with the combined data for each pokemon
    return(fullpoke2)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value or vector")
    }
    # Initialize empty lists for some potential looping
    idpokemon <- list()
    idspecies <- list()
    idfull <- list()
    # If multiple id numbers were given to id argument, we can loop through them
    # Pretty sure this is where the id variable gets weird.  Couldn't figure out
    # How to make it more flexible.
    for(i in id){
      # Grab the data from the pokemon endpoint
      idpokemon[[i]] <- getPokemon(id = i)
      # Grab the data from the pokemon-species endpoint
      idspecies[[i]] <- getPokeInfo(id = i)
      # Combine the two into one list
      idfull[[i]] <- c(idpokemon[[i]], idspecies[[i]])
    }
    # Subset the full list into just the variables specified with vars
    idfull2 <- lapply(idfull, function(x)x[vars])
    # Return the list with the combined data for each pokemon
    return(idfull2)
  }
}
```

``` r
# Define getBerry function, can be done with a vector of strings (names) or id numbers
getBerry <- function(berry = NULL, id = NULL){
  # Check if both berry and id arguments passed, if so, need to throw this error message
  if(!is.null(berry) & !is.null(id)){
    stop("Only can provide a vector of berry names or ids, not both")
  }
  # If a berry argument was provided
  if(!is.null(berry)){
    # Check if the berry argument is a character type
    if(!is.character(berry)){
      # If not a character type, show this error message
      stop("Berry name must be provided as a string")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/berry/"
    # Part of URL that is based on argument
    name <- berry
    # Paste the two together into one string
    fullURL <- paste0(baseURL, name)
    # Read in the JSON file from the API with the full URL string
    berryinfo <- fromJSON(fullURL)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/berry/"
    # Part of URL that is based on argument
    berrynum <- id
    # Paste the two together into one string
    fullURL <- paste0(baseURL, berrynum)
    # Read in the JSON file from the API with the full URL string
    berryinfo <- fromJSON(fullURL)
  }
  # Return the info on the pokemon
  return(berryinfo)
}
```

``` r
# Now we can customize a function that will allow us to get the info for a vector of berry names 
# or id numbers.  Also we have an optional vars argument to pass a vector of variable names to 
# keep, with all variables being the default
berryInfo <- function(berry = NULL, id = NULL, vars = all()){
  # Check if both berry and id arguments were passed, if so, need to throw this error message
  if(!is.null(berry) & !is.null(id)){
    stop("Only can provide a vector of berry names or ids, not both")
  }
  # If a berry argument was provided
  if(!is.null(berry)){
    # Check if the berry argument is a character type
    if(!is.character(berry)){
      # If not a character type, show this error message
      stop("Berry name must be provided as a string")
    }
    # Initialize an empty list
    berryinfo <- list()
    # Add the berry info for each requested berry to our berryinfo list
    for(i in berry){
      berryinfo[[i]] <- getBerry(berry = i)
    }
    # Subset each iteration of our berryinfo list to contain the vars specified
    berrylist <- lapply(berryinfo, function(x)x[vars])
    # Return the list
    return(berrylist)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value or vector")
    }
    # Initialize an empty list
    idberry <- list()
    # Add the berry info for each requested berry to our idberry list
    for(i in id){
      idberry[[i]] <- getBerry(id = i)
    }
    # Subset each iteration of our idberry list to contain the vars specified
    idlist <- lapply(idberry, function(x)x[vars])
    # Return the list
    return(idlist)
  }
}
```

``` r
# Create a function that will convert one of our output lists to a tibble.  I can't put this
# In the functions above because some of the objects that are returned in the list won't fit into
# A data frame with other objects.  We return several data frames, values, vectors, list, etc. so
# we have to parse it all separately
convertToTibble <- function(list){
  # Create a data frame from a matrix created from a list
  df = as.data.frame(matrix(unlist(list), nrow = length(list), byrow = T))
  # Convert data frame to tibble so it will be compliant with tidyverse functions
  tibble = tibble(df)
  # Return the tibble
  return(tibble)
}
```

``` r
# Abilities are stored in a data frame, so I need a function to help convert to a tibble
getAbilities <- function(list){
  # Subset to just the abilities for each pokemon in list
  abilities <- lapply(list, function(x)x["abilities"]$abilities)
  # Initialize empty vectors, some pokemon may have 3 possible abilities
  ability1 <- vector()
  ability2 <- vector()
  ability3 <- vector()
  # Loop through each pokemon's abilities list
  for(i in 1:length(abilities)){
    # If there is not a 1st ability say "none", else return ability name to ability1
    ability1[i] <- ifelse(is.na(abilities[[i]]$ability$name[1]), "none", abilities[[i]]$ability$name[1])
    # If there is not a 2nd ability say "none", else return ability name to ability2
    ability2[i] <- ifelse(is.na(abilities[[i]]$ability$name[2]), "none", abilities[[i]]$ability$name[2])
    # If there is not a 3rd ability say "none", else return ability name to ability3
    ability3[i] <- ifelse(is.na(abilities[[i]]$ability$name[3]), "none", abilities[[i]]$ability$name[3])
  }
  # Return a tibble of the three ability columns
  return(tibble(ability1, ability2, ability3))
}
```

``` r
# Since stats is also stored in a list, it will take some work to get out what I want
getStats <- function(list){
  # Subset to just the stats list for each pokemon in list
  stats <- lapply(list, function(x)x["stats"]$stats)
  # Every pokemon has six stats, initialize lists for them
  hp <- vector()
  attack <- vector()
  defense <- vector()
  special_attack <- vector()
  special_defense <- vector()
  speed <- vector()
  # Loop through each pokemon's stats list
  for(i in 1:length(stats)){
    # Append vectors with the corresponding stat from base_stat
    hp[i] <- stats[[i]]$base_stat[1]
    attack[i] <- stats[[i]]$base_stat[2]
    defense[i] <- stats[[i]]$base_stat[3]
    special_attack[i] <- stats[[i]]$base_stat[4]
    special_defense[i] <- stats[[i]]$base_stat[5]
    speed[i] <- stats[[i]]$base_stat[6]
  }
  # Return a tibble of stats for each pokemon
  return(tibble(hp, attack, defense, special_attack, special_defense, speed))
}
```

``` r
# Last but not least, we need types
getTypes <- function(list){
  # Subset to just the types list for each pokemon in list
  types <- lapply(list, function(x)x["types"]$types)
  # Every pokemon has one or two types, intialize empty lists
  type1 <- vector()
  type2 <- vector()
  # Loop through each pokemon's types list
  for(i in 1:length(types)){
    # If there is not a 1st type, return "none", otherwise return the type name
    type1[i] <- ifelse(is.na(types[[i]]$type$name[1]), "none", types[[i]]$type$name[1])
    # If there is not a 2nd type, return "none", otherwise return the type name
    type2[i] <- ifelse(is.na(types[[i]]$type$name[2]), "none", types[[i]]$type$name[2])
  }
  # Return tibble of types
  return(tibble(type1, type2))
}
```

``` r
# Function that will parse a pokemon query into a tibble 
pokeTibble <- function(pokemon = NULL, id = NULL, vars){
  # Get a list from fullPokeInfo()
  pokelist <- fullPokeInfo(pokemon = pokemon, id = id, vars = vars)
  # These variables are data frames in the list, need to have use helper functions to get them usable
  dfs <- c("types", "abilities", "stats")
  # These variables are not data frames and can be easily put into a tibble
  nondfs <- c("name", "id", "height", "weight", "capture_rate", "base_experience", "base_happiness", "is_baby", "is_legendary", "is_mythical", "is_default", "order", "hatch_counter")
  # These variables are either pretty useless, too large, contain urls, or are hard to work with and give me errors
  badvars <- c("forms", "game_indices", "held_items", "location_area_encounters", "moves", "past_types", "species", "sprites", "color", "egg_groups", "evolution_chain", "flavor_text_entries", "form_descriptions", "has_gender_differences", "gender_rate", "evolves_from_species", "forms_switchable", "genera", "generation", "growth_rate", "habitat", "names", "pal_park_encounters", "pokedex_numbers", "shape", "varities")
  # These variables are numeric, need to be explicitly coerced into numeric variables later
  numericvars <- c("height", "weight", "capture_rate", "base_experience", "base_happiness", "order", "hatch_counter")
  # These variables are logical, need to be explicitly coerced into logical variables later
  logicalvars <- c("is_baby", "is_legendary", "is_mythical", "is_default")
  # First I want to throw an error if a "badvar" was passed in the vars argument, since they can't go
  # in a tibble
  for(i in badvars){
    if(i %in% vars){
      stop(paste0(i, " is not a valid variable, please remove.  Valid variables include name, id, height, weight, capture_rate, base_experience, base_happiness, is_baby, is_legendary, is_mythical, is_default, order, evolves_from_species, forms_switchable, gender_rate, has_gender_differences, hatch_counter, types, abilities, and stats"))
    }
  }
  # Next, initialize an empty vector for variables
  nondfvars = c()
  # For every nondf variable
  for(i in nondfs){
    # Check if it was passed in the vars argument
    if(i %in% vars){
      # If so, append it to the nondfvars vector
      nondfvars[i] <- i
    }
  }
  # Now that we have a vector nondf vars that were passed, we can subset each pokemon's list and call it values
  values <- lapply(pokelist, function(x)x[nondfvars])
  # Convert to a tibble
  valuestibble <- convertToTibble(values)
  # Rename the columns
  colnames(valuestibble) <- nondfvars
  # Next we need to consider our variables that are stuck in data frames, initialize an empty vector named dfvars
  dfvars = c()
  # For every df variable
  for(i in dfs){
    # Check if it was passed in the vars argument
    if(i %in% vars){
      # If so, append it to the dfvars vector
      dfvars[i] <- i
    }
  }
  # Check if "types" was a variable passed, if so, create a types tibble using getTypes() and append it to the valuestibble
  if("types" %in% dfvars){
    types <- getTypes(pokelist)
    valuestibble <- tibble(valuestibble, types)
  }
  # Check if "abilities" was a variable passed, if so, create an abilities tibble using getAbilities() and append it to the valuestibble
  if("abilities" %in% dfvars){
    abilities <- getAbilities(pokelist)
    valuestibble <- tibble(valuestibble, abilities)
  }
  # Check if "stats" was a variable passed, if so, create a stats tibble using getStats() and append it to the valuestibble
  if("stats" %in% dfvars){
    stats <- getStats(pokelist)
    valuestibble <- tibble(valuestibble, stats)
  }
  # Now we need to convert numeric variables into integer values if possible
  for(i in colnames(valuestibble)){
    # If a column name is in our numericvars vector
    if(i %in% numericvars){
      # Replace the values in that column with the integer value of it
      valuestibble[[i]] <- as.integer(valuestibble[[i]])
    }
    # If a column name is in our logicalvars vector
    if(i %in% logicalvars){
      # Replace the values in that column with the logical value of it
      valuestibble[[i]] <- as.logical(valuestibble[[i]])
    }
  }
  # Return our tibble
  return(valuestibble)
}
```

Next, I want to do get a similar table for Berries.

``` r
# Firmness is stored in a list, need a function to parse this data
getFirmness <- function(list){
  # Subset to just the firmness list for each berry in list
  firm <- lapply(list, function(x)x["firmness"]$firmness)
  # Initialize firmness vector
  firmness <- vector()
  # Loop through each berry's firmness list
  for(i in 1:length(firm)){
    # Append the "name" value of firmness onto the vector for each berry
    firmness[i] <- firm[[i]]$name
  }
  # Return tibble of types
  return(tibble(firmness))
}
```

<https://bulbapedia.bulbagarden.net/wiki/Natural_Gift_(move)>

``` r
# Natural Gift Type is stored in a list, need a function to parse this data
getNatGiftType <- function(list){
  # Subset to just the natural_gift_type list for each berry in list
  giftType <- lapply(list, function(x)x["natural_gift_type"]$natural_gift_type)
  # Initialize natural_gift_type vector
  natural_gift_type <- vector()
  # Loop through each berry's giftType list
  for(i in 1:length(giftType)){
    # Append the "name" value of giftType onto the vector for each berry
    natural_gift_type[i] <- giftType[[i]]$name
  }
  # Return tibble of types
  return(tibble(natural_gift_type))
}
```

``` r
# Since flavors is also stored in a list, it will take some work to get out what I want
getFlavors <- function(list){
  # Subset to just the flavors list for each berry in list
  flavors <- lapply(list, function(x)x["flavors"]$flavors)
  # Every berry has five flavors, initialize lists for them
  spicy <- vector()
  dry <- vector()
  sweet <- vector()
  bitter <- vector()
  sour <- vector()
  # Loop through each berry's flavors list
  for(i in 1:length(flavors)){
    # Append vectors with the corresponding stat from potency
    spicy[i] <- flavors[[i]]$potency[1]
    dry[i] <- flavors[[i]]$potency[2]
    sweet[i] <- flavors[[i]]$potency[3]
    bitter[i] <- flavors[[i]]$potency[4]
    sour[i] <- flavors[[i]]$potency[5]
  }
  # Return a tibble of flavor values for each berry
  return(tibble(spicy, dry, sweet, bitter, sour))
}
```

``` r
# item name is stored in a list, need a function to parse this data
getItem <- function(list){
  # Subset to just the item list for each berry in list
  items <- lapply(list, function(x)x["item"]$item)
  # Initialize item_name vector
  item_name <- vector()
  # Loop through each berry's items list
  for(i in 1:length(items)){
    # Append the "name" value of items onto the vector for each berry
    item_name[i] <- items[[i]]$name
  }
  # Return tibble of item names
  return(tibble(item_name))
}
```

``` r
# Similar function but for berries, can take in berry or id and a vector of variables
berryTibble <- function(berry = NULL, id = NULL, vars){
  # Get a list of berry info from the berryInfo() function
  berrylist <- berryInfo(berry = berry, id = id, vars = vars)
  # These are the variables that require extra parsing because they are in list/data frames
  dfs <- c("flavors", "natural_gift_type", "firmness", "item")
  # These variables do no require extra parsing
  nondfs <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", "size", "smoothness", "soil_dryness")
  # These variables are supposed to be numeric, will have to convert them to numeric later
  numericvars <- c("growth_time", "max_harvest", "natural_gift_power", "size", "smoothness", "soil_dryness")
  # Initialize an empty vector to hold nondf variables that were passed through vars argument
  nondfvars = c()
  # For every variable in nondfs
  for(i in nondfs){
    # Check if it was passed through vars argument
    if(i %in% vars){
      # If so, append that variable to the nondfvars vector
      nondfvars[i] <- i
    }
  }
  # Subset each berry list to contain the nondf vars variables
  values <- lapply(berrylist, function(x)x[nondfvars])
  # Convert to a tibble
  valuestibble <- convertToTibble(values)
  # Rename the columns
  colnames(valuestibble) <- nondfvars
  # Initialize an empty bector to hold df variables that were passed through vars argument
  dfvars = c()
  # For every variable in dfs
  for(i in dfs){
    # Check if it was passed through vars argument
    if(i %in% vars){
      # If so, append that variable to the dfvars vector
      dfvars[i] <- i
    }
  }
  # If "flavors" was in the vars argument, get the flavors tibble using getFlavors, 
  # then combine that tibble with the valuestibble
  if("flavors" %in% dfvars){
    flavors <- getFlavors(berrylist)
    valuestibble <- tibble(valuestibble, flavors)
  }
  # If "natural_gift_type" was in the vars argument, get the tibble using getNatGiftType, 
  # then combine that tibble with the valuestibble
  if("natural_gift_type" %in% dfvars){
    natural_gift_type <- getNatGiftType(berrylist)
    valuestibble <- tibble(valuestibble, natural_gift_type)
  }
  # If "firmness" was in the vars argument, get the flavors tibble using getFirmness, 
  # then combine that tibble with the valuestibble
  if("firmness" %in% dfvars){
    firmness <- getFirmness(berrylist)
    valuestibble <- tibble(valuestibble, firmness)
  }
  # If "item" was in the vars argument, get the item tibble using getItem, 
  # then combine that tibble with the valuestibble
  if("item" %in% dfvars){
    item <- getItem(berrylist)
    valuestibble <- tibble(valuestibble, item)
  }
  # Convert the numeric variables to numeric, check each column name in the tibble
  for(i in colnames(valuestibble)){
    # If the column name is in numericvars
    if(i %in% numericvars){
      # Convert that column to integer values
      valuestibble[[i]] <- as.integer(valuestibble[[i]])
    }
  }
  # Return the tibble
  return(valuestibble)
}
```

<https://bulbapedia.bulbagarden.net/wiki/Berry>

``` r
# Lastly, we can wrap everything together to produce nice tibbles for either pokemon or berry queries
queryAPI <- function(pokemon = NULL, pokeid = NULL, pokevars = NULL, berry = NULL, berryid = NULL, berryvars = NULL){
  # If only pokemon and pokevars arguments were passed
  if(!is.null(pokemon) & !is.null(pokevars) & is.null(c(pokeid, berry, berryid, berryvars))){
    # Create a pokeTibble from the pokemon and pokevars arguments and return it
    poketibble <- pokeTibble(pokemon = pokemon, vars = pokevars)
    return(poketibble)
  }
  # Else if only pokeid and pokevars arguments were passed
  else if(!is.null(pokeid) & !is.null(pokevars) & is.null(c(pokemon, berry, berryid, berryvars))){
    # Create a pokeTibble from the pokeid and pokevars arguments and return it
    poketibble <- pokeTibble(id = pokeid, vars = pokevars)
    return(poketibble)
  }
  # Else if only berry and berryvars arguments were passed
  else if(!is.null(berry) & !is.null(berryvars) & is.null(c(pokemon, pokeid, pokevars, berryid))){
    # Create a berryTibble from the berry and berryvars arguments and return it
    berrytibble <- berryTibble(berry = berry, vars = berryvars)
    return(berrytibble)
  }
  # Else if only berryid and berryvars arguments were passed
  else if(!is.null(berryid) & !is.null(berryvars) & is.null(c(pokemon, pokeid, pokevars, berry))){
    # Create a berryTibble the berryid and berryvars arguments and return it
    berrytibble <- berryTibble(id = berryid, vars = berryvars)
    return(berrytibble)
  }
  # If some other combination of arguments was passed, it is invalid, return this error message
  else{
    stop("Must provided either names and variables or ids and variables for pokemon or for berries, not both!")
  }
}
```

``` r
# These are the variables I am going to be working with in my EDA for pokemon and berries
myPokeVars <- c("name", "id", "types", "abilities", "height", "weight", "is_baby", "is_legendary", "is_mythical", "capture_rate", "stats")
myBerryVars <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", "size", "smoothness", "soil_dryness", "flavors", "natural_gift_type", "firmness", "item")
```

``` r
# Test that it works for pokemon and pokevars combination
queryAPI(pokemon = c("pikachu", "mewtwo", "greninja", "igglybuff", "rayquaza"), pokevars = myPokeVars)
```

    ## # A tibble: 5 × 19
    ##   name     id    height weight capture_rate is_baby is_legendary
    ##   <chr>    <chr>  <int>  <int>        <int> <lgl>   <lgl>       
    ## 1 pikachu  25         4     60          190 FALSE   FALSE       
    ## 2 mewtwo   150       20   1220            3 FALSE   TRUE        
    ## 3 greninja 658       15    400           45 FALSE   FALSE       
    ## 4 igglybu… 174        3     10          170 TRUE    FALSE       
    ## 5 rayquaza 384       70   2065           45 FALSE   TRUE        
    ## # … with 12 more variables: is_mythical <lgl>, type1 <chr>,
    ## #   type2 <chr>, ability1 <chr>, ability2 <chr>,
    ## #   ability3 <chr>, hp <int>, attack <int>, defense <int>,
    ## #   special_attack <int>, special_defense <int>, speed <int>

``` r
# Test that it works for pokeid and pokevars combination
queryAPI(pokeid = c(1:10), pokevars = myPokeVars)
```

    ## # A tibble: 10 × 19
    ##    name    id    height weight capture_rate is_baby is_legendary
    ##    <chr>   <chr>  <int>  <int>        <int> <lgl>   <lgl>       
    ##  1 bulbas… 1          7     69           45 FALSE   FALSE       
    ##  2 ivysaur 2         10    130           45 FALSE   FALSE       
    ##  3 venusa… 3         20   1000           45 FALSE   FALSE       
    ##  4 charma… 4          6     85           45 FALSE   FALSE       
    ##  5 charme… 5         11    190           45 FALSE   FALSE       
    ##  6 chariz… 6         17    905           45 FALSE   FALSE       
    ##  7 squirt… 7          5     90           45 FALSE   FALSE       
    ##  8 wartor… 8         10    225           45 FALSE   FALSE       
    ##  9 blasto… 9         16    855           45 FALSE   FALSE       
    ## 10 caterp… 10         3     29          255 FALSE   FALSE       
    ## # … with 12 more variables: is_mythical <lgl>, type1 <chr>,
    ## #   type2 <chr>, ability1 <chr>, ability2 <chr>,
    ## #   ability3 <chr>, hp <int>, attack <int>, defense <int>,
    ## #   special_attack <int>, special_defense <int>, speed <int>

``` r
# Test that it works for berry and berryvars combination
queryAPI(berry = c("cheri", "rawst", "chesto"), berryvars = myBerryVars)
```

    ## # A tibble: 3 × 16
    ##   name   id    growth_time max_harvest natural_gift_power  size
    ##   <chr>  <chr>       <int>       <int>              <int> <int>
    ## 1 cheri  1               3           5                 60    20
    ## 2 rawst  4               3           5                 60    32
    ## 3 chesto 2               3           5                 60    80
    ## # … with 10 more variables: smoothness <int>,
    ## #   soil_dryness <int>, spicy <int>, dry <int>, sweet <int>,
    ## #   bitter <int>, sour <int>, natural_gift_type <chr>,
    ## #   firmness <chr>, item_name <chr>

``` r
# Test that it works for berryid and berryvars combination
queryAPI(berryid = 1:64, berryvars = myBerryVars)
```

    ## # A tibble: 64 × 16
    ##    name   id    growth_time max_harvest natural_gift_power  size
    ##    <chr>  <chr>       <int>       <int>              <int> <int>
    ##  1 cheri  1               3           5                 60    20
    ##  2 chesto 2               3           5                 60    80
    ##  3 pecha  3               3           5                 60    40
    ##  4 rawst  4               3           5                 60    32
    ##  5 aspear 5               3           5                 60    50
    ##  6 leppa  6               4           5                 60    28
    ##  7 oran   7               4           5                 60    35
    ##  8 persim 8               4           5                 60    47
    ##  9 lum    9              12           5                 60    34
    ## 10 sitrus 10              8           5                 60    95
    ## # … with 54 more rows, and 10 more variables: smoothness <int>,
    ## #   soil_dryness <int>, spicy <int>, dry <int>, sweet <int>,
    ## #   bitter <int>, sour <int>, natural_gift_type <chr>,
    ## #   firmness <chr>, item_name <chr>
