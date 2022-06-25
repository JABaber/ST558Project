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

id must be a continuous range!!

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
pokeTibble <- function(pokemon = NULL, id = NULL, vars){
  pokelist <- fullPokeInfo(pokemon = pokemon, id = id, vars = vars)
  dfs <- c("types", "abilities", "stats")
  nondfs <- c("name", "id", "height", "weight", "capture_rate", "base_experience", "base_happiness", "is_baby", "is_legendary", "is_mythical", "is_default", "order", "hatch_counter")
  badvars <- c("forms", "game_indices", "held_items", "location_area_encounters", "moves", "past_types", "species", "sprites", "color", "egg_groups", "evolution_chain", "flavor_text_entries", "form_descriptions", "has_gender_differences", "gender_rate", "evolves_from_species", "forms_switchable", "genera", "generation", "growth_rate", "habitat", "names", "pal_park_encounters", "pokedex_numbers", "shape", "varities")
  numericvars <- c("height", "weight", "capture_rate", "base_experience", "base_happiness", "order", "hatch_counter")
  logicalvars <- c("is_baby", "is_legendary", "is_mythical", "is_default")
  for(i in badvars){
    if(i %in% vars){
      stop(paste0(i, " is not a valid variable, please remove.  Valid variables include name, id, height, weight, capture_rate, base_experience, base_happiness, is_baby, is_legendary, is_mythical, is_default, order, evolves_from_species, forms_switchable, gender_rate, has_gender_differences, hatch_counter, types, abilities, and stats"))
    }
  }
  nondfvars = c()
  for(i in nondfs){
    if(i %in% vars){
      nondfvars[i] <- i
    }
  }
  values <- lapply(pokelist, function(x)x[nondfvars])
  # Convert to a tibble
  valuestibble <- convertToTibble(values)
  # Rename the columns
  colnames(valuestibble) <- nondfvars
  dfvars = c()
  for(i in dfs){
    if(i %in% vars){
      dfvars[i] <- i
    }
  }
  if("types" %in% dfvars){
    types <- getTypes(pokelist)
    valuestibble <- tibble(valuestibble, types)
  }
  if("abilities" %in% dfvars){
    abilities <- getAbilities(pokelist)
    valuestibble <- tibble(valuestibble, abilities)
  }
  if("stats" %in% dfvars){
    stats <- getStats(pokelist)
    valuestibble <- tibble(valuestibble, stats)
  }
  for(i in colnames(valuestibble)){
    if(i %in% numericvars){
      valuestibble[[i]] <- as.integer(valuestibble[[i]])
    }
    if(i %in% logicalvars){
      valuestibble[[i]] <- as.logical(valuestibble[[i]])
    }
  }
  return(valuestibble)
}
```

Next, I want to do get a similar table for Berries.

``` r
# Grab every variable except item because it is redundant
berryVars = c("name", "id", "firmness", "flavors", "growth_time", "max_harvest", "natural_gift_power", "natural_gift_type", "size", "smoothness", "soil_dryness")
# Create a vector of berry numbers, there are 64 total
berries = c(1:64)
# List of info on every berry named allBerries
allBerries <- berryInfo(id = berries, vars = berryVars)
```

``` r
# These are the variables whose values I can quickly convert to tibble
berrynondfs <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", "size", "smoothness", "soil_dryness")
# Go ahead and subset to these values for each berry
berryvalues <- lapply(allBerries, function(x)x[berrynondfs])
# Convert to a tibble
berryvaluestibble <- convertToTibble(berryvalues)
# Rename the columns
colnames(berryvaluestibble) <- berrynondfs
```

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
# Return a tibble with all the data I want
myBerries <- tibble(berryvaluestibble, getFirmness(allBerries), getNatGiftType(allBerries), getFlavors(allBerries))
myBerries <- myBerries %>% mutate(growth_time = as.integer(growth_time), max_harvest = as.integer(max_harvest), natural_gift_power = as.integer(natural_gift_power), size = as.integer(size), smoothness = as.integer(smoothness), soil_dryness = as.integer(soil_dryness))
myBerries
```

    ## # A tibble: 64 × 15
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
    ## # … with 54 more rows, and 9 more variables: smoothness <int>,
    ## #   soil_dryness <int>, firmness <chr>,
    ## #   natural_gift_type <chr>, spicy <int>, dry <int>,
    ## #   sweet <int>, bitter <int>, sour <int>

<https://bulbapedia.bulbagarden.net/wiki/Berry>

``` r
megaTibble <- c("name", "id", "height", "weight", "capture_rate", "base_experience", "base_happiness", "is_baby", "is_legendary", "is_mythical", "is_default", "order", "hatch_counter", "types", "abilities", "stats")
pokeTibble(pokemon = c("squirtle", "ivysaur", "charizard"), vars = megaTibble)
```

    ## # A tibble: 3 × 24
    ##   name      id    height weight capture_rate base_experience
    ##   <chr>     <chr>  <int>  <int>        <int>           <int>
    ## 1 squirtle  7          5     90           45              63
    ## 2 ivysaur   2         10    130           45             142
    ## 3 charizard 6         17    905           45             267
    ## # … with 18 more variables: base_happiness <int>,
    ## #   is_baby <lgl>, is_legendary <lgl>, is_mythical <lgl>,
    ## #   is_default <lgl>, order <int>, hatch_counter <int>,
    ## #   type1 <chr>, type2 <chr>, ability1 <chr>, ability2 <chr>,
    ## #   ability3 <chr>, hp <int>, attack <int>, defense <int>,
    ## #   special_attack <int>, special_defense <int>, speed <int>
