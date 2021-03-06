Pokemon API Vignette
================
Josh Baber
6/23/2022

-   [Interacting With PokeAPI](#interacting-with-pokeapi)
    -   [Introduction](#introduction)
    -   [Packages](#packages)
    -   [Pokemon Query Functions](#pokemon-query-functions)
        -   [`getPokemon()` Function](#getpokemon-function)
        -   [`getPokeInfo()` Function](#getpokeinfo-function)
        -   [`fullPokeInfo()` Function](#fullpokeinfo-function)
    -   [Berry Query Functions](#berry-query-functions)
        -   [`getBerry()` Function](#getberry-function)
        -   [`berryInfo()` Function](#berryinfo-function)
    -   [Helper Functions for Creating Tibbles from
        Queries](#helper-functions-for-creating-tibbles-from-queries)
        -   [`convertToTibble()` Function](#converttotibble-function)
        -   [Pokemon Helper Functions](#pokemon-helper-functions)
            -   [`getAbilities()` Function](#getabilities-function)
            -   [`getStats()` Function](#getstats-function)
            -   [`getTypes()` Function](#gettypes-function)
        -   [Berry Helper Functions](#berry-helper-functions)
            -   [`getFirmness()` Function](#getfirmness-function)
            -   [`getNatGiftType()` Function](#getnatgifttype-function)
            -   [`getFlavors()` Function](#getflavors-function)
            -   [`getItem()` Function](#getitem-function)
    -   [Create Pokemon Tibble with
        `pokeTibble()`](#create-pokemon-tibble-with-poketibble)
    -   [Create Berry Tibble with
        `berryTibble()`](#create-berry-tibble-with-berrytibble)
    -   [Combine Tibble Functions Into One-Size-Fits-All Query Function
        with
        `queryAPI()`](#combine-tibble-functions-into-one-size-fits-all-query-function-with-queryapi)
    -   [Demonstrate `queryAPI()`](#demonstrate-queryapi)
    -   [Exploratory Data Analysis
        (EDA)](#exploratory-data-analysis-eda)
        -   [Create My Data Sets](#create-my-data-sets)
        -   [Create generation Variable](#create-generation-variable)
        -   [Create Some Contingency
            Tables](#create-some-contingency-tables)
        -   [Do Some Numerical Summaries](#do-some-numerical-summaries)
        -   [Plots](#plots)
    -   [Conclusions](#conclusions)

# Interacting With PokeAPI

Josh Baber 6/26/2022

## Introduction

This is a vignette created to interact with the
[pokeAPI](https://pokeapi.co/) which houses just about any and all data
related to main series Pokemon games. This vignette will contain the
functions I created to not only pull data from the API, but to parse
some of it into workable data and format it into tibbles. The endpoints
I pull from include <https://pokeapi.co/api/v2/pokemon/> ,
<https://pokeapi.co/api/v2/berry/> , and
<https://pokeapi.co/api/v2/pokemon-species/> . By the end, I will have a
`queryAPI()` function that will allow users to query information about
any Pokemon or berry, either by name or by id number. Lastly, I will use
my `queryAPI()` function to produce data sets that I can perform some
Exploratory Data Analysis on, including plots and tables.

## Packages

Below is a list of packages that were used in this vignette:

-   [jsonlite](https://www.rdocumentation.org/packages/jsonlite/versions/1.8.0)
    Needed to convert raw JSON data from the API into lists via the
    `fromJSON()` function
-   [tidyverse](https://www.tidyverse.org/) Provided easy data
    manipulation via functions from
    [dplyr](https://dplyr.tidyverse.org/) and graph creation via
    functions from [ggplot2.](https://ggplot2.tidyverse.org/)
-   [forcats](https://forcats.tidyverse.org/) A package I used to
    implement one specific change I needed regarding one of the graphs.
    I used the `fct_rev()` function for this.

``` r
# Packages
library(jsonlite)
library(tidyverse)
```

    ## ?????? Attaching packages ??????????????????????????????

    ## ??? ggplot2 3.3.6     ??? purrr   0.3.4
    ## ??? tibble  3.1.7     ??? dplyr   1.0.9
    ## ??? tidyr   1.2.0     ??? stringr 1.4.0
    ## ??? readr   2.1.2     ??? forcats 0.5.1

    ## ?????? Conflicts ?????????????????????????????????????????????????????????
    ## ??? dplyr::filter()  masks stats::filter()
    ## ??? purrr::flatten() masks jsonlite::flatten()
    ## ??? dplyr::lag()     masks stats::lag()

``` r
library(forcats)
```

## Pokemon Query Functions

### `getPokemon()` Function

**Please Note That id must be a continuous range from 1 to a number!!**

The first function is going to query data from the ???pokemon??? endpoint.
It can take in a Pokemon name as a string, or it can take in an id
number, which corresponds to a Pokemon???s ???Pokedex??? number. [A list of
Pokemon can be found
here.](https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_National_Pok%C3%A9dex_number)

The pokeAPI???s information matches up with much of what Bulbapedia has,
so it can be useful to consider Bulbapedia in addition to the pokeAPI
docs.

When the function is run, it returns a list containing many variables
from the ???pokemon??? endpoint.

``` r
# Define getPokemon function, can be done with vector of strings (names) or ID 
# numbers, comes from pokemon endpoint
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

### `getPokeInfo()` Function

There is another endpoint that contains information about Pokemon. This
is the ???pokemon-species??? endpoint. It works similarly to the previous
function, taking in either a Pokemon name via string or an id number.
This returns a list that contains new information on a Pokemon, that is
not returned in the `getPokemon()` function. Next, I am going to combine
both of these lists into one ???mega-list??? of information on a supplied
Pokemon, and make it iterable.

``` r
# Define getPokeInfo function, can be done with strings (names) or id numbers,
# comes from pokemon-species endpoint
getPokeInfo <- function(pokemon = NULL, id = NULL){
  # Check if both pokemon and id arguments were passed, if so, throw this error 
  # message
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

### `fullPokeInfo()` Function

Since the `getPokemon()` and `getPokeInfo()` functions both take in the
same arguments, and both return lists, we can combine them into one
list. Additionally, there is a **vars** argument that can take in a
vector of variables to subset this list on. For example, if we only
wanted the name and height variables, we would say
`vars = c("name", "height")`. I also made this iterable, so that we can
pass a vector of Pokemon names (strings) or a vector of id numbers.
Unfortunately, I ran into trouble with the id argument, so it can
strictly only take in a continuous vector from 1 to some number. I tried
to make this more flexible, but to no avail. The id argument is far more
useful for grabbing a ton of Pokemon, since it isn???t really feasible to
supply a long vector of names. This function will return a list that
contains the specified variables for each of the provided Pokemon. The
tibble functions later on remove some of the variables from this list,
so if the user wanted to keep those variables, they could use this
function, however, it won???t be formatted into a tibble and will remain a
list.

``` r
# Function that combines the data from the pokemon endpoint and the 
# pokemon-species endpoint, can pass it a vector of strings (names) or id 
# numbers and optional argument vars which will select only specified variables 
# from each pokemon
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

## Berry Query Functions

**Please Note That id must be a continuous range from 1 to a number!!**

### `getBerry()` Function

Much like we did for reaching out to the Pokemon endpoints, we can reach
out to the ???berry??? endpoint to return information about berries. In
Pokemon games, berries have many unique properties. They all have
different flavors, growth times, sizes, firmnesses, and more. [More
information about berries can be found
here.](https://bulbapedia.bulbagarden.net/wiki/Berry)

Similar to the `getPokemon()` function, `getBerry()` queries a berry via
name or id, then it returns a list of variables from the API that have
to do with that particular berry.

``` r
# Define getBerry function, can be done with a vector of strings (names) or id 
# numbers
getBerry <- function(berry = NULL, id = NULL){
  # Check if both berry and id arguments passed, if so, need to throw this error 
  # message
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
  # Return the info on the berry
  return(berryinfo)
}
```

### `berryInfo()` Function

Since there is only one endpoint that contains info about berries, we
are ready to take on multiple iterations of berries. We can provide a
vector of berry names, or a continuous vector of id numbers that start
at 1. Additionally, we have a **vars** argument that takes in a vector
of variable names and subsets each iteration of berry that is passed to
contain just the specified variables. This function will return a list
that contains the specified variables for each berry that is specified.

``` r
# Now we can customize a function that will allow us to get the info for a vector 
# of berry names or id numbers.  Also we have an optional vars argument to pass a 
# vector of variable names to keep, with all variables being the default
berryInfo <- function(berry = NULL, id = NULL, vars = all()){
  # Check if both berry and id arguments were passed, if so, need to throw this 
  # error message
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

## Helper Functions for Creating Tibbles from Queries

Many of the items that are returned in these lists are data frames or
lists themselves. Thus, they require further parsing as they have items
or variables of their own. When I convert these lists into tibbles, I
need to turn these smaller lists into tibbles first, so that I can
concatenate them on. I am going to call these ???problem variables???.

I ran into many problem variables when parsing this data. The
`fullPokeInfo()` function returns 45 variables in total. Of these, I
would say 30 of them are problem variables. Out of those 30, they are
mainly text strings like Pokedex entries, locations, sprites (which are
images of the Pokemon used in game), many other flavor texts (often in
other languages), and more. I figured that not much analysis could be
run on these, so I mainly focused on three of them: abilities, types,
and stats. I wrote helper functions for each of these and ignored the
others, since I figured most users wouldn???t really need them. However,
if a user is truly curious they can still use the `fullPokeInfo()` to
get a list that contains this information, then parse it from there.

### `convertToTibble()` Function

First is the `convertToTibble()` function. I will be using this to
convert the ???non-problem variables??? into tibbles. It unpacks the lists
and sorts them by row into a matrix, then converts it all to a data
frame, then to a tibble. Most of the variables I use later are
???non-problem variables???.

``` r
# Create a function that will convert one of our output lists to a tibble.  I 
# can't put this in the functions above because some of the objects that are 
# returned in the list won't fit into a data frame with other objects.  We 
# return several data frames, values, vectors, list, etc. so we have to parse 
# it all separately
convertToTibble <- function(list){
  # Create a data frame from a matrix created from a list
  df = as.data.frame(matrix(unlist(list), nrow = length(list), byrow = T))
  # Convert data frame to tibble so it will be compliant with tidyverse functions
  tibble = tibble(df)
  # Return the tibble
  return(tibble)
}
```

### Pokemon Helper Functions

So when it comes to the Pokemon, the problem variables I want to look at
are the abilities, types, and stats. These are all super important in
the Pokemon games, and a summary of a Pokemon would be incomplete
without them.

#### `getAbilities()` Function

First I want to start with the `getAbilities()` function. It goes into
the ???abilities??? variable for a Pokemon and pulls out the names of the
abilities that that Pokemon has. A Pokemon (with the exception of five
or six) has a maximum of three potential abilities, almost all of them
have only one or two. [More information about abilities can be read
here](https://bulbapedia.bulbagarden.net/wiki/Ability)

To parse this data, I create a vector for each ability and fill them
accordingly for each Pokemon. Then I return a tibble of three columns
for each ability that I can later concatenate with.

``` r
# Abilities are stored in a data frame, so I need a function to help convert to a 
# tibble
getAbilities <- function(list){
  # Subset to just the abilities for each pokemon in list
  abilities <- lapply(list, function(x)x["abilities"]$abilities)
  # Initialize empty vectors, some pokemon may have 3 possible abilities
  ability1 <- vector()
  ability2 <- vector()
  ability3 <- vector()
  # Loop through each pokemon's abilities list
  for(i in 1:length(abilities)){
    # If there is not a 1st ability say "none", else return ability name to 
    # ability1
    ability1[i] <- ifelse(is.na(abilities[[i]]$ability$name[1]), "none", 
                          abilities[[i]]$ability$name[1])
    # If there is not a 2nd ability say "none", else return ability name to 
    # ability2
    ability2[i] <- ifelse(is.na(abilities[[i]]$ability$name[2]), "none", 
                          abilities[[i]]$ability$name[2])
    # If there is not a 3rd ability say "none", else return ability name to 
    # ability3
    ability3[i] <- ifelse(is.na(abilities[[i]]$ability$name[3]), "none", 
                          abilities[[i]]$ability$name[3])
  }
  # Return a tibble of the three ability columns
  return(tibble(ability1, ability2, ability3))
}
```

#### `getStats()` Function

Next, I wrote the `getStats()` function to go into the ???stats??? variable
of a Pokemon. Every Pokemon has 6 stats: hp (hit points or ???health???),
attack, defense, special attack, special defense, and speed. These are
technically a Pokemon???s base stats, they can get a lot more involved
with stuff like IVs and EVs. [If you want to read more about stats you
can here.](https://bulbapedia.bulbagarden.net/wiki/Stat)

To parse this data, I create a vector for each stat and fill them
accordingly for each Pokemon. Then I return a tibble of six columns for
each stat that I can later concatenate with.

``` r
# Since stats is also stored in a list, it will take some work to get out what I 
# want
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

#### `getTypes()` Function

Lastly for Pokemon, I wrote a `getTypes()` function. Every Pokemon has
at least one type, some Pokemon have two. No Pokemon has more than two
types. Types are super important in determining a Pokemon???s
characteristics and battle strategy. [More information on types can be
found here.](https://bulbapedia.bulbagarden.net/wiki/Type)

To parse this data, I create a vector for each type and fill them
accordingly for each pokemon in a list. Then I return a tibble that has
two columns so that I can concatenate it later on.

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
    type1[i] <- ifelse(is.na(types[[i]]$type$name[1]), "none", 
                       types[[i]]$type$name[1])
    # If there is not a 2nd type, return "none", otherwise return the type name
    type2[i] <- ifelse(is.na(types[[i]]$type$name[2]), "none", 
                       types[[i]]$type$name[2])
  }
  # Return tibble of types
  return(tibble(type1, type2))
}
```

### Berry Helper Functions

Next, I have some problem variables that are returned from the
`berryInfo()` function. Fortunately, there is much less information on
berries, which is to be expected since they are a far more minor part of
the games. There are only 12 variables returned, and of those, 4 are
problem variables. These are firmness, Natural Gift type, flavors, and
item name.

#### `getFirmness()` Function

The first berry helper function deals with firmness. Honestly, I don???t
know what firmness is or does, but I suppose it has a numeric value.
There isn???t even a Bulbapedia article about it.

This function simply accesses the ???firmness??? variable for each berry and
returns the values as a single column tibble, to concatenate later.

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

#### `getNatGiftType()` Function

Natural Gift is a unique move in the Pokemon games. It allows for a
Pokemon to consume a held berry, and depending on the berry, will do a
certain amount of damage and have a certain type. [You can read more
about Natural Gift
here](https://bulbapedia.bulbagarden.net/wiki/Natural_Gift_(move))

This function is relatively simple. It accesses the ???natural_gift_type???
variable from each of a list of berries and returns the types as a
single column tibble, which can later be concatenated.

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

#### `getFlavors()` Function

Every berry has varying values of five flavors. Flavors include spicy,
dry, sweet, bitter, and sour. In the games, Pokemon of certain natures
react differently to flavors and will have preferences. [If you want to
read more about flavors you can
here](https://bulbapedia.bulbagarden.net/wiki/Flavor)

This function is relatively similar to the `getStats()` function. It
grabs the ???flavors??? variable for each berry in a list, then initializes
empty vectors for each of the five flavors. Then it iterates through the
list and appends the vectors accordingly. It returns a tibble of flavors
which has five columns, which can be concatenated later.

``` r
# Since flavors is also stored in a list, it will take some work to get out what I 
# want
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

#### `getItem()` Function

Last but not least (actually scratch that, certainly least) is the
`getItem()` function. This function gets the item name, which is just
the berry name, for the item??? so yes, it is redundant. But I did it
simply for the sake of completion because it was the only variable I had
left.

This is an straightforward function that grabs the ???item??? variable from
a berry in a list. Then it gets the ???name??? for each berry, appends it to
a vector, then converts that vector into a single column tibble ready to
concatenate.

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

## Create Pokemon Tibble with `pokeTibble()`

At last, the end is in sight. We can use `pokeTibble()` to query
whatever set of variables and Pokemon we want from the pokeAPI and
convert it into a tibble. Using a bunch of conditional logic and
explicit coercion, we can get a nicely parsed data set. A thing to note
is that this throws an error if the user tries to query variables that
are too problematic. Again, these variables can still be easily accessed
in a list using `fullPokeInfo()`.

``` r
# Function that will parse a pokemon query into a tibble 
pokeTibble <- function(pokemon = NULL, id = NULL, vars){
  # Get a list from fullPokeInfo()
  pokelist <- fullPokeInfo(pokemon = pokemon, id = id, vars = vars)
  # These variables are data frames in the list, need to have use helper functions 
  # to get them usable
  dfs <- c("types", "abilities", "stats")
  # These variables are not data frames and can be easily put into a tibble
  nondfs <- c("name", "id", "height", "weight", "capture_rate", "base_experience", 
              "base_happiness", "is_baby", "is_legendary", "is_mythical", 
              "is_default", "order", "hatch_counter")
  # These variables are either pretty useless, too large, contain urls, or are 
  # hard to work with and give me errors
  badvars <- c("forms", "game_indices", "held_items", "location_area_encounters", 
               "moves", "past_types", "species", "sprites", "color", "egg_groups", 
               "evolution_chain", "flavor_text_entries", "form_descriptions", 
               "has_gender_differences", "gender_rate", "evolves_from_species", 
               "forms_switchable", "genera", "generation", "growth_rate", 
               "habitat", "names", "pal_park_encounters", "pokedex_numbers", 
               "shape", "varities")
  # These variables are numeric, need to be explicitly coerced into numeric 
  # variables later
  numericvars <- c("height", "weight", "capture_rate", "base_experience", 
                   "base_happiness", "order", "hatch_counter")
  # These variables are logical, need to be explicitly coerced into logical 
  # variables later
  logicalvars <- c("is_baby", "is_legendary", "is_mythical", "is_default")
  # First I want to throw an error if a "badvar" was passed in the vars argument,
  # since they can't go in a tibble
  for(i in badvars){
    if(i %in% vars){
      stop(paste0(i, " is not a valid variable, please remove.  Valid variables 
                  include name, id, height, weight, capture_rate, base_experience, 
                  base_happiness, is_baby, is_legendary, is_mythical, is_default, 
                  order, evolves_from_species, forms_switchable, gender_rate, 
                  has_gender_differences, hatch_counter, types, abilities, and 
                  stats"))
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
  # Now that we have a vector nondf vars that were passed, we can subset each 
  # pokemon's list and call it values
  values <- lapply(pokelist, function(x)x[nondfvars])
  # Convert to a tibble
  valuestibble <- convertToTibble(values)
  # Rename the columns
  colnames(valuestibble) <- nondfvars
  # Next we need to consider our variables that are stuck in data frames, 
  # initialize an empty vector named dfvars
  dfvars = c()
  # For every df variable
  for(i in dfs){
    # Check if it was passed in the vars argument
    if(i %in% vars){
      # If so, append it to the dfvars vector
      dfvars[i] <- i
    }
  }
  # Check if "types" was a variable passed, if so, create a types tibble using 
  # getTypes() and append it to the valuestibble
  if("types" %in% dfvars){
    types <- getTypes(pokelist)
    valuestibble <- tibble(valuestibble, types)
  }
  # Check if "abilities" was a variable passed, if so, create an abilities tibble 
  # using getAbilities() and append it to the valuestibble
  if("abilities" %in% dfvars){
    abilities <- getAbilities(pokelist)
    valuestibble <- tibble(valuestibble, abilities)
  }
  # Check if "stats" was a variable passed, if so, create a stats tibble using 
  # getStats() and append it to the valuestibble
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

## Create Berry Tibble with `berryTibble()`

This is another function that uses a bunch of conditional logic and
explicit coercion to produce a nicely formatted tibble from a given set
of berries and variables. This does not have an overly problematic
variables, so any variable that can be returned from the `berryInfo()`
function can be returned here but in a tibble.

``` r
# Similar function but for berries, can take in berry or id and a vector of 
# variables
berryTibble <- function(berry = NULL, id = NULL, vars){
  # Get a list of berry info from the berryInfo() function
  berrylist <- berryInfo(berry = berry, id = id, vars = vars)
  # These are the variables that require extra parsing because they are in 
  # list/data frames
  dfs <- c("flavors", "natural_gift_type", "firmness", "item")
  # These variables do no require extra parsing
  nondfs <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", 
              "size", "smoothness", "soil_dryness")
  # These variables are supposed to be numeric, will have to convert them to 
  # numeric later
  numericvars <- c("growth_time", "max_harvest", "natural_gift_power", "size", 
                   "smoothness", "soil_dryness")
  # Initialize an empty vector to hold nondf variables that were passed through 
  # vars argument
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
  # Initialize an empty bector to hold df variables that were passed through vars 
  # argument
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
  # If "natural_gift_type" was in the vars argument, get the tibble using 
  # getNatGiftType, then combine that tibble with the valuestibble
  if("natural_gift_type" %in% dfvars){
    natural_gift_type <- getNatGiftType(berrylist)
    valuestibble <- tibble(valuestibble, natural_gift_type)
  }
  # If "firmness" was in the vars argument, get the flavors tibble using 
  # getFirmness, then combine that tibble with the valuestibble
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

## Combine Tibble Functions Into One-Size-Fits-All Query Function with `queryAPI()`

At last, we have reached the end, the almighty `queryAPI()` function.
This is essentially a wrapper function that returns either a nicely
parsed tibble for (nearly) any combination of Pokemon and (nearly) any
combination of Pokemon variables or a tibble for any combination of
berries and berry variables from the pokeAPI. This can take in two of
six potential arguments, some combination of Pokemon names/ids and
variables or berry names/ids and variables. If some combination other
than this is passed, it will throw an error message notifying the user
that something is wrong. Otherwise, it will appropriately return a
tibble from one of either `pokeTibble()` for Pokemon data or
`berryTibble()` for berry data.

Also, last time I???m going to put this warning message:  
**Please Note That id must be a continuous range from 1 to a number!!**

``` r
# Lastly, we can wrap everything together to produce nice tibbles for either 
# pokemon or berry queries
queryAPI <- function(pokemon = NULL, pokeid = NULL, pokevars = NULL, berry = NULL, 
                     berryid = NULL, berryvars = NULL){
  # If only pokemon and pokevars arguments were passed
  if(!is.null(pokemon) & !is.null(pokevars) & is.null(c(pokeid, berry, berryid, 
                                                        berryvars))){
    # Create a pokeTibble from the pokemon and pokevars arguments and return it
    poketibble <- pokeTibble(pokemon = pokemon, vars = pokevars)
    return(poketibble)
  }
  # Else if only pokeid and pokevars arguments were passed
  else if(!is.null(pokeid) & !is.null(pokevars) & is.null(c(pokemon, berry, 
                                                            berryid, berryvars))){
    # Create a pokeTibble from the pokeid and pokevars arguments and return it
    poketibble <- pokeTibble(id = pokeid, vars = pokevars)
    return(poketibble)
  }
  # Else if only berry and berryvars arguments were passed
  else if(!is.null(berry) & !is.null(berryvars) & is.null(c(pokemon, pokeid, 
                                                            pokevars, berryid))){
    # Create a berryTibble from the berry and berryvars arguments and return it
    berrytibble <- berryTibble(berry = berry, vars = berryvars)
    return(berrytibble)
  }
  # Else if only berryid and berryvars arguments were passed
  else if(!is.null(berryid) & !is.null(berryvars) & is.null(c(pokemon, pokeid, 
                                                              pokevars, berry))){
    # Create a berryTibble the berryid and berryvars arguments and return it
    berrytibble <- berryTibble(id = berryid, vars = berryvars)
    return(berrytibble)
  }
  # If some other combination of arguments was passed, it is invalid, return this 
  # error message
  else{
    stop("Must provide either names and variables or ids and variables for 
         pokemon or for berries, not both!")
  }
}
```

## Demonstrate `queryAPI()`

I want to demonstrate that queryAPI works for any combination of Pokemon
names/ids and variables or berry names/ids and variables.

First, I want to create a vector of Pokemon variables that I am
interested in. Then I want to create a vector of berry variables that I
am interested in.

``` r
# These are the variables I am going to be working with in my EDA for pokemon and 
# berries
myPokeVars <- c("name", "id", "types", "abilities", "height", "weight", "is_baby", 
                "is_legendary", "is_mythical", "capture_rate", "stats")
myBerryVars <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", 
                 "size", "smoothness", "soil_dryness", "flavors", 
                 "natural_gift_type", "firmness", "item")
```

This returns a tibble for Pikachu, Mewtwo, Greninja, and Jigglypuff
containing all of the variables I want.

``` r
# Test that it works for pokemon and pokevars combination
queryAPI(pokemon = c("pikachu", "mewtwo", "greninja", "igglybuff", "rayquaza"), 
         pokevars = myPokeVars)
```

    ## # A tibble: 5 ?? 19
    ##   name      id    height weight
    ##   <chr>     <chr>  <int>  <int>
    ## 1 pikachu   25         4     60
    ## 2 mewtwo    150       20   1220
    ## 3 greninja  658       15    400
    ## 4 igglybuff 174        3     10
    ## 5 rayquaza  384       70   2065
    ## # ??? with 15 more variables:
    ## #   capture_rate <int>,
    ## #   is_baby <lgl>,
    ## #   is_legendary <lgl>,
    ## #   is_mythical <lgl>,
    ## #   type1 <chr>, type2 <chr>,
    ## #   ability1 <chr>, ???

This returns a tibble for each of the first 10 Pokemon in the Pokedex
containing all of the variables I want.

``` r
# Test that it works for pokeid and pokevars combination
queryAPI(pokeid = c(1:10), pokevars = myPokeVars)
```

    ## # A tibble: 10 ?? 19
    ##    name      id    height weight
    ##    <chr>     <chr>  <int>  <int>
    ##  1 bulbasaur 1          7     69
    ##  2 ivysaur   2         10    130
    ##  3 venusaur  3         20   1000
    ##  4 charmand??? 4          6     85
    ##  5 charmele??? 5         11    190
    ##  6 charizard 6         17    905
    ##  7 squirtle  7          5     90
    ##  8 wartortle 8         10    225
    ##  9 blastoise 9         16    855
    ## 10 caterpie  10         3     29
    ## # ??? with 15 more variables:
    ## #   capture_rate <int>,
    ## #   is_baby <lgl>,
    ## #   is_legendary <lgl>,
    ## #   is_mythical <lgl>,
    ## #   type1 <chr>, type2 <chr>,
    ## #   ability1 <chr>, ???

This returns a tibble for Cheri, Rawst, and Chesto berries containing
all of the variables that I want.

``` r
# Test that it works for berry and berryvars combination
queryAPI(berry = c("cheri", "rawst", "chesto"), berryvars = myBerryVars)
```

    ## # A tibble: 3 ?? 16
    ##   name   id    growth_time
    ##   <chr>  <chr>       <int>
    ## 1 cheri  1               3
    ## 2 rawst  4               3
    ## 3 chesto 2               3
    ## # ??? with 13 more variables:
    ## #   max_harvest <int>,
    ## #   natural_gift_power <int>,
    ## #   size <int>,
    ## #   smoothness <int>,
    ## #   soil_dryness <int>,
    ## #   spicy <int>, dry <int>, ???

This returns a tibble for all of the berries (there are 64 total)
containing all of the variables that I want.

``` r
# Test that it works for berryid and berryvars combination
queryAPI(berryid = 1:64, berryvars = myBerryVars)
```

    ## # A tibble: 64 ?? 16
    ##    name   id    growth_time
    ##    <chr>  <chr>       <int>
    ##  1 cheri  1               3
    ##  2 chesto 2               3
    ##  3 pecha  3               3
    ##  4 rawst  4               3
    ##  5 aspear 5               3
    ##  6 leppa  6               4
    ##  7 oran   7               4
    ##  8 persim 8               4
    ##  9 lum    9              12
    ## 10 sitrus 10              8
    ## # ??? with 54 more rows, and 13
    ## #   more variables:
    ## #   max_harvest <int>,
    ## #   natural_gift_power <int>,
    ## #   size <int>,
    ## #   smoothness <int>,
    ## #   soil_dryness <int>, ???

## Exploratory Data Analysis (EDA)

Now that we can easily query pokeAPI, it???s time to grab some nicely
formatted data and analyze it with tables and graphs.

### Create My Data Sets

Before I start, I want to make sure I get the variables I want, so I
create vectors containing variable names for Pokemon and berries.

``` r
# These are the variables I am going to be working with in my EDA for pokemon and 
# berries
myPokeVars <- c("name", "id", "types", "abilities", "height", "weight", "is_baby", 
                "is_legendary", "is_mythical", "capture_rate", "stats")
myBerryVars <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", 
                 "size", "smoothness", "soil_dryness", "flavors", 
                 "natural_gift_type", "firmness", "item")
```

I create a tibble named ???myPokeData??? which contains all of the Pokemon
from the first four generations (the first 493 Pokemon) and all of the
19 variables I specified. This takes a decently long time to run. I
tried to get a data set that contained all 905 current Pokemon, but that
takes far too long and often returns a connection error with pokeAPI. So
I decided to just stick with the first four generations, since those are
the generations I am most familiar with. Also, sometimes this code chunk
throws an error, most of the time it does not, it kind of just depends
on how pokeAPI is feeling at the moment.

``` r
# Generate data set for first four generations of pokemon (My childhood)
myPokeData <- queryAPI(pokeid = 1:493, pokevars = myPokeVars)
```

I also created a tibble containing berry data and named it
???myBerryData???. This more or less contains everything from the ???berry???
endpoint of pokeAPI, since it has all of the variables and all of the 64
berries, formatted into a tibble.

``` r
# Generate data set for all berries (there are 64 total)
myBerryData <- queryAPI(berryid = 1:64, berryvars = myBerryVars)
```

### Create generation Variable

The first thing I want to do is create a ???generation??? variable for the
myPokeData data set. This will basically take the id number (which is
also the National Pokedex number) of a Pokemon and bin it into the
correct generation. The list of generations, and their respective
cutoffs, [can be found
here.](https://bulbapedia.bulbagarden.net/wiki/Generation)

To create this variable I created a vector of id integers. Then I
created a vector called ???generation??? through a bunch of if/else logic
based on the generational cutoff values. Then I appended generation onto
myPokeData using the `mutate()` function from `dplyr`. Lastly, I printed
off the `head()` and `tail()` of the new data set to make sure that it
worked. You can use the slider to look at the full code chunk.

``` r
# Convert id to numeric just for the purpose of subsetting
idnums <- as.integer(myPokeData$id)
# Bin based on the id variables using ifelse() and save as a vector named 
# generation.  These values came from bulbapedia.
generation <- ifelse(idnums <= 151, "1",
                       ifelse(idnums > 151 & idnums <= 251 , "2",
                                ifelse(idnums > 251 & idnums <= 386, "3",
                                         ifelse(idnums > 386 & idnums <= 493, "4",
                                                  ifelse(idnums > 493 & idnums <= 649, "5",
                                                           ifelse(idnums > 649 & idnums <= 721, "6",
                                                                    ifelse(idnums > 721 & idnums <= 809, "7",
                                                                             ifelse(idnums > 809 & idnums <= 905, "8", "None"))))))))
# Append the generation vector onto the myPokeData tibble
myPokeData <- myPokeData %>% mutate(generation = generation)
# Print the first few observations with just three columns to check
head(myPokeData) %>% select(name, id, generation)
```

    ## # A tibble: 6 ?? 3
    ##   name       id    generation
    ##   <chr>      <chr> <chr>     
    ## 1 bulbasaur  1     1         
    ## 2 ivysaur    2     1         
    ## 3 venusaur   3     1         
    ## 4 charmander 4     1         
    ## 5 charmeleon 5     1         
    ## 6 charizard  6     1

``` r
# Print the last few observations with just three columns to check
tail(myPokeData) %>% select(name, id, generation)
```

    ## # A tibble: 6 ?? 3
    ##   name         id    generation
    ##   <chr>        <chr> <chr>     
    ## 1 cresselia    488   4         
    ## 2 phione       489   4         
    ## 3 manaphy      490   4         
    ## 4 darkrai      491   4         
    ## 5 shaymin-land 492   4         
    ## 6 arceus       493   4

### Create Some Contingency Tables

We can now make some tables with the data. First, a one-way contingency
table counting how many legendary Pokemon there are using `table()`.

``` r
# Counts of legendaries and non-legendaries
table(myPokeData$is_legendary)
```

    ## 
    ## FALSE  TRUE 
    ##   467    26

Looks like there were 26 legendary Pokemon and 467 non-legendary Pokemon
in generations 1 through 4.

Next, look at a one-way contingency table counting how many Pokemon are
of each type in the type 1 slot.

``` r
# Counts of type 1s
table(myPokeData$type1)
```

    ## 
    ##      bug     dark   dragon 
    ##       42       12       13 
    ## electric    fairy fighting 
    ##       26        8       15 
    ##     fire    ghost    grass 
    ##       31       14       46 
    ##   ground      ice   normal 
    ##       21       15       72 
    ##   poison  psychic     rock 
    ##       24       30       27 
    ##    steel    water 
    ##       14       83

Looks like water is by far the most common type in the first four
generations with 83 Pokemon, with normal not far behind at 72. The least
common types were fairy at 8 and dark at 12.

Next, let???s do the same thing but for the type 2 slot.

``` r
# Counts of type 2s
table(myPokeData$type2)
```

    ## 
    ##      bug     dark   dragon 
    ##        3       11        6 
    ## electric    fairy fighting 
    ##        2       12       10 
    ##     fire   flying    ghost 
    ##        2       64        4 
    ##    grass   ground      ice 
    ##        9       26        7 
    ##     none   poison  psychic 
    ##      258       26       23 
    ##     rock    steel    water 
    ##       10       11        9

258 Pokemon have a single type, more than half of the 493 Pokemon. Of
the second types, the most common was flying at 64 and the least common
were fire and electric at 2.

We can get a better look at this with a two-way contingency table of
type1 and type2.

``` r
# Counts of type 1s and type 2s
table(myPokeData$type1, myPokeData$type2)
```

    ##           
    ##            bug dark dragon
    ##   bug        0    0      0
    ##   dark       0    0      0
    ##   dragon     0    0      0
    ##   electric   0    0      0
    ##   fairy      0    0      0
    ##   fighting   0    0      0
    ##   fire       0    0      0
    ##   ghost      0    1      1
    ##   grass      0    3      0
    ##   ground     0    0      2
    ##   ice        0    0      0
    ##   normal     0    0      0
    ##   poison     1    3      0
    ##   psychic    0    0      0
    ##   rock       2    1      0
    ##   steel      0    0      1
    ##   water      0    3      2
    ##           
    ##            electric fairy
    ##   bug             0     0
    ##   dark            0     0
    ##   dragon          0     0
    ##   electric        0     0
    ##   fairy           0     0
    ##   fighting        0     0
    ##   fire            0     0
    ##   ghost           0     0
    ##   grass           0     0
    ##   ground          0     0
    ##   ice             0     0
    ##   normal          0     4
    ##   poison          0     0
    ##   psychic         0     5
    ##   rock            0     0
    ##   steel           0     1
    ##   water           2     2
    ##           
    ##            fighting fire flying
    ##   bug             1    0     12
    ##   dark            0    2      2
    ##   dragon          0    0      4
    ##   electric        0    0      1
    ##   fairy           0    0      2
    ##   fighting        0    0      0
    ##   fire            4    0      3
    ##   ghost           0    0      2
    ##   grass           1    0      4
    ##   ground          0    0      2
    ##   ice             0    0      2
    ##   normal          0    0     17
    ##   poison          2    0      3
    ##   psychic         1    0      3
    ##   rock            0    0      1
    ##   steel           0    0      1
    ##   water           1    0      5
    ##           
    ##            ghost grass ground
    ##   bug          1     3      1
    ##   dark         1     0      0
    ##   dragon       0     0      3
    ##   electric     1     0      0
    ##   fairy        0     0      0
    ##   fighting     0     0      0
    ##   fire         0     0      2
    ##   ghost        0     0      0
    ##   grass        0     0      1
    ##   ground       0     0      0
    ##   ice          1     0      3
    ##   normal       0     0      0
    ##   poison       0     0      2
    ##   psychic      0     1      0
    ##   rock         0     2      6
    ##   steel        0     0      1
    ##   water        0     3      7
    ##           
    ##            ice none poison
    ##   bug        0   12      8
    ##   dark       2    5      0
    ##   dragon     0    4      0
    ##   electric   0   21      0
    ##   fairy      0    6      0
    ##   fighting   0   12      0
    ##   fire       0   20      0
    ##   ghost      0    7      3
    ##   grass      2   21     12
    ##   ground     0   12      0
    ##   ice        0    4      0
    ##   normal     0   49      0
    ##   poison     0   13      0
    ##   psychic    0   20      0
    ##   rock       0    6      0
    ##   steel      0    1      0
    ##   water      3   45      3
    ##           
    ##            psychic rock steel
    ##   bug            0    1     2
    ##   dark           0    0     0
    ##   dragon         2    0     0
    ##   electric       0    0     3
    ##   fairy          0    0     0
    ##   fighting       2    0     1
    ##   fire           0    1     1
    ##   ghost          0    0     0
    ##   grass          2    0     0
    ##   ground         2    3     0
    ##   ice            2    0     0
    ##   normal         1    0     0
    ##   poison         0    0     0
    ##   psychic        0    0     0
    ##   rock           2    0     3
    ##   steel          6    3     0
    ##   water          4    2     1
    ##           
    ##            water
    ##   bug          1
    ##   dark         0
    ##   dragon       0
    ##   electric     0
    ##   fairy        0
    ##   fighting     0
    ##   fire         0
    ##   ghost        0
    ##   grass        0
    ##   ground       0
    ##   ice          3
    ##   normal       1
    ##   poison       0
    ##   psychic      0
    ##   rock         4
    ##   steel        0
    ##   water        0

Since there are 17 types, this table is mostly 0s, but the most common
types were normal/none and water/none. Of the Pokemon with second types,
the most common was normal/flying with 17 Pokemon.

One common complaint about Generation 4 was that there were hardly any
new fire-type Pokemon available. This meant that players who liked using
fire types were at a disadvantage, so many players chose the fire type
starter Chimchar. We can look at a two-way contingency table between
fire-types in type slot 1 and generation to see how many fire types were
introduced in each generation.

``` r
# Counts of Fire type 1s against generations
table(myPokeData$type1 == "fire", myPokeData$generation)
```

    ##        
    ##           1   2   3   4
    ##   FALSE 139  92 129 102
    ##   TRUE   12   8   6   5

We should not forget to do this for the second type slot.

``` r
# Counts of Fire type 2s against generations
table(myPokeData$type2 == "fire", myPokeData$generation)
```

    ##        
    ##           1   2   3   4
    ##   FALSE 151  98 135 107
    ##   TRUE    0   2   0   0

Overall, there were 12 new fire-types in Generation 1, 10 in Gen 2, 6 in
Gen 3, and 5 in Gen 4. While Generation 4 did have the least, generation
3 was not far behind.

I wanted to make a few contingency tables for berries as well, so first
let???s look at the counts for Natural Gift type. How many berries are of
each type?

``` r
# One-way Contingency Table of Natural Gift Types
table(myBerryData$natural_gift_type)
```

    ## 
    ##      bug     dark   dragon 
    ##        4        4        4 
    ## electric fighting     fire 
    ##        4        4        4 
    ##   flying    ghost    grass 
    ##        4        4        4 
    ##   ground      ice   normal 
    ##        4        4        1 
    ##   poison  psychic     rock 
    ##        4        4        4 
    ##    steel    water 
    ##        3        4

It looks like most Natural Gift types have 4 corresponding berries, but
normal only has 1 (which is the Chilan Berry). Steel has 3.

Natural Gift does either 60, 70, or 80 base damage depending on the
berry. We can look at how many berries do which with a one-way
contingency table.

``` r
# One-way contingency table of Natural Gift Power
table(myBerryData$natural_gift_power)
```

    ## 
    ## 60 70 80 
    ## 33 16 15

33 berries do 60 damage, 16 berries do 70 damage, and 15 berries do 80
damage.

Lastly, let???s look at a two-way contingency table of Natural Gift power
and firmness. This probably will not tell us much.

``` r
# Two-way contingency table of natural gift power and firmness
table(myBerryData$natural_gift_power, myBerryData$firmness)
```

    ##     
    ##      hard soft super-hard
    ##   60    6    9          8
    ##   70    6    5          2
    ##   80    3    4          2
    ##     
    ##      very-hard very-soft
    ##   60         5         5
    ##   70         2         1
    ##   80         4         2

Again, not much meaning here. Some berries are hard and do less damage,
others are soft and do more damage. Doesn???t really make sense.

### Do Some Numerical Summaries

Next, let???s look at some summaries of data, mainly across levels of
categorical variables.

We can quickly use the `summary()` function to look at a five-number
summary of a variable. Let???s see the five-number summary of hp.

``` r
# Five number summary of hp across all four generations
summary(myPokeData$hp)
```

    ##    Min. 1st Qu.  Median    Mean 
    ##    1.00   50.00   65.00   67.91 
    ## 3rd Qu.    Max. 
    ##   80.00  255.00

It looks like the average hp for a Pokemon in generations 1 through 4 is
about 66. The Pokemon that has 1 hp is Shedinja, and the Pokemon that
has 255 hp is Blissey.

We can use the `group_by()` and `summarize()` functions to look at
custom summary statistics of numeric variables grouped by levels of
categorical variables. Let???s see the mean, standard deviation, minimum,
and maximum of capture rates, grouped by generation. The lower a capture
rate is, the harder it is to catch in a Pokeball. [More on capture rates
here.](https://bulbapedia.bulbagarden.net/wiki/Catch_rate)

``` r
# Get numerical summaries for capture rates of pokemon across each generation
myPokeData %>% 
  group_by(generation) %>% 
  summarize(mean = mean(capture_rate), sd = sd(capture_rate), 
            min = min(capture_rate), max = max(capture_rate))
```

    ## # A tibble: 4 ?? 5
    ##   generation  mean    sd   min
    ##   <chr>      <dbl> <dbl> <int>
    ## 1 1          106.   77.1     3
    ## 2 2           91.9  71.7     3
    ## 3 3          113.   83.8     3
    ## 4 4           78.9  69.5     3
    ## # ??? with 1 more variable:
    ## #   max <int>

It looks like the average capture rates for generations 1 and 3 were
higher than generations 2 and 4. This means that Pokemon in generations
2 and 4 were on average harder to catch, gen 4 especially so.

Next, let???s find the mean, standard deviation, minimum, and maximum of
capture rates of legendary Pokemon versus non-legendary Pokemon.
Legendary Pokemon are notoriously hard to catch. [More on legendary
Pokemon
here.](https://bulbapedia.bulbagarden.net/wiki/Legendary_Pok%C3%A9mon)

``` r
# Get numerical summaries for capture rates of pokemon based on whether or not 
# they are legendary
myPokeData %>%
  group_by(is_legendary) %>%
  summarize(mean = mean(capture_rate), sd = sd(capture_rate), 
            min = min(capture_rate), max = max(capture_rate))
```

    ## # A tibble: 2 ?? 5
    ##   is_legendary   mean    sd
    ##   <lgl>         <dbl> <dbl>
    ## 1 FALSE        105.   76.0 
    ## 2 TRUE           4.62  8.24
    ## # ??? with 2 more variables:
    ## #   min <int>, max <int>

The average capture rate for legendary Pokemon is 4.6, whereas it is 104
for non-legendaries. This shows the huge disparity in how hard legendary
Pokemon are to catch. Even the easiest legendary Pokemon to catch only
has a capture rate of 45.

Baby Pokemon are what they sound like, Pokemon that are babies. Pokemon
must be really small and really weak to be classified as a baby. Often,
they will evolve into much stronger Pokemon, however. Since baby Pokemon
are relatively weak, let???s see how they stack up in the attack stat
against non-baby Pokemon using summary statistics. [More on baby Pokemon
here.](https://bulbapedia.bulbagarden.net/wiki/Baby_Pok%C3%A9mon)

``` r
# Get numerical summaries for attack stat of pokemon based on whether or not they 
# are a baby pokemon
myPokeData %>%
  group_by(is_baby) %>%
  summarize(mean = mean(attack), sd = sd(attack), min = min(attack), 
            max = max(attack))
```

    ## # A tibble: 2 ?? 5
    ##   is_baby  mean    sd   min
    ##   <lgl>   <dbl> <dbl> <int>
    ## 1 FALSE    74.9  28.6     5
    ## 2 TRUE     39.2  24.1     5
    ## # ??? with 1 more variable:
    ## #   max <int>

It looks like babies are weaker, but not by a huge amount. The average
attack for a non-baby Pokemon is about 75 and the average attack for a
baby Pokemon is about 39. The standard deviations of these are also
relatively close.

Next, we should look at some numeric summaries for our berry data.
Grouped on each value of Natural Gift power, look at summary statistics
for Growth Time.

``` r
# Get numerical summaries for growth times grouped by Natural Gift powers
myBerryData %>%
  group_by(natural_gift_power) %>%
  summarize(mean = mean(growth_time), sd = sd(growth_time), min = min(growth_time), 
            max = max(growth_time))
```

    ## # A tibble: 3 ?? 5
    ##   natural_gift_power  mean    sd
    ##                <int> <dbl> <dbl>
    ## 1                 60 11.5   6.99
    ## 2                 70  6.88  3.98
    ## 3                 80 22.2   3.73
    ## # ??? with 2 more variables:
    ## #   min <int>, max <int>

It looks like berries that do more damage take longer to grow. This
makes sense, since they should be harder to obtain.

Lastly, let???s look at the summary statistics for size, grouped on each
level of firmness.

``` r
# Get numerical summaries for size grouped by firmness
myBerryData %>%
  group_by(firmness) %>%
  summarize(mean = mean(size), sd = sd(size), min = min(size), max = max(size))
```

    ## # A tibble: 5 ?? 5
    ##   firmness    mean    sd   min
    ##   <chr>      <dbl> <dbl> <int>
    ## 1 hard        119.  59.9    32
    ## 2 soft        120.  82.1    20
    ## 3 super-hard  120.  97.6    34
    ## 4 very-hard   122.  76.3    28
    ## 5 very-soft   124. 118.     28
    ## # ??? with 1 more variable:
    ## #   max <int>

It appears that the average size remains relatively constant, near 120,
for each level of firmness.

### Plots

The final thing I???m going to do in this vignette is create some plots of
categorical and numeric data. All of these plots use the `ggplot2`
package and involve some sort of multi-level layering or grouping.

The first plot I created is a scatter plot colored by type 1 slot, with
attack on the x axis and special attack on the y axis for generations 1
through 4. The distinction between special and regular attack stats is
that there are two types of attacking moves: Special and Physical. An
easy way to think of this, is that a fighting type Pokemon will attack
with their fists, but a psychic type Pokemon will attack with their
mind. Most Pokemon use a mix to these two types of attacks, but certain
types are more prone to having one be higher than the other.

``` r
# Scatterplot of Special Attack vs. Attack, colored by Type 1.  I gave custom 
# labels and title.  Also change the opacity and size of the point and the size 
# of the legend.
ggplot(myPokeData, aes(x = attack, y = special_attack, color = type1)) + 
  geom_point(alpha = 0.5, size = 2) + theme(legend.key.size = unit(0.5, "cm")) + 
  labs(x = "Attack", y = "Special Attack", 
       title = "Attack vs. Special Attack Colored by Type 1 (Gens 1 Thru 4)") +
  scale_color_discrete("Type 1")
```

![](README_files/figure-gfm/attack%20vs%20special%20attack%20scatterplot-1.png)<!-- -->

This distribution of special attack and attack does not seem to have
much correlation to it. Most Pokemon tend to have either higher special
attack or higher attack. As a fan of the games, I know that psychic type
Pokemon often have high special attack and low regular attack. This
graph confirms it, as there are three pink dots in the upper left hand
corner of the graph, which indicate three psychic type Pokemon with high
special attack and low regular attack, as well as many other psychic
types in that corner of the graph. Another thing worth noting it the
obvious straight line that seems to bisect the middle of the graph. This
indicates that there are many Pokemon that have equal, or almost equal,
special attack and attack stats.

Next, we can take a look at the number of new Pokemon introduced into
each generation (1-4). I also wanted to see the number of new
legendaries introduced as a part of that. To do this, I set fill equal
to `as.factor(is_legendary)`. By default, this put the legendaries on
the bottom of the bars, which I thought looked dumb, so I used the
`fct_rev()` function from the `forcats` package to reverse their order.

``` r
# I needed to call in this fct_rev function to put the legendary pokemon on top, 
# since they were on the bottom at first and I didn't like that.
library(forcats)
# Create a bar plot of total pokemon counts by generation.  Also filled bars based 
# on whether or not pokemon were legendary.  This allows us to see the total amount 
# of legendary pokemon as well.  Added custom labels, title, legend, and colors 
# with labs() and scale_fill_manual()
ggplot(myPokeData, aes(x = generation)) + 
  geom_bar(aes(fill = forcats::fct_rev(as.factor(is_legendary)))) + 
  labs(x = "Generation", y = "Total Number of Pokemon", 
       title = "Total Number Of Pokemon and Legendaries by Generation") +
  scale_fill_manual(breaks = c(FALSE, TRUE), values = c("navy", "maroon"), 
                    name = " ", labels = c("Is Legendary", "Is Not Legendary"))
```

![](README_files/figure-gfm/total%20pokemon%20by%20generation%20bar%20plot-1.png)<!-- -->

Of the first four generations, Gen 1 had the most Pokemon, but all four
introduced at least 100 new Pokemon. Gen 1 also had the fewest legendary
Pokemon, which off the top of my head are the three legendary birds:
Articuno, Zapdos, and Moltres, as well as Mew and Mewtwo. Gens 3 and 4
had the most new legendary Pokemon.

My next graph is a histogram of the speed stat for Pokemon in
generations 1 and 2. I made sure to fill in each bin of the histogram
with the corresponding colors. I used 25 bins with default binwidth, and
I also overlaid a kernel density smoother on top of the histogram.

``` r
# Subset the data into just gens 1 and 2
gens1and2 <- myPokeData %>% filter(generation < 3)
# Create two histograms, one for each generation, with 25 bins for the speed stat. 
# Overlay a Density plot on each of them.  Customize labels, title, legend, and 
# colors.
ggplot(gens1and2, aes(x = speed)) + 
  geom_histogram(bins = 25, aes(y = ..density.., fill = generation)) +
  geom_density(aes(fill = generation), alpha = 0.6, position = "stack") + 
  labs(x = "Speed", y = "Density", 
       title = "Histogram and Kernel Smoother of Speed for Generations 1 and 2") +
  scale_fill_manual(name = "Generation", breaks = c("1", "2"), 
                    values = c("red", "orange"))
```

![](README_files/figure-gfm/speed%20in%20generations%201%20and%202%20histogram-1.png)<!-- -->

At first I looked at this graph and thought ???Wow Gen 1 Pokemon were much
faster???, but then I realized that this is all one histogram and that the
distribution is about the same for each generation. However, one thing
that I noticed is that the low end of the histogram is dominated by
generation 2 Pokemon and the high end of the histogram is mostly Gen 1
Pokemon, albeit not many. It looks like most Pokemon in these
generations had a speed of about 40.

Next is a graph of three boxplots of berry size, at each level of
Natural Gift power. Natural Gift does either 60, 70, or 80 damage and I
thought that perhaps larger berries would do more damage than smaller
berries.

``` r
# Create a boxplot for size for each value of natural gift power, since it's 
# technically continuous, the colors will be a gradient. Made custom legend, 
# labels, and title.
ggplot(myBerryData, aes(x = as.factor(as.character(natural_gift_power)), y = size, 
                        fill = natural_gift_power)) + geom_boxplot() +
  labs(x = "Natural Gift Power", y = "Size", 
       title = "Boxplots for Size at Each Level of Natural Gift Power") +
  scale_fill_continuous(name = "Natural Gift Power")
```

![](README_files/figure-gfm/boxplot%20of%20size%20by%20natural%20gift%20powers-1.png)<!-- -->

It appears as though the berries that do more damage are, in fact,
larger. The medians for 70 and 80 power are higher than 60???s, but the
spread of 80???s is much larger. Also, there are a couple of outliers in
the 60 boxplot, indicating that there are a couple abnormally large
berries that do only 60 damage. Also, I had to explicitly coerce the
natural_gift_power into three separate categories, since I read it in as
numeric, but GGPlot still defaults to giving the colors a gradient, I
tried to fix this but was not sure how to.

The last plot I want to look at is another scatter plot, this time about
size versus growth time for berries. The intuition behind this is that I
figured it would take more time to grow larger berries. I also colored
each berry by its corresponding Natural Gift type, although I don???t
think there will be much of a pattern with that.

``` r
# Created a scatterplot of growth time vs. size, colored by natural gift type. 
# Customized labels and title to make it look nice, also changed the size of 
# legends and points, as well as opacity of points.
ggplot(myBerryData, aes(x = growth_time, y = size, color = natural_gift_type)) + 
  geom_point(alpha = 0.5, size = 2) + theme(legend.key.size = unit(0.5, "cm")) + 
  labs(x = "Growth Time", y = "Size", 
       title = "Growth Time vs. Size, Colored by Natural Gift Type") +
  scale_color_discrete("Natural Gift Type")
```

![](README_files/figure-gfm/growth%20time%20vs%20size%20scatterplot-1.png)<!-- -->

It appears that there is some positive correlation between size and
growth time. The graph kind of starts out positively correlated from 1
to 15, but the categories at 18 and 24 have roughly the same
distribution. The largest berries generally take 15, 18, or 24 units of
time to grow.

## Conclusions

Overall, this has been a really fun project for me. While some parts
were challenging, I found it very convenient how easy it is to read in
data from an API into an R session. The amount of data that pokeAPI has
is incredible and leads to an overwhelming amount of possibilities when
it comes to analysis. I will likely revisit this program in the future
and see what kinds of interesting graphs or tables I can come up with,
since Pokemon is something that has always interested me. Working with
berries was also interesting, since that was a part of the game that I
mostly overlooked. Despite berries being a minor part of the games, it
is clear that Game Freak put much thought into them, which is reflected
in the last scatter plot of growth time and size. I am curious to see if
I can find any other neat tidbits that flew under my radar, whether it
be for Pokemon or berries, or data from another endpoint that I can now
write a function for.
