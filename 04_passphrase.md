Advent of Code: Day 4
================
Irene Steves
March 3, 2018

Let's get down to business with [Day 4](https://adventofcode.com/2017/day/4).

![](https://blog.goennounce.com/wp-content/uploads/2015/12/lets-get-down-to-business-gif.gif)

Part I
------

    A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password. A passphrase consists of a series of words (lowercase letters) separated by spaces.

    To ensure security, a valid passphrase must contain no duplicate words.

    For example:

    aa bb cc dd ee is valid.
    aa bb cc dd aa is not valid - the word aa appears more than once.
    aa bb cc dd aaa is valid - aa and aaa count as different words.

    The system's full passphrase list is available as your puzzle input. How many passphrases are valid?

Here's what the beginning of my puzzle input looked like:

``` r
puzzle_input <-
    "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp
xmuf znkhaes pggrlp zia znkhaes znkhaes
nti rxr bogebb zdwrin
sryookh unrudn zrkz jxhrdo gctlyz
bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken
flawt cpott xth ucwgg xce jcubx wvl qsysa nlg
qovcqn zxcz vojsno nqoqvc hnf gqewlkd uevax vuna fxjkbll vfge
qrzf phwuf ligf xgen vkig elptd njdm gvqiu epfzsvk urbltg dqg
sfpku viwihi fje umdkwvi ejzhzj qrbl sfpku sad nawnow ksnku
nzhj mfudick ueaa jnhz kpy pzk
euiin xvl elaoelu wbdd xlv jtm nohtq gfdbgdg gdfggdb edtym
xfmkn wyww woe hwysuh gjw dtk utryasc dela eluk vmmun
nmag qfwe cwslmgd nlhf hpf
ifs sszo iod isf jna
pjptwg wreera leyb hmlbpf qcrbma ylgue
rwlpo jhla rprxvgs quguh pyybwgl qqvcb
rxtcpdy wmpci mpcwi vwvdzdn nfpnj rcsxinl itatg ycy hrctg ron wveju
zmkfn wip pyiz pyiz tnyg dvftf elks ezhotbj wip
sgmtfdd xdl sch sch yaxzh wphgksh knzrixp yaxzh etm czqbaa jldta
gnbr rnpd upe eeb sbq sbq oxc rwvugoj
cshk thcc emfxx emfxx pbtcf jpim vltkqar czy iudkac jhpcc nqs
uzbvx fkiuyk izxdiu yutntvn dixuzi hkyfnud oyz ynutntv"
```

We are interested in determining if each passphrase is valid, so let's start with a function:

``` r
library(tidyverse)

validate_passphrase <- function(passphrase) {
    words <- str_split(passphrase, " ")[[1]] #vector of words
    if(any(duplicated(words)) == TRUE){ #if any words are duplicated
        valid = FALSE #then the passcode is not valid
    } else{ #otherwise, it'll be false
        valid = TRUE
    }
    return(valid)
}
```

Let's test it out to make sure it works the way we think it does:

``` r
validate_passphrase("I love starfruit and nectarines")
```

    ## [1] TRUE

``` r
validate_passphrase("I love chocolate cake and chocolate ice cream")
```

    ## [1] FALSE

With our function, we just need to reformat our puzzle input a bit and pipe everything through.

``` r
puzzle_input %>% 
    str_split("\n", simplify = TRUE) %>% #split by line
    t() %>% #transpose
    as_tibble() %>% 
    rowwise() %>% #run the function on each row separately
    mutate(validation = validate_passphrase(V1)) %>% 
    ungroup() %>% #ungroup to get the sum of ALL rows, not just each row separately
    summarise(sum = sum(validation)) #since TRUE = 1, the sum(validation) is the number of valid passphrases
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <int>
    ## 1    16

Easy peasy!

Part II
-------

    For added security, yet another system policy has been put in place. Now, a valid
    passphrase must contain no two words that are anagrams of each other - that is, a
    passphrase is invalid if any word's letters can be rearranged to form any other word
    in the passphrase.

    For example:

    abcde fghij is a valid passphrase.
    abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
    a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
    iiii oiii ooii oooi oooo is valid.
    oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.

    Under this new system policy, how many passphrases are valid?

To deal with anagrams, let's write another function to rearrange all words alphabetically, and then revise `validate_passphrase` to account for anagrams. We'll call this new function, `validate_anagrams`.

``` r
alphabetize <- function(word) { 
    word %>% 
        str_split("", simplify = TRUE) %>% #split into single letters
        sort() %>% #alphabetize
        str_c(collapse = "") #put back into a word
}

validate_anagrams <- function(passphrase) {
    words <- str_split(passphrase, " ")[[1]] #vector of words
    words_alpha <- unlist(lapply(words, alphabetize))
    if(any(duplicated(words_alpha)) == TRUE){ #if any words are duplicated
        valid = FALSE #then the passcode is not valid
    } else{ #otherwise, it'll be false
        valid = TRUE
    }
    return(valid)
}
```

Now that we have our new function, we can stick it into the same pipe as before to get our answer! :star:

``` r
puzzle_input %>% 
    str_split("\n", simplify = TRUE) %>%
    t() %>% 
    as_tibble() %>% 
    rowwise() %>% 
    mutate(validation = validate_anagrams(V1)) %>% 
    ungroup() %>% 
    summarise(sum = sum(validation)) 
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <int>
    ## 1     9
