## ٩(´ᗜ`)و (´-`) .｡oO (Common function, 2020-09-15)

## General parameters == (2020-09-28) ========================
gp. <- function(...) {
  # skipMess.(suppressPackageStartupMessages('easypackages'::libraries(c('bindrcpp', 'hablar', 'lubridate', 'naturalsort', 'readxl', 'tidyverse'))))
  # if (dev.list() > 0) dev.new(width = 3 * (1+ sqrt(5))/2, height = 3)  # 4.5, 3.3
  # quartz.options(width = 5.682819, height = 3.004405); dev.new(); par(mar = c(2.4, 3.3, 1.1, 2.8), tcl = 0.35)
  # windows(width = 4.5, height = 3.3)  # for Windows
  # Ubuntu Avenir Open Sans Light
    par(mgp = c(0, 0.2, 0), ann = F, xaxs = 'i', yaxs = 'i', col = 'grey13', col.axis = 'grey13', fg = 'grey13', ps = 13, lwd = 1.3)
    par(mar = c(2.4, 4, 0.5, 1), tcl = 0.25, cex.axis = 1, las = 1, family = ifelse(Sys.getenv('OS') == '', 'Avenir Next', 'sans'))
    formals(axis)[c('col.ticks', 'lwd.ticks', 'lwd')] <- list('grey13', 1.3, 0)
}


## Skip warning messages ================================================
skipMess. <- function(x) suppressWarnings(suppressMessages(invisible(x)))


## Path control == (2019-11-04) ================================================
setwd. <- function(...) {  # Needed to copy a file path on the console in advance
    chr <- pp.()
    if (Sys.getenv('OS') == '') {
        chr %>% {if (str_detect(., pattern = 'csv$|xls$|xlsx$')) dirname(.) else .} %>% setwd(.)
    } else {
        if (!str_detect(chr, pattern = '\\\\')) stop('Not available file path...\n\n', call. = F)
        gsub('\\\\', '\\/', chr) %>% setwd(.)
    }
}  # setwd.()


## Lightly vroom() == (2020-04-15) ========================
vroom. <- function(file = NULL, col_names = T, skip = 0, n_max = Inf, ...) {
    if (Sys.getenv('OS') != '' && str_detect(file, pattern = '\\p{Hiragana}|\\p{Katakana}|\\p{Han}')) {
        out <- skipMess.(read_csv(file, locale = locale(encoding = 'cp932'), col_names = col_names, skip = skip, n_max = n_max))
    } else {  # for Mac
        if (is.null(file)) {
            file <- dir(pattern = 'csv|CSV|xls|xlsx') %>% {.[!str_detect(., '\\$')]} %>%
                    {if (length(.) > 1) {chooseOne.(., '\"Target File\"')} else .}
        }
        out <- skipMess.('vroom'::vroom(file, locale = locale(encoding = 'cp932'), col_names = col_names, skip = skip, n_max = n_max))
    }           
    return (out)
}


## Reading data == (2020-09-17) ================================================
getData. <- function(filePath = NULL, file = NULL, timeSort = F, timeFactor = NULL, sheet = F, ...) {
    oldDir <- getwd()
    if (!is.null(filePath)) setwd(filePath)
    if (is.null(file)) {
        file <- dir(pattern = 'csv|CSV|xls|xlsx') %>% {.[!str_detect(., '\\$')]} %>%
                {if (length(.) > 1) {chooseOne.(., '\"Target File\"')} else .}
        if (length(file) == 0) stop('No data file in this directory...\n\n', call. = F)
    }

    if (str_detect(file, pattern = 'csv|CSV')) {
        d <- vroom.(file, col_names = col_names, skip = skip)
    } else if (str_detect(file, pattern = 'xls|xlsx')) {
        if (sheet == TRUE) {
            seqs <- excel_sheets(file) %>% parse_number()  # To mutate(sheet = ~), map_dfr() is not used
            tenta <- map(seqs, ~ skipMess.(read_excel(file, sheet = ., col_names = col_names, skip = skip, n_max = n_max)))
            d <- map(seqs, ~ mutate(tenta[[.]], sheet = .)) %>% bind_rows()
        } else {
            d <- skipMess.(read_excel(file, sheet = 1, col_names = T, skip = 0, n_max = Inf))
        }
    }
    d <- d %>% 'dplyr'::filter(rowSums(is.na(.)) != ncol(.)) %>% 'hablar'::retype() %>% dt2time.(., timeSort, timeFactor) %>%
         mutate_if(., ~ is.character(.), ~ correctChr.(.)) %>% select_if(colSums(is.na(.)) != nrow(.))

    if (!is.null(filePath)) setwd(oldDir)
    return (d)
}


## Set arguments in the function which you're trying to improve == (2020-09-16) ================================================
lazy_arg. <- function(...) {  # Needed to copy a concerned argments in advance
    chrs <- pp.() %>% unlist()
    lazy_do <- function(chr, ...) {
        if(str_detect(chr, 'function')) {  # Delete 'name <- function(' part
            chr <- str_locate(chr, 'function') %>% .[2] %>% {str_sub(chr, . +1, str_length(chr))}
        }
        if (str_detect(chr, '#')) {  # Delete comment out
            chr <- str_locate(chr, '#') %>% .[2] %>% {str_sub(chr, 1, . -1)}
        }
        chr2 <- gsub(',', ';', chr) %>% gsub('\\{|\\}|\\(|\\)|\\...', '', .) %>% gsub('vec;|x;|d;|df;|dt;|dL;', '', .) %>% str_squish(.)
        eval(parse(text = chr2), envir = globalenv())
    }
    for (i in seq_along(chrs)) lazy_do(chrs[i])
}  # lazy_arg.()


## Find whether quasi-time format or not == (2020-09-04) ================================================
lazy_any. <- function(x, Fun = is.na, ...) {
    for (i in seq_along(x)) {
        if (!is.na(x[i])) {
            if (Fun(x[i]) && tryReturn.(date(x[i])) %>% {!is.na(.)}) {
                return (TRUE)
            } else {
                return (FALSE)
            }
            break
        }
        if (i == length(x) && !Fun(x[i])) return (FALSE)
    }
}

is_time. <- function(x, ...) if (is.list(x)) map_lgl(x, ~ is.POSIXct(.) | is.Date(.)) else is.POSIXct(x) | is.Date(x)

is_quasi_time. <- function(x, ...) {
    if (all(is.na(x)) || !is.character(x)) return (FALSE)
    posTime_TF <- is.POSIXct(x)
    dateTime_TF <- is.Date(x)
    is_digit <- function(x) x %>% {str_detect(., '[:digit:]') && !str_detect(., '[:upper:]|[:lower:]') && !is.na(.)}  # No alphabet
    digit_TF <- lazy_any.(x, Fun = is_digit)
    is_timeChr <- function(x) x %>% {str_count(., '/') == 2 || str_count(., '-') == 2 && !str_count (., '/|-') > 2 & !is.na(.)}
    chr_TF <- lazy_any.(x, Fun = is_timeChr)
    return (!posTime_TF & !dateTime_TF & digit_TF & chr_TF)
}  # c('2019/1/8', '2019/1/8 12:34', '2019/1/8 12:34:56', '2019-11-14', '2019-11-14 12:34', '2019-11-14 12:34:56')

is_quasi_period. <- function(x, ...) {
    if (all(is.na(x)) || !is.character(x)) return (FALSE)
    if (x[!is.na(x)] %>% {all(str_count(., ':') %in% 1:2 & str_detect(., '/', negate = T) & str_detect(., '[:digit:]'))}) {
        return (TRUE)
    } else {
        return (FALSE)
    }
}  # c('0:00', '24:00', '123:45', NA, '1:30:00')


## Time style conversion in the tibble level == (2020-09-17) ================================================
dt2time. <- function(d, timeSort = F, timeFactor = NULL, ...) {  # Use this by getData.() & pp.()
    if (map_lgl(d, ~ is_quasi_time.(.) || is_quasi_period.(.)) %>% any() %>% `!`) return (d)
    if (is.data.frame(d) && nrow(d) == 0) return (d)  # Safety net for pp.() when copying a mere cell as vector
    ## Time style conversion in the vector level
    chr2time <- function(x) {
        colons <- str_count(x, ':') %>% max.(.)
        if (is_quasi_time.(x)) {
            time_style <- whichSize.(ref = colons, vec = 0:2, c('Ymd', 'YmdHM', 'YmdHMOS'))  # 'HMOS' reacts millisec as well as sec
            timeVec <- parse_date_time2(x, orders = time_style, tz = 'Asia/Tokyo')
            return (timeVec)
        } else if (is_quasi_period.(x)) {  # Make <Period> vector
            timeVec <- x %>% {
                if (colons == 1) {
                    hm(., quiet = T)
                } else if (colons == 2) {
                    map_chr(x, function(y) if (!is.na(y) && str_count(y, ':') == 1) str_c(y, ':00') else y) %>% hms(., quiet = T)
                }
            }
            return (timeVec)
        } else {
            return (x)  # Return x as raw
        }
    }  #  END of chr2time()
    ## Then, time converting to the tibble; if dt has no time possibility, it will return dt with no change
    if (is.atomic(d)) return (chr2time(d))
    d <- d %>% mutate_if(~ is_quasi_time.(.) || is_quasi_period.(.), ~ chr2time(.))

    ## New colulmn and sorting with time series to distinguish the favorite from time columns
    if (timeSort == TRUE) {
        timeColN <- map_lgl(d, ~ is_time.(.)) %>% names(d)[.]
        if (!is.null(timeFactor)) {
            d <- d[[timeFactor]] %>% {if (is.null(.)) NA else .} %>% mutate(d, Time = .)  # Create 'Time' new column
        } else {
            if (length(timeColN) == 1)  d <- select_if(d, ~ is_time.(.)) %>% pull() %>% mutate(d, Time = .)
            if (length(timeColN) > 1)  d <- chooseOne.(timeColN, '\"TIME factor\"') %>% d[[.]] %>% mutate(d, Time = .)
        }
        if (d$Time %>% {length(.[is.na(.)]) / length(.) > 0.15}) d <- 'dplyr'::filter(d, !is.na(Time))  # Too many NA is crap (15 %)
        d <- arrange(d, Time)  # Ascending sort in time vector
    }
    return (d)
}  # dt2time.(nya0)


## Powerful copy & paste == (2020-09-16) ================================================
pp. <- function(...) {
    type_taste <- function(d) d %>% 'hablar'::retype() %>% map_df(~ type_sum(.))
    type_watch <- function(d) {
        tenta <- pmin(nrow(d), 20) %>% {d[seq(.), ]}  # Using less than 20 rows to minimize the burden of processing of reading
        tenta_type <- map_dfr(1:nrow(tenta), function(i) type_taste(tenta[i, ]))
        body_type <- tenta_type %>% group_by_all() %>% count() %>% select(!n)

        if (nrow(body_type) == 1) {  # No column names
            if (body_type %>% unlist() %>% unique() %>% length() > 1) {
                out <- d %>% 'hablar'::retype() %>% dt2time.(., timeSort = F)
            } else if (ncol(d) == 1) {  # just vector you want
                out <- d %>% 'hablar'::retype() %>% unlist()
            }
        } else if (nrow(body_type) == 2 || nrow(body_type) > 4) {  # row1 = column names, row2 ~ body (Including copy of all chr data
          # d <- tibble(X1 = c('Animal','Bird','Cat','Dog')))  # Including all chr data
            out <- d[-1, ] %>% 'hablar'::retype() %>% dt2time.(., timeSort = F) %>% set_names(d[1, ])
        } else {  # row1~3 = colum names, other ~ body
            body_row1st <- tenta_type %>% group_by_all() %>% add_count() %>% .[['n']] %>% which.max()
            body_out <- d[-1:(-(body_row1st -1)), ]

            tits <- d[1:(body_row1st -1), ] %>% mutate_if(~!is.character(.), ~ as.character(.))
            for (i in seq(nrow(tits))) {  # Combined cells --> Overwrite repeated chr into blank cells
                i4 <- tits[i, ] %>% unlist()
                for (j in seq(ncol(tits))) {
                    i4[j] <- if (is.na(i4[j]) && j == 1) '' else if (is.na(i4[j])) i4[j-1] else i4[j]
                    tits[i,j] <- i4[j]
                }
            }
            tit2 <- map_chr(1:ncol(tits), function(k) tits[[k]] %>% str_flatten(collapse = '.'))
            trim_period <- str_sub(tit2, start = 1, end = 1) == '.'
            if (any(trim_period)) str_sub(tit2[trim_period], start = 1, end = 1) <- ''  # Distinguish '.' on the top location of names

            out <- body_out %>% 'hablar'::retype() %>% dt2time.(., timeSort = F) %>% set_names(tit2)
        }
        return (out)
    }
    clip <- 'readr'::clipboard()
    clip2 <- if (length(clip) > 1) {
        skipMess.(read_delim(clip, col_names = F, delim = '\t')) %>% 'dplyr'::filter(rowSums(is.na(.)) != ncol(.))
    } else {
        str_split(clip, '\t')[[1]]
    }
    clip3 <- if (is.list(clip2)) {
        type_watch(clip2) %>% {
            if (is.atomic(.)) {
                .
            } else {
                mutate_if(., ~ is.character(.), ~ correctChr.(.)) %>% select_if(colSums(is.na(.)) != nrow(.) | !str_detect(names(.), 'X'))
            }
        }
    } else {  # Just copying 1 row;  return a vector
        skipMess.(as.numeric(clip2)) %>% {if (sum(is.na(.)) > length(.) *0.9) clip2 else .}
    }
    return (clip3)
}  # END of pp.()


## Transform list data to tibble == (2020-09-16) ================================================
list2tibble. <- function(dL, ...) {
    if (is.data.frame(dL))  return (as_tibble(dL))  # Safety net; dL is already tibble
    if (map.(dL, ~ class(.)) %>% {!'data.frame' %in% .}) {  # Each nested data is atomic
        tenta <- which(length.(dL) == 0)  # In case of logical(0)
        if(length(tenta) != 0) dL[tenta] <- NA
        dL <- map(dL, ~ enframe(.) %>% .[, -1] %>% set_names(''))  # In case that the list is merely consisted of vectors
    } else {
        dL <- map(dL, ~ as_tibble(., .name_repair = 'minimal'))  # In case that the list has a data.frame (x = ... , y = ...)
    }
    if (is.null(names(dL))) names(dL) <- str_c('List', seq_along(dL))  # Note: dL is all consited of tibble for now

    max_row <- map_dbl(dL, nrow) %>% max.(.)
    dL2 <- map(dL, function(nya) nya[1: max_row, ])  # You must make every length of the list the same before using bind_cols()
    dt <- bind_cols(dL2) %>% set_names(names(dL2))
    return (dt)
}  # list2tibble.(as.list(iris))


## Transform any data to list == (2020-09-17) ================================================
dLformer. <- function(d, naturalOrder = F, ...) {  # naturalOrder = T/F, or desirable order like c(3, 4, 1, 2) accoding to the list's names
    if (is.atomic(d)) dL <- tibble(d) %>% as.list(.)
    if ('data.frame' %in% class(d)) {
        numTF <- map_lgl(d, ~ is.numeric(.))
        chrTF <- map_lgl(d, ~ is.factor(.) | is.character(.))
        timeTF <- map_lgl(d, ~ is_time.(.))
        if (all(numTF)) {  # [y1, y2, ...]
            dL <- as.list(d)  # %>% map(~as_tibble(.))
        } else {
            if (sum(numTF) == 1 && sum(chrTF) == 1) {  # [ID, y]
                dL <- split(d, d[chrTF]) %>% map(~ select_if(., ~ is.numeric(.)) %>% set_names(NULL))  # dLformer.(iris[4:5])
            } else if (sum(numTF) == 1 && sum(timeTF) == 1) {  # [time, y]
                abbre_time <- function(x) {  # ex.) humidity, temperature, yield, monthly report, ...
                    aT <- if (delta.(x, 'year') >= 3) format(x, '%Y年') else if (delta.(x, 'month') >= 4) format(x, '%B') else format(x, '%H時')
                    return (aT)
                }
                d <- mutate_if(d, ~ is_time.(.), ~ abbre_time(.))
                dL <- naturalorder(d[timeTF]) %>% d[., ] %>% split(., .[timeTF]) 
            } else {  # [ID1, ID2, ..., y1, y2, ...] --> [y1, y2, ...]
                dL <- d[numTF] %>% as.list(.)
            }
        }
    } else {  # In case of list
        dL <- d
    }
    dL <- map(dL, ~ .[!is.na(.)]) %>% {if (naturalOrder == TRUE) .[naturalorder(names(.))] else .}
    return (dL)
}  # dLformer.(iris[1]) dLformer.(iris[4:5]) dLformer.(iris[3:5])


## Extract xy from dt as list == (2020-09-17) ================================================
xyL. <- function(d, ...) {  # In case of data.frame [x1, y1, x2, y2, ...] --> [x1, y1], [x2, y2], ... as a list
    if (ncol(d) %% 2 != 0) stop('Make data column number even...\n\n', call. = F)
    out <- seq(ncol(d) /2) %>% map(~ d[c(2 *. -1, 2 *.)] %>% as_tibble())
    return (out)
}  # plt.(psd[-1] %>% xyL.(.))
xyL2. <- function(x, dy, ...) {  # In case of vector {x} & list or data.frame [y1, y2, ...] --> [x, y1], [x, y2], ... as a list
    if (!is.list(dy)) {
        stop('Make data list or data.frame constituded of only y...\n\n', call. = F)
    } else if (is.data.frame(dy)) {
        map2(rep(list(x), ncol(dy)), as.list(dy), ~ bind_cols(x = .x, y = .y)) %>% set_names(names(dy))
    } else {
        map2(rep(list(x), length(dy)), dy, ~ bind_cols(x = .x, y = .y)) %>% set_names(names(dy))
    }
}


## Split ID data into list in a tidy way == (2020-09-08) ========================
splits. <- function(d, ...) {
    chrTF <- map_lgl(d, function(x) is.character(x) | is.factor(x))
    if (sum(chrTF) > 1) {
        stop('Try again with a ID data...\n\n', call. = F)
    } else if (sum(chrTF) == 1) {  # [ID, y1, y2, ...]
        split(d, d[chrTF]) %>% map(~ .[!chrTF] %>% as_tibble())
    } else if (sum(chrTF) == 0){  # [y1, y2, ...]
        as.list(d)
    }
}  # splits.(iris)


## HTML table == (2020-09-17) ================================================
html. <- function(d, ...) {
    if (is.atomic(d) && !is.null(names(d))) d <- as.list(d) %>% list2tibble.()  # Case with a vector with names created by sapply()
    num2chr <- function(num) {
        if (!is.numeric(num)) return (num)
        Digits <- gsub('\\.', '', num) %>% gsub('^0', '', .) %>% str_length(.)
        Digits <- rep(3, length(num))  # mm unit allows 0.001; '%.3f'
        Digits[num >= 100] <- 0  # 100.000 looks bothersome, so change it to 100
        Digits[num %>% near(., as.integer(.))] <- 0  # In case of integer
        chr <- vector(mode = 'character', length = length(num))
        for (i in seq_along(num)) chr[i] <- sprintf(str_c('%.', Digits[i], 'f'), num[i])
        return (chr)
    }
    d <- d %>% mutate_if(~ is.numeric(.), ~ num2chr(.))  # All data is character
    d[is.na(d)] <- ''  # Printing blank instead of NA
    'DT'::datatable(d, rownames = F, options = list(pageLength = 100), filter = 'top')
}


## Quick check for basic statistics == (2020-05-19) ================================================
base_stats. <- function(d, ...) {
    statsN <- c('Median', 'Avg', 'SD', 'Max', 'Max without outliers', 'Min without outliers', 'Min', 'Range', 'Total',
                'Skew', 'Kurtosis', 'Number')
    if (is.list(d)) {
        dt <- list2tibble.(d) %>% select_if(~ is.numeric(.) | is_time.(.) & n_distinct(.) > 1)  # No column with the same value
        Stats <- rbind(median.(dt), mean.(dt), sd.(dt), max.(dt), max2.(dt, na = T), min2.(dt, na = T), min.(dt), delta.(dt), sum.(dt),
                       skew.(dt), kurt.(dt), length.(dt))
        res <- cbind(Basic = statsN, Stats) %>% as_tibble(.) %>% {skipMess.(type_convert(.))}
    } else if (is.atomic(d)) {
        Stats <- c(median.(d), mean.(d), sd.(d), max.(d), max2.(d), min2.(d), min.(d), delta.(d), sum.(d), length.(d))
        vecN <- substitute(d) %>% as.character(.) %>% {if (length(.) == 1) . else .[2]}  # Confirm; substitute(iris [[1]]) %>% as.character
        res <- bind_cols(Basic = statsN, x = Stats) %>% set_names(c('Basic', vecN))
    }
    return (res)
}  # base_stats.(iris) %>% html.(.)


## Search for the nearest number of which the target vector is almost equal to the reference value == (2020-09-16) ============
whichNear. <- function(vec, ref, back = F, ...) map_dbl(ref, ~ which(abs(vec -.) == min.(abs(vec -.))) %>% nth(., ifelse(back, -1, 1)))

## Select just value you want according to a condition (sorry for misleading name); Note ref = sizeVec in length == (2019-11-14) ======
whichSize. <- function(vec, ref, sizeVec, ...) whichNear.(vec, ref) %>% sizeVec[.]


## Japanese or not for label & legend == (2020-02-06) ================================================
jL. <- function(chr, ...) {
    if (class(chr) == 'character') {
        if (!exists('chr') || is.null(chr) || anyNA(chr)) return (ifelse(Sys.getenv('OS') == '', 'Avenir Next', 'sans'))
        tf <- str_detect(chr, pattern = '\\p{Hiragana}|\\p{Katakana}|\\p{Han}')
        if (any.(tf)) {
            ifelse(Sys.getenv('OS') == '', 'HiraginoSans-W3', 'Meiryo')
        } else {
            ifelse(Sys.getenv('OS') == '', 'Avenir Next', 'sans')  # Avenir  Ubuntu Open Sans Light  # names(pdfFonts())
        }
    } else {  # in case of 'expression()'
        return (ifelse(Sys.getenv('OS') == '', 'Avenir Next', 'sans'))
    }
}  # mtext(~, family = jL.(c(Xlab, Ylab)))


## Correct abnormal Jap characters == (2020-09-17) ================================================
hankana2zenkana. <- function(chr, ...) {
    if (!is.character(chr)) chr <- as.character(chr)
    ## Converting semi-dakuten
    han <- c('ｶﾞ','ｷﾞ','ｸﾞ','ｹﾞ','ｺﾞ','ｻﾞ','ｼﾞ','ｽﾞ','ｾﾞ','ｿﾞ','ﾀﾞ','ﾁﾞ','ﾂﾞ','ﾃﾞ','ﾄﾞ','ﾊﾞ','ﾋﾞ','ﾌﾞ','ﾍﾞ','ﾎﾞ','ﾊﾟ','ﾋﾟ','ﾌﾟ','ﾍﾟ','ﾎﾟ')
    zen <- c('ガ','ギ','グ','ゲ','ゴ','ザ','ジ','ズ','ゼ','ゾ','ダ','ヂ','ヅ','デ','ド','バ','ビ','ブ','ベ','ボ','パ','ピ','プ','ペ','ポ')
    for (i in seq_along(zen)) chr <- gsub(han[i], zen[i], chr)
    ## Converting full width space or double spaces to half one
    chr <- gsub('　|  ', ' ', chr)
    ## Converting 1bite character
    out <- chartr('ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰ',
                  'アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン。「」、・ヲァィゥェォャュョッー', chr)
    return (out)
}


## Zenkaku convert for Mac/Win == (2020-09-16) ================================================
zenk. <- function(chr, ...) {
    out <- chr %>% as.character(.) %>% gsub('\r\n', '', .) %>% gsub('  ', '', .) %>%  # gsub('ー', '-', .) # Avoid killing '\n' for box2.()
           {if (any.(validUTF8(.)) || Sys.getenv('OS') != '') . else iconv(., 'utf-8', 'cp932')} %>%
           hankana2zenkana.() %>% map_chr(~ 'stringi'::stri_trans_nfkc(.))
    return (out)
}


## Reshape text by cutting space & common characters == (2020-09-17) ================================================
correctChr. <- function(chr, ...) {
    if (str_detect(chr, '[:alpha:]') %>% any.(.) %>% `!`) {
        if (str_detect(chr, '%') %>% any.(.)) chr <- as.vector(chr) %>% parse_number(.) %>% {. /100}  # '12.3%'
        if (str_detect(chr, ',') %>% any.(.)) chr <- as.vector(chr) %>% gsub(',', '', .) %>% parse_number(.)  # "123,456,789", or "\1,000"
    }
    chr[chr %in% ''] <- NA  # str_detect(chr, '') doesn't work well...
    out <- zenk.(chr)
    return (out)
}


## Reshape text by cutting space & common characters == (2020-02-07) ================================================
neatChr. <- function(chr, ...) {  # c('nya :: A', 'nya :: B') --> c('A', 'B')
    ## Delete space character
    regularChar <- c('(', ')', '[', ']', '$', '^', '?', '*', '+', '{', '}', '|', '\'')  # Replace the regular chr that cannot use str_detect()
    for (i in seq_along(regularChar)) chr <- gsub(regularChar[i], ' ', chr, fixed = T) %>% str_trim(., side = 'both')
    if (length(chr) == 1) return (chr)
    ## Search for common characters and delete them.
    len <- min.(str_length(chr))
    ctr <- NULL
    for (i in 2:len) {  # Why it starts 2 is; if a string is 'A' and it's removed, then the string after cutting will be ''.
        tenta <- str_trunc(chr, width = i, ellipsis = '') %>% unique(.)
        if (length(tenta) == 1 && str_detect(tenta, '[:alpha:]') %>% all(.)) ctr <- i
    }
    if (!is.null(ctr)) chr <- str_replace(chr, pattern = substr(chr[1], 1, ctr), replacement = '')
    return (chr)
}


## Interactive filter == (2020-09-17) ================================================
chooseOne. <- function(factors, messText = NULL, freqs = NULL, chr = T, ...) {
    ## freqs denotes each N of the factors, chr = T returns text (F returns number).
    if (length(factors) == 1) return (factors)
    tenta <- rep(NA_character_, length(factors))
    if (!is.null(freqs)) {
        chrLen <- map_dbl(factors, ~ nchar(., type = 'width'))
        spaceLen <- max(chrLen) -chrLen +1
        freqN <- map.(seq_along(tenta), ~ str_c(str_flatten(rep(' ', spaceLen[.])), '(N = ', freqs[.], ')'))
    } else {
        freqN <- NULL
    }
    for (i in seq_along(tenta)) tenta[i] <- str_c('     [', i, ']', ifelse(i < 10, '   ', '  '), factors[i], freqN[i], '\n')
    cat(str_c('\n      Choose one from below;\n\n'), tenta)
    repeat {
        num <- readline(str_c('\n      Which No.', ifelse(is.null(messText), '', 'as '), messText, ' ?  \n\n   >>> '))
        if (correctChr.(num) %>% {skipMess.(as.numeric(.))} %>% {!is.na(.)} ) {
            num <- correctChr.(num) %>% as.numeric(.)  # To gurantee your input as numeric
            if (num >= 1 && num <= length(factors)) break  # This if () restricts proper range and prohibit minus or oversized.
        }
    }
    cat(str_c(str_dup('=', times = 75), '\n'))
    return (ifelse(chr, factors[num], num))  # text or its number
}


## Plot range for plot.window() & axisFun.() == (2020-01-21) ================================================
pr. <- function(vec, XYlims = NA, expand_ratio = 0.02, ...) {
    if ('list' %in% class(vec)) vec <- unlist(vec)
    vec[which(vec == Inf | vec == -Inf)] <- NA
    def.(c('Min', 'Max'), list(min.(vec), max.(vec)))  # Type free for vec, list, data.frame
    ## Type numbering; complicated but needed to use each  '&'  '&&' below...
    range_type <- XYlims %>% { c(length(.) > 1 & c(!anyNA (.), is.na(.[1]), is.na(.[2])), is.na(.) && length(.) == 1)} %>% which(.)
    xyR <- list(XYlims, c(Min, XYlims[2]), c(XYlims[1], Max), c(Min, Max))[[range_type]]
    expand_direction  <-  list (c(0, 0), c(-1, 0), c(0, 1), c(-1, 1))[[range_type]]
    XYlim2 <- xyR +diff(xyR) *expand_direction *expand_ratio
    return (XYlim2)
}  # pr.(vec = iris[, 1], XYlims = NA, 0.013)


## Axis value == (2020-02-07) ================================================
halfSeq. <- function(vec, ...) vec[-1] -diff(vec) /2  # Solution of bn = (an+1 - an)/2
axisFun. <- function(XYlims, n = 5, ...) pretty(XYlims, n = n) %>% list(mainTicks = ., subTicks = halfSeq.(.))


## Cyclic number if it's over range for the interactive input == (2020-06-26) ================================================
n_cyc. <- function(num, n_max, ...) {
    if (length(num) == 1) {
        out <- if (is.na(num)) NA else {num %% n_max} %>% ifelse(. != 0, ., n_max)
    } else {
        out <- num[!is.na(num)] %>% rep(., n_max) %>% .[1:n_max]
    }
    return (out)
}  # n_cyc.(11, 5) n_cyc.(1:3, 5) n_cyc.(c(1, NA, 9), 5)

            
## Creat translucent color == (2020-02-07) ================================================
colTr. <- function(color, tr, ...) {  # = adjustcolor(colors, tr, ...)
    if (is.null(color) || is.na(color) || color == 0) '#FFFFFF00' else rgb(t(col2rgb(color) /255), max = 1, alpha = tr)  #tr := transparency
}


## Color gradient with Y values == (2019-11-14) ================================================
colGra. <- function(vec, colors, ColorSteps = 13, ...) {
    colors2 <- c(colTr.(colors, tr = 0.6), 'grey80', colTr.(colors, tr = 0.6))  # sapply(vec, abs) %>% max.(.)
    kingMax <- abs(vec) %>% max.(.)  # NOTE: the 2nd para. of the following findInterval() are not {min, max} but {-max, +max}
    return (colorRampPalette(colors2)(ColorSteps)[findInterval(vec, seq(-kingMax, +kingMax, length.out = ColorSteps))])
}


## Auto color assignment == (2020-10-01) ================================================
colors. <- function(col = NULL, d = NULL, ...) {
    col_base <- c('black', 'antiquewhite3', 'cadetblue4', 'sienna3', 'palevioletred3', 'seagreen4', 'dodgerblue3', 'darkorange2',
                  'maroon4', 'hotpink2', 'peachpuff2', 'lightsalmon3', 'tomato2', 'deeppink3', 'slateblue2', 'deepskyblue4', 'darkseagreen3')
    ocean_pal <- colorRampPalette(c('#003000','#005E8C','#0096C8','#45BCBB','#8AE2AE','#BCF8B9'))
    land_pal <- colorRampPalette(c('#467832','#887438','#B19D48','#DBC758','#FAE769','#FCED93'))

    ## Cleaning
    col_clean <- function (...) {
        out0 <- map_chr(as.list(col), function(x) {  # col <- c('red', 'black', 0, 'abc')
            if (x %in% colors()) {
                x <- x
            } else if (str_detect(col, '#') && str_length(x) == 9) {
                x <- x
            } else {
                if (str_detect(x, pattern = '[:digit:]')) {
                    x <- if (x == '0') '#FFFFFF00' else parse_number(as.character(x)) %>% n_cyc.(., n_max = length(col_base)) %>% col_base[.]
                } else {
                    x <- 'black'  # wrong spell or NA
                }
            }
        })
        return (out0)
    }

    ## Arrange
    if (is.null(col) && is.null(d)) {  # colors.()
        out <- col_base
    } else if (!is.null(col) && is.null(d)) {  # colors.(c('red', 'black', 0, 'abc'))
        out <- col_clean()
    } else if (!is.null(col) && !is.null(d)) {  # colors.(c('grey35', 'blue3'), iris)
        out <- col_clean() %>% rep(., times = n_factor.(d)) %>% {.[1 : n_factor.(d)]}
    } else if (is.null(col) && !is.null(d)) {  # Auto assignment according to data
        if (n_factor.(d) < 3) {
           out <- c('black', 'grey60')
        } else if (n_factor.(d) < 4) {  # colors.(d = iris[1:3])
           out <- c('black', 'grey45', 'grey60')
        } else if (n_factor.(d) < 6) {  # colors.(d = iris)
           out <- col_base  # this is the same to colors.(), but is a kind of safety net...
        } else {  # colors.(d = c(iris, iris, iris))
         # NOTE: name conflict of map() due to loading 'maps' package by use of 'pals'
         # col_v <- n_factor.(d) %>% c('pals'::tol.rainbow(.), 'pals'::linearl(.), 'pals'::cubicl(.))[floor(runif(1, min = 1, max = 3 +1))]
         # col_v <- c('A','B','C','D')[floor(runif(1, min = 1, max = 4 +1))] %>% {'viridisLite'::viridis(n_factor.(d), option = .)} %>% rev(.)
           pal1 <- 'viridisLite'::viridis(n_factor.(d) +1, option = 'A')[-(n_factor.(d)+1)]
           pal2 <- ocean_pal(n_factor.(d))
           pal3 <- c(ocean_pal(n_factor.(d) %/% 2), land_pal(n_factor.(d) -n_factor.(d) %/% 2))
           out <- floor(runif(1, min = 1, max = 3 +1)) %>% list(pal1, pal2, pal3)[[.]]
        }
    }
    return (out)
}  # END of colors.()


## Halo around text == (2020-02-07) ================================================================================================
## https://stackoverflow.com/questions/25631216/r-plots-is-there-any-way-to-draw-border-shadow-or-buffer-around-text-labels
haloText. <- function(x, y, labels, cex, col = 'grey13', bg = 'white', theta = seq(0, 2 *pi, length.out = 36), r = 0.05, ...) {
    'purrr'::invoke_map(function(i) text(x +r *strwidth('A') *cos(i), y +r *strheight('A') *sin(i), labels, cex = cex, col = bg), as.list(theta))
    text(x, y, labels, cex = cex *0.95, col = col)  # Then draw actual text
}


## Optimum position of y-axis label == (2020-09-17) ================================================
yPos. <- function(Ylim2, ...) {
    axisFun.(Ylim2, n = 6)[[1]] %>% {.[between(., Ylim2[1], Ylim2[2])]} %>% {strwidth(.) /strwidth('|||')} %>% #{max(ceiling(.))}
    max(.) %>% whichSize.(vec = c(1, 2, 3, 3.5, 4), ref = ., c(2.3, 1.9, 1.3, 1.1, 0.9))  # same adjust; -0.1, 0.1, 100
}


## Positioning legend == (2019-11-09) ================================================
legeX. <- function(legePos_x, ...) par('usr')[1] +diff(par('usr')[1:2]) *legePos_x
legeY. <- function(legePos_y, ...) par('usr')[3] +diff(par('usr')[3:4]) *legePos_y


## Easy legend == (2020-06-21) ================================================
legend2. <- function(name, legePos = NA, col = NA, lty = NA, cex = NA, int = NA, ...) {
    par(family = jL.(name))
    nameLen <- 'stringi'::stri_numbytes(name) %>% max.(.)  # Count including multi bytes char and space
    legeX <- if (!anyNA(legePos)) legePos[1] else nameLen *(-0.013) +0.70
    legeY <- if (!anyNA(legePos) && length (legePos) == 2) legePos[2] else 0.975
    if (is.na(cex)) {
        cex <- whichSize.(ref = nameLen, vec = c(10, 20, 33, 38, 45, 54), c(0.85, 0.75, 0.65, 0.58, 0.49, 0.55)) %>%
               {. *whichSize.(ref = length(names), vec = c(5, 10, 20), c(1, 0.8, 0.6))}
    }
    if (is.na(int)) {
        int <- whichSize.(ref = nameLen, vec = c(10, 20, 33, 38, 45, 54), c(1.3, 1.3, 1.4, 1.4, 1.4, 1.4)) %>%
               {. *whichSize.(ref = length(names), vec = c(5, 10, 20), c(1, 1.1, 1.2))}
    }
    if (anyNA(col)) col <- colors.()
    if (anyNA(lty)) lty <- 1
    legend(x = legeX.(legeX), y = legeY.(legeY), legend = name, x.intersp = 0.65, y.intersp = int, cex = cex, horiz = F, lty = lty,
           box.lty = 0, lwd = 0.95, seg.len = 1.3, col = colTr.(col, tr = 0.9), text.col = col, bg = colTr.(NULL, 0.6))  # 'white'
    par(family = ifelse(Sys.getenv('OS') == '', 'Avenir Next', 'sans'))
}  # legend2.(letters[1:3], legePos = c(0.05, 0.9999), cex = 0.65, col = colors.()[1:3])


## Now is the time == (2020-09-17) ================================================
today2. <- function(...) today(tz = 'Asia/Tokyo') %>% gsub('-', '', .) %>% str_sub(3, 8)
now2. <- function(...) now(tz = 'Asia/Tokyo') %>% gsub('-|:', '', .) %>% gsub (' ', '-', .) %>% str_sub(3, 13)


## Save graphics == (2020-06-29) ================================================
save. <- function(name = NULL, type = 'jpg', wh = dev.size(), ...) {
    saveN <- name %||% today2.()
    if (type %in% c('jpg', 'jpeg', 'j')) {
        dev.copy(jpeg, file = str_c(saveN, '.jpg'), units = 'in', width = wh[1], height = wh[2], res = 150)
        dev.off()
    }
    if (type %in% 'png') {
        dev.copy(png, file = str_c(saveN, '.png'), units = 'in', width = wh[1], height = wh[2], res = 350)
        dev.off()
    }
}
save2. <- function(name = NULL, wh = c(4.3, 3.3), ...) {
    saveN <- name %||% now2.()
    if (names(dev.cur()) == 'cario_pdf') dev.off()
    tryPDF <- function(...) {
        try(skipMess.(cairo_pdf(str_c(saveN, '.pdf'), width = wh[1], height = wh[2], bg = 'transparent', onefile = T)), silent = T)
    }
    if (class(tryPDF()) == 'try-error') {
        if (names(dev.cur()) != 'null device') dev.off()
        str_c('Do close \"', saveN, '.pdf\" on your application !!\n\n') %>% stop(., call. = F)
    } else {
        dev.off(); tryPDF()
    }
    gp.()
}


## Write list data to csv/xlsx file == (2020-09-17) ================================================
write. <- function(d, name = NULL, ...) {
    if ('list' %in% class(d)) d <- list2tibble.(d)
    name <- {name %||% today2.()} %>% {if (str_detect(., '\\.csv')) . else str_c (., '.csv')}
    write.csv(d, name, row.names = F, na = '', fileEncoding = 'cp932')
}
write2. <- function(dL, name = NULL, ...) {
    if (!'list' %in% class(dL)) stop('Change the data into a list...\n\n', call. = F)
    if (is.null(names(dL))) names(dL) <- str_c('#', seq_along(dL))
    'openxlsx'::write.xlsx(dL, file = str_c(name %||% today2.(), '.xlsx'))
}  # write2.(list(iris, mtcars, chickwts, quakes))


## Fitting by GAM (Generized Additive Model) - GCV (Generized Cross Validation) == (2020-09-17) ========================
gamXY. <- function(x, y, mdlGet = F, boost = F, n.boost = NULL, ...) {
    skipMess.(library('mgcv'))
    x <- x[order(x)]
    y <- y[order(x)]
    ## Small data length cannot allow to use knot style
    gamKnots <- function(knots) try(gam(y ~ s(x, k = knots, bs = 'cr'), method = 'REML'), silent = T)
    minGCV <- if (class(gamKnots(15))[1] == 'try-error') gamKnots(5) else gamKnots(15)  # if true, return; [1] 'gam' 'glm' 'lm'
    if (mdlGet == TRUE) return (minGCV)
    if (boost == TRUE) {
        qXlen <- n.boost %||% whichSize.(ref = length(x), vec = c(50, 500), c(130, 888))
        qX <- seq(min.(x), max.(x), length = qXlen)
        xyFit  <-  data.frame(x = qX, y = predict(minGCV, newdata = data.frame(x = qX)))
    } else if (boost == FALSE) {  # GAM model always ignores NA and needs arranged. Even a plot with NA looks vacant, the fitted line shows proper position.
        y[!is.na(y)] <- fitted(minGCV)  # Use NA info of original y and express unnatural appearance of vacant data
        xyFit <- data.frame(x = x, y = y)             
    }
    return (xyFit)
}


## Finding curve intersection; different (x, y) version;  not so accurate if df has not so many data points == (2020-01-23) ==
## just vector but accurate analysis;  https://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
## another Ref;  https://stackoverflow.com/questions/31574382/intersection-between-density-plots-of-multiple-groups
intersectX. <- function(df1, df2, ...) {
    if (is.null(ncol(df1)) || is.null(ncol(df2))) {    #  for vector
        df1 <- data.frame(x = seq_along(df1), y = df1)
        df2 <- data.frame(x = seq_along(df2), y = df2)
    }
    s1 <- rep(NA_real_, nrow(df1))
    for (i in seq_along(s1)) {
        s1[i] <- whichNear.(vec = df2[[1]], ref = df1[[i, 1]]) %>% {abs(df1[[i, 2]] - df2[[., 2]])}
    }
    s1 <- scale(s1) %>% as.vector()
    fp <- 'pracma'::findpeaks(-s1, nups = 10, ndowns = 10)  # '-' is to search for valleys, nups & ndowns are stricts of successive peaks
  # plot(s1); points(-fp[, 1] ~ fp[, 2], pch = 19, cex = 0.8, col = 'tomato2')  # Valleys analysis needs '-fp[, 1]' for plot.
    crossX <- rep(NA_real_, nrow(fp))  # Variation filter > 0.01
    for (i in seq_along(crossX)) crossX[i] <- fp[i, 2] %>% {abs(s1[. +1] -s1[.]) /((. +1) -.)} %>% {ifelse(. > 0.01, fp[i, 2], NA)}
    out <- crossX[!is.na(crossX)] %>% df1[., 1]
    return (out)
}  # plt.(list(df1, df2); abline(v = intersectX.(df1, df2))


## Quick plot == (2020-08-23) ================================================
plt. <- function(d, natural = F, lty = NA, lwd = NA, xlab = '', ylab = '', col = NULL, Xlims = NA, Ylims = NA,
                 legePos = NA, name = NULL, PDF = T, add = 1, mar = par('mar'), tcl = par('tcl'), type = 0, ...) {
    ## You must prepare a data of list(tibble(x = ~, y = ~)) to draw x-y graph; otherwise n-x & n-y graph are separately drawn
    if ('list' %in% class(d)) {  # [[x1, y1], [x2, y2], ...]
        if (map_lgl(d, ~ ncol(.) == 2) && map.(d, map_lgl, is.numeric)) {
            dL <- d
        } else {
            stop('Try again with a data something like a list = [[x1, y1], [x2, y2], ...].\n\n', call. = F)
        }
    } else if ('data.frame' %in% class(d)) {
        if (ncol(d) == 2 && map_lgl(d, ~ is.numeric(.)) %>% all()) {  # [x, y]
            dL <- list(d)
        } else {  # [ID, y] or [y1, y2, ...]
            dL <- dLformer.(d, natural) %>% map(~ tibble(x = seq_along(.), y = .))  # Convert index type
        }
    } else if (is.atomic(d)) {
        dL <- dLformer.(d, natural) %>% map(~ tibble(x = seq_along(.), y = .))  # Convert index type
    } else {
        stop('Try again with a data something like [x, y], [ID, y] or [y1, y2, ...].\n\n', call. = F)
    }

    lty <- if (anyNA(lty)) seq_along(dL) else if (length(lty) == 1 && is.numeric(lty)) rep(lty, length(dL)) else n_cyc.(lty, length(dL))
    if (is.na(lwd)) lwd <- whichSize.(ref = length(dL), vec = c(5, 10, 25, 50), c(1.5, 1.1, 0.8, 0.15))
    col <- colors.(col, d = max(length(dL), length(name)) %>% seq(.))
    def.(c('Xlim2', 'Ylim2'), list(pr.(map.(dL, ~ .[1]), Xlims, 0.02), pr.(map.(dL, ~ .[2]), Ylims, 0.12)))
    if (add != 2) {  # add = 0; just prepare empty camvas, add = 1; normal plot, add = 2; add lines or points only
        par(mar = mar, tcl = tcl)
        plot.new()  # ex) plt.(d, add = 1); polygon (~); plt.(d, add = 2)
        plot.window(xlim = Xlim2, ylim = Ylim2)
        for (i in 1:2) for (j in 1:2) {
            axis(side = 2*j-1, at = axisFun.(Xlim2, n = 6)[[i]], labels = (i*j==1), tcl = par('tcl')/i, cex.axis = 1, lend = 'butt', padj = -0.1)
            axis(side = 2*j, at = axisFun.(Ylim2, n = 5)[[i]], labels = (i*j == 1), tcl = par('tcl')/i, cex.axis = 1, lend = 'butt')
        }
        box()
        mtext(xlab, side = 1, las = 1, cex = 1, family = jL.(xlab), line = par('mar')[1] -1.00)
        mtext(ylab, side = 2, las = 3, cex = 1, family = jL.(ylab), line = par('mar')[2] -yPos.(Ylim2))
        if (add == 0) return (cat('\n'))
    }
    for (i in seq_along(dL)) {
        if (type == 0) {
            lines(dL[[i]], lty = lty[i], col = colTr.(col[i], tr = 0.8), lwd = lwd)
        } else if (type == 1) {
            points(dL[[i]], pch = lty[i], col = colTr.(col[i], tr = 0.8), lwd = lwd /2)
        } else if (type == 2) {
            lines(dL[[i]], lty = 1, col = colTr.(col[i], tr = 0.8), lwd = lwd)
            points(dL[[i]], pch = lty[i], col = colTr.(col[i], tr = 0.8), lwd = lwd /2)
        }
    }
    if ((length(dL) != 1 || !is.null(name)) && !0 %in% name) {  # No legend is needed for one line at least. Or name = 0 returns no legend
        if (is.null(name)) name <- names(dL) %>% {if (!is.null(.)) . else str_c('#', seq_along(dL))}  # Auto assignment
        legend2.(name, legePos, col, lty)
    }
    if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
}  # plt.(iris[4:5])  plt.(iris[-5], legePos = c(0.01, 0.99), lty = 1)  plt.(psd[[1]])  plt.(list(psd[2:3]), Ylims = c(0, NA))


## Kernel Density Estimation plot == (2020-08-22) ================================================
dens. <- function(d, bw = 1, natural = F, lty = NA, lwd = NA, xlab = '', ylab = '', col = NULL, Xlims = NA, Ylims = c(0, NA),
                  legePos = NA, name = NULL, cum = F, ...) {
    kde_xy <- function(vec) {
        vec <- vec[!is.na(vec)]
        Bw <- list('nrd0', 'Sj-ste',  # Sheather-Jones
                   (4/3) ^(1/5) *sd(vec) *length(vec) ^(-1/5),  # Silverman's rule of thumb is sensitive to outliers
                ## https://www.jstage.jst.go.jp/article/transinf/E102.D/1/E102.D_2018EDP7177/_pdf
                   1.06 *median(abs(vec -median(vec))) /0.6745 *length(vec) ^(-1 /(2 *2 +1)),  # More robust sd
                   1.08 *median(abs(vec -median(vec))) /0.6745 *length(vec) ^(-1 /(2 *4 +1)),
                   1.08 *median(abs(vec -median(vec))) /0.6745 *length(vec) ^(-1 /(2 *6 +1))
              ) %>% .[[bw]]
        Dens <- density(vec, na.rm = T, bw = Bw, n = 1300)
        if (min(vec) >= 0 && any.(Dens$'x' < 0)) {
            for (i in seq(1, 0.5, by = -0.005)) {
                Dens <- density(vec, na.rm = T, bw = Bw, n = 1300, adjust = i)  # bw <- adjust *bw
                if (all(Dens$'x' >= 0)) break
            }
            if (any.(Dens$'x' < 0)) {
                for (i in seq(1, 0.5, by = -0.005)) {  # Sometimes warning; 'Auto-range choice cut-off at 0'
                    Dens <- skipMess.('logKDE'::logdensity(vec, na.rm = T, bw = Bw, n = 1300, adjust = i))  # bw <- adjust *bw
                    if (all(Dens$'x' >= 0)) break
                }
            }
        }
        return (tibble(x = Dens$'x', y = Dens$'y'))
    }
    dL <- dLformer.(d, natural) %>% {
          if (is.atomic(.[[1]])) {
              map(., ~ kde_xy(.)) %>% {if (cum) map(., ~ tibble(x = .[[1]], y = cumP0.(.))) else .}
          } else {
              stop('Only available for [ID,y] or [y1,y2, ...]', call. = F)
          }}
    plt.(dL, natural, lty, lwd, xlab, ylab, col, Xlims, Ylims, legePos, name)

    ## P-th pecentile
    dL_cum <- map(dL, function(nya) cumP.(nya$y) %>% tibble(x = nya$x, y = .))
    out <- map(dL_cum, function(nya) {
        whichNear.(nya$y, c(0.1, 1, 10, 10, 25, 50, 75, 90, 95, 99, 99.9) /100) %>% nya$x[.]
    }) %>% bind_cols(Xth_Percentile = str_c('X', c(0.1, 1, 10, 10, 25, 50, 75, 90, 95, 99, 99.9)), .)
    print(out)
}  # dens.(iris[4:5], cum = T)  # [ID, y] is OK


## Cumulative ratio plot == (2020-10-01) ================================================
crp. <- function(d, lty = NA, lwd = NA, xlab = '', ylab = '', col = NULL, Xlims = NA, Ylims = c(-0.01, 1.05), legePos = c(0.05, 0.98),
                 name = NULL, px = NULL, py = NULL, ...) {
    if (!exists('pLL_nu_lam')) {
        suppressMessages('devtools'::source_url('https://github.com/Nyu3/psd_R/blob/master/PSD_archive.R?raw=TRUE', keep.source = T, chdir = F))
    }
    quant <- function(vec) {
        vec <- {if (!is.atomic(vec)) vec[[1]] else vec} %>% .[!is.na(.)] %>% sort(.) %>% unique(.)
        dt <- tibble(x = vec, y = cumsum(vec) /sum(vec))

        pLL <- pLL_lam_al_be_ga_de  # Exponentiated generalized extended Gompertz # Generalized Gompertz
        f <- function(x,lam,al,be,ga,de) (1 -(1 -(1 -exp(-lam *de *(exp(x /lam) -1))) ^al) ^be) ^ga  # (1 -exp(al *lam *(1 -exp(x /lam)))) ^be
        mdl <- lazy_call.(x = dt, y = NULL, pLL, f, ext = T, y2 = 1)
        return(tesL = list(dt, mdl$xy, mdl$mdl))
    }
    dL123 <- dLformer.(d) %>% {if (is.atomic(.[[1]])) map(., ~ quant(.)) else stop('Only available for [ID,y] or [y1,y2, ...]', call. = F)}
    dL_raw <- dL123 %>% map(~ .[[1]])
    dL_mdl <- dL123 %>% map(~ .[[2]])
    dL_res <- dL123 %>% map(~ .[[3]])
    for (i in seq_along(dL_res)) {
        cat('< N =', i, '>\n')
        cat('Model := Exponentiated generalized extended Gompertz;\n')
        cat('y = (1 -exp(al *lam *(1 -exp(x /lam)))) ^be\n')
        dL_res[[i]] %>% print(.)
        cat(str_c(str_dup('=', 48), '\n'))
    }
    plt.(c(dL_raw, dL_mdl), PDF = F, xlab = xlab, ylab = ylab, Xlims = Xlims, Ylims = Ylims, add = 0)
    plt.(dL_mdl, PDF = F, type = 0, add = 2, name = 0, col = col, lty = lty, lwd = lwd)
    plt.(dL_raw, PDF = T, type = 1, add = 2, col = col, lty = lty, lwd = lwd, legePos = legePos)
    if (!is.null(px)) {
        out <- map(dL_mdl, function(nya) whichNear.(nya[[1]], px) %>% nya[[., 2]]) #%>% bind_rows(.)
        cat('\n    When px =', px, ',\n')
        print(out)
    }
    if (!is.null(py)) {
        out <- map(dL_mdl, function(nya) whichNear.(nya[[2]], py) %>% nya[[., 1]]) #%>% bind_rows(.)
        cat('\n    When py =', py, ',\n')
        print(out)
    }
}  # crp.(iris[1:2])


## Histograms plot == (2020-07-13) ================================================
hist. <- function(d, naturalOrder = F, binW = 'st', freq = T, xlab = '', ylab = '', col = NULL, Xlims = NA, Ylims = c(0, NA), plot = T, ...) {
    ## Cut data range by Xlims
    ## Make the bin width (if vec is only integer, the ticks are positioned in the center of each bar)
    whatBreak <- function(vec) {
        if (binW %in% c('St', 'st', 'Sturges') || all(abs(vec) <= 1)) {
            return ('Sturges')
        } else if (binW %in% c('Sc', 'sc', 'Scott')) {
            return ('Scott')
        } else if (all(vec %% 1 == 0, na.rm = T) && max(vec, na.rm = T) < 30) {
            return ((min(vec, na.rm = T) -0.5) : (max(vec, na.rm = T) +0.5))  # dL <- floor(runif(100, 0, 30))
        } else {
            return (seq(floor(min(vec, na.rm = T)), ceiling(max(vec, na.rm = T)), by = binW))
        }
    }
    dL <- dLformer.(d, naturalOrder) %>% map(~ .[!is.na(.)])  # list of vectors, not xy.
    ## NOTE: Screening the range you wanna observe        
    if (length(Xlims) > 1 && anyNA(Xlims)) {  # When Xlims = c(0, NA)
        dL <- map(dL, ~ . %>% {if (is.na(Xlims [1])) .[. <= Xlims[2]] else .[. >= Xlims[1]]})
    } else if (length(Xlims) > 1 && !anyNA(Xlims)) {  # When Xlims = c(0, 100)
        dL <- map(dL, ~ .[. >= Xlims[1] & . <= Xlims[2]])
    }

    if (plot == FALSE) {
        for (i in seq_along(dL)) {
            hist_data <- hist(dL[[i]], breaks = whatBreak(dL[[i]]), plot = FALSE)
            if (freq == TRUE) {
                tenta <- hist_data %>% {tibble(x = .$'mids', y = .$'counts')} %>% list(.)
            } else {
                tenta <- hist_data %>% {tibble(x = .$'mids', y = .$'density')} %>% list(.)
            }
            ten <- if (i == 1) tenta else c(ten, tenta)
            if (i == length(dL) && length(dL) == 1) return (ten[[1]])
            if (i == length(dL) && length(dL) > 1) return (ten)
        }
    }

    Xlim2 <- pr.(dL, Xlims, 0.02)
    tenta <- vector()
    for (i in seq_along(dL)) {
        tenta[i] <- dL[[i]] %>% {hist(., breaks = whatBreak(.), plot = F)} %>% {if (freq) .$'counts' else .$'density'} %>% max(.)
        if (i == length(dL)) Ymax <- max.(tenta)
    }
    Ylim2 <- pr.(vec = ifelse(is.na(Ylims[2]), Ymax, Ylims[2]), Ylims, 0.08)
    ylab <- if (ylab == '' && freq) 'Frequency' else if (ylab == '' && !freq) 'Density' else ylab
    par(mgp = c(0, 0.4, 0))
    for (i in seq_along(dL)) {
        hist(dL[[i]], ann = F, axes = F, freq = freq, xlim = Xlim2, ylim = Ylim2, col = colTr.(col[i], 0.80), breaks = whatBreak(dL[[i]]))
        for (i in 1:2) {
            axis(1, at = axisFun.(Xlim2, n = 5)[[i]], labels = (i == 1), tcl = -par('tcl') /i, cex.axis = 1, lend = 'butt', padj = -0.25)
            axis(2, at = axisFun.(Ylim2, n = 6)[[i]], labels = (i == 1), tcl = -par('tcl') /i, cex.axis = 1, lend = 'butt')
        }
        box(bty = 'l')
        mtext(xlab, side = 1, las = 1, cex = 1, family = jL.(xlab), line = par('mar')[1] -1.00)
        mtext(ylab, side = 2, las = 3, cex = 1, family = jL.(ylab), line = par('mar')[2] -yPos.(Ylim2))
    }
    if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
}  # hist.(psd[1], binW = 0.1)


## Pie chart for ratio == (2019-08-20) ================================================
pie. <- function(d, ...) {  # Desirable for character data
    dL <- dLformer.(d)  # Desirable for the data.frame with only ONE column
    pie(table(dL), col = NULL, border = 'grey13', clockwise = T, init.angle = 90, radius = 0.8, cex = 0.9)
    par(new = T)
    pie(table(dL), label = '', col = 'white', border = 'white', radius = 0.5)
    Theta <- seq(- pi, pi, length = 350)
    lines(0.5 *cos(Theta), 0.5 *sin(Theta), col = 'grey13')
    text(0, 0, labels = names(dL), family = jL.(names(dL)))
}  # pie.(iris[5])


## Linear correlation plot == (2020-08-20) ================================================
## NOTE.1  Regression analysis is strictly applicable to cause (x) and effect (y; random variable) on the assumption that x has NO error...
## NOTE.2  Correlation analysis sees BOTH x & y as random variables; thus don't use linear regression and prob. ellipse at the same time...
## Trivia.1  You'll see the cross points on the line and ellipse can draw y-axis parallel lines as tangent
## Trivia.2  The longer axis of the ellipse is corresponding to the axis of principal component analysis
corp. <- function(d, xlab = '', ylab = '', col = 5, legePos = NULL, li = F, el = T, fix = F, ...) {  # xy data
    dt <- list2tibble.(d) %>% mutate_if(~ is.numeric(.), ~ ifelse(. == -Inf | . == Inf, NA, .))  # For normalization to make it
    dt <- pmap(dt, ~ is.na(c(...)) %>% any(.)) %>% unlist(.) %>% `!` %>% dt[., ]  # Delete the row containing one NA at least
    dt <- dt %>% 'dplyr'::filter(rowSums(is.na(.)) == 0)  # Omit the row including any NA
    if (ncol(dt) != 2) stop('Make sure the data only consists 2-xy columns...\n\n', call. = F)
    if (nrow(dt) == 0) stop('No available data...\n\n', call. = F)
    def.(c('x', 'y'), list(dt[[1]], dt[[2]]))

    if (is.character(col)) stop('Don\'t use color name like \'blue\'.  Use numbers 1 to 6, thank you.\n\n', call. = F)
    colpal <- n_cyc.(col, 6) %>% c('Greys', 'Blues', 'Oranges', 'Purples', 'Reds', 'Greens')[.]

    ## 'elliplot'::ellipseplot(iris[c(5, 1)], iris[c(5, 2)])
    mdl0 <- 'robustbase'::lmrob(y ~x -1, na.action = na.exclude, setting = 'KS2014')  # 'robust'::lmRob(y ~ x -1, na.action = na.exclude)
    mdl1 <- 'robustbase'::lmrob(y ~x +1, na.action = na.exclude, setting = 'KS2014')  # 'robust'::lmRob(y ~ x +1, na.action = na.exclude)
    mdlNum <- map.(list(mdl0, mdl1), ~ summary(.) %>% .$'sigma') %>% which.min(.)
    mdl <- list(mdl0, mdl1)[[mdlNum]]  # Choose better
    Coef <- list(c(0, coef(mdl0)), coef(mdl1))[[mdlNum]] %>% set_names(NULL)
    ## Cor <- 'robust'::covRob(dt, corr = T)$'cov'[1, 2]  # No robust, including outliers; cor.test(x, y, method = 'pearson')$'estimate'
    ## Cnt <- 'robust'::covRob(dt, corr = T)$'center'  # If Coef[2] ~ +/-0.01 and shows strong Cor, don't care because it's 1to1 relationship
    ## Cor <- if (nrow(dt) > 13) 'robustbase'::covMcd(dt, cor = T)$'cor'[1, 2] else cor(dt)[1, 2]  # covMcd results are different for small data
    Cor <- cor.test(dt[[1]], dt[[2]], method = 'spearman', exact = F)$estimate
    Cnt <- 'robustbase'::covMcd(dt, cor = T)$'center'  # c(mean.(x), Coef[1] +Coef[2] *mean.(x))

    ## Legend position 1, 2, 3, 4 as quadrant
    text_pos <- function(...) {
        xyPos <- {Cnt -c(mean(par('usr')[1:2]), mean(par('usr')[3:4]))} %>% {. >= c(0, 0)}
        def.(c('xpos', 'ypos', 'yScMin', 'yScMax'), list(xyPos[1], xyPos[2], par('usr')[3], par('usr')[4]))
        cnt_pos <- if (xpos && ypos) 1 else if (!xpos && ypos) 2 else if (!xpos && !ypos) 3 else if (xpos && !ypos) 4
        out <- list(
            if (Coef[2] >= 0 && Coef[1] < yScMin) 2 else if (Coef[2] >= 0 && Coef[1] >= yScMin) 4 else if (Coef[2] < 0) 3,
            if (Coef[2] >= 0) 4 else if (Coef[2] < 0 && Coef[1] < yScMax) 1 else if (Coef[2] < 0 && Coef[1] >= yScMax) 3,
            if (Coef[2] >= 0 && Coef[1] < yScMin) 2 else if (Coef[2] >= 0 && Coef[1] >= yScMin) 4 else if (Coef[2] < 0) 1,
            if (Coef[2] >= 0) 2 else if (Coef[2] < 0 && Coef[1] < yScMax) 1 else if (Coef[2] < 0 && Coef[1] >= yScMax) 3
        )[[cnt_pos]]
        return (out)
    }
    ## Legend equation
    text3 <- function(text_pos) {
        Show <- function(x) ifelse(any.(Cnt > 10), 1, 2) %>% {sprintf(str_c('%.', ., 'f'), x)}
        Text1 <- bquote('(' *bar(italic(x)) *',' ~bar(italic(y)) *')' == '(' *.(Show(mean.(x))) *','  ~.(Show(mean.(y))) *')')
        Text2 <- Coef[1] %>% {c(. > 0, . < 0, . == 0)} %>% which(.) %>% list(
            bquote(hat(italic(y)) == .(sprintf('%.2f', Coef[2])) *italic(x) + .(sprintf('%.2f', Coef[1])) *phantom(')')),
            bquote(hat(italic(y)) == .(sprintf('%.2f', Coef[2])) *italic(x) ~ .(sprintf('%.2f', Coef[1])) *phantom(')')),
            bquote(hat(italic(y)) == .(sprintf('%.2f', Coef[2])) *italic(x) *phantom(')'))
        )[[.]]
        Text3 <- bquote(italic(R)[adj] ^2 == .(sprintf('%.2f', summary(mdl)$'adj.r.squared')))
        State <- function(x) if (abs(x) < 0.25) 'none' else if (abs(x) < 0.50) 'weak' else if (abs(x) < 0.75) 'moderate' else 'strong'
        Text4 <- bquote(italic(r)[Spearman] == .(sprintf('%.2f', Cor)) ~ (.(State(Cor))))  # No 'Perfect' due to robust cut
        Text5 <- if (cor.test(x, y, method = 'spearman', exact = F)$'p.value' > 0.004) bquote('p-value' == .(sprintf('%.2f', cor.test(x, y)$'p.value'))) else bquote('p-value ~ 0.00')
        ## Set proper text postion
        lege4 <- list(c(0.98, 1.00), c(0.05, 1.00), c(0.05, 0.35), c(0.98, 0.35))
        if (is.null(legePos)) legePos <- lege4[[text_pos]]
        ## Note: no co-existence
        text_num <- if (li && el) 1:3 else if (li && !el) 1:3 else if (!li && el) c(1, 4, 5) else if (!li && !el) 1
        for (i in seq_along(text_num)) {
            text(legeX.(legePos[1]), legeY.(legePos[2] -0.08 *i), adj = ifelse(legePos[1] < 0.5, 0, 1), cex = 0.8, col = '#22222295',
                 label = list(Text1, Text2, Text3, Text4, Text5)[[text_num[i]]],
                 family = ifelse(Sys.getenv('OS') == '', 'CenturySchoolbook', 'Times New Roman'))
        }
    }
    ## http://friendly.github.io/heplots/reference/covEllipses.html
    ## 'heplots'::covEllipses(dt, col=colTr.('grey35', 0.8), lwd=1, level=0.95, labels='', center.pch='', method='mcd', add=T)  # 'mve'
    draw_ellipse <- function(...) {  # Minimum Covariance Determinant (MCD)
        elli95 <- {if (nrow(dt) > 13) 'robustbase'::covMcd(dt)$'cov' else cov(dt)} %>%
                  'ellipse'::ellipse(., centre = Cnt, level = 0.95, npoints = 200) %>% as_tibble(.)
        lines(elli95, col = colTr. ('black', 0.35), lwd = 1.0)
    }
    ## Drawing
    if (between(min.(x) /min.(y), 0.9, 1.1) & between(max.(x) /max.(y), 0.9, 1.1) ) {  # When x & y differ slightly; keep the same scale
        def.(c('Xlim2', 'Ylim2'), list(pr.(c(x, y), NA, 0.13), pr.(c(x, y), NA, 0.13)))
    } else {  # When x & y differ largely; change proper scale to see easily
        def.(c('Xlim2', 'Ylim2'), list(pr.(x, NA, 0.13), pr.(y, NA, 0.13)))
    }
    if (fix == TRUE) {  # Wnen the same scale on both x and y-axis is desired
        def.(c('Xlim2', 'Ylim2'), list(range(Xlim2, Ylim2), range(Xlim2, Ylim2)))
    }
    par(mgp = c(0, 0.4, 0))
    plot.new()
    plot.window(xlim = Xlim2, ylim = Ylim2)
    Colcol <- if (nrow(dt) >= 20) {
        densCols(x, y, colramp = colorRampPalette(c('grey90', 'RColorBrewer'::brewer.pal(9, colpal))))
    } else {
        str_sub(colpal, start = -1, end = -1) <- ''  # Remove the last 's' like 'Greys' --> 'Grey'
        colpal %>% tolower(.) %>% colTr.(., 0.35)  # Sigle coloring for small data
    }
    points(x, y, lwd = 0.95,  pch = 19, cex = 1.3, col = Colcol)
    if (li == TRUE) abline(mdl, col = '#22222221', lwd = 3.5)  # KS2014 line
    if (li == FALSE && el == TRUE) draw_ellipse()  # Modified ecllipse
    for (i in 1:2) {
        axis(side = 1, at = axisFun.(Xlim2, n = 5)[[i]], labels = (i == 1), tcl = -par('tcl') /i, cex.axis = 1, lend = 'butt', padj = -0.2)
        axis(side = 2, at = axisFun.(Ylim2, n = 5)[[i]], labels = (i == 1), tcl = -par('tcl') /i, cex.axis = 1, lend = 'butt')
    }
    box(bty = 'l')
    mtext(xlab, side = 1, las = 1, cex = 1, family = jL.(xlab), line = par('mar')[1] -1.00)
    mtext(ylab, side = 2, las = 3, cex = 1, family = jL.(ylab), line = par('mar')[2] -yPos.(Ylim2))
    textP <- text_pos()
    text3(textP)
    if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
}  # corp.(iris[3:4])


## Boxplot oriented for quantile limit and full/half box == (2020-09-17) ================================================
boxplot2. <- function(dL, type, jit, val, wid, Ylims, col, name, xlab, ylab, rot, cex, cut, ...) {
    if (cut == TRUE) {
        for (i in seq_along(dL)) {
            vec <- dL[[i]]
            outs <- quantile(vec, probs = c(0.25, 0.75), na.rm = T) +c(-1, 1) *IQR(vec, na.rm = T) *3.0  # Last term is the cut-off criteria
            vec[which(vec < outs[1] | vec > outs[2])] <- NA  # Delete too large or small outliers
            dL[[i]] <- vec
        }
    }
    ## Scale of half or full boxplot
    if (type == 'full' || type == 'f') def.(c('AT', 'jitW', 'leftW', 'rightW'), list(0, wid *0.6, wid, wid))
    if (type != 'full' && type != 'f') def.(c('AT', 'jitW', 'leftW', 'rightW'), list(wid /2, wid /2, wid, 0))
    xPos <- 2 *seq(length(dL)) -1  # NA is already omitted
    CX <- length.(dL) %>% max.(.) %>% whichSize. (ref = ., vec = c(100, 13, 4), c(0.3, 0.7, 0.8))
    ## fivenum() is agreed with quantile() if vec is odd, but if even, fivenum() is a bit wider than quantile()
    ## Moreover, some cases make wrong whiskers that have no points more or less than 95th or 5th by quantile()
    c1 <- map.(dL, function(x) {fivenum(x)[2] -1.5 *IQR(x, na.rm = T)} %>% c(., min.(x)) %>% max.(.))
    c2 <- map.(dL, ~ fivenum(.)[2])
    c3 <- map.(dL, ~ fivenum(.)[3])
    c4 <- map.(dL, ~ fivenum(.)[4])
    c5 <- map.(dL, function(x) {fivenum(x)[4] +1.5 *IQR(x, na.rm = T)} %>% c(., max.(x)) %>% min.(.))
    ## for loop is needed because outlier isn't always just one ...
    points_outliers <- function(...) {
        stripC <- function(vec, pos, pch, bg, col) {
            stripchart(vec, at = pos +AT, vertical = T, method = 'jitter', jitter = jitW, add = T, lwd = 0.3, pch = pch, bg = bg, cex = CX,
                       col = if_else(col == '#FFFFFF00', 'grey13', col))
        }
        for (i in seq_along(dL)) {
            vec <- dL[[i]]
            Yin <- vec[vec >= c1[i] & vec <= c5[i]]
            Yout <- vec[! vec %in% Yin]
            if (jit == TRUE) {
                stripC(Yin, xPos[i], 21, bg = colTr.(col[i], 0.55), col = col[i])
                stripC(Yout, xPos[i], 4, bg = colTr.('grey13', 0.8), col = col[i])
            } else {
                points(rep(xPos[i], length(Yout)), Yout, lwd = 0.3, cex = CX)
            }
        }
    }
    ## Scale of half or full boxplot
    box_whiskers <- function(...) {
        rect(xPos -leftW, c2, xPos +rightW, c4, border = 0, col = colTr.(col, 0.55))
        segments(xPos -leftW, c3, xPos +rightW, c3, col = colTr.('grey13', 0.95), lwd = 3, lend = 'butt')
        rect(xPos -leftW, c2, xPos +rightW, c4, border = colTr.('grey13', 0.95))
        segments(xPos -leftW /2, c1, xPos +rightW/2, c1, col = colTr.('grey13', 0.95))
        segments(xPos -leftW/2, c5, xPos +rightW/2, c5, col = colTr.('grey13', 0.95))
        segments(xPos, c1, xPos, c2, lty = 'dashed', col = colTr.('grey13', 0.95))
        segments(xPos, c4, xPos, c5, lty = 'dashed', col = colTr.('grey13', 0.95))
    }
    ## Show values
    textFun <- function(...) {
        Digit <- unlist(dL) %>% delta.(.) %>% {if (. < 1 && all(median.(dL) < 1)) 3 else whichSize.(ref = ., vec = c(50, 5, 1), c(0, 1, 2))}
        tCex <- whichSize.(ref = length(dL), vec = c(4, 13, 30), c(0.6, 0.5, 0.4))
        dD  <-  map(dL, quantile, probs = c(0, 0.5, 1), na.rm = T) %>% data.frame(.)
        for (i in seq_along (dD)) {
            if (abs(round(dD[1, i] -dD[2, i], Digit +1)) < 0.7 *10 ^(-Digit)) dD[1, i] <- NA
            if (abs(round(dD[3, i] -dD[2, i], Digit +1)) < 0.7 *10 ^(-Digit)) dD[3, i] <- NA
        }
        # haloText.(xPos +AT, dD[2, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[2, ]), cex = tCex *1.5)  # Too slow ...
        text(xPos +AT, dD[2, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[2, ]), col = 'white', cex = tCex *1.5 *1.05)  # Alternative
        text(xPos +AT, dD[2, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[2, ]), col = 'grey13', cex = tCex *1.5 *0.95)
        text(xPos +AT, dD[1, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[1, ]), col = 'grey70', cex = tCex, adj = c(0.5, 1.8))
        text(xPos +AT, dD[3, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[3, ]), col = 'grey70', cex = tCex, adj = c(0.5, -1.0))
    }
    ## base plot
    par(mgp = c(0, 0.4, 0))
    if (length(dL) > 30) par(mar = c(4, 3.3, 0.1, 1.0))
    Xlim2 <- c(-1, 2 *length(dL) +1) +wid *c(1, -1)
    Ylim2 <- pr.(dL, Ylims, 0.07)  # NOTE: text of Max or Min is not shown by 0.05
    plot.new()
    plot.window(xlim = Xlim2, ylim = Ylim2)
    abline(v = par('usr')[1], h = par('usr')[3], col = 'grey13', lwd = 2 *par('lwd'))  # needed twice normal lwd
    axis(1, at = xPos, labels = F, lwd.ticks = par('lwd'), tcl = -par('tcl'), cex.axis = 1, lend = 'butt')
    walk(1:2, ~ axis(2, at =axisFun.(Ylim2, n=6)[[.]], labels =(.==1), lwd.ticks =par('lwd'), tcl =-par('tcl')/., cex.axis =1, lend ='butt'))
    mtext(xlab, side = 1, las = 1, cex = whichSize.(ref = nchar(xlab), vec = c(15, 35, 50), c(1, 0.8, 0.5)), family = jL.(xlab),
          line = par('mar')[1] -1.00)
    mtext(ylab, side = 2, las = 3, cex = whichSize.(ref = nchar(ylab), vec = c(15, 35, 50), c(1, 0.8, 0.5)), family = jL.(ylab),
          line = par('mar')[2] -yPos.(Ylim2))
    ## x-label
    labAdj <- function(name) {
        Line <- map.(str_count(name, '\n'), ~ whichSize.(ref = ., vec = 0:2, c(0.3, 1.3, 1.3))) %>%
                {. *whichSize.(ref = length(dL), vec = c(8, 15, 35, 60, 100), c(1, 1, 1, 1.5, 2))}
        Cex <- map.(str_count(name, '\n'), ~ whichSize.(ref = ., vec = 0:2, c(0.90, 0.85, 0.6))) %>%
               {. *whichSize. (ref = length(dL), vec = c(8, 15, 35, 60), c(1, 0.8, 0.6, 0.37))}
        return (list(line = Line, cex = cex %||% Cex))
    }
    name <- {name %||% names(dL) %||% str_c('#', seq_along(dL))} %>% gsub('\\n', '\n', ., fixed = T) %>% correctChr.()
    if (length(name) < length(xPos)) name <- c(name, rep(NA_character_, times = length(xPos) -length(name)))
    if (rot == 0) {
        mtext(name, at = xPos, side = 1, las = 1, cex = labAdj(name)$'cex', family = jL.(name), line = labAdj(name)$'line')
    } else {
        yPos <- par('usr')[3] -0.035 *delta.(par('usr')[3:4]) *whichSize.(ref = length(dL), vec = c(8, 15, 35, 60), c(0.9, 0.8, 0.7, 0.9))
        nameLen <- 'stringi'::stri_numbytes(name) %>% max.(.)  # Count including multi bytes char and space
        rot_cex <- whichSize.(ref = nameLen, vec = c(5, 10, 15), c(0.8, 0.7, 0.6)) %>%
                   {. *whichSize.(ref = length(dL), vec = c(8, 15, 35, 60, 100), c(0.9, 0.8, 0.7, 0.6, 0.4))}
        text(xPos, yPos, name, srt = rot,  xpd = T, adj = c(1, 1), cex = cex %||% rot_cex, family = jL.(name))
    }
    ## boxplot
    points_outliers()
    box_whiskers()
    if (val == TRUE) textFun()
}

box2. <- function(d, type = 'half', jit = T, val = T, natural = F, wid = 0.75, Ylims = NA, col = 0, name = NULL, xlab = '', ylab = '',
                  rot = 0, cex = NULL, cut = F, sel = NULL, med_order = F, name_marking = NULL, col_marking = NULL, PDF = T, ...) {
    dL <- dLformer.(d, natural)  # you can control an order something like  'OK' 'NG'  by sel = ~
    if (!is.null(sel)) dL <- n_cyc.(sel) %>% dL[.]  # try  as.list(iris)[0: 100] %>% dLformer.(.)
    if (med_order == T) dL <- median.(dL) %>% order(., decreasing = T) %>% dL[.]  # Show the graph like Pareto chart
    col <- colors.(col, d = dL)
    if (!is.null(name_marking)) {  # name_marking = list(c('Log-Normal', 'Nukiyama-Tanasawa', 'Three-parameter Log-Hyperbolic', 'Weibull'))
        for (i in seq_along(col_marking)) col[which(names(dL) %in% name_marking[[i]])] <- col_marking[[i]]  # col_marking = list('grey88')
    }
    ## Signle or multiple boxplot
    if (!'list' %in% class(dL[[1]])) {  # type: [ID, y] or [y1, y2, y3, ...]
        if (map_lgl(dL, ~ is.numeric(.)) %>% any(.) %>% `!`) stop('The data does NOT any numeric data...\n\n', call. = F)
        boxplot2.(dL, type, jit, val, wid, Ylims, col, name, xlab, ylab, rot, cex, cut)
    } else {  # type: [ID, y1, y2, y3, ...]
        for (i in seq_along(dL)) boxplot2.(dL[[i]], type, jit, val, wid, Ylims, col, name, xlab, ylab = names(dL[i]), rot, cut)
    }
    if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
}  # box2.(iris[-5], col = 1:4, rot = 22, cut = T)  box2.(iris)


## Bar plot == (2020-09-16) ================================================
barp. <- function(d, wid = 0.5, spacer = 0.5, cum = F, xyChange = F, digit = NULL, elementChange = F, xlab = '', ylab = '', Ylims = c(0, NA),
                  color = NULL, legePos = NULL, name = NULL, cex = NULL, rot = 0, ...) {
    d <- list2tibble.(d)
    d2 <- if (map_lgl(d, is.numeric) %>% all()) {  # [y1, y2, ...]
        if (is.atomic(d)) {
            d %>% set_names(if (!is.null(names(d))) names(d) else str_c('n', seq_along(d))) %>% t() %>% as_tibble()
        } else {
            d %>% as_tibble()
        }
    } else if (map_lgl(d, ~ is.character(.) | is.factor(.)) %>% sum() == 1) {  # [ID, y1, y2]
        d %>% group_by_if(~ is.numeric(.) %>% `!`)
    } else {
        stop('The data has more than TWO character columns; delete them to 0 or 1...\n\n', call. = F)
    }
    mat_avg <- summarise_all(d2, mean.) %>% select_if(is.numeric) %>% as.matrix()
    rownames(mat_avg) <- summarise_all(d2, mean.) %>% .[[1]]
    erH <- summarise_all(d2, function(x) {
        mean.(x) +sd.(x) /sqrt(length.(x)) *ifelse(length.(x) < 3, 2.8, qt(0.05 /2, length.(x) -1, lower.tail = F))
    }) %>% select_if(is.numeric)
    erL <- summarise_all(d2, function(x) {
        mean.(x) -sd.(x) /sqrt(length.(x)) *ifelse(length.(x) < 3, 2.8, qt(0.05 /2, length.(x) -1, lower.tail = F))
    }) %>% select_if(is.numeric)

    if (xyChange) {
        mat_avg <- mat_avg %>% {if (!cum) {
            matrix(rev(.), nrow(.), ncol(.), dimnames = list(rownames(.) %>% rev(), colnames(.) %>% rev()))
        } else {
            {.[, ncol(.):1]}
        }}
        erH <- erH %>% unlist() %>% rev()
        erL <- erL %>% unlist() %>% rev()
    } else if (elementChange) {
        mat_avg <- t(mat_avg)
        erH <- t(erH) %>% unlist()
        erL <- t(erL) %>% unlist()
    } else {
        erH <- erH %>% unlist()
        erL <- erL %>% unlist()
    }

    ## Base plot
    col2 <- {if (is.null(color)) colors.() else color} %>% colors.(., d = if (nrow(mat_avg) == 1) 1:ncol(mat_avg) else 1:nrow(mat_avg)) %>%
            colTr.(., tr = 0.6)
    pos <- barplot(mat_avg, beside = !cum, horiz = xyChange, plot = F,
                   space = if (!cum) NULL else spacer,
                   width = if (!cum) c(rep(wid, nrow(mat_avg)), spacer) else 1
           ) %>% as.vector()
    pos_range <- {range(pos) +0.5 *c(-1, 1)} %>% delta.() %>% {. *0.05}  # Spread out by 5%
    pos_lim <- {range(pos) +0.5 *c(-1, 1)} %>% {if (!xyChange) c(.[1] -pos_range, .[2]) else c(.[1], .[2] +pos_range)}
    bar_lim <- {if (!cum) mat_avg else apply(mat_avg, 2, sum.)} %>% range.() %>% {if (.[1] >= 0) c(0, .[2]) else .} %>% pr.(., Ylims, 0.13)
    par(mar = if (!xyChange) c(2.4, 4, 0.5, 1) else c(0.5, 4, 2.4, 1), mgp = if (!xyChange) c(0, 0.4, 0) else c(0, 0.2, 0))
    plot.new()
    plot.window(xlim = if(!xyChange) pos_lim else bar_lim, ylim = if(!xyChange) bar_lim else pos_lim)
    barplot(mat_avg, beside = !cum, horiz = xyChange, axes = F, axisnames = F, add = T, col = col2,
            space = if (!cum) NULL else spacer,
            width = if (!cum) c(rep(wid, nrow(mat_avg)), spacer) else 1
    )

    ## Error bar
    if (!cum) {
        erW <- wid /4
        ord <- if (!xyChange) 1:4 else c(2, 1, 4, 3)
        walk(list(list(pos, erH, pos, erL)[ord], list(pos -erW, erH, pos +erW, erH)[ord], list(pos -erW, erL, pos +erW, erL)[ord]),
             ~ segments(.[[1]], .[[2]], .[[3]], .[[4]], col = colTr.('grey13', 0.95)))
    }

    ## Show values
    if (is.null(digit)) {
        digit <- delta.(unlist(mat_avg)) %>% {if (all(unlist(mat_avg) < 1)) 3 else whichSize.(ref = ., vec = c(50, 5, 1), c(0, 1, 2))}
    }
    if (!cum) {  # multi bar
        textX <- if (!xyChange) pos else erH +diff(par('usr')[1:2]) *0.04
        textY <- if (!xyChange) erH +diff(par('usr')[3:4]) *0.04 else pos
    } else {  # cumulative bar
        tenta <- as_tibble(mat_avg)  # very bothersome...
        for (i in seq(nrow(tenta))) tenta[i, ] <- as_tibble(mat_avg)[i, ] /2 +if (i == 1) 0 else sum.(as_tibble(mat_avg)[1:(i-1), ])
        textX <- if (!xyChange) rep(pos, each = nrow(mat_avg)) else unlist(tenta)
        textY <- if (!xyChange) unlist(tenta) else rep(pos, each = nrow(mat_avg))
    }
    textL <- sprintf(str_c('%.', digit, 'f'), as.vector(mat_avg))
    text(textX, textY, labels = textL, col = 'grey13', cex = 0.7)

    ## Axis
    {if (!xyChange) list(NULL, par('usr')[1]) else list(par('usr')[4], NULL)} %>%
    {abline(h = .[[1]], v = .[[2]], col = 'grey13', lwd = 2 *par('lwd'))}
    walk(1:2, ~ axis(ifelse(!xyChange,2,3), at=axisFun.(bar_lim, n=6)[[.]], labels=(.==1), lwd.ticks=par('lwd'), tcl=-par('tcl')/.))
    mtext(ylab, side=ifelse(!xyChange,2,3), las=ifelse(!xyChange,3,1), cex=whichSize.(ref=nchar(ylab), vec=c(15, 35, 50), c(1, 0.8, 0.5)),
          family = jL.(ylab), line = ifelse(!xyChange, par('mar')[2] -yPos.(bar_lim), par('mar')[4] +0.30))

    ## Label names
    xPos <- if (!cum) matrix(pos, nrow = nrow(mat_avg)) %>% apply(2, mean.) else pos
    yPos <- whichSize.(ref = length(pos), vec = c(8, 15, 35, 60), c(0.9, 0.8, 0.7, 0.9)) %>%
            {. *par('usr')[ifelse(!xyChange, 3, 1)] -ifelse(!xyChange, 0.035, 0.015) *delta.(par('usr')[if(!xyChange) 3:4 else 1:2])}

    if (is.null(name)) name <- colnames(mat_avg)# %>% {if (!xyChange) . else rev(.)}
    nameLen <- 'stringi'::stri_numbytes(name) %>% max.(.)  # Count including multi bytes char and space
    rot_cex <- whichSize.(ref = nameLen, vec = c(5, 10, 15), c(0.8, 0.7, 0.6)) %>%
               {. *whichSize.(ref = length((xPos)), vec = c(3, 8, 15, 35, 60, 100), c(1, 0.9, 0.8, 0.7, 0.6, 0.4))}
    adj <- if (!xyChange) {if (rot == 0) NULL else c(1, 1)} else 1
    if (xyChange) def.(c('xPos', 'yPos'), list(yPos, xPos))
    text(xPos, yPos, name, srt = rot,  xpd = T, adj = adj, cex = ifelse(is.null(cex), rot_cex, cex),
         family = jL.(name))

    ## Legend
    if (nrow(mat_avg) > 1) {
        leges <- legePos %||% {if (!xyChange) c(0.75, 0.99) else c(0.7, 0.3)}
        legend2.(rownames(mat_avg), legePos = leges, cex = 0.65, col = col2)
    }

}  # barp.(iris)  barp.(iris,cum=T)  barp.(iris,xyChange=T,rot=25)  barp.(iris,cum=T,xyChange=T)  barp.(iris, elementChange=T,cex=.9)
   # barp.(iris[-5],spacer=-0.1)


## Scatter plot marix (Non-parametric evaluation) == (2020-08-20) ========================
sp. <- function(d, col = NULL, xlab = '', ylab = '', cut = F, conv = T, ...) {  # (conv = T) means normalization of all data
    dt <- list2tibble.(d) %>% mutate_if(~ is.numeric(.), ~ ifelse(. == -Inf | . == Inf, NA, .))  # For normalization to make it
    dt <- pmap(dt, ~ is.na(c(...)) %>% any(.)) %>% unlist(.) %>% `!` %>% dt[., ]  # Delete the row containing one NA at least
    if (nrow(dt) == 0) return (cat('\n    No available data...\n'))
    if (cut == TRUE) {
        dt <- mutate_if(dt, ~ is.numeric(.), function(vec) {
            outs <- quantile(vec, probs = c(0.25, 0.75), na.rm = T) +c(-1, 1) *IQR(vec, na.rm = T) *3.0  # Last term is the cut-off criteria
            vec[which(vec < outs[1] | vec > outs[2])] <- NA  # Delete too large or small outliers
            return (vec)
        })
    }
    okngCol_TF <- dt %>% select_if(~ is.character(.)) %>% map_lgl(., ~ str_detect(., pattern = 'OK|NG') %>% any.(.))
    resultCol <- which(okngCol_TF) %>% names(.) %>% str_subset(., pattern = '結果|Result|result') %>% .[1]  # If failed, return NA
    dt <- dt %>% select_if(~ n_distinct(.) > 1)  # sd.(.) > 0; sd = 0 causes error; delete somthing like machine number
    if (!is.na(resultCol)) {
        Results <- map.(dt[[resultCol]], function(x) if (x %in% 'OK') 1 else if (x %in% 'NG') 0 else NA)
        dt <- tibble (Results) %>% bind_cols(., dt)
    }
    if (conv) dt <- mutate_if(dt, ~ is.numeric(.), ~ scale.(.))
    if (is.character(col)) stop('The argument \"col\" must be numeric...\n\n', call. = F)
    cols <- list(c('grey88', 'grey55', 'grey35'), c('darkorange1', 'darkorange4', 'darkorange'),
                 c('springgreen1', 'springgreen4', 'seagreen3'), c('dodgerblue1', 'dodgerblue4', 'skyblue3'),
                 c('tomato1', 'tomato4', 'red1')) %>%
                 {if (is.null(col)) c(0, 0, 0) else .[[n_cyc.(col, 5)]]}
    par(family = jL.(names(dt)))
    'psych'::pairs.panels(dt, smooth = T, scale = T, density = T, ellipses = T, stars = T, lm = T, method = 'spearman', pch = 21, gap = 0.3,
                          cex.cor = 1.3, cex.labels = 0.8, col = cols[1], bg = colTr.(cols[2], 0.5), hist.col = colTr.(cols[3], 0.8),
                          cex.axis = ifelse(par('family') == 'Avenir Next', 1, 0.95)
    )
    mtext(xlab, side = 1, las = 1, cex = 1, family = jL.(xlab), outer = T, line = par('mar')[1] -1.00)
    mtext(ylab, side = 2, las = 3, cex = 1, family = jL.(ylab), outer = T, line = par('mar')[2] -0.88)
    par(family = ifelse(Sys.getenv('OS') == '', 'Avenir Next', 'sans'))
    if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
    corMat.(dt)  # Research of several types of correlations
    cat('\n    Data number = ', nrow(dt), '\n\n')
}  # sp.(iris, col = 3); save.('nya', WH = c (4.3, 3.3) *1.8)


## Research of several types of correlations == (2020-08-21) ========================
corMat. <- function(d, ...) {
    ## https://nigimitama.hatenablog.jp/entry/2018/03/02/165421
    ## Not use a data like mtcars but use [y1, y2, ...]
    dt <- list2tibble.(d) %>% select_if(~ is.numeric(.))
    ## Delete the row containing one NA, Inf, or -Inf at least
    dt <- dt %>% map_df(~ {!is.na(.) & . < Inf & . > -Inf}) %>% {rowSums(.) == ncol(dt)} %>% dt[., ]

    comb_mat <- combn(ncol(dt), 2)
    for(i in seq(ncol(comb_mat))) {
        tenta <- comb_mat[, i] %>% dt[.]
        dt_pile <- tibble(
            comb = names(tenta) %>% str_flatten(., '_vs_'),
            Pearson = cor.test(tenta[[1]], tenta[[2]], method = 'pearson', exact = F)$estimate,  # Linear (parameteric)
            Spearman = cor.test(tenta[[1]], tenta[[2]], method = 'spearman', exact = F)$estimate,  # Monotonic (non-parameteric)
            MIC = 'minerva'::mine(tenta[[1]], tenta[[2]])$'MIC',  # Non-linear (Maximum Information Coefficient)
            TIC = 'minerva'::mine(tenta[[1]], tenta[[2]], normalization = T)$'TIC'  # (Total Information Coefficient)
        )
        dt_cor <- if (i == 1) dt_pile else bind_rows(dt_cor, dt_pile)
    }
    print(dt_cor)
   # write.(dt_cor)
}  # corMat.(iris)


## matplot2 == (2020-02-08) ================================================
mat2. <- function(dt, Xlims = NA, Ylims = NA, xlab = '', ylab = '', ...) {  # matplot cannot draw a missing line across NA part
    ## dt := [x, y1, y2, ...]
    cols <- c('grey13', 'springgreen3', 'tomato2', 'dodgerblue3', 'maroon3', rep('grey13', 10))
    def.(c('x', 'dty', 'Xlims', 'Ylims'), list(dt[[1]], dt[, -1], range.(dt[[1]]), unlist(dt[, -1]) %>% range.(.)))
    Xlim2 <- pr.(vec = x, Xlims, 0.02)
    Ylim2 <- pr.(vec = dty, Ylims, 0.12)
    plot.new()
    plot.window(xlim = Xlim2, ylim = Ylim2)
    for (i in seq_along(dty)) data.frame(x, dty[[i]]) %>% na.omit(.) %>% lines(., col = cols[i])
    for (i in seq_along(dty)) data.frame(x, dty[[i]]) %>% na.omit(.) %>% points(., pch = 21, col = 'grey13', bg = colTr.(cols[i], 0.75))
    for (i in 1:2) {
        axis(side = 1, at = axisFun.(Xlim2, n = 6)[[i]], labels = (i == 1), tcl = par('tcl') /i, lend = 'butt', padj = -0.2)
        axis(side = 2, at = axisFun.(Ylim2, n = 6)[[i]], labels = (i == 1), tcl = par('tcl') /i, lend = 'butt')
    }
    box()
    mtext(xlab, side = 1, las = 1, cex = 1, family = jL.(xlab), line = par('mar')[1] -1.00)
    mtext(ylab, side = 2, las = 3, cex = 1, family = jL.(ylab), line = par('mar')[2] -yPos.(Ylims))
    if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
}  # mat2.(iris[-5])


## Inf & NaN -> zero == (2020-06-21) ========================
clean0. <- function(y, ...) y %>% {case_when(. == Inf ~ 0, . == -Inf ~ 0, is.na(.) ~ 0, TRUE ~ .)}
clean1. <- function(dt, ...) {
    dt <- dt %>% rowid_to_column('iD')
    clean_row <- dt %>% select_if(~ is.numeric(.)) %>% filter(rowSums(is.na(.)) == 0, rowSums(.) > -Inf, rowSums(.) < Inf) %>% .[['iD']]
    return (dt[clean_row, ] %>% 'dplyr'::select(!'iD'))
}

## Multiple definition == (2019-01-10) ================================================
def. <- function(defnames, values) for (i in seq_along(defnames)) assign(defnames[i], values[[i]], envir = parent.frame())
# def.(c('cat', 'run'), list(35, 1:23))

## map() returns vectors == (2019-01-09) ================================================
map. <- function(.x, .f, ... ) 'purrr'::map(.x, .f, ... ) %>% unlist(., use.names = F)

## Signature function for math treatment == (2020-01-05) ================================================
sgn. <- function(x) case_when(x > 0 ~ 1, x == 0 ~ 0, x < 0 ~ -1)

## Rescaling (min-max normalization) for math treatment == (2020-01-05) ================================================
rescaling. <- function(x) (x -min.(x)) / (max.(x) -min.(x))

## Short cut to kill bothersome etc. == (2020-09-17) ================================================
pmax. <- function(x) pmax(x, na.rm = T)
pmin. <- function(x) pmin(x, na.rm = T)
scale. <- function(x, mM = NULL) {
    scale(x, center = min.(x), scale = delta.(x)) %>%  # Normalization for 0-1;  Y = (y -y_min) /range
    {if (is.null(mM)) . else . *delta.(mM) +mM[1]}  #  Normalization for 0-1;  Y = (y -y_min) /range *(M -m) +m
}
any. <- function(x) as.logical(x) %>% any(., na.rm = T)
ymd. <- function(x) if (is.POSIXct (x)) floor_date(x, 'day') %>% as.character() %>% gsub(' JST', '', .) else x
range0. <- function(x) if (is.atomic(x) && is.numeric (x)) range(x, na.rm = T, finite = F) else NA
range. <- function(x) if (is.atomic(x)) range0.(x) else list2tibble.(x) %>% select_if(~ is.numeric(.)) %>% unlist() %>% range0.()
median0. <- function(x) if (is.atomic(x) && is.numeric(x) || is_time.(x)) median(x, na.rm = T) %>% ymd.() else NA
median. <- function(x){
    if (is.atomic(x)) {
        median0.(x)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(median0.) 
    } else {
        NA
    }
}
mean0. <- function(x, trim = 0) {
    out <- {if (is.atomic(x) && is.numeric(x) || is_time.(x)) mean(x, na.rm = T, trim) %>% ymd.(.) else NA} %>% ifelse(is.nan(.), NA, .)
    return (out)
}
mean. <- function(x, trim = 0) {  # mean0.(rep(NA, 3))  mean0.(rep(NA_real_, 3))
    if (is.atomic (x)) {
        mean0.(x, trim)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(~ mean0.(., trim))
    } else {
        NA
    }
}
sd0. <- function(x) if (is.atomic(x) && is.numeric(x)) sd(x, na.rm = T) else NA
sd. <- function(x) {
    if (is.atomic(x)) {
        sd0.(x)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(sd0.)
    } else {
        NA
    }
}
sd2. <- function(x) x[!is.na(x)] %>% {median(abs(. -median(.))) /0.6745}  # More robust sd
var0. <- function(x) if (is.atomic(x) && is.numeric(x)) var(x, y = NULL, na.rm = T) else NA
var. <- function(x) {
    if (is.atomic(x)) {
        var0.(x)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(var0.)
    } else {
        NA
    }
}
max0. <- function(x, Nth = 1) if (is.atomic(x) && is.numeric(x) || is_time.(x)) sort(x, decreasing = T) %>% .[Nth] %>% ymd.(.) else NA
max. <- function (x, Nth = 1) {
    if (is.atomic(x)) {
        max0.(x, Nth)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(~ max0.(., Nth))
    } else {
        NA
    }
}
min0. <- function(x, Nth = 1) if (is.atomic(x) && is.numeric(x) || is_time.(x)) sort(x, decreasing = F) %>% .[Nth] %>% ymd.(.) else NA
min. <- function(x, Nth = 1) {
    if (is.atomic(x)) {
        min0.(x, Nth)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(~ min0.(., Nth))
    } else {
        NA
    }
}
max2_base. <- function(x, na) {
    if (! is.atomic(x) || ! is.numeric(x)) return (NA)
    def.(c('whisker', 'Max'),  list(quantile(x, probs = 0.75, na.rm = T) + IQR(x, na.rm = T) * 1.5, max(x, na.rm = T)))
    out <- if (na == T && setequal(whisker, Max)) NA else min(c(whisker, Max), na.rm = T)
    return (out)
}
max2. <- function(x, na = F) {
    if (is.atomic(x)) {
        max2_base.(x, na)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(~ max2_base.(., na))
    } else {
        NA
    }
}
min2_base. <- function(x, na) {
    if (! is.atomic(x) || ! is.numeric(x)) return (NA)
    def.(c('whisker', 'Min'), list(quantile(x, probs = 0.25, na.rm = T) - IQR(x, na.rm = T) * 1.5, min(x, na.rm = T)))
    out <- if (na == T && setequal(whisker, Min)) NA else max(c(whisker, Min), na.rm = T)
    return (out)
}
min2. <- function(x, na = F) {
    if (is.atomic(x)) {
        min2_base.(x, na)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(~ min2_base.(., na))
    } else {
        NA
    }
}
which.max0. <- function(x, Nth = 1:1) if (is.atomic(x) && is.numeric(x) || is_time.(x)) max.(x, Nth) %>% map_dbl(~ which(x == .))
which.max. <- function(x, Nth = 1) {  # which.max0.(c(9, NA, 8:1), 1:2)  which.max0.(c(1,1,1))
    if (is.atomic (x)) {
        which.max0.(x, Nth)
    } else if (is.list(x)) {
        ten_num <- list2tibble.(x) %>% select_if(~is.numeric(.)) %>% which.min()
        ten_time <- list2tibble.(x) %>% select_if(~is_time.(.)) %>% which.min()
        if (length(ten_num) == 0 && length(ten_time) != 0) return (ten_time)
        if (length(ten_num) != 0 && length(ten_time) == 0) return (ten_num)
        if (length(ten_num) != 0 && length(ten_time) != 0) return (bind_cols(ten_num, ten_time))
        if (length(ten_num) == 0 && length(ten_time) == 0) return (1 %>% set_names(names(x)[1]))  # In case of all NA columns
    } else {
        NA
    }
}
which.min0. <- function(x, Nth = 1:1) if (is.atomic(x) && is.numeric(x) || is_time.(x)) min.(x, Nth) %>% map_dbl(~ which(x == .))
which.min. <- function(x, Nth = 1) {  # which.min0. (c (3,-10,5,-88), 1:2)
    if (is.atomic(x)) {
        which.min0.(x, Nth)
    } else if (is.list(x)) {
        ten_num <- list2tibble.(x) %>% select_if(~is.numeric(.)) %>% which.max()
        ten_time <- list2tibble.(x) %>% select_if(~is_time.(.)) %>% which.max()
        if (length(ten_num) == 0 && length(ten_time) != 0) return (ten_time)
        if (length(ten_num) != 0 && length(ten_time) == 0) return (ten_num)
        if (length(ten_num) != 0 && length(ten_time) != 0) return (bind_cols(ten_num, ten_time))
        if (length(ten_num) == 0 && length(ten_time) == 0) return (1 %>% set_names(names(x)[1]))  # In case of all NA columns
    } else {
        NA
    }
}
delta0. <- function(x, unit = 'day') {
    if (is.atomic(x) && is.numeric(x)) {
        range(x, na.rm = T) %>% diff() %>% as.numeric()
    } else if (is.atomic(x) && is_time.(x)) {
        range(x, na.rm = T) %>% diff() %>% time_length(., unit = unit) %>% as.numeric()
    } else {
        NA
    }
}
delta. <- function(x, unit = 'day') {
    if (is.atomic(x)) {
        delta0.(x, unit)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.)) %>% map_df(~ delta0.(., unit))
    } else {
        NA
    }
}
sum0. <- function(x) if (is.atomic(x) && is.numeric(x) || is.logical(x)) x[!is.na(x)] %>% sum() else NA  # Note; sum function eats T/F
sum. <- function(x) {
    if (is.atomic(x)) {
        sum0.(x)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.) | is.logical(.)) %>% map_df(sum0.)
    } else {
        NA
    }
}
length0. <- function(x) if (is.atomic(x)) x[!is.na(x)] %>% length() else NA
length. <- function(x) if (is.atomic(x)) length0.(x) else if (is.list(x)) map_df(x, length0.) else NA  # Forbitten for length.(list(NULL))
n_factor. <- function(x) if (is.atomic(x)) length0.(x) else if (is.data.frame(x)) ncol(x) else if ('list' %in% class(x)) length(x) else NA
skew0. <- function(x) if (is.atomic(x) && is.numeric(x)) x[!is.na(x)] %>% {(. -mean(.)) ^3 /sd(.) ^3} %>% mean()
skew. <- function(x) {
    if (is.atomic(x)) {
        skew0.(x)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.) | is.logical(.)) %>% map_df(skew0.)
    } else {
        NA
    }
}
kurt0. <- function(x) if (is.atomic(x) && is.numeric(x)) x[!is.na(x)] %>% {(. -mean(.)) ^4 /sd(.) ^4} %>% mean()
kurt. <- function(x) {
    if (is.atomic(x)) {
        kurt0.(x)
    } else if (is.list(x)) {
        list2tibble.(x) %>% select_if(~ is.numeric(.) | is_time.(.) | is.logical(.)) %>% map_df(kurt0.)
    } else {
        NA
    }
}


## Numeric conversion of lower and/or upper bounds generated by cut() == (2020-07-13) ==
## [0,0.5], (0.5,1]
## https://stackoverflow.com/questions/32356108/output-a-numeric-value-from-cut-in-r
cut_borders. <- function(x){
    pattern <- '(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])'
    start <- as.numeric(gsub(pattern, '\\2', x))
    end <- as.numeric(gsub(pattern, '\\3', x))
    tibble(start, end)
}  # cut_interval(iris$Sepal.Length, length = 0.5) %>% cut_borders.(.)


## Area of polygon (widely applicable to any polygon or closed curve with x order like convex-hull) == (2019-12-17) ==
area_poly. <- function(x, y, ...) {
    ## Newton-Cotes formulae (to only area surrounding x-axis and a curve): sum(0.5 * diff(x) *(y[-1] +y[-length(y)]))
    if (length(x) != length(y))  stop ('x and y do not have the same length.\n\n', call. = F)
    def.(c('x2', 'y2'), list(c(x[-1], x[1]), c(y[-1], y[1])))  # [x1, ... , xn] --> [x2, ... , xn, x1]
    calc <- abs(sum(x *y2 -x2 *y)) /2  # psd data; polygon type = 1.004, Newton type = 1
    return (calc)
}

## Area closed by curve & x axis (oriented for pdf curve) == (2020-06-05) ========================
area. <- function(x = NULL, y = NULL, ...) {
    if (is.list(x) && is.null(y)) def.(c('x', 'y'), list(x[[1]], x[[2]]))
    sum(0.5 * diff(x) *(y[-1] +y[-length(y)])) %>% return (.)
}

## Partial Area (Only applicable for PDF; whose cuve is surrounding x axis) == (2020-01-23) ========================
area_part. <- function(dt, LRx, ...) {  # dt is PDF, LRx is partial range of x
    rowRange <- whichNear.(dt$'x', c(LRx[1], LRx[2])) %>% {.[1] : .[2]}
    def.(c('x', 'y'), list(dt$'x'[rowRange], dt$'y'[rowRange]))
    calc <- sum(0.5 *diff(x) *(y[-1] +y[-length(y)]))
    return (calc)
}

## Cumulative probablity denstiy == (2020-07-02) ========================
cumP0. <- function(x, y, ...) {
    if (is.data.frame(x) && ncol(x) == 2) def.(c('x', 'y'), list(x[[1]], x[[2]]))
    out <- cumsum(0.5 *diff(x) *(y[-1] +y[-length(y)])) %>% c(0, .)  # Note: cumP0.() is over 0.6%...
    return (out)
}
cumP. <- function(y, ...) cumsum(y) /sum(y)  # plot(cumP0.(psd[[2]], psd[[3]])-cumP.(psd[[3]]),type = 'l')


## AIC calculation == (2019-05-11) ================================================
aic. <- function(model, ...) if (is.null(model) || is.na(model)) NA else model %>% {-2 *logLik(., REML = F)[1] +2 *(length(coef(.)) +1L)}
aic2. <- function(model, ...) {
    if (is.null(model) || is.na(model)) return (NA)
    qy <- fitted(model) %>% .[. > 0]  # Sometimes a model returns minus fitted values partially and then results in NaN or Inf.
    return ( -1 /length(qy) *sum(log(qy)) +(length(coef(model)) +1L) /length(qy))
}


## Other Information Criteria (OIC ?) calculation == (2020-09-17) ================================================
## summary(mdl)$sigma
rmse. <- function(model) if (is.null(model) || is.na(model)) NA else model %>% {sqrt(deviance(.) /(length(fitted(.)) -length(coef(.))))}
## Deviance
dev. <- function(model) if (is.null(model) || is.na(model)) NA else model %>% {log(2 *pi *deviance(.) /length(fitted(.))) +1}
## Estimated relative error
ere. <- function(model) if (is.null(model) || is.na(model)) NA else model %>% {deviance(.) /sum((fitted(.)) ^2)}
## BIC
bic. <- function(model) if (is.null(model) || is.na(model)) NA else model %>% BIC()
## deviance
deviance. <- function(model) if (is.null(model) || is.na(model)) NA else model %>% deviance()


## Return with no error-stop risk? == (2019-01-25) ================================================
tryReturn. <- function(modeling) suppressWarnings(try(modeling, silent = T) %>% {if (class(.) == 'try-error') NA else .})
# tryReturn.(nlsLM(y ~ x, start = ... ))  tryReturn.(date('123456-1-2'))


## Find the interval points on both sides of local plus/minus change in vector == (2019-05-23) ========================
interval2. <- function(vec, valley = T, ... ) {  # True means a "single" local valley
    vecTF <- diff(vec) %>% {if (valley) . >= 0 else . < 0}  # Search for a valley or peak
    if (sum(vecTF) %in% c(0, length(vecTF))) {
        if (valley == T && sum(vecTF) == 0) return (length(vec))  # Find a valley at the end
        if (valley == T && sum(vecTF) == length(vecTF)) return (1)  # Find a valley at the start
        if (valley == F && sum(vecTF) == 0) return (length(vec))  # Find a peak at the start
        if (valley == F && sum(vecTF) == length(vecTF)) return (1)  # Find a peak at the end
    } else {  # 2 Numbers on the change point
        if (valley == T) {
            vecTF %>% diff(.) %>% {which(. == 1)[1]} %>% {c(., . +2)} %>% {if (anyNA(.)) which.min(vec)[1] else .} %>% return (.)
        } else {
            vecTF %>% diff(.) %>% {which(. == 1)[1]} %>% {c(., . +2)} %>% {if (anyNA(.)) which.max(vec)[1] else .} %>% return (.)
        }
    }
}  # interval2.(vec = c(3,2,1,2,3), T); interval2.(vec = 4:1, T)


## Find sequencial vector list, dividing sequential numbers into small groups == (2019-01-14) ========================
seqCtr. <- function(hit, Seq = 1, ...) {  # 'hit' is the target number out of the rule, and 'magicSeq' is the sequence number.
    if (all(is.na(hit))) return (NA)
    hit <- hit[!is.na(hit)]  # hit <- c(1,2,9,10,11,14,17,18,19,22,40,41,42)
    dummy <- nth(hit, 1) :nth(hit, -1)  #  NOTE: both of the initial and last 'dummy' always are not NA.
    dummy[!dummy %in% hit] <- NA
    def.(c('grp', 'grpList', 'j'), list(vector(), list(), 1))
    for (i in seq_along(dummy)) {
        if (! is.na(dummy[i])) grp <- c(grp, dummy[i])
        if (is.na(dummy[i]) && !is.logical(grp) || i == length(dummy)) {
            grpList[[j]] <- grp
            grp <- vector()
            j <- j +1
        }
    }
    grpList <- {lapply(grpList, length) >= Seq} %>% grpList[.]  # Cropping larger than 'Seq'
    if (length(grpList) == 0) grpList <- NA  # In case that grpList = list () because of too large magicSeq or none of sequance.
    return (grpList)
}  # which(hit > 15) %>% seqCtr.(., Seq = 5)  :=  trueList.(hit > 15) %>% {.[map.(., ~ length(.) > 5)]}


## Pick up only true vector and return their list == (2020-02-07) ========================
trueList. <- function(tfVec, ...) {  # 'tfVec' is constituted of TRUE or FALSE and its length is the same to the original vec.
    def.(c('ctr', 'ctrList', 'grpList'), list(vector(), 1, list()))
    for (i in seq_along(tfVec)) {
        if (tfVec[i] == TRUE) {
            ctr <- c(ctr, i)
            if (i == length(tfVec)) {
                grpList[[ctrList]] <- ctr
            }
        } else if (i > 1 && tfVec[i-1] == TRUE && tfVec[i] == FALSE) {
            grpList[[ctrList]] <- ctr
            ctrList <- ctrList +1
            ctr <- vector()
        }
    }
    return (grpList)  # Return the true element number and make use of it
}


## Fast Anomaly Detection == (2020-01-11) ========================
cFilter. <- function(vec, shaper = 5, ...) {  # More odd shaper, more vivid change
    scaler1 <- length(vec) ^shaper *exp((mean.(vec) /sd.(vec)) ^(-1))  # Keep uniform for vector length in a way like coefficient of variation
    scaler2 <- 10 ^(-2 *shaper) *2 ^((shaper -3) /2)  # Keep uniform for many shapers
    vec %>% scale(.) %>% {cumsum(.) /length(.)} %>% {-. ^shaper} %>% {. *scaler1 *scaler2}
}
## 1st prototype == (2020-01-16) ========================
fad0. <- function(vec, shaper = 3, ...) {  # Strongly recommended shaper = 3
    scaler1 <- (1 *length (vec)) ^shaper *exp((mean.(vec) /sd.(vec)) ^(-1) )  # "
    scaler2 <- 10 ^(-2 *shaper) *2 ^((shaper -3) /2)  # "
    balancer <- c(0, 0, diff(vec, differences = 2) ^shaper) %>% scale(.)  # Improvement
    vec %>% scale(.) %>% {cumsum(.) /length(.)} %>% {-. ^shaper} %>% {. *scaler1 *scaler2 *balancer}  # Watch the last term
}
## 2nd keen to outbreak == (2020-01-17) ========================
fad_keen. <- function(vec, shaper = 3, ...) {  # Strongly recommended shaper = 3 (only odd)
    zero_leveler <- function(x) case_when(abs(x) < 1e-20 ~ 0, x == Inf ~ 0, TRUE ~ x)  # Fear of float Inf effect approx. 0 on diff() or . /.
    seq_diff <- function(x) c(0, 0, diff(x, differences = 2)) %>% {sgn.(.) *. ^(shaper -1)} %>% zero_leveler(.) %>%
                            {if ({. == 0} %>% all(.)) rep(0, length(x)) else .}
    sigmoid_weight <- {10 /length(vec)} %>% {(1 +exp(-. *(seq_along(vec) -length(vec) /2))) ^(-1)}
    mu_increment <- cumsum(vec) /seq_along(vec)
    var_increment <- {1 /(seq_along(vec) -1) *(cumsum(vec ^2) -1 /seq_along(vec) *(cumsum(vec)) ^2)}
    var_increment[1] <- var_increment[2]
    punisher <- log(sigmoid_weight *((mu_increment -vec) ^2 +var_increment))
    stabler <- 2 ^((shaper -3) /2)  # To control variation due to many shapers
    seq_diff(vec) %>% {cumsum(scale(.)) /100} %>% {-. ^shaper} %>% {. *punisher *stabler} %>% zero_leveler(.) %>% {sgn.(.) *log(1 +abs(.))}
}
## 3rd robust to change, for Melon analysis == (2020-01-19) ========================
fad. <- function(vec, shaper = 3, ...) {  # Strongly recommended shaper = 3 (only odd)
    zero_leveler <- function(x) case_when(abs(x) < 1e-20 ~ 0, x == Inf ~ 0, TRUE ~ x)  # Fear of float Inf effect approx. 0 on diff() or . /.
    seq_diff <- function(x) c(0, 0, diff(x, differences = 2)) %>% {sgn.(.) *. ^(3 -1)} %>% zero_leveler(.) %>%
                            {if ({. == 0} %>% all(.)) rep(0, length(x)) else .}
    sigmoid_weight <- {10 /length(vec)} %>% {(1 +exp(-. *(seq_along(vec) -length(vec) /2))) ^(-1)}
    mu_increment <- cumsum(vec) /seq_along(vec)
    var_increment <- {1 /(seq_along(vec) -1) *(cumsum(vec ^2) -1 /seq_along(vec) *(cumsum(vec)) ^2)}
    var_increment[1] <- var_increment[2]
    punisher <- log(sigmoid_weight *((mu_increment -vec) ^2 +var_increment)) %>% {case_when(. == Inf ~ 1, . == -Inf ~ 1, TRUE ~ .)}
    stabler <- 10 ^(length(vec) %>% log10(.) %>% {floor(.) +2}) *2 ^(6 *(shaper -3))
    {seq_diff(vec) *seq_diff(punisher)} %>% {cumsum(scale(.)) /length(vec)} %>% {-. ^shaper *stabler}# %>% {sgn.(.) *log(1 +abs(.))}
}
# vec <- psd[[1]]; plot(vec, type = 'l', lwd = 0.8); lines(cpDetecter.(vec), type = 'l', col = 'yellow')
# par(new = T); plot(cFilter.(vec), type = 'l', lwd = 0.8, col = 'darkseagreen', axes = F); axis(4, col.axis = 'darkseagreen')
# par(new = T); plot(fad0.(vec), type = 'l', lwd = 0.8, col = 'blue', axes = F)


## Change points detection == (2019-12-13) ================================================
cpDetecter. <- function(vec, Lper = 0.05, entryRate = 0.25, gapRate = 0.60, ...) {
## https://qiita.com/hoxo_m/items/1afa288178422fad9076
## https://speakerdeck.com/hoxom/bi-nu-nizhen-rarenaitamefalsebian-hua-jian-zhi-ru-men
## https://www.slideshare.net/siroyui/20160924-r-57
## PELT (Pruned Exact Linear Time) is the fastest & most precise to seek several change points.
## CROPS (Change points for a Range Of PenaltieS) makes decision of optimized change points. Without it, there produce too many points.
## The original cpt program can detect even tiny change but industrial trend requires a big change so that any can recognize it as cpt.
## NOTE: changepoint package doesn't follow NA action.
## NOTE: 'minseglen' := minmum of cut segment length: Kill neighbors in the same large deviation
    vec0 <- vec  # vec0 (original) --> vec (no NA) --> vec1 (mean bars in the whole span)
    if (anyNA(vec)) vec <- vec0[!is.na(vec0)]
    if (length(vec) < 10) {cat(paste('\n    CAUTION !!  Element length of the object is too short,', length(vec), '\n')); return (NULL)}
    skipMess.(suppressPackageStartupMessages(library('changepoint')))
    penValues <- c(5, 1000)  # Very significant
    Minseg <- ifelse(length(vec) < 100, 5, ceiling(length(vec) *Lper))  #Any time it's larger than 5. Try Lper (length %).
    invisible(capture.output(  # cpt.meanvar() produces a model but also crap output: Needed invisible (cap ~)
        craps <- cpt.meanvar(vec, method = 'PELT', penalty = 'CROPS', pen.value = penValues, minseglen = Minseg)
    ))
    ## 1) List up x position of cpts with some penalty
    ten0 <- craps %>% cpts.full(.) %>% as.data.frame(.)  # NOTE:  6 is my limit numer of. cpts
    selectCptRow <- pmap(ten0, ~ is.na(c(...)) %>% {ncol(ten0) -sum(.)}) %>% {which(. <= 6)} %>% {
                    if (length (.) != 0) {
                        min.(.) : nrow(ten0)
                    } else {
                        1 : nrow(ten0)
                    }}
    ten1 <- ten0[selectCptRow, ] %>% as.data.frame(.)  # NOTE: if fully deleted it will change into integer.
    players <- unlist(ten1) %>% table(.)
    players[seq(players)] <- 0  # Once reset zero
    if (length (players) > 0) {
        for (i in seq(nrow(ten1))) {for (j in seq(ncol(ten1))) {
            if (!is.na(ten0[i, j])) {
                ijPos <- which(names(players) %>% parse_number(.) == ten1[i, j])
                players[ijPos] <- players[ijPos] +pen.value.full(craps)[selectCptRow][i]
            }
        }}  # This method always leave 1 cpt at least.
    }
    players <- players /max.(players)  # plot(vec, type = 'l'); abline(v = names(players) %>% parse_number(.))
    players <- players[players > entryRate]  # TRY AGAIN  # plot(vec, type = 'l'); abline(v = names(players) %>% parse_number(.))
    ## plot(craps, cpt.width = 1.5, cpt.col = 'blue', ncpts = whichSize.(ref = length(vec), vec = c(30, 80, 150, 300), c(1, 3, 3, 6)))
    ## ten0
    ## NOTE: cpts' candidates in ten0 are placed at NOT the start but the END of avg lines.
    ## pen.value.full (craps)
    ## plot(craps, diagnostic = T); lines(seq(pen.value.full(craps)) -1, rev(pen.value.full(craps)), col = 'blue', lwd = 3, lty = 3)

    ## 2) Check how vivid the changes are.
    gap <- gapRate *sd.(vec)  # Very touchable on you observer for its stepwise appearance
    xcpt <- names(players) %>% as.numeric() %>% c(1, ., length(vec))
    finalSelect1 <- function(...) {  # Updating with re-calculation of avg
        if (length(xcpt) > 2) {
            j <- 1; while(j <= 3) {  # Difference situation will change once any points vanish.
                muSeg <- rep(NA_real_, length(xcpt))
                for (i in seq_along(muSeg) [-1]) muSeg[i-1] <- mean.(vec[xcpt[i-1] : xcpt[i]])
                bigPoint <- which(abs(diff(muSeg)) >= gap) %>% {xcpt[. +1]}
                xcpt <- c(1, bigPoint, length(vec))
                j <- j +1
            }
            if (length(xcpt) > 2) {  # Kill a change point with tiny band
                del_tiny_Xwidth <- which(diff(xcpt) <= 2) %>% c(., . +1) %>% unique(.)
                if (length(del_tiny_Xwidth) != 0) xcpt <- xcpt[-del_tiny_Xwidth]
            }
        }
        return (xcpt)
    }
    finalSelect2 <- function(...) {  # No considering update with re-calculation of avg
        if (length(xcpt) > 2) {
            muSeg <- rep(NA_real_, length(xcpt) -1)
            for (i in seq_along(muSeg)) muSeg[i] <- mean.(vec[xcpt[i] : xcpt[i+1]])
            bigPoint <- diff(muSeg) %>% {which(abs(.) >= gap)} %>% {xcpt[. +1]}
            xcpt <- c(1, bigPoint, length(vec))
        }
        return (xcpt)
    }
    xcpt <- finalSelect1()  # plot(vec, type = 'l'); abline(v = finalSelect1())
  # xcpt <- finalSelect2()  # plot(vec, type = 'l'); abline(v = finalSelect2())

    ## 3) Assign all the element numbers on each of mean Yi or NA
    ## Prepare avgs' vector including NA
    xseg <- {xcpt[- c(1, length(xcpt))] +1} %>% c(xcpt, .) %>% sort() %>% matrix(., ncol = 2, byrow = T)  # Shift 1 for segmentation
    vec1 <- rep(NA_real_, nrow(xseg))
    for (i in seq_along(vec1)) vec1[xseg[i, 1] : xseg[i, 2]] <- mean.(vec[xseg[i, 1] : xseg[i, 2]])
    vec0[!is.na(vec0)] <- vec1  # At this moment, vec0 has been rewritten to avg info with NAs.
    ## Insert 2 medium points at each of xcpts. This is just a gimmic so that the cpt graph looks stepwise at cpt points
    xcpt2 <- xcpt[-c(1, length(xcpt))] # If skip NA, lines() draws a single line. But the procedure below draws multiple lines in NA space
    num <- vec0  # xcpt2 is based on vec without NA, and needs adjusted to numbering with NA inserting
    num[!is.na(num)] <- which(!is.na(num)) %>% seq()
    dtCpt <- tibble(num = num, x = seq(vec0), y = vec0)    #  x is changing but num keeps original element numbers
    for (i in seq_along(xcpt2)) {
        iRow <- which(dtCpt$'num' == xcpt2[i]) # Considered a data frame after i-th inserting
        xInsert2 <- iRow %>% dtCpt[., 'x'] %>% pull(.) %>% {rep(. +0.5, times = 2)}  # It's very confusing...
        yInsert2 <- dtCpt[iRow : (iRow +1), 'y'] %>% pull(.)
        dtCpt <- add_row(dtCpt, num = NA, x = xInsert2, y = yInsert2, .after = iRow)
    }
    df <- data.frame('dplyr'::select(dtCpt, - num))  # NOTE: lines() cannot work with tibble style
    return (df)  # In case of no change point, it returns just a all avg line
  # plot(vec, type = 'l'); lines(df, lwd = 3.5, col = 'dodgerblue3')
}


## My built-in data sets == (2020-10-02) ========================
psd <- 'tibble'::tibble (
    D50_trend = c(10.3505,10.3097,9.4281,9.2974,10.3038,10.0402,10.2097,10.0547,10.1309,10.1652,10.1814,10.0664,10.0646,9.8605,9.9342,9.7808,9.9886,9.7485,9.5912,9.9526,9.5072,9.6256,9.483,9.7851,9.7415,9.8569,9.8133,9.7731,9.831,9.8736,10.1973,10.1464,10.0612,10.0225,9.8804,9.8155,9.9157,9.9467,9.4711,9.3546,9.4517,9.75,10.0349,9.4525,9.4903,9.4697,9.4011,10.311,10.0956,10.2623,9.9476,9.9276,9.4301,9.4204,9.2182,10.2067,10.2306,10.2957,9.8977,10.0762,10.0518,10.2966,10.2803,10.2331,10.1465,10.0151,10.3837,10.0558,10.3344,9.0899,9.1398,9.1047,9.1635,8.618,9.0998,9.3331,9.2716,9.1881,9.2299,9.2615,9.0782,9.2421,9.2541,9.2267,9.1766,9.0327,8.8731,8.8742,8.8404,9.0275,8.9354,9.1421,8.8086,8.9932,9.0468,9.4939,9.5158,9.3488,9.3252,9.4686,9.2631,8.9989,9.0577,9.0464,8.8967,9.0058,9.2276,9.0795,9.0424,8.965,8.9938,9.0738,9.0631,8.9404,8.8106,8.7996,9.2329,8.8762,9.2519,9.2447,9.1554,9.2142,9.1968,9.1114,9.1923,9.1031,9.103,9.2033,9.2678,9.1331,9.2128,9.334,9.1667,9.2228,9.209,9.2497,9.2587,9.2215,9.0974,9.0589,8.9901,9.227,9.2319,9.1942,9.1959,9.2053,9.2142,9.1342,8.449,8.4339,8.5195,8.4926,8.2906,8.4999,8.4717,8.4853,8.4439,8.4278,8.4237,8.5516,8.7148,8.7185,8.7048,8.7232,8.4943,8.439,8.3935,8.6768,8.4648,8.5077,8.7209,8.8719,8.6227,8.3717,8.5017,8.7542,8.4899,8.4815,8.723,8.6122,8.5992,8.4312,8.641,8.6623,8.6808,9.1556,8.5823,8.5338,8.8629,8.8065,8.955,8.9868,8.6425,8.9018,8.9346,9.0891,9.1269,8.974,8.7781,8.7591),
     x_IMPMNN30_0612 = c(4.52,4.66597989949749,4.81195979899497,4.95793969849246,5.10391959798995,5.24989949748744,5.39587939698492,5.54185929648241,5.6878391959799,5.83381909547739,5.97979899497487,6.12577889447236,6.27175879396985,6.41773869346734,6.56371859296482,6.70969849246231,6.8556783919598,7.00165829145729,7.14763819095477,7.29361809045226,7.43959798994975,7.58557788944724,7.73155778894472,7.87753768844221,8.0235175879397,8.16949748743719,8.31547738693467,8.46145728643216,8.60743718592965,8.75341708542713,8.89939698492462,9.04537688442211,9.1913567839196,9.33733668341709,9.48331658291457,9.62929648241206,9.77527638190955,9.92125628140704,10.0672361809045,10.213216080402,10.3591959798995,10.505175879397,10.6511557788945,10.797135678392,10.9431155778894,11.0890954773869,11.2350753768844,11.3810552763819,11.5270351758794,11.6730150753769,11.8189949748744,11.9649748743719,12.1109547738693,12.2569346733668,12.4029145728643,12.5488944723618,12.6948743718593,12.8408542713568,12.9868341708543,13.1328140703518,13.2787939698492,13.4247738693467,13.5707537688442,13.7167336683417,13.8627135678392,14.0086934673367,14.1546733668342,14.3006532663317,14.4466331658291,14.5926130653266,14.7385929648241,14.8845728643216,15.0305527638191,15.1765326633166,15.3225125628141,15.4684924623116,15.614472361809,15.7604522613065,15.906432160804,16.0524120603015,16.198391959799,16.3443718592965,16.490351758794,16.6363316582915,16.7823115577889,16.9282914572864,17.0742713567839,17.2202512562814,17.3662311557789,17.5122110552764,17.6581909547739,17.8041708542714,17.9501507537688,18.0961306532663,18.2421105527638,18.3880904522613,18.5340703517588,18.6800502512563,18.8260301507538,18.9720100502513,19.1179899497487,19.2639698492462,19.4099497487437,19.5559296482412,19.7019095477387,19.8478894472362,19.9938693467337,20.1398492462312,20.2858291457286,20.4318090452261,20.5777889447236,20.7237688442211,20.8697487437186,21.0157286432161,21.1617085427136,21.3076884422111,21.4536683417085,21.599648241206,21.7456281407035,21.891608040201,22.0375879396985,22.183567839196,22.3295477386935,22.475527638191,22.6215075376884,22.7674874371859,22.9134673366834,23.0594472361809,23.2054271356784,23.3514070351759,23.4973869346734,23.6433668341709,23.7893467336683,23.9353266331658,24.0813065326633,24.2272864321608,24.3732663316583,24.5192462311558,24.6652261306533,24.8112060301508,24.9571859296482,25.1031658291457,25.2491457286432,25.3951256281407,25.5411055276382,25.6870854271357,25.8330653266332,25.9790452261307,26.1250251256281,26.2710050251256,26.4169849246231,26.5629648241206,26.7089447236181,26.8549246231156,27.0009045226131,27.1468844221106,27.292864321608,27.4388442211055,27.584824120603,27.7308040201005,27.876783919598,28.0227638190955,28.168743718593,28.3147236180905,28.4607035175879,28.6066834170854,28.7526633165829,28.8986432160804,29.0446231155779,29.1906030150754,29.3365829145729,29.4825628140704,29.6285427135678,29.7745226130653,29.9205025125628,30.0664824120603,30.2124623115578,30.3584422110553,30.5044221105528,30.6504020100503,30.7963819095477,30.9423618090452,31.0883417085427,31.2343216080402,31.3803015075377,31.5262814070352,31.6722613065327,31.8182412060302,31.9642211055276,32.1102010050251,32.2561809045226,32.4021608040201,32.5481407035176,32.6941206030151,32.8401005025126,32.98608040201,33.1320603015075,33.278040201005,33.4240201005025,33.57),
     y_IMPMNN30_0612 = c(-7.9530711793018e-05,0.00101274158900976,0.00213908660736725,0.00333357706083414,0.00463028566696521,0.00606328981383565,0.0076718256345041,0.00951014919878543,0.0116352351121738,0.0141040579801634,0.0169735924082484,0.0202989680702933,0.0241025701561287,0.0283786596416507,0.0331205833319908,0.0383216880322807,0.0439753205476521,0.0500746972987648,0.056584416920589,0.0634047417560321,0.0704271531669352,0.0775431325151394,0.0846441611624855,0.0916217204708148,0.0983672918019682,0.104781250622823,0.110809729355957,0.116413533697992,0.121553476084998,0.126190368953045,0.130285024738203,0.133798255876541,0.136690874804129,0.138924703928163,0.14049614375949,0.141443976523298,0.141809575426262,0.141634313675059,0.140959564476364,0.139826701036854,0.138277096563204,0.13635212426209,0.134093157340188,0.131541106399515,0.128731282785419,0.125695277394384,0.122464614076339,0.119070816681213,0.115545409058934,0.111919915059433,0.108225858532637,0.104494763328476,0.100758153296879,0.0970475522877748,0.0933940955359903,0.0898162479173802,0.0863173006287028,0.0828996504801904,0.0795656942820756,0.0763178288445908,0.0731584509779685,0.0700899574924412,0.0671147451982412,0.0642352109056011,0.0614537514247533,0.0587727635659303,0.0561946441393645,0.0537217317389861,0.0513540462524939,0.0490885629512234,0.0469220503236397,0.0448512768582075,0.0428730110433916,0.0409840213676569,0.0391810763194681,0.0374609443872902,0.035820394059588,0.0342561938248262,0.0327651121714698,0.0313439175879835,0.0299893785628322,0.0286982635844807,0.0274673532239345,0.0262939152803951,0.0251758608057169,0.024111144896071,0.0230977226476284,0.0221335491565599,0.0212165795190366,0.0203447688312295,0.0195160721893095,0.0187284446894476,0.0179798414278147,0.017268217500582,0.0165915280039202,0.0159477280340005,0.0153347726869937,0.0147506170590708,0.0141932162464029,0.0136605256504285,0.0131508009024678,0.0126631599419899,0.0121968745653802,0.0117512165690241,0.0113254577493068,0.0109188699026136,0.0105307248253299,0.0101602943138409,0.009806850164532,0.0094696641737885,0.00914800813799573,0.00884115385353899,0.00854837311680358,0.00826893772417484,0.00800211947203808,0.0077471901567786,0.00750342157478173,0.00727008552243277,0.00704645379611708,0.00683179846144114,0.00662551610763445,0.00642732526782091,0.00623699581200669,0.00605429761019795,0.00587900053240083,0.00571087444862149,0.00554968922886612,0.00539521474314083,0.00524722086145182,0.00510547745380526,0.00496975439020727,0.00483982154066401,0.00471544877518168,0.00459640596376639,0.00448246297642436,0.00437338968316172,0.00426895595398461,0.00416893165889921,0.00407308666791167,0.00398119085102817,0.00389301407825486,0.00380832621959787,0.00372689719860161,0.00364851179001374,0.00357298981458826,0.00350015614608493,0.00342983565826351,0.00336185322488378,0.00329603371970547,0.00323220201648841,0.00317018298899233,0.00310980151097699,0.00305088245620217,0.00299325069842762,0.00293673111141313,0.00288114856891845,0.00282632794470337,0.00277209411252761,0.00271827194615098,0.00266468631933323,0.00261116210583415,0.00255752417941347,0.00250359741383098,0.00244920668284643,0.00239417686021961,0.00233833281971025,0.00228149943507817,0.00222350158008309,0.00216416412848481,0.00210332057195625,0.00204092796064925,0.0019770357763079,0.00191169572834714,0.00184495952618196,0.00177687887922735,0.00170750549689826,0.0016368910886097,0.00156508736377662,0.001492146031814,0.00141811880213687,0.00134305738416017,0.00126701348729885,0.00119003882096792,0.00111218509458238,0.00103350401755716,0.000954047299307289,0.000873866649247704,0.000793013776793396,0.000711540391359329,0.000629498202360514,0.00054693891921194,0.000463914251328528,0.000380475908125277,0.000296675599017218,0.000212565033419267,0.000128195920746418,4.36199704136631e-05,-4.11111081640382e-05,-0.000125945605571681,-0.000210831812394303),
    x_IMPRBN30_0612 = c(3.84,3.95763819095477,4.07527638190955,4.19291457286432,4.3105527638191,4.42819095477387,4.54582914572864,4.66346733668342,4.78110552763819,4.89874371859296,5.01638190954774,5.13402010050251,5.25165829145729,5.36929648241206,5.48693467336683,5.60457286432161,5.72221105527638,5.83984924623116,5.95748743718593,6.0751256281407,6.19276381909548,6.31040201005025,6.42804020100503,6.5456783919598,6.66331658291457,6.78095477386935,6.89859296482412,7.0162311557789,7.13386934673367,7.25150753768844,7.36914572864322,7.48678391959799,7.60442211055276,7.72206030150754,7.83969849246231,7.95733668341709,8.07497487437186,8.19261306532663,8.31025125628141,8.42788944723618,8.54552763819095,8.66316582914573,8.7808040201005,8.89844221105528,9.01608040201005,9.13371859296482,9.2513567839196,9.36899497487437,9.48663316582915,9.60427135678392,9.72190954773869,9.83954773869347,9.95718592964824,10.074824120603,10.1924623115578,10.3101005025126,10.4277386934673,10.5453768844221,10.6630150753769,10.7806532663317,10.8982914572864,11.0159296482412,11.133567839196,11.2512060301508,11.3688442211055,11.4864824120603,11.6041206030151,11.7217587939698,11.8393969849246,11.9570351758794,12.0746733668342,12.1923115577889,12.3099497487437,12.4275879396985,12.5452261306533,12.662864321608,12.7805025125628,12.8981407035176,13.0157788944724,13.1334170854271,13.2510552763819,13.3686934673367,13.4863316582915,13.6039698492462,13.721608040201,13.8392462311558,13.9568844221106,14.0745226130653,14.1921608040201,14.3097989949749,14.4274371859296,14.5450753768844,14.6627135678392,14.780351758794,14.8979899497487,15.0156281407035,15.1332663316583,15.2509045226131,15.3685427135678,15.4861809045226,15.6038190954774,15.7214572864322,15.8390954773869,15.9567336683417,16.0743718592965,16.1920100502513,16.309648241206,16.4272864321608,16.5449246231156,16.6625628140704,16.7802010050251,16.8978391959799,17.0154773869347,17.1331155778894,17.2507537688442,17.368391959799,17.4860301507538,17.6036683417085,17.7213065326633,17.8389447236181,17.9565829145729,18.0742211055276,18.1918592964824,18.3094974874372,18.427135678392,18.5447738693467,18.6624120603015,18.7800502512563,18.8976884422111,19.0153266331658,19.1329648241206,19.2506030150754,19.3682412060301,19.4858793969849,19.6035175879397,19.7211557788945,19.8387939698492,19.956432160804,20.0740703517588,20.1917085427136,20.3093467336683,20.4269849246231,20.5446231155779,20.6622613065327,20.7798994974874,20.8975376884422,21.015175879397,21.1328140703518,21.2504522613065,21.3680904522613,21.4857286432161,21.6033668341709,21.7210050251256,21.8386432160804,21.9562814070352,22.0739195979899,22.1915577889447,22.3091959798995,22.4268341708543,22.544472361809,22.6621105527638,22.7797487437186,22.8973869346734,23.0150251256281,23.1326633165829,23.2503015075377,23.3679396984925,23.4855778894472,23.603216080402,23.7208542713568,23.8384924623116,23.9561306532663,24.0737688442211,24.1914070351759,24.3090452261307,24.4266834170854,24.5443216080402,24.661959798995,24.7795979899497,24.8972361809045,25.0148743718593,25.1325125628141,25.2501507537688,25.3677889447236,25.4854271356784,25.6030653266332,25.7207035175879,25.8383417085427,25.9559798994975,26.0736180904523,26.191256281407,26.3088944723618,26.4265326633166,26.5441708542714,26.6618090452261,26.7794472361809,26.8970854271357,27.0147236180905,27.1323618090452,27.25),
    y_IMPRBN30_0612 = c(0.000116369884183836,0.000562546873840057,0.00104105778304609,0.00158423653135167,0.00222441703830658,0.0029939299905362,0.00392362508095315,0.00504052898363828,0.00637105978617603,0.00794163557615081,0.00977867444114713,0.0119092146571373,0.0143777136891423,0.0172480051745098,0.020584935727051,0.0244533519605769,0.0289181004888987,0.0340440279258276,0.0398729965418128,0.0463340399023076,0.0533216265134535,0.0607302159237083,0.0684542676815296,0.0763882413353749,0.084426596433702,0.0924646194407076,0.100418353256255,0.108225610919073,0.115825235268786,0.123156069145017,0.130156955387388,0.136766736835523,0.142924256329045,0.148568356707577,0.153644732388217,0.158133910475971,0.162027460052259,0.165316954637097,0.1679939677505,0.170050072912482,0.17147684364306,0.172265853462247,0.172408675890059,0.171896884958288,0.170734928722463,0.16897357898561,0.166674022945905,0.163897447801523,0.16070504075064,0.15715798899143,0.153317479722069,0.149244700140732,0.145000837445596,0.140647078834834,0.136244611506623,0.131852232406191,0.127497895704255,0.123188102643232,0.118928920118378,0.114726415024946,0.110586654258189,0.106515704713362,0.102519633285718,0.0986045068705102,0.0947763923629935,0.0910413566584212,0.0874054666520471,0.0838747892391249,0.0804551448373589,0.0771483205544838,0.0739528120037685,0.070867018607886,0.0678893397895098,0.0650181749713132,0.0622519235759693,0.0595889850261517,0.0570277587445335,0.0545666441537882,0.0522040406765889,0.0499383477356091,0.0477679647535219,0.0456912911530008,0.0437067263567191,0.0418126332347276,0.0400066268937417,0.0382856229360858,0.0366465100767197,0.0350861770306034,0.0336015125126966,0.0321894052379592,0.030846743921351,0.0295704172778317,0.0283573140223614,0.0272043228698997,0.0261083325354065,0.0250662317338416,0.0240749091801649,0.0231312535893362,0.0222321536763152,0.0213744981560619,0.0205551823106917,0.0197717277529182,0.0190227911186038,0.018307148764763,0.0176235770484103,0.0169708523265602,0.0163477509562272,0.0157530492944257,0.0151855236981704,0.0146439505244757,0.0141271061303561,0.0136337668728262,0.0131627091089003,0.0127127091955931,0.0122825434899191,0.0118709883488926,0.0114768201295283,0.0110988151888407,0.0107357498838441,0.0103864005715533,0.0100495554474117,0.00972442654271301,0.00941075629701428,0.00910832063950523,0.00881689549937552,0.0085362568058149,0.00826618048801305,0.00800644247515973,0.0077568186964446,0.00751708508105741,0.00728701755818786,0.00706639205702563,0.00685498450676049,0.00665257083658209,0.00645892697568022,0.00627382885324452,0.00609705239846474,0.00592837354053058,0.00576756820863178,0.00561441233195803,0.00546868183969902,0.00533015266104451,0.00519860072518417,0.00507379969101563,0.00495546594165864,0.00484325562437493,0.00473682202364686,0.00463581842395682,0.00453989810978717,0.00444871436562029,0.00436192047593852,0.00427916972522429,0.00420011539795991,0.00412441077862779,0.00405170915171026,0.00398166380168973,0.00391392801304856,0.0038481550702691,0.00378399825783373,0.00372111086022485,0.00365914616192481,0.00359775744741596,0.0035365980011807,0.00347532110770137,0.00341358005146039,0.00335102811694008,0.00328731858862282,0.00322210475099099,0.00315503988852697,0.00308577768404197,0.0030140868905578,0.00294000997147197,0.00286362920697759,0.0027850268772679,0.00270428526253596,0.00262148664297494,0.00253671329877812,0.00245004751013854,0.00236157155724939,0.00227136772030387,0.00217951827949509,0.00208610551501626,0.00199121170706054,0.00189491913582104,0.00179731008149096,0.00169846682426345,0.00159847164433169,0.00149740682188881,0.00139535463712802,0.00129239737024243,0.00118861730142523,0.00108409671086957,0.000978917878768635,0.000873163085315551,0.000766914610703496,0.000660254735125634,0.000553265738775129,0.000446029901845128,0.000338629504528811,0.00023114682701933),
    x_MBM_0510 = c(2.23,2.31688442211055,2.40376884422111,2.49065326633166,2.57753768844221,2.66442211055276,2.75130653266332,2.83819095477387,2.92507537688442,3.01195979899498,3.09884422110553,3.18572864321608,3.27261306532663,3.35949748743719,3.44638190954774,3.53326633165829,3.62015075376884,3.7070351758794,3.79391959798995,3.8808040201005,3.96768844221106,4.05457286432161,4.14145728643216,4.22834170854271,4.31522613065327,4.40211055276382,4.48899497487437,4.57587939698492,4.66276381909548,4.74964824120603,4.83653266331658,4.92341708542714,5.01030150753769,5.09718592964824,5.18407035175879,5.27095477386935,5.3578391959799,5.44472361809045,5.53160804020101,5.61849246231156,5.70537688442211,5.79226130653266,5.87914572864322,5.96603015075377,6.05291457286432,6.13979899497487,6.22668341708543,6.31356783919598,6.40045226130653,6.48733668341709,6.57422110552764,6.66110552763819,6.74798994974874,6.8348743718593,6.92175879396985,7.0086432160804,7.09552763819095,7.18241206030151,7.26929648241206,7.35618090452261,7.44306532663317,7.52994974874372,7.61683417085427,7.70371859296482,7.79060301507538,7.87748743718593,7.96437185929648,8.05125628140703,8.13814070351759,8.22502512562814,8.31190954773869,8.39879396984925,8.4856783919598,8.57256281407035,8.6594472361809,8.74633165829146,8.83321608040201,8.92010050251256,9.00698492462312,9.09386934673367,9.18075376884422,9.26763819095477,9.35452261306533,9.44140703517588,9.52829145728643,9.61517587939698,9.70206030150754,9.78894472361809,9.87582914572864,9.9627135678392,10.0495979899497,10.1364824120603,10.2233668341709,10.3102512562814,10.397135678392,10.4840201005025,10.5709045226131,10.6577889447236,10.7446733668342,10.8315577889447,10.9184422110553,11.0053266331658,11.0922110552764,11.1790954773869,11.2659798994975,11.352864321608,11.4397487437186,11.5266331658291,11.6135175879397,11.7004020100503,11.7872864321608,11.8741708542714,11.9610552763819,12.0479396984925,12.134824120603,12.2217085427136,12.3085929648241,12.3954773869347,12.4823618090452,12.5692462311558,12.6561306532663,12.7430150753769,12.8298994974874,12.916783919598,13.0036683417085,13.0905527638191,13.1774371859296,13.2643216080402,13.3512060301508,13.4380904522613,13.5249748743719,13.6118592964824,13.698743718593,13.7856281407035,13.8725125628141,13.9593969849246,14.0462814070352,14.1331658291457,14.2200502512563,14.3069346733668,14.3938190954774,14.4807035175879,14.5675879396985,14.654472361809,14.7413567839196,14.8282412060302,14.9151256281407,15.0020100502513,15.0888944723618,15.1757788944724,15.2626633165829,15.3495477386935,15.436432160804,15.5233165829146,15.6102010050251,15.6970854271357,15.7839698492462,15.8708542713568,15.9577386934673,16.0446231155779,16.1315075376884,16.218391959799,16.3052763819095,16.3921608040201,16.4790452261307,16.5659296482412,16.6528140703518,16.7396984924623,16.8265829145729,16.9134673366834,17.000351758794,17.0872361809045,17.1741206030151,17.2610050251256,17.3478894472362,17.4347738693467,17.5216582914573,17.6085427135678,17.6954271356784,17.7823115577889,17.8691959798995,17.95608040201,18.0429648241206,18.1298492462312,18.2167336683417,18.3036180904523,18.3905025125628,18.4773869346734,18.5642713567839,18.6511557788945,18.738040201005,18.8249246231156,18.9118090452261,18.9986934673367,19.0855778894472,19.1724623115578,19.2593467336683,19.3462311557789,19.4331155778894,19.52),
    y_MBM_0510 = c(-0.000327318093696737,0.000835733645400564,0.00200474890905977,0.00318569122184272,0.00438452410831129,0.00561315764626261,0.006958975240445,0.00856124522924285,0.0105602561437762,0.0130962965151652,0.0163049838217485,0.0202551298888539,0.0249656539667749,0.0304542774861463,0.0367387218776032,0.0438367085717805,0.0517600746672222,0.0604758836251104,0.0699306172435053,0.0800706486992754,0.0908423511692893,0.102192097830416,0.114066261859523,0.126387022612734,0.138964437832094,0.151576108722749,0.163999633181261,0.176012609104191,0.1873926343881,0.197917306929549,0.207364224625099,0.215545642738184,0.22245522223327,0.228145777880875,0.232670157047752,0.236081207100652,0.238431775406325,0.239774709331525,0.240162856243002,0.239649063507508,0.238286652606477,0.236138096712236,0.233274141307921,0.229765829552448,0.225684204604734,0.221100309623695,0.216085187768248,0.210709882197308,0.205045436069792,0.199162892544616,0.193133294780697,0.187024777943342,0.18087579979768,0.174707465148626,0.16854066045531,0.16239627217686,0.156295186772404,0.150258290701072,0.144306470421992,0.138460612394293,0.132741603077103,0.127170328929551,0.121767676410766,0.116554531979875,0.111547991973824,0.106747221272044,0.102146087022894,0.0977384555900449,0.093518193337168,0.089479166627934,0.0856152418260143,0.0819202852950797,0.0783881633988011,0.0750127425008499,0.071787888964897,0.0687074691546134,0.0657653494336701,0.0629553961657383,0.0602714757540107,0.0577078780720282,0.055260362373377,0.0529250064008833,0.0506978878973732,0.048575084605673,0.0465526742686089,0.0446267346290071,0.0427933434296938,0.0410485784134953,0.0393885173232376,0.0378092379017471,0.0363068178918499,0.0348773350363723,0.0335168670781404,0.0322214917599805,0.0309872868247188,0.0298103300151816,0.0286868026848876,0.0276140844230653,0.0265903249221866,0.0256136865777987,0.0246823317854487,0.0237944229406836,0.0229481224390508,0.022141592676097,0.0213729960473696,0.0206404949484156,0.0199422517747822,0.0192764289220164,0.0186411887856653,0.018034693761276,0.0174551062443957,0.0169005886305715,0.0163693033153504,0.0158594126942796,0.0153690791629061,0.0148964651167772,0.0144397589735714,0.0139978747367525,0.0135705305154474,0.0131574861968837,0.0127585016682894,0.012373336816892,0.0120017515299193,0.0116435056945991,0.0112983591981589,0.0109660719278266,0.0106464037708299,0.0103391146143965,0.0100439643457541,0.00976071285213043,0.0094891200207532,0.00922894573885017,0.00897994989364903,0.00874189237237749,0.00851453306226326,0.00829763185053412,0.00809094862441774,0.00789424327114187,0.00770727567793419,0.00752980573202244,0.00736157552357313,0.00720218092328931,0.00705114604199447,0.00690799449504263,0.00677224989778783,0.00664343586558408,0.00652107601378537,0.00640469395774572,0.00629381331281919,0.00618795769435979,0.00608665071772151,0.00598941599825839,0.00589577715132442,0.00580525779227369,0.00571738153646013,0.00563167199923784,0.0055476527959608,0.00546484754198305,0.00538277985265854,0.00530097334334138,0.00521895162938553,0.00513623832614505,0.00505235704897392,0.00496683141322619,0.00487918503425586,0.00478894152741695,0.00469562450806351,0.00459875773066275,0.0044980017837137,0.00439341027059674,0.00428510691925268,0.00417321545762251,0.00405785961364711,0.00393916311526736,0.00381724969042415,0.00369224306705842,0.00356426697311106,0.00343344513652295,0.00329990128523503,0.00316375914718818,0.00302514245032328,0.00288417492258127,0.00274098029190301,0.00259568228622945,0.00244840463350147,0.00229927106165996,0.00214840529864582,0.00199593107239997,0.00184197211086331,0.00168665214197674,0.00153009489368115,0.00137242409391742,0.00121376347062648,0.00105423675174925,0.000893967665226628,0.000733079938999473,0.000571697301008709,0.000409943479195256,0.00024794220149999,8.58171958638476e-05)
)
nya0 <- 'tibble'::tibble(t1=c(3,5,-3,-5), t2=c(1.2,3.4,5.6,7.8), t3=c('Cats','can','run','fun'),
               t4=c('2020/9/1', '2020/9/2', '2020/9/3', '2020/9/4'), t5=NA, t6=c('24:00', '123:00', NA, '1:30:00'),
               t7=c('2020/9/1 00:00:00', '2020/9/2 01:23:45', '2020/9/3 12:34:56', '2020/9/4 23:59:59'))  # 'hablar'::retype(nya0)
assign('iris', 'tibble'::as_tibble('datasets'::iris), envir = .GlobalEnv)

## END ##
