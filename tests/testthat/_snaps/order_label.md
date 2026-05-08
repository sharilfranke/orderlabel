# Ungrouped, stacked gg

    Code
      as.character(dplyr::pull(order_label(frequencies, stacked = "gg"), label))
    Condition
      Warning in `order_label()`:
      You used a "stacked" ordering system without specifying group_var. Is your data grouped?
    Output
      [1] "Five"  "Four"  "Three" "Two"   "One"  

# Ungrouped, stacked ms

    Code
      as.character(dplyr::pull(order_label(frequencies, stacked = "ms"), label))
    Condition
      Warning in `order_label()`:
      You used a "stacked" ordering system without specifying group_var. Is your data grouped?
    Output
      [1] "One"   "Two"   "Three" "Four"  "Five" 

# Ungrouped, percent_all

    Code
      order_label(frequencies, percent_all = TRUE)
    Condition
      Warning:
      The `percent_all` argument of `order_label()` is deprecated as of orderlabel 0.4.3.
      i Please use the `percent_var` argument instead.
    Output
      # A tibble: 5 x 7
        variable value label     n stat    result percent_label
        <chr>    <dbl> <fct> <dbl> <chr>    <dbl> <chr>        
      1 s_test       5 Five     40 percent   0.9  90%          
      2 s_test       3 Three    15 percent   0.4  40%          
      3 s_test       4 Four     20 percent   0.31 31%          
      4 s_test       2 Two      15 percent   0.3  30%          
      5 s_test       1 One      10 percent   0.1  10%          

# horizontal = TRUE is deprecated

    Code
      order_label(frequencies, group_var = group_var, horizontal = TRUE, stacked = "ms")
    Condition
      Warning:
      The `horizontal` argument of `order_label()` is deprecated as of orderlabel 0.4.3.
      i Please use the `direction` argument instead.
    Output
      # A tibble: 9 x 8
        group_var variable value label     n stat    result percent_label
        <fct>     <chr>    <dbl> <fct> <dbl> <chr>    <dbl> <chr>        
      1 Group 3   s_test       3 Three    10 percent   0.01 1            
      2 Group 1   s_test       3 Three    10 percent   0.3  30           
      3 Group 2   s_test       3 Three    10 percent   0.03 3            
      4 Group 3   s_test       2 Two      10 percent   0.8  80           
      5 Group 1   s_test       2 Two      10 percent   0.2  20           
      6 Group 2   s_test       2 Two      10 percent   0.02 2            
      7 Group 3   s_test       1 One      10 percent   0.5  50%          
      8 Group 1   s_test       1 One      10 percent   0.1  10           
      9 Group 2   s_test       1 One      10 percent   0.01 1            

# percent_all = TRUE is deprecated and maps to percent_var = 'All'

    Code
      frequencies <- order_label(make_pv_fixture(), group_var = group_var,
      percent_all = TRUE)
    Condition
      Warning:
      The `percent_all` argument of `order_label()` is deprecated as of orderlabel 0.4.3.
      i Please use the `percent_var` argument instead.

