# add_partyrollup maps all 7 levels from character input

    Code
      out$partyid_7
    Output
      <labelled<double>[8]>
      [1]  1  2  7  6  3  4  5 NA
      
      Labels:
       value                    label
           1        Strong Republican
           2 Not so strong Republican
           3          Lean Republican
           4              Independent
           5            Lean Democrat
           6   Not so strong Democrat
           7          Strong Democrat

# add_partyrollup reads value labels from haven_labelled input

    Code
      out$partyid_7
    Output
      <labelled<double>[6]>
      [1] 1 2 7 6 3 5
      
      Labels:
       value                    label
           1        Strong Republican
           2 Not so strong Republican
           3          Lean Republican
           4              Independent
           5            Lean Democrat
           6   Not so strong Democrat
           7          Strong Democrat

# add_partyrollup output is haven_labelled with the expected labels

    Code
      haven::labelled(attr(out$partyid_7, "labels"))
    Output
      <labelled<double>[7]>
             Strong Republican Not so strong Republican          Lean Republican 
                             1                        2                        3 
                   Independent            Lean Democrat   Not so strong Democrat 
                             4                        5                        6 
               Strong Democrat 
                             7 

# add_partyrollup errors when a variable cannot be found

    Code
      add_partyrollup(df)
    Condition
      Error in `resolve_party_var()`:
      ! Could not find a `party_lean` variable in `df`.
      i Specify the column directly with `party_lean`.

# add_partyrollup errors when multiple variables match

    Code
      add_partyrollup(df)
    Condition
      Error in `resolve_party_var()`:
      ! Found multiple possible `party_id` variables: "party_id" and "party_affiliation".
      i Specify which to use with `party_id`.

# add_partyrollup errors with hints when a supplied column is missing

    Code
      add_partyrollup(df, party_id = party_idd)
    Condition
      Error in `resolve_party_var()`:
      ! Column "party_idd" not found in `df`.
      i Did you mean "party_id", "party_strength", and "party_lean"?

