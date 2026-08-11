# IRT_modeling_in_brms
**Assignment and Task**

In this assignment, you will be scaling roll-call vote data from the Bundestag.  

A good reference exists in the notion page, here: [Multi-dimensional Scaling and IRT Models](https://www.notion.so/Multi-dimensional-Scaling-and-IRT-Models-23d41d4453aa80adb2ccf2bc8cbf3d24?pvs=21) with code examples.

**Data and setting**

The data consist of every recorded roll-call vote (*namentliche Abstimmung*) of the 20th German Bundestag — Wahlperiode 20, 2021-09-29 to 2025-03-24, the Ampel coalition period. For each vote, every Bundestag member is recorded as voting *ja*, *nein*, *Enthaltung* (abstain), or *nicht abgegeben* (not cast).  We will adopt the notation “ja”=1, “nein”=0, and anything else is missing = “NA.”  

The data is available below.

1. `bundestag_wp132_votes_clean.csv` — the cleaned roll-call vote matrix. Each row is one legislator. The first two columns are name and party; the remaining columns are votes, named v001, v002, and so on. Vote cells are coded 1 for ja, 0 for nein, and NA for Enthaltung or nicht abgegeben. The same data is reshaped into long form for regression below.
    
    [bundestag_wp132_votes_clean.csv](attachment:d360a2d9-7ab2-4054-b8f5-2f1a97e63151:bundestag_wp132_votes_clean.csv)
    
    [bundestag_wp132_votes_clean_long.csv.zip](attachment:9b29cf94-74b0-4a5e-874d-3d3cac9063f3:bundestag_wp132_votes_clean_long.csv.zip)
    
2. `bundestag_wp132_vote_codebook.csv` — the vote codebook. This maps each vote column in the matrix, such as v001, to the original poll id, vote label, committee, whether the motion was accepted, and the Abgeordnetenwatch URL.
    
    [bundestag_wp132_vote_codebook.csv](attachment:b07a6add-a6f1-4178-ba06-107bd8fcbfdc:bundestag_wp132_vote_codebook.csv)
    

**Task**

A binary vote matrix is a standard input for ideal-point estimation. It is normally a combination of 1’s, 0’s, and NA’s or equivalent for missing votes.  Every legislator is placed at a point on a latent scale, and the votes reveal where legislators tend to fall on that scale.

**The Analysis**

For this assignment, you will scale the roll-call matrix three ways.

1. The matrix methods require a complete rectangular matrix (the *brms* method does not, though ignoring missingness can always induce biases!).  Ensure the data is arranged with legislators in rows and votes in columns.  The missing data need to be filled in with something.  The quickest approach that is reasonable is double-mean imputation: fill in each missing value with the mean of the observed values in its row + the mean of the observed values in its column - the overall mean of the observed data.  Please use the data from the *votes_clean.csv* for this component.
2. Generate the SVD of this matrix. Look along the first column of latent scalings for the legislators. 
    
    a) Who falls on one side?  The other? Does the ordering make sense, given your knowledge of German politics?  
    
    b) Look at the most/least extreme votes.  Do these make sense? What dimension is being pulled out?
    
    c) Look at the second dimension of legislators. Does it look like a meaningful dimension, or noise?
    
    d) What proportion of the variance is explained by the first dimension? For dimension 1, this is
    
    $var_1 = d_1^2/\sum_j d_j^2$
    
    where the $d$ comes from the D component of the SVD decomposition.
    
3. We are now going to do the same for the double-centered matrix.  To double-center the matrix, start with the original data, then for each element of the matrix calculate
    
    $\widetilde X_{ij} = X_{ij}-\overline X_{i\cdot} -\overline X_{\cdot j} + \overline X_{\cdot\cdot}$
    
    Confirm that the double-centered matrix does have mean zero for all rows and columns, then repeat the analysis in (ii). 
    
4. Next, scale the roll call votes using *brms* or an equivalent method in Python. Please use that long data, where rows are legislator-vote, in *votes.csv.* Note that, unlike the matrix decomposition, you can omit the NAs from the scaling model by just not including them in the regression.  Fit a one-dimensional 2-PL IRT model. Specify what priors were used for each set of parameters (you can use the defaults), and compare the posterior distirbution of the ideal points to what you recovered in part (i)-(iii).  Do the results agree/correlate? 
5. Choose one substantive claim about the first dimension in the 20th Bundestag from the IRT model and use your scaling results to evaluate it. A strong answer uses the Bayesian posterior as a distribution, not just as another point estimate. It distinguishes what the posterior directly shows from what you infer, and it distinguishes uncertainty about one MP's ideal point from uncertainty about the ordering of two MPs, parties, or groups.  (Note that uncertainty estimates are a major advantage of the IRT model over the SVD approach.)
