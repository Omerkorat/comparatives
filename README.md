This repo contains the code for the paper <link-to-the-paper>. The code contains documentation and references to the appropriate parts of the paper. Data was collected from the Corpus of Contemporary American English (Davies, 2008).

To set up, unzip data.zip and set data.dir to the current working directory.

Each row is a numerical expression found in the raw text. Data were collected from the written (not spoken) portion of COCA, which contains around 400 million words.

Column descriptions are as follows:

**num** Raw numerical value of expression

**is_p** 1 iff the expression is a percentage (e.g. "50 percent", "50%"), 0 otherwise

**is_frac** 1 iff the expression is a fraction (e.g. "1/2", "one half"), 0 otherwise

**nat_hyp** 1 iff the expression satisfies the natural roundness hypothesis described in the paper

**neg** 1 iff the expression is negated (e.g. "no more than 3"), 0 otherwise

**mt** 1 iff the expression contains 'more than', 0 otherwise

**al** 1 iff the expression contains 'at least', 0 otherwise

**lt**	1 iff the expression contains 'less than', 0 otherwise

**am** 1 iff the expression contains 'at most', 0 otherwise



@book{davies2008corpus,
  title={The corpus of contemporary American English},
  author={Davies, Mark},
  year={2008},
  publisher={BYE, Brigham Young University}
}
