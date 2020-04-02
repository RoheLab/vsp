 

This package contains the data and the computer code to reproduce the results in 
Ji and Jin's paper titled "Coauthorship and Citation Networks for Statisticians"
  in the Annals of Applied Statistics. 

The Data folder contains 4 data files: 

authorList.txt      -- the complete list of 3607 author names

authorPaperBiadj.txt-- the bipartite (authorship) adjacency matrix between authors(rows)
                       and papers (columns); the element at (i,j) is 1 iff author i 
                       is the author or one of the coauthors of paper j, and 0 otherwise

paperCitAdj.txt     -- the 3248x3248 adjacency matrix for citations between papers; 
                       the element at (i,j) is 1 iff paper i is cited by paper j, and 0 otherwise

paperList.txt       -- the complete list of 3248 papers including DOI, year, title, citation count and abstract  for each


The Code folder contains our own code to reproduce the results in the paper, as well as
the matlab code in separate folders for the BCPL (Bickel and Chen, 2009) and the APL 
(Amini, Chen, Bickel and Levina, 2013) with the permission from the authors. Our own 
code files include

main.R      -- the main code to produce the tables and figures in the paper; 
               to reproduce the results, please start with this file and you will be 
			   prompted run other code when necessary

functions.R -- the functions used in the main code, loaded automatically if the working 
               directory is set correctly

			   

We are happy to provide other files that may be useful for other purposes, such as the cleaned
bib text files, etc.  Please feel to let us know if there are any questions regarding the data 
set, the code and the paper. Our email addresses are psji@uga.edu and jiashun@stat.cmu.edu. 
Thank you very much. 

Please cite the following refernces if you use the data set or the computer code.

(1) Ji and Jin (2016). Coauthorship and citation networks for statisticians. Ann. Appl. Stat. Volume 10, Number 4, 1779-1812.

(2) Jin (2015). Fast community detection by SCORE. Ann. Statist. Volume 43, Number 1, 57-89.


Pengsheng Ji (psji@uga.edu) and Jiashun Jin (jiashun@stat.cmu.edu)