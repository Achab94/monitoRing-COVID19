# Credits and informations

---

##### Author

This platform was created by Emanuele Degani [(web)](https://achab94.github.io), PhD student at the Department of Statistical Sciences of the University of Padua. Suggestions, criticisms and reports of any bugs are strongly suggested at **degani [at] stat [dot] unipd [dot] it**.

A big thanks to Emanuele Aliverti, Pietro Belloni, Laura D'Angelo, Nicola Pilia, Stefano Corona, Francesco Mancone for having contributed with valuable suggestions and criticisms.

**Update (22nd March 2020)** : Giorgio Sestili, manager of the Facebook page [Coronavirus - Dati e Analisi Scientifiche](https://www.facebook.com/DatiAnalisiCoronavirus), interviewed me and we exchanged some words about this portal, on how it works and how it was born. I suggest you to follow their Facebook page to stay updated on scientifically reliable content on the evolution of the phenomenon. Sadly, that page and the following interview are both in italian.

[![](http://img.youtube.com/vi/4lCxc4fH8Ws/0.jpg)](http://www.youtube.com/watch?v=4lCxc4fH8Ws "")

**Update (16th March 2020)** : in the last few hours I have recorded a high number of accesses which has compromised the usability of the portal, given the limited computational resources made available by the free rate of the server. I therefore found myself forced to rent more computational resources to the provider (ShinyApps), for a monthly cost of 35 euros. I decided to ask a free offer to anyone wishing to contribute to the expenses, according to their availability, through a [MoneyBox Paypal](https://paypal.me/pools/c/8nqxplRvVc) which would automatically close as soon as the amount necessary would have been achieved (to avoid receiving further unnecessary offers). In less than two hours the total amount was reached: this was possible thanks to the donations of Guido Milano, Gianpaolo Natale, Franco Bocci, Enrico Bonan, Fabio Comini, Stefano De Lazzari. Thank you so much.

---

##### Data source

The platform automatically takes the data, whenever a user logs in, from the latest updated version of the database [(link GitHub)](https://github.com/pcm-dpc/COVID-19) made available by the Civil Protection daily around at 18:30pm, immediately after the press conference of the Head of the Department of Civil Protection Angelo Borrelli. It is advisable to consult [this link](https://github.com/pcm-dpc/COVID-19/blob/master/README.md) for any information on the data available and on the data-entry (this is because often some data every day are missing, or arrive late).

---

##### How it was made

The platform was created entirely in **R** language, using the features of the Shiny library [(website)](https://shiny.rstudio.com). Other libraries used are `ggplot2` and` ggiraph` (for interactive data-visualization), `dplyr` (for data-processing) and` shinyWidgets` (for some of the widgets included).
The platform lies online on the computing space made available by ShinyApps [(website)](https://www.shinyapps.io).

The whole code used to make this portal is available on [this Github repository](https://github.com/Achab94/monitoRing-COVID19).

---

##### License to use

![GitHub license](https://img.shields.io/badge/License-Creative%20Commons%20Attribution%204.0%20International-blue)
