# neutrostat

neutrostat is an R package that provides tools for analyzing neutrosophic data, which involves imprecise and vague information. The package implements neutrosophic descriptive statistics, probability distributions, and utilities for handling neutrosophic numbers, based on the neutrosophic statistical framework proposed by Florentin Smarandache (2013).

---

## Key Features

* Summary statistics for neutrosophic data.
* Interval arithmetic functions 
* Neutrosophic Probability distributions:
* Neutrosophic Exponential 
* Real Dataset are included in the package:
  - citytemp: Daily temperature intervals of five cities in Pakistan (July 2022)
  - dioxin`: Daily ingestion of dioxins in Japan with uncertainty
  - goldprice`: Monthly gold prices across six Indian cities (2022–2023)

---

#### Author

* Zahid Khan
* Zsolt T. Kosztyan

#### Maintainer

* Zsolt T. Kosztyan

## Installation

Install the released version from CRAN :

install.packages("neutrostat")


Or install the development version from GitHub:

```
library(devtools)
install_github("kzst/neutrostat")
library(neutrostat)

```


## Acknowledgement

This work has been implemented by the TKP2021-NVA-10 project with the support provided by the Ministry of Culture and Innovation of Hungary from the National Research, Development and Innovation Fund, financed under the 2021 Thematic Excellence Programme funding scheme.
