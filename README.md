# FIND

**Objective Comparison of Phase I Dose-Finding Designs**

The FIND package provides tools to compare decision tables and evaluate operating characteristics for Phase I dose-finding trial designs. It implements five popular designs:

- **3+3**: Traditional rule-based design
- **BOIN**: Bayesian Optimal Interval design
- **mTPI-2**: Modified Toxicity Probability Interval
- **i3+3**: Interval-based 3+3
- **G3+3**: Generalized 3+3

## Installation

You can install the development version of FIND from GitHub:

```r
# Install remotes if you haven't already
install.packages("remotes")

# Install FIND from GitHub
remotes::install_github("oliviazhang0416/FIND")
```

Or using devtools:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install FIND from GitHub
devtools::install_github("oliviazhang0416/FIND")
```

## Quick Start

```r
library(FIND)

# Create design specifications
boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)

# Generate and compare decision tables
decision_table(boin, i3)

# Run simulations
p.true <- c(0.05, 0.10, 0.20, 0.30, 0.45)
mtd.true <- c(0, 0, 1, 0, 0)

boin_sim <- design_boin(pT = 0.25, EI = c(0.15, 0.35), ncohort = 10)
results <- run_simulation(boin_sim, p.true = p.true, mtd.true = mtd.true)

# Visualize operating characteristics
oc_plot(results)
```

## Main Functions

| Function | Description |
|----------|-------------|
| `design_boin()`, `design_i3plus3()`, `design_mtpi2()`, `design_g3plus3()`, `design_3plus3()` | Design constructors |
| `get_decision()` | Get dose escalation/de-escalation decisions |
| `run_simulation()` | Simulate trial operating characteristics |
| `decision_table()` | Generate and compare decision tables |
| `oc_plot()` | Visualize operating characteristics |
| `select_mtd()` | Select maximum tolerated dose |

## References

- Liu S. and Yuan, Y. (2015). Bayesian Optimal Interval Designs for Phase I Clinical Trials. *Journal of the Royal Statistical Society: Series C*, 64, 507-523.
- Guo, W., Wang, S. J., Yang, S., Lynn, H. S., and Ji, Y. (2017). A Bayesian interval dose-finding design addressing Ockham's razor: mTPI-2. *Contemporary Clinical Trials*, 58, 23-33.
- Liu, M., Wang, S. J., and Ji, Y. (2020). The i3+3 design for phase I clinical trials. *Journal of Biopharmaceutical Statistics*, 30(2), 294-304.

## License

MIT
