# BART-comp-model
Computational Modelling of the BART

---
July 21, 2025
model output naming convention: outputXX_Yk
  XX = number of SubjIDs in the data set
  Yk = number of iterations

Current(output40_4k) errors:

Divergent transitions after warmup, see https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
Transitions after warmup that exceed max treedepth, see https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
Max R-hat is 3.88 -> chains have not mixed, see https://mc-stan.org/misc/warnings.html#r-hat
Bulk effect sample size (ESS) is too low -> may need more iterations, see https://mc-stan.org/misc/warnings.html#bulk-ess
Tail Effective samples size (ESS) is too low -> may need more iterations, see https://mc-stan.org/misc/warnings.html#tail-ess


Helpful references and papers:

Qin 2025 Neural Correlates of risk decision-making: insights from the balloon analogue risk task and exponential-weight mean-varience model
  Paper: https://www.sciencedirect.com/science/article/pii/S0010945225000991
  Supplimentary material: https://www.sciencedirect.com/science/article/pii/S0010945225000991#appsec2
  
Jenkinson 2024 Greater risk taking in cosmetic surgery acceptance and history: An experimental computational study
  Paper: https://pmc.ncbi.nlm.nih.gov/articles/PMC7616178/
  Data & code: https://github.com/katlaboratory/risktaking_cosmetic
  
Lasagna 2022 Mathematical modeling of risk-taking in bipolar disorder
  Paper: https://pmc.ncbi.nlm.nih.gov/articles/PMC9897236/
  Data & code: https://osf.io/zjmy8/?view_only=4bd534b2c3db4304be941f9414541440.
  
Park 2021 Development of a novel computational model for the Balloon Analogue Risk Task: The exponential-weight mean-varience model
  Paper: https://www.sciencedirect.com/science/article/pii/S0022249621000274
  
