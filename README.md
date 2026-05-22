# Partial-Noncompetes

This repository contains the data and code to generate estimates in the Agglomerations post [The Babysitter Clause and the Problem of Partial Noncompete Bans](https://agglomerations.eig.org/p/the-babysitter-clause-and-the-problem). Accurate as of the time of publication, May 22nd, 2026. Contact thomas@eig.org or benjamin@eig.org with any questions.

All data required to replicate the analysis is included in this repository or pulled from IPUMS in the code.

----------

### Project Summary
In this project, we estimate the percentage of every state's (with some exceptions: see note) labor force and total earned income protected by a noncompete ban. We accomplish this by encoding state noncompete statutes into banned or unbanned status for ACS occupation codes, industry codes, income levels, ages, full-time status, wage type or any other type of worker included in or excluded by each state's ban and applying it to the 2024 ACS 1-year survey. Minors, individuals currently in the armed forces, and those listed as employed with no income are excluded from the sample.

We find that apart from the five states (California, Washington, Oklahoma, North Dakota, and Minnesota) with full bans, noncompete ban coverage is relatively sparse. In total, we estimate that about 33% of workers and 32% of total income are protected by a noncompete ban. Job-type noncompete bans, in which only specific occupations or classifications of worker are covered by a noncompete ban, are the most common but the least protective. Most job-type bans cover less than 5% of their state's workforce. Income-based bans, in which all workers earning below a specific income level, cover large shares of the workforce but substantially smaller shares of the state's total income.

----------

### Data
Data for this piece comes from the following sources:

1. <b>IPUMS USA:</b> [American Community Survey 2024 1-Year](https://usa.ipums.org/usa/)
2. <b>IPUMS CPS:</b> [Current Population Survey 2025 ASEC](https://cps.ipums.org/cps/)
3. Original analysis of each state's most current noncompete laws

----------

### Notes
Although the data and code in this repository includes estimates of the workforce and income coverage of noncompetes in all 50 states and Washington, D.C., 20 states were ultimately excluded because I was not confident in the accuracy of the way one or more provisions from their noncompete statutes was encoded into data. Those states are Alabama, Arizona, Colorado, Connecticut, Washington, D.C., Georgia, Idaho, Illinois, Indiana, Iowa, Kentucky, Maine, Massachusetts, Minnesota, Montana, New Jersey, South Dakota, Utah, Virginia, and Wyoming.
