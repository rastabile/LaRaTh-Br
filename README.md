# LaRaTh-Br — Landslide Rainfall Thresholds Brazil

**Version:** 1.1

LaRaTh-Br is an R-based computational framework for analyzing rainfall conditions associated with landslides using an event-based approach. It supports rainfall event identification, the association of rainfall events with landslide records, and logistic regression modeling and evaluation.

The framework was developed for the analysis of rainfall and landslide databases from Cemaden, Brazil. It can be adapted to other study areas and databases, provided that the required input information is available and the data structures are appropriately configured.

## Main components

The framework consists of three computational modules:

1. **Rainfall event definition:** identifies rainfall events and calculates rainfall characteristics, including event duration, accumulated rainfall, rainfall intensity, and antecedent rainfall.
2. **Rainfall–landslide association:** associates rainfall events with landslide records according to specified spatial and temporal criteria and labels events as landslide-associated or non-landslide-associated.
3. **Logistic regression modeling and evaluation:** fits logistic regression models and evaluates their ability to discriminate between rainfall events with and without associated landslide records.

## Software requirements

- **R:** version 4.3.1 was used in the development of LaRaTh-Br.
- **R packages:** the required packages and dependencies should be checked in the scripts before execution.

The framework was developed using the R statistical computing environment. For information about R, visit the [official R project website](https://www.r-project.org/).

## Execution instructions

The code is organized into modules and individual steps. Each module has a specific objective and may be executed separately, provided that the required input data are available. Within each module, the steps must be executed sequentially.

The modules are:

1. `LaRaTh_1.1_Module01.R`
2. `LaRaTh_1.1_Module02.R`
3. `LaRaTh_1.1_Module03.R`

Before execution, check the file paths, input data structures, and required columns specified in the scripts. The scripts were developed for rainfall and landslide databases from Cemaden, Brazil, and may require adaptations when applied to other databases.

To facilitate adaptation, input databases should use column names and contain information consistent with the examples and requirements specified in the scripts.

## Data sources

The original data used in the associated study are available through the following Cemaden platforms:

- **Rainfall data:** [Cemaden Pluviometric Data Platform](https://ped.cemaden.gov.br/)
- **Landslide occurrence records:** [Cemaden Register of Flood and Landslide Events (REINDESC)](https://reindesc.cemaden.gov.br/)

This repository provides the R scripts implementing the framework and the consolidated rainfall-event dataset used in the associated study, including derived rainfall variables and landslide-association labels. The consolidated dataset does not replace the original source databases.

## Software registration

LaRaTh-Br is registered with the Brazilian National Institute of Industrial Property (INPI):

- **Registration number:** BR512024001508-6
- **Registration date:** 8 May 2024

## License

LaRaTh-Br is distributed under the GNU Affero General Public License, version 3 or any later version (AGPL-3.0-or-later).

See the `LICENSE` file for the full license text. For more information about this license, visit the [GNU website](https://www.gnu.org/licenses/agpl-3.0.html).

## Citation

If you use LaRaTh-Br in your research, please cite the associated publication:

[]

## Contact

**Rodrigo Augusto Stabile**  
Brazilian National Center for Monitoring and Early Warning of Natural Disasters (Cemaden), Brazil  
Email: [rodrigo.stabile@cemaden.gov.br]
