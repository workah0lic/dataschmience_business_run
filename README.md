# Analyse zum Wiener Business Run
## Für www.dataschmience.at | 2022-09-05

**Quickstart**
- Installation von R/RStudio/GIT
- git pull dieses Repos
- öffnen der .RProj-Datei
- ausführen von renv::restore() 

Damit solltest du startklar für deine Analysen sein - alle erforderlichen R-Packages werden installiert!

**Projekt-Struktur**
- ./src
  - businessrun_scrape.R *File für das Scrapen der Business Run Infos*
  - businessrun_plot.R *Aufbereiten der Daten und Erstellen der Plots*
- ./data
  - df_target.RDS *Raw-Data (Ergebnisse von businessrun_scrape.R)*
- ./results
  - charts.RDS *alle Plots (Ergebnisse von businessrun_plot.R)*

Bei Anmerkungen, Feedback oder Fragen - schreib mir einfach eine E-Mail: [Kontakt](mailto:dataschmience@gmail.com)
