FROM rocker/r-ver:4.4.2 AS base

RUN R -e "install.packages('renv', version='1.1.1')"
WORKDIR /app

# --- Development target (used by devcontainer) ---
FROM base AS development
# git is needed to install vscDebugger
RUN apt-get update && apt-get install -y git && rm -rf /var/lib/apt/lists/*

# --- Production target ---
FROM base AS production

# Copy renv infrastructure and restore dependencies
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile
RUN Rscript -e 'renv::restore(prompt = FALSE)'

# Copy application code
COPY src/ src/

EXPOSE 3838

CMD ["Rscript", "-e", "message('\\n App running at http://localhost:3838\\n'); shiny::runApp('src', host = '0.0.0.0', port = 3838)"]
