# CryptoPulse

<img src="img/logo.png" width="160"/>

## Welcome!

Welcome! 🎉 Willkommen! 🎊 Bienvenue! 🎈

Thank you for visiting the `CryptoPulse` project repository. This README covers what the dashboard does, how to run it, and how to deploy it.

-   [Who Are We?](#who-are-we)
    -   [Project Summary](#project-summary)
    -   [Motivation and Purpose](#motivation-and-purpose)
-   [Features of CryptoPulse](#features-of-cryptopulse)
-   [How Does It Work?](#how-does-it-work)
-   [Get Started](#get-started)
-   [Deploy](#deploy)
-   [Contribute](#contribute)
-   [Data Sources and Licensing](#data-sources-and-licensing)

## Who Are We?

We are a team of data scientists and developers passionate about finance and technology, particularly in the cryptocurrency space. Our expertise in data visualization and interactive platforms drives the development of `CryptoPulse`.

### Project Summary

`CryptoPulse` is a live Shiny dashboard for BTC, ETH, SOL, and BNB. It pulls spot and USDT-M futures data from public Binance APIs, overlays a 200-day moving average, and estimates liquidation clusters for traders, analysts, and crypto enthusiasts.

### Motivation and Purpose

**Target audience:** Traders, financial analysts, cryptocurrency enthusiasts, and anyone who wants a faster read on market structure.

Crypto markets move quickly, and a single price chart is rarely enough. `CryptoPulse` puts price, volume, open interest, long/short mix, and estimated liquidation levels in one place so users can judge trend, positioning, and nearby risk without jumping between terminals.

## Features of CryptoPulse

<img src="img/cryptopulse.png" width="800"/>

-   **Four assets:** BTC, ETH, SOL, and BNB, with search in the header
-   **Overview, Volume, Derivatives, and Liquidations** tabs on the same live snapshot
-   **Live Binance data** for price, 24h change, spot volume, futures volume, and open interest
-   **MA200 overlay** on candlestick or close charts, plus bull/bear regime and deviation
-   **Timeframes:** 24H, 7D, 30D, 90D, 1Y, ALL, or a custom date range
-   **Auto-refresh** with a 15/30/60 second interval, countdown, and a manual Refresh button
-   **Estimated liquidation heatmap** and key leverage lines (10x–100x)
-   **Local Kaggle history** as the BTC/ETH fallback when the network is unavailable

## How Does It Work?

The app is an R Shiny page with an Apple-style toolbar. Choose an asset and timeframe, then move between tabs.

<img src="img/demo.gif" width="800"/>

-   **Overview:** Price, 24h change, MA200, spot volume, futures volume, and open interest, plus the main price chart
-   **Volume:** Spot versus futures notional volume and the spot volume trend
-   **Derivatives:** Open interest, long/short account mix, and a compact futures snapshot
-   **Liquidations:** Model-estimated long/short clusters around the current mark, not exchange force-order prints

Local BTC/ETH CSVs backfill history. SOL and BNB use live Binance daily klines (enough history for MA200). Refreshing the Updates row clears the in-memory cache and pulls a new snapshot.

## Get Started

### Option 1: Conda + RStudio or R

1.  **Clone the repository:**

    ```bash
    git clone git@github.com:iris0614/CryptoPulse.git
    cd CryptoPulse
    ```

2.  **Create the environment:**

    ```bash
    conda env create -f environment.yml
    conda activate CryptoPulse
    ```

3.  **Run the app:**

    In RStudio, open `src/app.R` and click **Run App**. Or from a terminal:

    ```bash
    cd src
    Rscript -e "shiny::runApp('.', host='127.0.0.1', port=6218)"
    ```

    Then open <http://127.0.0.1:6218/>. Live metrics need internet access to Binance.

### Option 2: Docker

```bash
docker compose up --build
```

Then open <http://localhost:3838/>.

## Deploy

CryptoPulse is an **R Shiny** app and needs a persistent server. Vercel and Netlify cannot host it.

**Render**

1.  Open this repository in [Render](https://render.com) as a Web Service.
2.  Runtime is Docker (`render.yaml` is already in the repo).
3.  Share the `*.onrender.com` URL after the first deploy.

**shinyapps.io**

```r
rsconnect::deployApp("src")
```

`vercel.json` is included only to fail fast if someone tries to deploy this Shiny app to Vercel.

## Contribute

We welcome contributions from the community! Whether it's enhancing the dashboard, adding new features, or fixing bugs, your input is highly appreciated. Please review our [Contribution Guidelines](CONTRIBUTING.md) for more information.

## Data Sources and Licensing

### Data Sources

Live market metrics are fetched from public exchange APIs and refreshed in the dashboard:

-   **Binance Spot** (`api.binance.com`) for BTCUSDT / ETHUSDT / SOLUSDT / BNBUSDT prices, 24h change, and spot volume
-   **Binance USDT-M Futures** (`fapi.binance.com`) for derivatives volume, open interest, and long/short account ratios
-   **CoinGecko** for an optional global 24h volume cross-check
-   **Local Kaggle history** (`Bitcoin & Ethereum prices (2014-2024)`) as the historical backbone and offline fallback. The dataset is [here](https://www.kaggle.com/datasets/kapturovalexander/bitcoin-and-ethereum-prices-from-start-to-2023?select=BTC-USD+%282014-2024%29.csv)

Liquidation bands are **model estimates** (open interest × common leverage buckets × recent volume-weighted entries), not exchange-reported force-order heatmaps.

### Licensing

`CryptoPulse` is released under the MIT License. See the [LICENSE](LICENSE.md) file for details.
