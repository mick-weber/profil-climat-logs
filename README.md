## Visualise shinylogs from production apps

This small auxiliary &#128296; shiny app shows some very basic visualisations &#128202; of logs generated by `{shinylogs}` in a shiny app. It is used to display connections over time &#128338; to the [`eneRgyVD` production app](https://github.com/mick-weber/eneRgyVD).

&#9888; The only requirement is :

-   `{shinylogs}` **json** log files must be set in the `/logs_files/`

The app will clear these files as they are stored in `logs_df.rds` (it will be created on the fly, if needed).

The app is meant to be hosted publicly alongside production apps : a cron job would typically access the logs generated in the production app to dump them in `/logs_files/` directory.
