# Summer GSR Scenarios

- Helper app to calculate plausible summer GSR appointments to maximize payout to student for a given pool of money.

## Download
- Unix
   - ```sh
         wget https://raw.githubusercontent.com/bschulth/for_clara/main/GSR/summer_gsr.R
     ```
    
- Windows
   - Navigate to https://github.com/bschulth/for_clara/blob/main/GSR/summer_gsr.R
   - Click the `download` icon
      - ![](./img/git_download.png)

## Run From Command Line
- Unix
   - ```sh
     # chmod 700 summer_gsr.R
     ./summer_gsr.R
     
     # or maybe
     Rscript --vanilla summer_gsr.R
     ```
     
- Windows
   - ```sh
         # If R is on path:
         Rscript summer_gsr.R
         
         # If R is not on path, then maybe something like:
         %LOCALAPPDATA%\Programs\R\R-4.3.0\bin\x64\Rscript --vanilla summer_gsr.R
     ```

## Example View

![summer gsr image](img/summer_gsr.png)
