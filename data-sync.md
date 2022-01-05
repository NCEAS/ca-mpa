## Data collection sync system

We decided that it will be easier for participants to upload their data to Google Drive and that we will be using [rclone](https://rclone.org/drive/) to synchronize this specific folder to Aurora.

Path on the WG Google Drive:
`.. > Data > Raw-Data`


The folder on Aurora it is syncing to is:  

`/home/shares/ca-mpa/GD_data/Raw-Data`

### Update Aurora's content

The command to update the folder on Aurora from Google Drive: 
 **Should be run inside the `Raw-Data` folder on Aurora**

`rclone copy mpa-gd-data: .`

