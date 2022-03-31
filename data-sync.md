## Data collection sync system

We decided that it will be easier for participants to upload their data to Google Drive and that we will be using [rclone](https://rclone.org/drive/) to synchronize this specific folder to Aurora.

Path on the WG Google Drive:
`.. > Data > Raw-Data`


The folder on Aurora it is syncing to is:  

`/home/shares/ca-mpa/data/sync-data`

### Update Aurora's content

The command to update the folder on Aurora from Google Drive: 
 **Should be run inside the `sync-data` folder on Aurora**

`rclone --config="rclone_mpa.conf" copy mpa-gd-data: .`

