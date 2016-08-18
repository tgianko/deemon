## As I am a lazy person and vboxmanager already
## provides all the tools necessary to do
## * spawn VM
## * freeze VM    (Let it GOOOOOOO!)
## * save VM      
## * terminate VM (I'll be back!)
## * snapshotmanagement
## I'll just give a short summary here


## spawn VM
# It is assumed tha the vm already exists and is
# registered with vboxmanager
vboxmanager startvm <vm-id>


### freeze & resume VM

## FREEZE
# It is assumed that the vm is running
vboxmanager controlvm <vm-id> pause

## RESUME
# It is assumed that the vm has been frozen
vboxmanager controlvm <vm-id> resume


### save VM & start back up

## SAVE
# It is assumed that the vm is running
vboxmanager controlvm <vm-id> savestate

## START BACK UP
# It is assumed that the vm has been saved
# else this will just start up the machine
vboxmanager startvm <vm-id>


### terminate VM

## TERMINATE
# it is assumed the vm is running
vboxmanager controlvm <vm-id> poweroff


### snapshotmanagement

## MAKE SNAPSHOT
# can be running
vboxmanage snapshot <vm-id> take <snapshot-name>

## LIST SNAPSHOT(S)
# can be running
vboxmanage snapshot <vm-id> list

## REVERT TO SNAPSHOT
# must not be running
vboxmanage snapshot <vm-id> restore <snapshot-name>

## DELETE SNAPSHOT
vboxmanage snapshot <vm-id> delete <snapshot-name>