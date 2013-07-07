#!/bin/bash
## Shortcut to search, add, and play a song (after clearing current playlist).
## Use -a (for "add") to add the song to the playlist without clearing current playlist.
 
if [ $1 = -a ]
then
shift
mpc search title "$*" | mpc add 
mpc search artist "$*" | mpc add 
mpc search album "$*" | mpc add ; mpc play
exit
fi
 
mpc clear
mpc search title "$*" | mpc add 
mpc search artist "$*" | mpc add
mpc search album "$*" | mpc add ; mpc play


/home/dries/bin/deleteDuplicatesFromMPCPlaylist
