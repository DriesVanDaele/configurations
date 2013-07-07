current_brightness=$(cat /sys/class/backlight/psb-bl/brightness)

if [ $current_brightness -gt 1 ]
then
    new_brightness=$(expr $current_brightness - 1)
    echo $new_brightness > /sys/class/backlight/psb-bl/brightness
fi




